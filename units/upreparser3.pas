unit upreparser3;

//
// Takes a line of assembler text and pre-parses it into:
//
//   * Optional label --> LabelText
//   * Optional directive, and if present... --> Directive
//   * Optional operands --> OperandCount and Operands list
//   * Comment (discarded)
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, uasmglobals, udfa_preparser;

type
  TParserState = (psNone,
                  ppLabel,
                  ppCommand,
                  ppInstruction,
                  ppMacro,
                  ppOperand,
                  psComment,
                  psUnknown,
                  psGlob,
                  psComma,
                  psWhitespace,
                  psDQStr,
                  psDQEsc,
                  psSQChr,
                  psDone);

  TParserProp = record
    State:    TParserState;  // Parser object type
    Payload:  string;        // Payload, the string that caused this
    Column:   integer;       // Column number in the line 1..n
    Index:    integer;       // Index of the opcode or directive
    Level:    integer;       // Bracket level at end of object, e.g LOW(( -> 2
  end;

  TPreparser = class(specialize TList<TParserProp>)
    private
      FDFA:           TPreparserDFA;
      FErrorMsg:      string;
      FEscape:        char;
      FEscaped:       TSetOfChar;
      FForceColon:    boolean;
      procedure AdjustComments;
      procedure AllocateKeywords;
      procedure AllocateLabels;
      procedure AllocateOperands;
      procedure AllocateMacros;
      procedure CombineBrackets;
      procedure CombineGlobs;
      procedure RemoveComments;
      procedure RemoveWhitespace;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Init;
      function  Parse(_input: string): boolean;
      property DFA:           TPreparserDFA read FDFA            write FDFA;
      property ErrorMsg:      string        read FErrorMsg;
      property Escape:        char          read FEscape         write FEscape;
      property Escaped:       TSetOfChar    read FEscaped        write FEscaped;
      property ForceColon:    boolean       read FForceColon     write FForceColon;
  end;


implementation

uses
  uutility, typinfo, umessages, udmnfa;


constructor TPreparser.Create;
begin
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Entering function TPreparser.Create']);
{$ENDIF}
  inherited Create;
  FEscape  := DEFAULT_ESCAPE;
  FEscaped := DEFAULT_ESCAPED;
  FForceColon := False;
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Leaving function TPreparser.Create']);
{$ENDIF}
end;

destructor TPreparser.Destroy;
begin
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Entering function TPreparser.Destroy']);
{$ENDIF}
  inherited Destroy;
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Leaving function TPreparser.Destroy']);
{$ENDIF}
end;

// Fix problems with // comments, part will be left on the payload of the
// previous item

procedure TPreparser.AdjustComments;
var rec: TParserProp;
begin
  if (Count > 1) and
     (Items[Count-1].State = psComment) and
     (Items[Count-1].Payload <> '') and
     (Items[Count-1].Payload[1] = '/') then
    begin
      rec := Items[Count-1];
      rec.Payload := '/' + Items[Count-1].Payload;
      Items[Count-1] := rec;
      rec := Items[Count-2];
      rec.Payload := LeftStr(Items[Count-2].Payload,Length(Items[Count-2].Payload)-1);
      Items[Count-2] := rec;
      if Items[Count-2].Payload = '' then
        Delete(Count-2);
    end;
end;

procedure TPreparser.AllocateKeywords;
var i,j: integer;
    rec: TParserProp;
    cmd: string;
    token: TNFAToken;
begin
  for i := 0 to Count-1 do
    if Items[i].State = psGlob then
      begin
        rec := Items[i];
        cmd := rec.Payload;
        token := FDFA.Tokenise(cmd);
        if token >= FDFA.OffsetOpcode then
          begin
            if token >= FDFA.OffsetCommand then
              rec.State := ppCommand
            else
              rec.State := ppInstruction;
            Items[i] := rec;
          end;
      end;
end;

procedure TPreparser.AllocateOperands;
var i: integer;
    rec: TParserProp;
    valid_op: set of TParserState = [psGlob,psSQChr,psDQStr];
begin
  // Find first glob / char / string
  i := 0;
  while (i < Count) and not (Items[i].State in valid_op) do
    Inc(i);
  if (i < Count) then
    begin
      rec := Items[i];
      rec.State := ppOperand;
      Items[i] := rec;
      Inc(i);
      while (i < Count-1) and
            (Items[i].State = psComma) and
            (Items[i+1].State in valid_op) do
        begin
          Delete(i);
          rec := Items[i];
          rec.State := ppOperand;
          Items[i] := rec;
          Inc(i);
        end;
    end;
end;

procedure TPreparser.AllocateLabels;
var rec: TParserProp;
begin
  if (Count > 0) and (Items[0].State = psGlob) then
    begin
      rec := Items[0];
      rec.State := ppLabel;
      Items[0] := rec;
    end;
end;

procedure TPreparser.AllocateMacros;
var i: integer;
    rec: TParserProp;
begin
  // Skip any initial label or whitespace
  i := 0;
  while (i < Count) and (Items[i].State in [ppLabel,psWhitespace]) do
    Inc(i);
  // If it's a glob make it a macro
  if (i < Count) and (Items[i].State = psGlob) then
    begin
      rec := Items[i];
      rec.State := ppMacro;
      Items[i] := rec;
    end;
end;

procedure TPreparser.CombineBrackets;
var i:         integer;
    rec:       TParserProp;
    nextlevel: integer;
begin
  i := 0;
  while (i < Count-1) do
    begin
      while (i < Count-1) and (Items[i].Level = 0)  do
        Inc(i);
      if (i < Count-1) then
        begin // We are at an opening item with level > 0
          rec := Items[i];
          rec.State := psGlob;
          repeat
            rec.Payload := rec.Payload + Items[i+1].Payload;
            nextlevel := Items[i+1].Level;
            rec.Level := nextlevel;
            Delete(i+1);
          until (nextlevel = 0) or (i >= Count-1);
          Items[i] := rec;
        end;
      Inc(i);
    end;
end;

procedure TPreparser.CombineGlobs;
var i:         integer;
    rec:       TParserProp;
begin
  // Glob Glob -> Glob
  i := 0;
  while i < Count-1 do
    begin
      while (i < Count-1) and
            (Items[i].State in [psGlob,psDQStr,psSQChr]) and
            (Items[i+1].State in [psGlob,psDQStr,psSQChr]) do
        begin
          rec := Items[i];
          rec.State := psGlob;
          rec.Payload := Items[i].Payload + Items[i+1].Payload;
          Items[i] := rec;
          Delete(i+1);
        end;
      Inc(i);
    end;
  // Glob Whitespace Glob -> Glob
  i := 0;
  while i < Count-2 do
    begin
      while (i < Count-2) and
            (Items[i].State in [psGlob,psDQStr,psSQChr]) and
            (Items[i+1].State = psWhitespace) and
            (Items[i+2].State in [psGlob,psDQStr,psSQChr]) do
        begin
          rec := Items[i];
          rec.State := psGlob;
          rec.Payload := Items[i].Payload + Items[i+1].Payload + Items[i+2].Payload;
          Items[i] := rec;
          Delete(i+2);
          Delete(i+1);
        end;
      Inc(i);
    end;
end;

procedure TPreparser.Init;
begin
  FErrorMsg      := '';
  Clear;
end;

function TPreparser.Parse(_input: string): boolean;
var i: integer;
    payload: string;
    column:  integer;
    state:   TParserState;
    ch:      char;
    prev_ch: char;
    inp_len: integer;
    level:   integer;
    iter:    TParserProp;

  procedure NewState(_newstate: TParserState);
  var prop: TParserProp;
  begin
    if _newstate = psUnknown then
      payload := payload + ch;
    if _newstate <> state then // Only deal with changed state
      begin
        if not (state in [psNone,psUnknown]) then
          begin
            prop.State    := state;
            prop.Column   := column;
            prop.Payload  := payload;
            prop.Level    := level;
            prop.Index    := -1;
            Add(prop);
          end;
        state := _newstate;
        column := i;
        payload := '';
      end;
  end;

begin
  ErrorObj.ColNumber := 0;
  Parse := True;
  Init;
  inp_len := Length(_input);
  level := 0;
  prev_ch := #0;
  if inp_len = 0 then
    Exit;
  state := psNone;
  if _input[1] = '*' then
    state := psComment;
  // Split into whitespace, commas, "globs", and comments
  // where globs are things like ASC(LEFT(sVal,1))
  for i := 1 to inp_len do
    begin
      ErrorObj.ColNumber := i;
      ch := _input[i];
      case state of
        psNone:
          case ch of
            ' ': NewState(psWhitespace);
            '*',
            ';': NewState(psComment);
            otherwise
              NewState(psGlob);
          end;
        psUnknown:
          case ch of
            ' ': NewState(psWhitespace);
            ',': NewState(psComma);
            SQ:  NewState(psSQChr);
            DQ:  NewState(psDQStr);
            ';': NewState(psComment);
            otherwise
              NewState(psGlob);
          end;
        psComment:
          ; // Do nothing, once in a comment we are staying there
        psGlob:
          case ch of
            ' ': NewState(psWhitespace);
            ',': NewState(psComma);
            SQ:  if UpperCase(payload) <> 'AF' then
                   NewState(psSQChr);
            DQ:  NewState(psDQStr);
            ';': NewState(psComment);
            '/': if prev_ch = '/' then
                   NewState(psComment);
          end;
        psComma:
          case ch of
            ' ': NewState(psWhitespace);
            ',': NewState(psComma);
            SQ:  NewState(psSQChr);
            DQ:  NewState(psDQStr);
            ';': NewState(psComment);
            otherwise
              NewState(psGlob);
          end;
        psWhitespace:
          case ch of
            ' ': NewState(psWhitespace);
            ',': NewState(psComma);
            SQ:  NewState(psSQChr);
            DQ:  NewState(psDQStr);
            ';': NewState(psComment);
            otherwise
              NewState(psGlob);
          end;
        psDQStr:
          case ch of
            DQ:     NewState(psUnknown);
            otherwise
              if ch = FEscape then
                state := psDQEsc;
          end;
        psDQEsc:
          if ch in ESCAPED then
            state := psDQStr
          else
            ErrorObj.Show(ltError,E2001_ILLEGAL_ESCAPE_CHARACTER,[ch,CharSetToStr(ESCAPED)]);
        psSQChr:
          case ch of
            SQ:     NewState(psUnknown);
          end;
      end;
      if (ch = '(') and (state = psGlob) then
        Inc(level);
      if (ch = ')') and (state = psGlob) then
        Dec(level);
      payload := payload + ch;
      prev_ch := ch;
    end;
  if state in [psSQChr,psDQStr,psDQEsc] then
    ErrorObj.Show(ltError,E2002_UNTERMINATED_STRING,[payload]);
  NewState(psDone);
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[_input]);
  for iter in Self do
    begin
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[
            Format('    %d: %s [%s] Idx=%d, Lev=%d  ',
                  [iter.Column,
                   GetEnumName(TypeInfo(TParserState),Ord(iter.State)),
                   iter.Payload,
                   iter.Index,
                   iter.Level]
                   )]);
    end;
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['    ' + StringOfChar('-',80-4)]);
{$ENDIF}
  AdjustComments;
  RemoveComments;
  AllocateKeywords;
  CombineBrackets;
  CombineGlobs;
  AllocateLabels;
  AllocateMacros;
  RemoveWhitespace;
  AllocateOperands;
{$IFDEF DEBUG_LOG}
  for iter in Self do
    begin
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[
            Format('    %d: %s [%s] Idx=%d, Lev=%d  ',
                  [iter.Column,
                   GetEnumName(TypeInfo(TParserState),Ord(iter.State)),
                   iter.Payload,
                   iter.Index,
                   iter.Level]
                   )]);
    end;
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[StringOfChar('=',80)]);
{$ENDIF}
  // Check for unresolved globs etc.
  for iter in Self do
    begin
      if not (iter.State in [ppLabel,ppCommand,ppInstruction,ppMacro,ppOperand]) then
        begin
          ErrorObj.ColNumber := iter.Column;
          ErrorObj.Show(ltError,E2003_UNRECOGNISED_CONTENT,[iter.Payload]);
        end;
      // @@@@@ Other checks here
    end;
  Parse := True;
end;

procedure TPreparser.RemoveComments;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].State <> psComment) do
    Inc(i);
  // All comments and everything after must go
  while (i < Count) do
    Delete(i);
end;

procedure TPreparser.RemoveWhitespace;
var i: integer;
begin
  i := 0;
  while (i < Count) do
    begin
      while (i < Count) and (Items[i].State <> psWhitespace) do
        Inc(i);
      while (i < Count) and (Items[i].State = psWhitespace) do
        Delete(i);
      Inc(i);
    end;
end;

end.

