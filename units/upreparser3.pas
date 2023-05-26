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
  Classes, SysUtils, Generics.Collections, uasmglobals;

type
  TParserState = (psNone,
                  psLabel,
                  psDirective,
                  psCommand,
                  psInstruction,
                  psMacro,
                  psOperand,
                  psComment,
                  psUnknown,
                  psGlob,
                  psComma,
                  psWhitespace,
                  psDQStr,
                  psDQEsc,
                  psSQStr,
                  psSQEsc,
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
      FCommands:      TStringList;
      FErrorMsg:      string;
      FEscape:        char;
      FEscaped:       TSetOfChar;
      FForceColon:    boolean;
      FInstructions:  TStringList;
      FMacros:        TStringList;
      procedure AdjustComments;
      procedure AssignCommands;
      procedure AssignDirective;
      procedure AssignIndirect;
      procedure AssignInstructions;
      procedure AssignLabel;
      procedure AssignMacros;
      procedure AssignOperands;
      procedure Crunch;
      procedure CrunchCommaSpace;
      procedure CrunchOperands;
      procedure CrunchOperandsAugment;
      procedure CrunchWhitespace;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Init;
      function  Parse(_input: string): boolean;
      property Commands:      TStringList read FCommands;
      property ErrorMsg:      string      read FErrorMsg;
      property Escape:        char        read FEscape         write FEscape;
      property Escaped:       TSetOfChar  read FEscaped        write FEscaped;
      property ForceColon:    boolean     read FForceColon     write FForceColon;
      property Instructions:  TStringList read FInstructions;
      property Macros:        TStringList read FMacros;
  end;


implementation

uses
  uutility, typinfo, umessages;


constructor TPreparser.Create;
begin
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Entering function TPreparser.Create']);
{$ENDIF}
  inherited Create;
  FEscape  := DEFAULT_ESCAPE;
  FEscaped := DEFAULT_ESCAPED;
  FForceColon := False;
  FCommands := TStringList.Create;
  FCommands.Sorted := True;
  FInstructions := TStringList.Create;
  FInstructions.Sorted := True;
  FMacros := TStringList.Create;
  FMacros.Sorted := True;
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Leaving function TPreparser.Create']);
{$ENDIF}
end;

destructor TPreparser.Destroy;
begin
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Entering function TPreparser.Destroy']);
{$ENDIF}
  FreeAndNil(FCommands);
  FreeAndNil(FInstructions);
  FreeAndNil(FMacros);
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

procedure TPreparser.AssignCommands;
var i,j: integer;
    rec: TParserProp;
    cmd: string;
begin
  i := 0;
  while (i < Count) and (Items[i].State <> psDirective) do
    Inc(i);
  if (i < Count) then
    begin
      rec := Items[i];
      cmd := UpperCase(rec.Payload);
      if FCommands.Find(cmd,j) then
        begin
          rec.State := psCommand;
          rec.Index := j;
          Items[i] := rec;
        end;
    end;
end;

procedure TPreparser.AssignDirective;
var i: integer;
    rec: TParserProp;
begin
  // Turn the first available glob into a directive
  i := 0;
  while (i < Count) and (Items[i].State <> psGlob) do
    Inc(i);
  if (i < Count) then
    begin
      rec := Items[i];
      rec.State := psDirective;
      Items[i] := rec;
    end;
end;

procedure TPreparser.AssignIndirect;
var i: integer;
    rec: TParserProp;
begin
  for i := 0 to Count-1 do
    begin
      rec := Items[i];
      if (rec.State = psOperand) and
         (Length(rec.Payload) >= 3) and
         (LeftStr(rec.Payload,1) = '(') and
         (RightStr(rec.Payload,1) = ')') and
         (Indirected(rec.Payload,FEscape,FEscaped)) then
        begin
          rec.Payload[1] := '[';
          rec.Payload[Length(rec.Payload)] := ']';
          Items[i] := rec;
        end;
    end;
end;

procedure TPreparser.AssignInstructions;
var i,j: integer;
    rec: TParserProp;
    mnemonic: string;
begin
  i := 0;
  while (i < Count) and (Items[i].State <> psDirective) do
    Inc(i);
  if (i < Count) then
    begin
      rec := Items[i];
      mnemonic := UpperCase(rec.Payload);
      if FInstructions.Find(mnemonic,j) then
        begin
          rec.State := psInstruction;
          rec.Index := j;
          Items[i] := rec;
        end;
    end;
end;

procedure TPreparser.AssignLabel;
var rec: TParserProp;
begin
  if Count = 0 then
    Exit;
  // Label must be in first place
  // Even if force colon, some labels will not have a colon on them
  // e.g.  TABLE EQU 17
  if (Items[0].State = psGlob) then
    begin
      rec := Items[0];
      rec.State := psLabel;
      Items[0] := rec;
    end;
end;

procedure TPreparser.AssignMacros;
var i,j: integer;
    rec: TParserProp;
    cmd: string;
begin
  i := 0;
  while (i < Count) and (Items[i].State <> psDirective) do
    Inc(i);
  if (i < Count) then
    begin
      rec := Items[i];
      cmd := UpperCase(rec.Payload);
      if FMacros.Find(cmd,j) then
        begin
          rec.State := psMacro;
          rec.Index := -1;
          Items[i] := rec;
        end;
    end;
end;

procedure TPreparser.AssignOperands;
var i: integer;
    rec: TParserProp;
begin
  i := 0;
  while (i < Count) and (not (Items[i].State in [psGlob,psDQStr,psSQStr])) do
    Inc(i);
  // Process the first operand
  if (i < Count) then
    begin
      rec := Items[i];
      rec.State := psOperand;
      Items[i] := rec;
      // Scan for more
      Inc(i);
      while (i < Count-1) do
        begin
          if  (Items[i].State = psComma) and (Items[i+1].State in [psGlob,psDQStr,psSQStr]) then
            begin
              Delete(i); // Get rid of comma
              rec := Items[i];
              rec.State := psOperand;
              Items[i] := rec;
            end;
          Inc(i);
        end;
    end;
end;

procedure TPreparser.Crunch;
begin
  AdjustComments;
  CrunchOperands;
  AssignLabel;
  AssignDirective;
  CrunchCommaSpace;
  AssignOperands;
  CrunchOperandsAugment;
  CrunchWhitespace;
  AssignIndirect;
  AssignCommands;
  AssignInstructions;
  AssignMacros;
end;

procedure TPreparser.CrunchCommaSpace;
var i: integer;
begin
  // Crunch  whitespace comma -> comma
  i := 0;
  while i < Count-1 do
    begin
      while (i < Count-1) and (Items[i].State = psWhitespace) and (Items[i+1].State = psComma) do
        Delete(i);
      Inc(i);
    end;
  // Crunch  comma whitespace -> comma
  i := 0;
  while i < Count-1 do
    begin
      while (i < Count-1) and (Items[i].State = psComma) and (Items[i+1].State = psWhitespace) do
        Delete(i+1);
      Inc(i);
    end;
end;

procedure TPreparser.CrunchOperands;
var i: integer;
    rec: TParserProp;
begin
  // Join the bracketed levels
  i := 0;
  while i < Count do
    begin
      while (Items[i].Level > 0) and (i < (Count-1)) do
        begin
          rec := Items[i];
          rec.Payload := rec.Payload + Items[i+1].Payload;
          rec.Level   := Items[i+1].Level;
          Items[i] := rec;
          Delete(i+1);
        end;
      Inc(i);
    end;
end;

procedure TPreparser.CrunchOperandsAugment;
var i: integer;
    rec: TParserProp;
begin
  // Crunch <operand> <whitespace> <glob>
  i := 0;
  while i < (Count-2) do
    begin
      while (i < (Count-2)) and (Items[i].State = psOperand) and (Items[i+1].State = psWhitespace) and (Items[i+2].State = psGlob) do
        begin
          rec := Items[i];
          rec.Payload := rec.Payload + Items[i+1].Payload + Items[i+2].Payload;
          Items[i] := rec;
          Delete(i+2);
          Delete(i+1);
        end;
      Inc(i);
    end;
end;

procedure TPreparser.CrunchWhitespace;
var i: integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].State in [psWhitespace,psComment] then
      Delete(i);
end;

procedure TPreparser.Init;
begin
  FErrorMsg      := '';
  Clear;
end;

function TPreparser.Parse(_input: string): boolean;
var i: integer;
    starts_with_spc: boolean;
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
  starts_with_spc := (_input[1] = ' ');
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
            SQ:  NewState(psSQStr);
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
            SQ:  NewState(psSQStr);
            DQ:  NewState(psDQStr);
            ';': NewState(psComment);
            '/': if prev_ch = '/' then
                   NewState(psComment);
          end;
        psComma:
          case ch of
            ' ': NewState(psWhitespace);
            ',': NewState(psComma);
            SQ:  NewState(psSQStr);
            DQ:  NewState(psDQStr);
            ';': NewState(psComment);
            otherwise
              NewState(psGlob);
          end;
        psWhitespace:
          case ch of
            ' ': NewState(psWhitespace);
            ',': NewState(psComma);
            SQ:  NewState(psSQStr);
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
        psSQStr:
          case ch of
            SQ:     NewState(psUnknown);
            otherwise
              if ch = FEscape then
                state := psSQEsc;
          end;
        psSQEsc:
          if ch in ESCAPED then
            state := psSQStr
          else
            ErrorObj.Show(ltError,E2001_ILLEGAL_ESCAPE_CHARACTER,[ch,CharSetToStr(ESCAPED)]);
      end;
      if (ch = '(') and (state = psGlob) then
        Inc(level);
      if (ch = ')') and (state = psGlob) then
        Dec(level);
      payload := payload + ch;
      prev_ch := ch;
    end;
  if state in [psSQStr,psSQEsc,psDQStr,psDQEsc] then
    ErrorObj.Show(ltError,E2002_UNTERMINATED_STRING,[payload]);
  NewState(psDone);
  Crunch;
  // Check for unresolved globs etc.
  for iter in Self do
    if not (iter.State in [psLabel,psCommand,psInstruction,psMacro,psOperand]) then
      begin
        ErrorObj.ColNumber := iter.Column;
        ErrorObj.Show(ltError,E2003_UNRECOGNISED_CONTENT,[iter.Payload]);
      end;
  Parse := True;
end;

end.

