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
  Classes, SysUtils, uasmglobals, udfa_preparser,
  lacogen_types, ucommand, uinstruction, upreparser3_defs, umacro;

type
  TPreparser = class(TPreparserBase)
    private
      FCaseSensitive:     boolean;
      FCmdFlags:          TCommandFlags;
      FCommandIndex:      integer;
      FCommandList:       TCommandList;
      FDefiningMacro:     boolean;
      FDFA:               TPreparserDFA;
      FErrorMsg:          string;
      FEscape:            char;
      FEscaped:           TSetOfChar;
      FForceColon:        boolean;
      FLabelX:            string;
      FMacroIndex:        integer;
      FMacroList:         TMacroList;
      FMacroReference:    boolean;
      FOpcodeCol:         integer;
      FOpcodeIndex:       integer;
      FOpcodeList:        TInstructionList;
      FPass:              integer;
      procedure AdjustComments;
      procedure Allocate;
      procedure AllocateKeywords;
      procedure AllocateLabels;
//    procedure XXAllocateLabels;
//    procedure AllocateOperands;
      procedure AllocateMacros;
      procedure Categorise(const _input: string);
      procedure CombineBrackets;
      procedure CombineGlobs;
//    procedure ExtractCommand;
//    procedure ExtractLabel;
//    procedure ExtractMacro;
//    procedure ExtractOpcode;
      procedure GlobsToOperands;
      procedure PopulateDFA;
//    procedure RemoveAfterEnd;
      procedure RemoveCommas;
      procedure RemoveComments;
      procedure RemoveLeadingWhitespace;
      procedure RemoveWhitespace;
      procedure SetDefiningMacro(_v: boolean);
      procedure SetIndices;
      procedure SetMacroList(_lst: TMacroList);
      procedure ShowPPinfo(const _s: string = '');
    public
      constructor Create(_cmd_list: TCommandList; _opcode_list: TInstructionList);
      destructor Destroy; override;
      procedure Init;
      procedure Parse(const _input: string);
      property CaseSensitive:    boolean          read FCaseSensitive      write FCaseSensitive;
      property CmdFlags:         TCommandFlags    read FCmdFlags;
      property CommandIndex:     integer          read FCommandIndex;
      property CommandList:      TCommandList     read FCommandList    write FCommandList;
      property DefiningMacro:    boolean          read FDefiningMacro  write SetDefiningMacro;
      property DFA:              TPreparserDFA    read FDFA            write FDFA;
      property ErrorMsg:         string           read FErrorMsg;
      property Escape:           char             read FEscape         write FEscape;
      property Escaped:          TSetOfChar       read FEscaped        write FEscaped;
      property ForceColon:       boolean          read FForceColon     write FForceColon;
      property LabelX:           string           read FLabelX;
      property MacroIndex:       integer          read FMacroIndex;
      property MacroList:        TMacroList       read FMacroList      write SetMacroList;
      property MacroReference:   boolean          read FMacroReference;
      property OpcodeCol:        integer          read FOpcodeCol;
      property OpcodeIndex:      integer          read FOpcodeIndex;
      property OpcodeList:       TInstructionList read FOpcodeList     write FOpcodeList;
      property Pass:             integer          read FPass           write FPass;
  end;


implementation

uses
  uutility, typinfo, umessages, udmnfa;


constructor TPreparser.Create(_cmd_list: TCommandList; _opcode_list: TInstructionList);
begin
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Entering function TPreparser.Create']);
{$ENDIF}
  inherited Create;
  FCommandList := _cmd_list;
  FOpcodeList  := _opcode_list;
  FOpcodeCol   := 0;
  FEscape  := DEFAULT_ESCAPE;
  FEscaped := DEFAULT_ESCAPED;
  FForceColon := True;
  FDFA := TPreparserDFA.Create;
  PopulateDFA;
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Leaving function TPreparser.Create']);
{$ENDIF}
end;

destructor TPreparser.Destroy;
begin
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Entering function TPreparser.Destroy']);
{$ENDIF}
  FreeAndNil(FDFA);
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
type TTokenMode = (tmOpcode,tmCommand,tmOperand,tmError);
var i: integer;
    rec: TParserProp;
    cmd: string;
    token: TNFAToken;
    tmode: TTokenMode;
    directive: string;
begin
  directive := '';
  for i := 0 to Count-1 do
    if (Items[i].State = psGlob) and (Items[i].Column <> 1) then
      begin
        rec := Items[i];
        cmd := rec.Payload;
        token := FDFA.Tokenise(cmd);
        tmode := tmError;
        if (token >= FDFA.OffsetOpcode)  and (token < FDFA.OffsetCommand) then tmode := tmOpcode;
        if (token >= FDFA.OffsetCommand) and (token < FDFA.OffsetOperand) then tmode := tmCommand;
        if (token >= FDFA.OffsetOperand)                                  then tmode := tmOperand;
        if (tmode <> tmError) then
          begin
            case tmode of
              tmOpcode:
                begin
                  rec.State := ppInstruction;
                  rec.Index := token - FDFA.OffsetOpcode;
                  FOpcodeCol := Items[i].Column;
                end;
              tmCommand:
                begin
                  if directive <> '' then
                    begin
                      ErrorObj.ColNumber := Items[i].Column;
                      ErrorObj.Show(ltError,E2060_UNEXPECTED_DIRECTIVE,[cmd,directive]);
                    end;
                  rec.State := ppCommand;
                  rec.Index := token - FDFA.OffsetCommand;
                  FCmdFlags := FCommandList[token - FDFA.OffsetCommand].CommandFlags;
                  directive := cmd;
                end;
              tmOperand:
                begin
                  rec.State := ppOperand;
                  rec.Index := token - FDFA.OffsetOperand;
                end
            end; // Case
            rec.Token := token;
            Items[i] := rec;
          end;
      end;
end;

procedure TPreparser.AllocateLabels;
var rec: TParserProp;
    _pos: integer;
begin
  _pos := 0;
  if Count = 0 then
    Exit;
  if (Items[0].State = psGlob) and ((Items[0].Column = 1) or (cfEQU in FCmdFlags)) then
    begin  // This is a label
      if not FDefiningMacro then
        begin
          InvalidLabelCharacters(Items[0].Payload,_pos);
          if (_pos > 0) and (Items[0].Payload[_pos] <> ':') then
            begin
              ErrorObj.ColNumber := _pos;
              ErrorObj.Show(ltError,E2036_INVALID_LABEL_CHARACTER);
            end;
        end;
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
      FMacroReference := True;
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
  // Glob Whitespace Glob -> Glob but skip slot 0 as it will be a label
  i := 1;
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
  FCmdFlags      := [];
  FErrorMsg      := '';
  FLabelX        := '';
  FCommandIndex  := -1;
  FOpcodeIndex   := -1;
  FMacroIndex    := -1;
  Clear;
end;


{
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

{$IFDEF DEBUG_LOGX}
  procedure ShowDebugInfo(const _msg: string);
  begin
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[_msg]);
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Label   = ' + FLabelX]);
    if FCommandIndex >= 0 then
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Command = ' + IntToStr(FCommandIndex) + ' (' + FCommandList[FCommandIndex].CommandName + ')'])
    else
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Command = ' + IntToStr(FCommandIndex)]);
    if FOpcodeIndex >= 0 then
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Opcode  = ' + IntToStr(FOpcodeIndex) + ' (' + FOpcodeList.OpcodeAtIndex(FOpcodeIndex) + ')'])
    else
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Opcode  = ' + IntToStr(FOpcodeIndex)]);
    for iter in Self do
      begin
        if iter.index >= 0 then
          ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[
                Format('    %d: %s [%s] Idx=%d %s, Lev=%d  ',
                      [iter.Column,
                       GetEnumName(TypeInfo(TParserState),Ord(iter.State)),
                       iter.Payload,
                       iter.Index,
                       OperandSanitised[TOperandOption(iter.Index)],
                       iter.Level]
                       )])
        else
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
  end;
{$ENDIF}

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
            prop.Token    := -1;
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
  FMacroReference := False;
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
            ':': NewState(psUnknown);
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
{$IFDEF DEBUG_LOGX}ShowDebugInfo('Post NewState(psDone)');{$ENDIF}
  AdjustComments;
  RemoveComments;
  AllocateKeywords;
  RemoveAfterEnd;
  CombineBrackets;
  AllocateLabels;
  AllocateMacros;
{$IFDEF DEBUG_LOGX}ShowDebugInfo('Post CombineBrackets');{$ENDIF}
  CombineGlobs;
{$IFDEF DEBUG_LOGX}ShowDebugInfo('Post CombineGlobs');{$ENDIF}
  RemoveWhitespace;
  AllocateOperands;
  ExtractLabel;
  ExtractCommand;
  if not FDefiningMacro then
    begin
      ExtractMacro;
      if not FMacroReference then
        ExtractOpcode;
      // Check for reserved word in position 0 - using reserved word as a label
      {
      if (FLabelX <> '') and FDFA.IsReserved(FLabelX) then
        begin
          ErrorObj.ColNumber := 1;
          ErrorObj.Show(ltError,E2030_USING_RESERVED_AS_LABEL,[FLabelX]);
        end;
      }
      // Check for operands without command or instruction
      // @@@@@ CHECK HERE FOR IF THE ITEM IS A MACRO
      // This probably means we are using the wrong processor / instruction set
      if (FCommandIndex < 0) and (FOpcodeIndex < 0) and (FMacroIndex < 0) and (Count > 0) then
        begin
          ErrorObj.ColNumber := Items[0].Column;
          ErrorObj.Show(ltError,E2043_INVALID_COMMAND_OPCODE,[Items[0].Payload]);
        end;
      // Check for unresolved globs etc.
      for iter in Self do
        begin
          if not (iter.State in [ppLabel,ppCommand,ppInstruction,ppMacro,ppOperand]) then
            begin
              ErrorObj.ColNumber := iter.Column;
              ErrorObj.Show(ltError,E2003_UNRECOGNISED_CONTENT,[iter.Payload]);
            end;
        end;
    end
  else if not FMacroReference then
    ExtractOpcode;
  Parse := True;
end;
}

procedure TPreparser.GlobsToOperands;
var rec: TParserProp;
    i:   integer;
begin
  for i := 0 to Count-1 do
    begin
      rec := Items[i];
      if rec.State = psGlob then
        rec.State := ppOperand;
      Items[i] := rec;
    end;
end;

procedure TPreparser.PopulateDFA;
{$IFDEF DEBUG_LOG}
var _strm:    TStringStream;
{$ENDIF}
begin
  // Add all the commands and instructions
  FDFA.AddOpcodesCommands(FOpcodeList,FCommandList);
  // Dumps
{$IFDEF DEBUG_LOG}
  _strm := TStringStream.Create;
  try
    FDFA.DumpNFAList(_strm);
    FDFA.DumpNFATable(_strm);
    FDFA.DumpDFATable(_strm);
    WriteLn(_strm.DataString);
  finally
    FreeAndNil(_strm);
  end;
{$ENDIF}
end;

procedure TPreparser.RemoveCommas;
var i: integer;
begin
  i := Count-1;
  while i > 0 do
    begin
      if Items[i].State = psComma then
        Delete(i);
      Dec(i);
    end;
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

procedure TPreparser.RemoveLeadingWhiteSpace;
begin
  if (Count > 0) and (Items[0].State = psWhitespace) then
    Delete(0);
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

procedure TPreparser.SetDefiningMacro(_v: boolean);
begin
  FDefiningMacro := _v;
end;

procedure TPreparser.SetIndices;
var rec: TParserProp;
    i:   integer;
    token: TNFAtoken;
    index: integer;
begin
  // Macros first - we need to know if there's one here
  for i := Count-1 downto 0 do
    begin
      rec := Items[i];
      if (rec.State = ppMacro) and (not FDefiningMacro) then
        begin
          if FCaseSensitive then
            FMacroIndex := FMacroList.IndexOf(rec.Payload)
          else
            FMacroIndex := FMacroList.IndexOf(UpperCase(rec.Payload));
          if FMacroIndex < 0 then
            ErrorObj.Show(ltError,E2057_MACRO_NOT_FOUND,[Items[i].Payload]);
          FMacroReference := True;
          Delete(i);
        end;
    end;
  // Now do the others
  for i := Count-1 downto 0 do
    begin
      rec := Items[i];
      case rec.State of
        ppLabel:
          if not FDefiningMacro then
            begin
              FLabelX := StripColon(rec.Payload);
              Delete(i);
              ErrorObj.ColNumber := Items[i].Column;
              // Check to see if the label is used as a command anywhere
              token := FDFA.Tokenise(FLabelX);
              if (token >= FDFA.OffsetCommand) and (token < FDFA.OffsetOperand) then
              begin  // Label used here is already a command
                index := token - FDFA.OffsetCommand;
                if FCommandList.Items[Index].CommandStatus = csUsedAsNormal then
                  ErrorObj.Show(ltError,E2063_COMMAND_ALREADY_USED,[FLabelX]);
                FCommandList.Items[Index].CommandStatus := csUsedAsLabel;
                if FPass = 1 then
                  ErrorObj.Show(ltWarning,W1006_COMMAND_AS_SYMBOL,[FLabelX]);
              end
            else if token > TOKEN_WHITESPACE then
              begin
                ErrorObj.Show(ltError,E2030_USING_RESERVED_AS_LABEL,[FLabelX]);
              end;
            Delete(i);
          end;
        ppCommand:
          begin
            FCommandIndex := rec.Index;
            if FCommandList.Items[FCommandIndex].CommandStatus = csUsedAsLabel then
              begin
                ErrorObj.ColNumber := Items[i].Column;
                ErrorObj.Show(ltError,E2062_COMMAND_ALREADY_USED_AS_LABEL,[FCommandList.Items[FCommandIndex].CommandName]);
              end;
            FCommandList.Items[FCommandIndex].CommandStatus := csUsedAsNormal;
            Delete(i);
          end;
        ppInstruction:
          if not (FDefiningMacro or FMacroReference) then
            begin
              FOpcodeIndex := rec.Index;
              if not FMacroReference then
                Delete(i);
            end;
      end;
    end;
end;

procedure TPreparser.SetMacroList(_lst: TMacroList);
begin
  FMacroList := _lst;
end;



//----------------------------------------------------------------------------
//
//  Preparsing routine
//
//  Covers parsing 1/3 = Categorise
//                 2/3 = Pre-parse, allocate labels, commands, macros
//
//  Part 3/3 full parse on each operand is covered by the main assembler
//
//----------------------------------------------------------------------------

{$IFDEF DEBUG_LOGPP}
procedure TPreparser.ShowPPinfo(const _s: string);
var i: integer;
    obj: TParserProp;
begin
  if _s <> '' then
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[_s])
  else
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['DUMP']);
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['    Number of segments is ' + IntToStr(Count)]);
  for i := 0 to Count-1 do
    begin
      obj := Items[i];
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['    Segment ' + IntToStr(i)]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        State:    ' + GetEnumName(TypeInfo(TParserState),Ord(obj.State))]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        Payload:  ' + obj.Payload]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        Token:    ' + IntToStr(obj.Token)]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        Column:   ' + IntToStr(obj.Column)]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        Index:    ' + IntToStr(obj.Index)]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        Level:    ' + IntToStr(obj.Level)]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        DataType: ' + GetEnumName(TypeInfo(TLCGParserStackType),Ord(obj.DataType))]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        IntValue: ' + IntToStr(obj.IntValue)]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        StrValue: ' + obj.StrValue]);
      ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['        DataType: ' + GetEnumName(TypeInfo(TParserStackSource),Ord(obj.Source))]);
    end;
end;
{$ELSE}
procedure TPreparser.ShowPPinfo(const _s: string);
begin

end;
{$ENDIF}

procedure TPreparser.Categorise(const _input: string);
var i:       integer;
    state:   TParserState;
    ch:      char;
    prev_ch: char;
    inp_len: integer;
    payload: string;
    column:  integer;
    level:   integer;

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
            prop.Payload  := payload;
            prop.Token    := -1;
            prop.Column   := column;
            prop.Index    := -1;
            prop.Level    := level;
            prop.DataType := pstNone;
            prop.IntValue := 0;
            prop.StrValue := '';
            prop.Source   := pssUndefined;
            Add(prop);
          end;
        state := _newstate;
        column := i;
        payload := '';
      end;
  end;

begin
  payload := '';
  ErrorObj.ColNumber := 0;
  FMacroReference := False;
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
            ':': NewState(psUnknown);
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
  AdjustComments;
  RemoveComments;
  AllocateKeywords;
  RemoveLeadingWhitespace;
  ShowPPinfo('TPreparser.Categorise()');
end;

procedure TPreparser.Allocate;
begin
  AllocateLabels;
  AllocateMacros;
  CombineBrackets;
  CombineGlobs;
  RemoveWhitespace;
  GlobsToOperands;
  RemoveCommas;
  SetIndices;
  ShowPPinfo('TPreparser.Allocate()');
end;

procedure TPreparser.Parse(const _input: string);
begin
{$IFDEF DEBUG_LOGPP}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Source line: ' + _input]);
{$ENDIF}
  Categorise(_input);
  Allocate; // Allocate the main types
end;

end.

