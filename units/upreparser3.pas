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
  Classes, SysUtils, Generics.Collections;

type
  TParserState = (psNone,
                  psLabel,
                  psDirective,
                  psCommand,
                  psInstruction,
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
                  psDone,
                  psError);

  TParserProp = record
    State:    TParserState;
    Payload:  string;
    Column:   integer;
    Level:    integer;
  end;

  TPreparser = class(specialize TList<TParserProp>)
    private
      FErrorMsg:      string;
      FForceColon:    boolean;
      procedure AssignDirective;
      procedure AssignIndirect;
      procedure AssignInstructions;
      procedure AssignLabel;
      procedure AssignOperands;
      procedure Crunch;
      procedure CrunchComments;
      procedure CrunchOperands;
      procedure CrunchWhitespace;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Init;
      function  Parse(_input: string): boolean;
      property ErrorMsg:      string      read FErrorMsg;
      property ForceColon:    boolean     read FForceColon     write FForceColon;
  end;


implementation

uses
  uasmglobals, uutility, typinfo;

var
  Instructions: array[0..66] of string = ('ADC',
                                          'ADD',
                                          'AND',
                                          'BIT',
                                          'CALL',
                                          'CCF',
                                          'CP',
                                          'CPD',
                                          'CPDR',
                                          'CPI',
                                          'CPIR',
                                          'CPL',
                                          'DAA',
                                          'DEC',
                                          'DI',
                                          'DJNZ',
                                          'EI',
                                          'EX',
                                          'EXX',
                                          'HALT',
                                          'IM',
                                          'IN',
                                          'INC',
                                          'IND',
                                          'INDR',
                                          'INI',
                                          'INIR',
                                          'JP',
                                          'JR',
                                          'LD',
                                          'LDD',
                                          'LDDR',
                                          'LDI',
                                          'LDIR',
                                          'NEG',
                                          'NOP',
                                          'OR',
                                          'OTDR',
                                          'OTIR',
                                          'OUT',
                                          'OUTD',
                                          'OUTI',
                                          'POP',
                                          'PUSH',
                                          'RES',
                                          'RET',
                                          'RETI',
                                          'RETN',
                                          'RL',
                                          'RLA',
                                          'RLC',
                                          'RLCA',
                                          'RLD',
                                          'RR',
                                          'RRA',
                                          'RRC',
                                          'RRCA',
                                          'RRD',
                                          'RST',
                                          'SBC',
                                          'SCF',
                                          'SET',
                                          'SLA',
                                          'SRA',
                                          'SRL',
                                          'SUB',
                                          'XOR');


constructor TPreparser.Create;
begin
  inherited Create;
  FForceColon := False;
end;

destructor TPreparser.Destroy;
begin
  inherited Destroy;
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
         (Indirected(rec.Payload)) then
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
begin
  i := 0;
  while (i < Count) and (Items[i].State <> psDirective) do
    Inc(i);
  if (i < Count) then
    begin
      rec := Items[i];
      j := 0;
      while (j < 67) and (rec.Payload <> Instructions[j]) do
        Inc(j);
      if j < 67 then
        begin
          rec.State := psInstruction;
          Items[i] := rec;
        end;
    end;
end;

procedure TPreparser.AssignLabel;
var rec: TParserProp;
begin
  if Count = 0 then
    Exit;
  if FForceColon then
    begin // Colon required, must be in first place and have a colon
      if (Items[0].State = psGlob) and (RightStr(Items[0].Payload,1) = ':') then
        begin
          rec := Items[0];
          rec.State := psLabel;
          Items[0] := rec;
        end;
    end
  else
    begin // Colon not required, label must be in first place
      if Items[0].State = psGlob then
        begin
          rec := Items[0];
          rec.State := psLabel;
          Items[0] := rec;
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
      while (i < Count-1) and (Items[i].State = psComma) and (Items[i+1].State in [psGlob,psDQStr,psSQStr]) do
        begin
          Delete(i); // Get rid of comma
          rec := Items[i];
          rec.State := psOperand;
          Items[i] := rec;
          Inc(i);
        end;
    end;
end;

procedure TPreparser.Crunch;
begin
  CrunchComments;
  CrunchOperands;
  AssignLabel;
  CrunchWhitespace;
  AssignDirective;
  AssignOperands;
  AssignIndirect;
  AssignInstructions;
end;

procedure TPreparser.CrunchComments;
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

procedure TPreparser.CrunchOperands;
var i: integer;
    rec: TParserProp;
begin
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

  procedure RaiseError(const _msg: string);
  begin
    state := psError;
    FErrorMsg := _msg + ' in column ' + IntToStr(i);
    Parse := False;
  end;

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
            Add(prop);
          end;
        state := _newstate;
        column := i;
        payload := '';
      end;
  end;

begin
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
            ESCAPE: state := psDQEsc;
          end;
        psDQEsc:
          if ch in ESCAPED then
            state := psDQStr
          else
            RaiseError('Illegal escape sequence');
        psSQStr:
          case ch of
            SQ:     NewState(psUnknown);
            ESCAPE: state := psSQEsc;
          end;
        psSQEsc:
          if ch in ESCAPED then
            state := psSQStr
          else
            RaiseError('Illegal escape sequence');
        psError:
          Exit;
      end;
      if (ch = '(') and (state = psGlob) then
        Inc(level);
      if (ch = ')') and (state = psGlob) then
        Dec(level);
      payload := payload + ch;
      prev_ch := ch;
    end;
  NewState(psDone);
  Crunch;
  Parse := True;
end;

end.

