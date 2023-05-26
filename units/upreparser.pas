unit upreparser;

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
  Classes, SysUtils, uasmglobals;

type
  // The different states for our simple state machine

  TPreparserState = (ppsComma,
                     ppsComment,
                     ppsDirective,
                     ppsEOL,
                     ppsError,
                     ppsLabel,
                     ppsLabelColon,
                     ppsOperand,
                     ppsOperandDQuote,
                     ppsOperandDQBS,
                     ppsOperandSQuote,
                     ppsOperandSQBS,
                     ppsStart,
                     ppsWhitespace1,
                     ppsWhitespace2);

  TPreparser = class(TObject)
    protected
      FDirective:  string;          // The directive _or_ opcode
      FLabel:      string;          // The label from the line
      FOperands:   TStringList;     // The operands
      FForceColon: boolean;
      FTabSize:    integer;
      function  ExpandTabs(const _input: string): string;
      function  GetOperandCount: integer;
      procedure Init;
      procedure SetTabSize(_v: integer);
      procedure TidyOperands;
    public
      constructor Create;
      destructor Destroy; override;
      function Parse(_input: string; var _message: string): boolean;
    published
      property ForceColon:   boolean     read FForceColon     write FForceColon;
      property LabelText:    string      read FLabel;
      property Directive:    string      read FDirective;
      property OperandCount: integer     read GetOperandCount;
      property Operands:     TStringList read FOperands;
      property TabSize:      integer     read FTabSize        write SetTabSize;
  end;

implementation

constructor TPreparser.Create;
begin
  inherited Create;
  FOperands := TStringList.Create;
  FTabSize := DEFAULT_TAB_SIZE;
  Init;
end;

destructor TPreparser.Destroy;
begin
  FreeAndNil(FOperands);
  inherited Destroy;
end;

function TPreparser.ExpandTabs(const _input: string): string;
var output: string;
    i:      integer;
begin
  output := '';
  for i := 1 to Length(_input) do
    if _input[i] = #9 then
      repeat
        output := output + ' '
      until (Length(output) mod FTabSize) = 0
    else
      output := output + _input[i];
  Result := output;
end;

function TPreparser.GetOperandCount: integer;
begin
  Result := FOperands.Count;
end;

procedure TPreparser.Init;
begin
  FLabel := '';
  FDirective := '';
  FOperands.Clear;
end;

function TPreparser.Parse(_input: string; var _message: string): boolean;
var i: integer; // Index into input
    terminated: boolean;
    ch:         char;
    buf:        string;
    state:      TPreparserState;

  procedure KeepState;
  begin
    buf := buf + ch;
  end;

  procedure SwitchState(_newstate: TPreparserState; _keepwithprev: boolean = False; _flushbuf: boolean = True);
  begin
    if _keepwithprev then
      buf := buf + ch;
    // Deal with old state first
    case state of
      ppsDirective:   FDirective := buf;
      ppsLabel:       FLabel     := buf;
      ppsOperand:     if _newstate in [ppsComma,ppsComment,ppsEOL] then
                        FOperands.Add(buf);
    end; // case
    // Switch to new state
    state := _newstate;
    // Finally clear buf
    if _flushbuf then
      buf := '';
    if not _keepwithprev then
      buf := buf + ch;
  end;

  procedure SetError(const _msg: string);
  begin
    state := ppsError;
    _message := _msg;
  end;

  procedure SwapToLabel; // Swap a directive to a label
  begin
    if FLabel <> '' then
      SetError(Format('Attempting to define label %s when a label of %s has already been defined',[buf,FLabel]))  // Label was already defined, sorry
    else
      state := ppsLabel;
  end;

begin
  Init;
  _input := ExpandTabs(_input) + LF; // Use linefeed as EOL (End Of Line)
  i := 1;
  terminated := False;
  buf        := '';
  state      := ppsStart;
  _message   := '';
  while (i <= Length(_input)) and (not terminated) do
    begin
      ch := _input[i];
      if ch in BAD then
        SetError(Format('Bad ASCII character %d in input',[Ord(ch)]));
      case state of
        ppsComma:
            if ch <> ',' then
              case ch of
                #39: SwitchState(ppsOperandSQuote,False);
                #34: SwitchState(ppsOperandDQuote,False);
                otherwise SwitchState(ppsOperand,False);
              end // case
            else
              SetError('Cannot have comma after another comma');
        ppsComment:
            if ch = LF then
              SwitchState(ppsEOL,False)
            else
              KeepState;
        ppsDirective:
            case ch of
              LF:  SwitchState(ppsEOL,False);
              ' ': SwitchState(ppsWhitespace2,False);
              ';': SwitchState(ppsComment,False);
              otherwise if ch in ALPHA then
                KeepState
              else if (ch in LABELX) and (FForceColon) then
                SwapToLabel
              else if (ch = ':') and FForceColon then
                begin
                  SwapToLabel;
                  if state <> ppsError then
                    SwitchState(ppsLabelColon,False);
                end
              else
                SetError(Format('Unexpected character %s in directive/opcode',[ch]));
            end; // Case
        ppsEOL:
            terminated := True;
        ppsError:
            terminated := True;
        ppsLabel:
            case ch of
              LF:  if not FForceColon then
                     SwitchState(ppsEOL,False)
                   else
                     SetError('No colon on label when forced colon has been defined');
              ' ': if not FForceColon then
                     SwitchState(ppsWhitespace1,False)
                   else
                     SetError('No colon on label when forced colon has been defined');
              ';': if not FForceColon then
                     SwitchState(ppsComment,False)
                   else
                     SetError('No colon on label when forced colon has been defined');
              ':': SwitchState(ppsLabelColon,False);
              otherwise if ch in ALPHA + DIGITS + LABELX then
                KeepState
              else
                SetError(Format('Unexpected character %s in label',[ch]));
            end; // Case
        ppsLabelColon:
            case ch of
              #10: SwitchState(ppsEOL,False);
              ' ': SwitchState(ppsWhitespace1,False);
              ';': SwitchState(ppsComment,False);
              otherwise if ch in ALPHA then
                SwitchState(ppsDirective,False)
              else
                SetError(Format('Unexpected character %s after colon',[ch]));
            end; // Case
        ppsOperand:
            case ch of
              #10: SwitchState(ppsEOL,False);
              ';': SwitchState(ppsComment,False);
              ',': SwitchState(ppsComma,False);
              SQ:  SwitchState(ppsOperandSQuote,False,False);
              DQ:  SwitchState(ppsOperandDQuote,False,False)
              otherwise KeepState;
            end; // Case
        ppsOperandDQuote:
            case ch of
              DQ:  SwitchState(ppsOperand,True,False);
              '\': SwitchState(ppsOperandDQBS,False,False);
              otherwise KeepState;
            end; // Case
        ppsOperandDQBS:
            if ch in ESCAPED then
              SwitchState(ppsOperandDQuote,True,False)
            else
              SetError(Format('Illegal escape sequence \%s',[ch]));
        ppsOperandSQuote:
            case ch of
              SQ:  SwitchState(ppsOperand,True,False);
              '\': SwitchState(ppsOperandSQBS,False,False);
              otherwise KeepState;
            end; // case
        ppsOperandSQBS:
            if ch in ESCAPED then
              SwitchState(ppsOperandSQuote,True,False)
            else
              SetError(Format('Illegal escape sequence \%s',[ch]));
        ppsStart:
            case ch of
              LF:  SwitchState(ppsEOL,False);
              '*',
              ';': SwitchState(ppsComment,False);
              ' ': SwitchState(ppsWhitespace1,False);
              otherwise
                if FForceColon and (ch in ALPHA) then SwitchState(ppsDirective,False)
                else if ch in ALPHA + LABELX then
                  SwitchState(ppsLabel,False)
                else
                  SetError(Format('Unexpected character %s at start of line',[ch]));
            end; // case
        ppsWhitespace1:
            case ch of
              LF:  SwitchState(ppsEOL,False);
              ' ': KeepState;
              ';': SwitchState(ppsComment,False);
              otherwise if ch in ALPHA then
                SwitchState(ppsDirective,False)
              else
                SetError(Format('Unexpected character %s after label',[ch]));
            end; // case
        ppsWhitespace2:
            case ch of
              LF:  SwitchState(ppsEOL,False);
              ' ': KeepState;
              ';': SwitchState(ppsComment,False);
              SQ:  SwitchState(ppsOperandSQuote,False,True);
              DQ:  SwitchState(ppsOperandDQuote,False,True);
              ',': SetError(Format('Unexpected , after directive or opcode',[ch]));
              otherwise SwitchState(ppsOperand,False);
            end; // case
      end; // case
      Inc(i);
    end;
  if state <> ppsEOL then
    state := ppsError;
  Result := state <> ppsError;
  if state = ppsError then
    _message := 'Pre-parser error on input at column ' + IntToStr(i) + #13 + #10 + _message
  else
    TidyOperands;
end;

procedure TPreparser.SetTabSize(_v: integer);
begin
  if _v < 1 then
    raise Exception.Create('TabSize must be >= 1');
  FTabSize := _v;
end;

// Tidy the operands. Performs two steps:
// 1. Remove any leading or trailing spaces from the operand
// 2. Replace enclosing ( ) with [ ] so ($300) -> [$300], (HL) -> [HL]

procedure TPreparser.TidyOperands;
var i: integer;

  function Tidy(const _str: string): string;
  var s: string;
  begin
    s := Trim(_str);
    if Indirected(s) then
      s := '[' + Copy(s,2,Length(s)-2) + ']';
    Result := s;
  end;

begin
  for i := 0 to OperandCount-1 do
    Operands[i] := Tidy(Operands[i]);
end;

end.

