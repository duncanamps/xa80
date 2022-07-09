program oc_comp;

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  deployment_parser_module_12, deployment_parser_types_12,
uinstruction;

type
  TMyReduceFunc = function (Parser: TLCGParser): TLCGParserStackEntry of object;

  { TOpcodeCompiler }

  TOpcodeCompiler = class(TCustomApplication)
  protected
    FDstName: string;
    FInstruction:     TInstructionRec;
    FInstructionList: TInstructionList;
    FLineNum:         integer;
    FParser:          TLcgParser;
    FPass:            integer;
    FSrcName:         string;
    FVerbose:         boolean;
    ReduceProcs:      array of TMyReduceFunc;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function  ActCodeBin(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeDec(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeHex(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeIM(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeR8(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeRST(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeS8(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeU8(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActCodeU16(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActIgnore(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActOpcode0(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActOpcode1(Parser: TLCGParser): TLCGParserStackEntry;
    function  ActOpcode2(Parser: TLCGParser): TLCGParserStackEntry;
    procedure Compile(_sl: TStringList; _pass: integer);
    procedure CreateParser;
    procedure DestroyParser;
    function  ExtractOperandNo(const _s: string): integer;
    procedure InitCompiler;
    procedure InitLine;
    procedure MyMonitor(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
    function  MyReduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
    function  OperandToIndex(const _operand: string; var _index: TOperandOption): boolean;
    procedure RegisterCode(_elementtype: TCodeElementType; _operand: byte; _value: word; _offset: byte);
    procedure RegisterInstruction(const _opcode, _oper1, _oper2: string);
    procedure RegisterOpcode(const _opcode: string);
    procedure WriteHelp; virtual;
  end;

{ TOpcodeCompiler }

procedure TOpcodeCompiler.DoRun;
var
  ErrorMsg: String;
  sl:       TStringList;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hv', 'help verbose');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  FVerbose := HasOption('v', 'verbose');
  if ParamCount = 0 then
    begin
      WriteLn('No filename specified');
      Terminate;
      Exit;
    end;
  FSrcName := Params[1];
  FDstName := FSrcName + '.bin';

  { add your program here }

  CreateParser;
  try
    InitCompiler;
    sl := TStringList.Create;
    try
      WriteLn('Loading file ' + FSrcName);
      sl.LoadFromFile(FSrcName);
      Compile(sl,1);
      Compile(sl,2);
      FInstructionList.ConstructHashTable;
      FInstructionList.SaveToFile(FDstName);
    finally
      sl.Free;
    end;
  finally
    DestroyParser;
  end;
  WriteLn('Success. Compiled to ' + FDstName);

  // stop program loop
  Terminate;
end;

constructor TOpcodeCompiler.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TOpcodeCompiler.Destroy;
begin
  inherited Destroy;
end;

function TOpcodeCompiler.ActCodeBin(Parser: TLCGParser): TLCGParserStackEntry;
var s: string;
    s1,s2,s3: string;
    code_element: TCodeElementType;
    binval: integer;
begin
  // @@@@@ Need to split out embedded parts, e.g. %01[1:B3]111
  s := Parser.ParserStack[Parser.ParserSP-1].Buf;
  if Pos('[',s) > 0 then
    begin // Has embedded RST or B3 parse into 3 sections
      s1 := Copy(s,1,Pos('[',s)-1);
      Delete(s,1,Length(s1));
      s2 := Copy(s,1,Pos(']',s));
      Delete(s,1,Length(s2));
      s3 := s;
      if Pos('RST',s2) > 0 then
        code_element := cetRST
      else if Pos('B3',s2) > 0 then
        code_element := cetB3
      else
        raise Exception.Create('Code element not catered for');
      binval := StrToInt(s1 + '000' + s3);
      RegisterCode(code_element,ExtractOperandNo(s2),binval,Length(s3));
    end
  else // Straight binary with no embedded
    RegisterCode(cetHex,0,StrToInt(s),0);
end;

function TOpcodeCompiler.ActCodeDec(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetHex,0,StrToInt(Parser.ParserStack[Parser.ParserSP-1].Buf),0);
end;

function TOpcodeCompiler.ActCodeHex(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetHex,0,StrToInt(Parser.ParserStack[Parser.ParserSP-1].Buf),0);
end;

function TOpcodeCompiler.ActCodeIM(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetIM,ExtractOperandNo(Parser.ParserStack[Parser.ParserSP-1].Buf),0,0);
end;

function TOpcodeCompiler.ActCodeR8(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetR8,ExtractOperandNo(Parser.ParserStack[Parser.ParserSP-1].Buf),0,0);
end;

function TOpcodeCompiler.ActCodeRST(Parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TOpcodeCompiler.ActCodeS8(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetS8,ExtractOperandNo(Parser.ParserStack[Parser.ParserSP-1].Buf),0,0);
end;

function TOpcodeCompiler.ActCodeU8(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetU8,ExtractOperandNo(Parser.ParserStack[Parser.ParserSP-1].Buf),0,0);
end;

function TOpcodeCompiler.ActCodeU16(Parser: TLCGParser): TLCGParserStackEntry;
begin
  RegisterCode(cetU16,ExtractOperandNo(Parser.ParserStack[Parser.ParserSP-1].Buf),0,0);
end;

function TOpcodeCompiler.ActIgnore(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := '';
end;

function TOpcodeCompiler.ActOpcode0(Parser: TLCGParser): TLCGParserStackEntry;
begin
  if FPass = 1 then
    RegisterOpcode(Parser.ParserStack[Parser.ParserSP-1].Buf)
  else
    RegisterInstruction(Parser.ParserStack[Parser.ParserSP-1].Buf,
                        '',
                        '');
end;

function TOpcodeCompiler.ActOpcode1(Parser: TLCGParser): TLCGParserStackEntry;
begin
  if FPass = 1 then
    RegisterOpcode(Parser.ParserStack[Parser.ParserSP-2].Buf)
  else
    RegisterInstruction(Parser.ParserStack[Parser.ParserSP-2].Buf,
                        Parser.ParserStack[Parser.ParserSP-1].Buf,
                        '');
end;

function TOpcodeCompiler.ActOpcode2(Parser: TLCGParser): TLCGParserStackEntry;
begin
  if FPass = 1 then
    RegisterOpcode(Parser.ParserStack[Parser.ParserSP-3].Buf)
  else
    RegisterInstruction(Parser.ParserStack[Parser.ParserSP-3].Buf,
                        Parser.ParserStack[Parser.ParserSP-2].Buf,
                        Parser.ParserStack[Parser.ParserSP-1].Buf);
end;

procedure TOpcodeCompiler.Compile(_sl: TStringList; _pass: integer);
var i: integer;
    j: integer;
    sstream: TStringStream;
begin
  FPass := _pass;
  Write(Format('Compiling pass %d',[_pass]));
  for i := 0 to _sl.Count-1 do
    if _sl[i] <> '' then
      begin
        FLineNum := i + 1;
        sstream := TStringStream.Create(_sl[i]);
        InitLine;
        try
          FParser.Parse(sstream);
          Write('.');
        finally
          sstream.Free;
        end;
        if (_pass = 2) and (FInstruction.OpcodeIndex <> $FFFF) then
          FInstructionList.Add(FInstruction);
      end;
  WriteLn;
  if _pass = 1 then
    WriteLn('Total of ',FInstructionList.OpcodeCount,' opcodes extracted')
  else if FVerbose then
    FInstructionList.Dump;
end;

procedure TOpcodeCompiler.CreateParser;
var i: integer;
    procnames: TStringArray;

  procedure MakeSetProc(const _procname: string; _reduce: TMyReduceFunc);
  var i: integer;
      changed: boolean;
  begin
    changed := False;
    for i := 0 to FParser.Rules-1 do
      if procnames[i] = _procname then
        begin
          ReduceProcs[i] := _reduce;
          changed := True;
        end;
    if not changed then
      raise Exception.Create('Procedure named ' + _procname + ' not found');
  end;

begin
  FParser := TLcgParser.Create;
  try
    FInstructionList := TInstructionList.Create;
    FParser.LoadFromResource('OPCODE_COMPILER');
    FParser.OnMonitor := @MyMonitor;
    FParser.OnReduce  := @MyReduce;
    SetLength(ReduceProcs,FParser.Rules);
    procnames := FParser.RuleProcs;
    MakeSetProc('ActCodeBin', @ActCodeBin);
    MakeSetProc('ActCodeDec', @ActCodeDec);
    MakeSetProc('ActCodeHex', @ActCodeHex);
    MakeSetProc('ActCodeIM',  @ActCodeIM);
    MakeSetProc('ActCodeR8',  @ActCodeR8);
    MakeSetProc('ActCodeS8',  @ActCodeS8);
    MakeSetProc('ActCodeU8',  @ActCodeU8);
    MakeSetProc('ActCodeU16', @ActCodeU16);
    MakeSetProc('ActIgnore',  @ActIgnore);
    MakeSetProc('ActOpcode0', @ActOpcode0);
    MakeSetProc('ActOpcode1', @ActOpcode1);
    MakeSetProc('ActOpcode2', @ActOpcode2);
  except
    WriteLn('Unable to load OPCODE_COMPILER resource');
  end;
end;

procedure TOpcodeCompiler.DestroyParser;
begin
  FreeAndNil(FParser);
  FreeAndNil(FInstructionList);
end;

function TOpcodeCompiler.ExtractOperandNo(const _s: string): integer;
begin
  // Input will be [1:RST] etc. so we need to get the digit after the [
  Result := 0;
  if Pos('[',_s) > 0 then
    Result := StrToInt(Copy(_s,Pos('[',_s)+1,1));
end;

procedure TOpcodeCompiler.InitCompiler;
begin
  // Code here to initialise whole run
end;

procedure TOpcodeCompiler.InitLine;
begin
  // Code here to initialise each line
  FInstruction.OpcodeIndex      := $FFFF;
  FInstruction.Operand1Index    := OPER_NULL;
  FInstruction.Operand2Index    := OPER_NULL;
  FInstruction.CodeElementCount := 0;
end;

procedure TOpcodeCompiler.MyMonitor(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
begin
  case LogType of
    ltInternal:  Write('INTERNAL ERROR: ');
    ltError:     Write('ERROR: ');
    ltWarning:   Write('WARNING: ');
  end;
  WriteLn(Message);
  WriteLn(Format('Raised at line %d column %d',[FLineNum,Parser.InputColumn]));
end;

function TOpcodeCompiler.MyReduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  if Assigned(ReduceProcs[RuleIndex]) then
    Result := ReduceProcs[RuleIndex](Parser)
  else
    raise Exception.Create(Format('No procedure available for rule index %d',[RuleIndex]));
end;

function TOpcodeCompiler.OperandToIndex(const _operand: string; var _index: TOperandOption): boolean;
var i: TOperandOption;
    op: string;
begin
  Result := False;
  op := UpperCase(_operand);
  for i in TOperandOption do
    if OperandStrings[i] = op then
      begin
        Result := True;
        _index := i;
        break;
      end;
end;

procedure TOpcodeCompiler.RegisterCode(_elementtype: TCodeElementType; _operand: byte; _value: word; _offset: byte);
begin
  FInstruction.CodeElements[FInstruction.CodeElementCount].ElementType := _elementtype;
  FInstruction.CodeElements[FInstruction.CodeElementCount].OperandNo     := _operand;
  FInstruction.CodeElements[FInstruction.CodeElementCount].Value       := _value;
  FInstruction.CodeElements[FInstruction.CodeElementCount].Offset      := _offset;
  FInstruction.CodeElementCount := FInstruction.CodeElementCount + 1;
end;

procedure TOpcodeCompiler.RegisterInstruction(const _opcode, _oper1, _oper2: string);
var opc_index: integer;
    op1_index: TOperandOption;
    op2_index: TOperandOption;
begin
  if not FInstructionList.FindOpcode(UpperCase(_opcode),opc_index) then
    raise Exception.Create(Format('Internal error: Opcode %s not found',[_opcode]));
  if not OperandToIndex(_oper1,op1_index) then
    raise Exception.Create(Format('Internal error: Operand %s not found',[_oper1]));
  if not OperandToIndex(_oper2,op2_index) then
    raise Exception.Create(Format('Internal error: Operand %s not found',[_oper2]));
  FInstruction.OpcodeIndex   := opc_index;
  FInstruction.Operand1Index := op1_index;
  FInstruction.Operand2Index := op2_index;
end;

procedure TOpcodeCompiler.RegisterOpcode(const _opcode: string);
var idx: integer;
begin
  if not FInstructionList.FindOpcode(_opcode,idx{%H-}) then
    FInstructionList.AddOpcode(UpperCase(_opcode));
end;

procedure TOpcodeCompiler.WriteHelp;
var fn: string;
begin
  { add your help code here }
  fn := ExtractFileName(ExeName);
  writeln('Usage: ', fn, ' -h');
  writeln('       ', fn, ' myname.opcode');
  WriteLn;
end;

var
  Application: TOpcodeCompiler;

{$R *.res}

begin
  Application:=TOpcodeCompiler.Create(nil);
  Application.Title:='Opcode Compiler';
  Application.Run;
  Application.Free;
end.

