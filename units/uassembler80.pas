{$WARN 5024 off : Parameter "$1" not used}
unit uassembler80;

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

interface

uses
  Classes, SysUtils, deployment_parser_module_12, deployment_parser_types_12,
  ufilestack, usymbol, uasmglobals, uoutput, uifstack, umacro, udebuglist,
  uinstruction, Generics.Collections, upreparser2;


type

  TOperand = record
    oper_opt: TOperandOption;
    value:    integer;
  end;

  TOperandArray = array[0..MAX_OPERANDS-1] of TOperand;

  TExprListItemType = (litString,litInteger);

  TExprListItem = record
    ItemType:     TExprListItemType;
    StringValue:  string;
    IntegerValue: integer;
  end;

  TExprList = class(specialize TList<TExprListItem>)

  end;

  TAssembler80 = class(TLCGParser)
    protected
      FAddr:           UINT16;
//    FAddrMode:       TAddrMode;
      FAssemblyEnd:    TDateTime;
      FAssemblyStart:  TDateTime;
      FBytesFromLine:  integer;
      FBytesTotal:     integer;
      FCmdDefines:     TStringList;
      FCmdIncludes:    TStringList;
      FDebugList:      TDebugList;
      FDefiningMacro:  boolean;
      FEnded:          boolean;
      FExprList:       TExprList;
      FForceList:      boolean;
      FFilenameSrc:    string;
      FFileStack:      TFileStack;
      FIfStack:        TIfStack;
      FIncludeNext:    string;
      FInstructionList: TInstructionList;
      FLineCount:      integer;
      FList:           boolean;
      FListNext:       boolean;
      FListing:        TStringList;
      FLocalPrefix:    string;
      FMacroCapture:   TStringList;
      FMacroName:      string;
      FMacroNestLevel: integer;
      FMacroList:      TMacroList;
//    FOpCode:         TOpCode;
      FOperandIndex:   integer;
      FOperands:       TOperandArray;
      FOrg:            UINT16;
      FOutput:         TOutput;
      FOutputArr:      TOutputSequence;
      FPass:           integer;
      FPreparser:      TPreparser;
      FProcArray:      array of TLCGParserProc;
      FProcessMacro:   string;
      FProcessor:      string;
      FProcessParms:   string;
      FStreamLog:      TFileStream;
      FSymbols:        TSymbolTable;
      FTitle:          string;
      FVerbose:        boolean;
      function  ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirCPU(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExprC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefMacro(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirEnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirEndm(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIncludeList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMacro(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMacroNoexpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirNolist(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirSet(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirTitle(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprListItem(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOpcode0(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOpcode1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOpcode2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndOff(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpSimple(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpSimpleW(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpBracketed(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSymbolDef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      function  DoOpcode(_parser: TLCGParser; _operands: integer): TLCGParserStackEntry;
      procedure FilesClose;
      procedure FilesOpen;
      function  GetSource: string;
      procedure InitLine;
      procedure InitPass;
      procedure InitStart;
      function  Logical0: TLCGParserStackEntry;
      function  Logical1: TLCGParserStackEntry;
      procedure OutputDebugLine(const _asmline: string);
      procedure OutputListingLine(const _asmline: string);
      function  ParserAsInt(_index: integer): int32;
      procedure ProcessFile(const _fn: string; _listing: boolean = True);
      procedure ProcessFileInner;
      function  ProcessingAllowed: boolean;
      procedure ProcessMacroExpansion;
      procedure ProcessLine(const _line: string);
      procedure PumpCode(_r: TInstructionRec);
      procedure PushOperand(_op: TOperandOption; _v: integer);
      procedure RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
      procedure RegisterProcs;
      procedure SetFilenameSrc(const _fn: string);
      procedure SetIntEntry(var _e: TLCGParserStackEntry; _intval: int32);
      procedure SetOnMonitor(_monitor: TLCGMonitorProc);
      procedure SetStringEntry(var _e: TLCGParserStackEntry; _strval: string);
      procedure WriteMapFile;
    public
      FilenameDbg:    string;
      FilenameCom:    string;
      FilenameHex:    string;
      FilenameLst:    string;
      FilenameLog:    string;
      FilenameMap:    string;
      FilenameObj:    string;
      constructor Create(const _grammar: string; const _processor: string);
      destructor Destroy; override;
      procedure Assemble;
      procedure AssemblePass(_pass: integer);
      procedure Monitor(LogType: TLCGLogType; const Message: string); override;
      procedure Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const); override;
      function  Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
      property CmdDefines:  TStringList     read FCmdDefines;
      property CmdIncludes: TStringList     read FCmdIncludes;
      property FilenameSrc: string          read FFilenameSrc write SetFilenameSrc;
      property OnMonitor:   TLCGMonitorProc read FOnMonitor   write SetOnMonitor;
      property Source:      string          read GetSource;
      property TabSize:     integer         read FTabSize     write FTabSize;
  end;


implementation

uses
  uexpression, strutils;


{ TAssembler80 }

constructor TAssembler80.Create(const _grammar: string; const _processor: string);
begin
  inherited Create;
  LoadFromResource(_grammar);
  SetLength(FProcArray,Rules);
  FSymbols := TSymbolTable.Create;
  FFileStack := TFileStack.Create(Self);
  FIfStack   := TIfStack.Create(Self);
  FOutput := TOutput.Create;
  FDebugList := TDebugList.Create;
  FListing := TStringList.Create;
  FMacroList := TMacroList.Create;
  FCmdDefines := TStringList.Create;
  FCmdIncludes := TStringList.Create;
  FInstructionList := TInstructionList.Create(_processor);
  FExprList        := TExprList.Create;
  FProcessor := _processor;
  FPreparser := TPreparser.Create;
  FPass := 0;
  FTabSize := 4;
  FVerbose := False;
  FOnReduce := @Reduce;
  RegisterProcs;
end;

destructor TAssembler80.Destroy;
begin
  FreeAndNil(FPreparser);
  FreeAndNil(FExprList);
  FreeAndNil(FInstructionList);
  FCmdIncludes.Free;
  FCmdDefines.Free;
  FreeAndNil(FMacroList);
  FListing.Free;
  FDebugList.Free;
  FOutput.Free;
  FIfStack.Free;
  FFileStack.Free;
  FSymbols.Free;
  inherited Destroy;
end;

function TAssembler80.ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,IntFromBinLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler80.ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if ParserAsInt(-3) = ParserAsInt(-1) then
    result := Logical1
  else
    result := Logical0;
end;

function TAssembler80.ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if ParserAsInt(-3) >= ParserAsInt(-1) then
    result := Logical1
  else
    result := Logical0;
end;

function TAssembler80.ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if ParserAsInt(-3) > ParserAsInt(-1) then
    result := Logical1
  else
    result := Logical0;
end;

function TAssembler80.ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if ParserAsInt(-3) <= ParserAsInt(-1) then
    result := Logical1
  else
    result := Logical0;
end;

function TAssembler80.ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if ParserAsInt(-3) < ParserAsInt(-1) then
    result := Logical1
  else
    result := Logical0;
end;

function TAssembler80.ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if ParserAsInt(-3) <> ParserAsInt(-1) then
    result := Logical1
  else
    result := Logical0;
end;

function TAssembler80.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := _parser.ParserStack[_parser.ParserSP-1];
end;

function TAssembler80.ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
var s: string;
begin
  Result := EmptyStackEntry;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  // Get rid of the trailing D if present
  if (Length(s) > 0) and (UpperCase(RightStr(s,1)) = 'D') then
    s := LeftStr(s,Length(s)-1);
  SetIntEntry(Result,StrToInt(s));
end;

function TAssembler80.ActDirCPU(_parser: TLCGParser): TLCGParserStackEntry;
var cpu: string;
    entry: TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  if (not ProcessingAllowed) or (FPass > 1) then
    Exit;
  entry := _parser.ParserStack[_parser.ParserSP-1];
  cpu := entry.Buf;
  Monitor(ltWarning,'Specifying CPU %s has no effect, please use --processor=%s parameter',[cpu,cpu]);
end;

function TAssembler80.ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    op:  integer;
    bval: integer;
    ch:   char;
begin
  Result := EmptyStackEntry;
  if not ProcessingAllowed then
    Exit;
  // Get number of bytes in total
  FBytesFromLine := 0;
  for i := 0 to FExprList.Count-1 do
    case FExprList[i].ItemType of
      litString:  FBytesFromLine := FBytesFromLine + Length(FExprList[i].StringValue);
      litInteger: FBytesFromLine := FBytesFromLine + 1;
      otherwise
        Monitor(ltInternal,'Expression list type not catered for');
    end; // Case
  // Now output the bytes
  SetLength(FOutputArr,FBytesFromLine);
  op := 0;
  for i := 0 to FExprList.Count-1 do
    case FExprList[i].ItemType of
      litString:  for ch in FExprList[i].StringValue do
                    begin
                      FOutputArr[op] := Ord(ch);
                      Inc(op);
                    end;
      litInteger:   begin
                      bval := FExprList[i].IntegerValue;
                      if (bval > 255) or (bval < -128) then
                        Monitor(ltError,'Byte value %d not in range',[bval]);
                      FOutputArr[op] := bval and $00FF;
                      Inc(op);
                    end;
    end; // Case
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirDC(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    op:  integer;
    bval: integer;
    ch:   char;
begin
  Result := EmptyStackEntry;
  if not ProcessingAllowed then
    Exit;
  // Get number of bytes in total
  FBytesFromLine := 0;
  for i := 0 to FExprList.Count-1 do
    case FExprList[i].ItemType of
      litString:  FBytesFromLine := FBytesFromLine + Length(FExprList[i].StringValue);
      litInteger: FBytesFromLine := FBytesFromLine + 1;
      otherwise
        Monitor(ltInternal,'Expression list type not catered for');
    end; // Case
  // Now output the bytes
  SetLength(FOutputArr,FBytesFromLine);
  op := 0;
  for i := 0 to FExprList.Count-1 do
    case FExprList[i].ItemType of
      litString:    begin
                      for ch in FExprList[i].StringValue do
                        begin
                          FOutputArr[op] := Ord(ch);
                          Inc(op);
                        end;
                      FOutputArr[op-1] := FOutputArr[op-1] or $80;
                    end;
      litInteger:   begin
                      bval := FExprList[i].IntegerValue;
                      if (bval > 255) or (bval < -128) then
                        Monitor(ltError,'Byte value %d not in range',[bval]);
                      FOutputArr[op] := bval and $00FF;
                      Inc(op);
                    end;
    end; // Case
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,symbolname);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler80.ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := IntToStr(ParserAsInt(-1));
  message := FSymbols.Define(FPass,False,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler80.ActDirDefineExprC(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  if (Length(symbolname) > 0) and (RightStr(symbolname,1) = ':') then
    symbolname := LeftStr(symbolname,Length(symbolname)-1);
  expression := IntToStr(ParserAsInt(-1));
  message := FSymbols.Define(FPass,False,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler80.ActDirDefMacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not FIfStack.Allowed then
    exit;
  if FDefiningMacro then
    Monitor(ltError,'MACRO definition cannot be nested');
  FDefiningMacro := True;
  FMacroName := UpperCase(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if FMacroList.IndexOf(FMacroName) >= 0 then
    Monitor(ltError,'Macro %s is already defined',[FMacroName]);
  FMacroCapture := TStringList.Create;
  Monitor(ltWarAndPeace,'Defining macro %s',[FMacroName]);
end;

function TAssembler80.ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    bytes: integer;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  bytes := ParserAsInt(-1);
  if (bytes < 1) or (bytes > 32767) then
    begin
      Monitor(ltError,'Number of bytes for storage directive not in the range 1-32767');
      Exit;
    end;
  FBytesFromLine := bytes;
  SetLength(FOutputArr,bytes);
  for i := 1 to bytes do
    FOutputArr[i-1] := 0;
  FOutput.Write(FOutputArr,FOrg,bytes);
end;

function TAssembler80.ActDirDS2(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    bytes: integer;
    code:  integer;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  bytes := ParserAsInt(-3);
  code  := ParserAsInt(-1);
  if (bytes < 1) or (bytes > 32767) then
    begin
      Monitor(ltError,'Number of bytes for storage directive not in the range 1-32767');
      Exit;
    end;
  if (code < 0) or (code > 255) then
    begin
      Monitor(ltError,'Fill byte for storage directive not in the range 0-255');
      Exit;
    end;
  FBytesFromLine := bytes;
  SetLength(FOutputArr,bytes);
  for i := 1 to bytes do
    FOutputArr[i-1] := code;
  FOutput.Write(FOutputArr,FOrg,bytes);
end;

function TAssembler80.ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    bval: integer;
    op: integer;
begin
  Result := EmptyStackEntry;
  if not ProcessingAllowed then
    Exit;
  // Get number of bytes in total
  FBytesFromLine := 0;
  for i := 0 to FExprList.Count-1 do
    case FExprList[i].ItemType of
      litInteger: FBytesFromLine := FBytesFromLine + 2;
      otherwise
        Monitor(ltInternal,'Expression list type not catered for');
    end; // Case
  // Now output the words
  SetLength(FOutputArr,FBytesFromLine);
  op := 0;
  for i := 0 to FExprList.Count-1 do
    begin
      bval := FExprList[i].IntegerValue;
      if (bval > 65535) or (bval < -32768) then
        Monitor(ltError,'Word value %d not in range',[bval]);
      FOutputArr[op] := bval and $00FF;
      Inc(op);
      FOutputArr[op] := (bval shr 8) and $00FF;
      Inc(op);
    end;
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  FIfStack.ElseSwap;
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirEnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if FDefiningMacro then
    Monitor(ltError,'END directive encountered in the middle of a macro definition');
  if FIfStack.Count > 0 then
    Monitor(ltError,'END directive encountered inside an IF statement');
  FEnded := True;
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  FIfStack.Pop;
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirEndm(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not FIfStack.Allowed then
    exit;
  FMacroList.Add(FMacroName,FMacroCapture);
  FDefiningMacro := False;
  FMacroName := '';
  FreeAndNil(FMacroCapture);
  Monitor(ltWarAndPeace,'End of defining macro %s',[FMacroName]);
end;

function TAssembler80.ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  Monitor(ltError,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler80.ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  Result := EmptyStackEntry;
  succeeded := ParserAsInt(-1) <> 0;
  FIfStack.Push(succeeded);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  Result := EmptyStackEntry;
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := FSymbols.ExistsInPass(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  Result := EmptyStackEntry;
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := not FSymbols.ExistsInPass(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
var parentfile: string;
    myfile:     string;
    newfile:    string;
    saveddir:   string;
    i:          integer;
  // Check if a file combo exists, return true if it does
  function CheckFile(folder: string; fn: string): boolean;
  begin
    SetCurrentDir(folder);
    try
      newfile := ExpandFilename(myfile);
    finally
      SetCurrentDir(saveddir);
    end;
    Monitor(ltDebug,'Searching for include file %s in folder %s',[myfile,folder]);
    if FileExists(newfile) then
      begin
        Monitor(ltDebug,'Found include file %s in folder %s',[myfile,folder]);
        CheckFile := True;
        FIncludeNext := newfile;
      end
    else
      CheckFile := False;
  end;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  // Rules for processing the include file
  // The file will first be processed relative folders in the following order
  // 1. The parent file which called the include
  // 2. The folder of the initial source file (could be same as 1.)
  // 3. The current directory used to start the software (could also be same as 1.)
  // 4. The folders specified in the --include directive
  // 5.  :
  // 6.  :
  saveddir := GetCurrentDir;
  parentfile := FFileStack.Filename;
  myfile := _parser.ParserStack[_parser.ParserSP-1].Buf;
  if CheckFile(ExtractFilePath(parentfile),myfile) then
    Exit;
  for i := 0 to CmdIncludes.Count-1 do
    if CheckFile(CmdIncludes[i],myfile) then
      Exit;
  Monitor(ltError,'Include file %s could not be found',[myfile]);
end;

function TAssembler80.ActDirIncludeList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FForceList := True;
  Result := ActDirInclude(_parser);
end;

function TAssembler80.ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  FList := True;
  FListNext := True;
end;

function TAssembler80.ActDirMacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  FProcessMacro := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FProcessParms := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler80.ActDirMacroNoexpr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  FProcessMacro := UpperCase(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FProcessParms := '';
end;

function TAssembler80.ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  Monitor(ltInfo,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler80.ActDirNoList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  FListNext := False;
end;

function TAssembler80.ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  FOrg := ParserAsInt(-1);
end;

function TAssembler80.ActDirSet(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := IntToStr(ParserAsInt(-1));
  message := FSymbols.Define(FPass,False,symbolname,expression,true);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler80.ActDirTitle(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  if FTitle <> '' then
    Monitor(ltWarning,'TITLE has previously been set');
  FTitle := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler80.ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Undefine(symbolname);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler80.ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  Monitor(ltWarning,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler80.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,ParserAsInt(-3) + ParserAsInt(-1));
end;

function TAssembler80.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,ParserAsInt(-3) and ParserAsInt(-1));
end;

function TAssembler80.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,ParserAsInt(-2));
end;

function TAssembler80.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,ParserAsInt(-3) div ParserAsInt(-1));
end;

function TAssembler80.ActExprListItem(_parser: TLCGParser): TLCGParserStackEntry;
var r:    TExprListItem;
begin
  Result := EmptyStackEntry;
  case _parser.ParserStack[_parser.ParserSP-1].BufType of
    pstString:
      begin
        r.ItemType := litString;
        r.StringValue := _parser.ParserStack[_parser.ParserSP-1].Buf;
        FExprList.Add(r);
        SetStringEntry(Result,'');
      end;
    pstINT32:
      begin
        r.ItemType := litInteger;
        r.IntegerValue := ParserAsInt(-1);
        FExprList.Add(r);
        SetStringEntry(Result,'');
      end;
    otherwise
      Monitor(ltInternal,'Parser stack type not catered for');
  end;
end;

function TAssembler80.ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,-ParserAsInt(-1));
end;

function TAssembler80.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) mod ParserAsInt(-1));
end;

function TAssembler80.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) * ParserAsInt(-1));
end;

function TAssembler80.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,ParserAsInt(-1) xor $FFFFFFFF);
end;

function TAssembler80.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) or ParserAsInt(-1));
end;

function TAssembler80.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) shl ParserAsInt(-1));
end;

function TAssembler80.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) shr ParserAsInt(-1));
end;

function TAssembler80.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) - ParserAsInt(-1));
end;

function TAssembler80.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-3) xor ParserAsInt(-1));
end;

function TAssembler80.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, (ParserAsInt(-2) shr 8) and $FF);
end;

function TAssembler80.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
var expr:     integer;
    trueval:  integer;
    falseval: integer;
    resval:   integer;
begin
  Result := EmptyStackEntry;
  expr     := ParserAsInt(-6);
  trueval  := ParserAsInt(-4);
  falseval := ParserAsInt(-2);
  if expr <> 0 then
    resval := trueval
  else
    resval := falseval;
  SetIntEntry(Result, resval);
end;

function TAssembler80.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result, ParserAsInt(-2) and $FF);
end;

function TAssembler80.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,IntFromHexLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler80.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
end;

function TAssembler80.ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (ParserAsInt(-3) <> 0) and (ParserAsInt(-1) <> 0) then
    Result := Logical1
  else
    Result := Logical0;
end;

function TAssembler80.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (ParserAsInt(-1) = 0) then
    Result := Logical1
  else
    Result := Logical0;
end;

function TAssembler80.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (ParserAsInt(-3) <> 0) or (ParserAsInt(-1) <> 0) then
    Result := Logical1
  else
    Result := Logical0;
end;

function TAssembler80.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,IntFromOctLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler80.ActOpcode0(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := DoOpcode(_parser,0);
end;

function TAssembler80.ActOpcode1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := DoOpcode(_parser,1);
end;

function TAssembler80.ActOpcode2(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := DoOpcode(_parser,2);
end;

function TAssembler80.ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
var index_reg: string;
    operand_opt: TOperandOption;
begin
  Result := EmptyStackEntry;
  // Index operand like [IX], [IY]
  index_reg  := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
  operand_opt := OPER_NULL;
  if index_reg = 'IX' then
    operand_opt := OPER_IX_IND
  else if index_reg = 'IY' then
    operand_opt := OPER_IY_IND
  else
      Monitor(ltInternal,'Could not cater for index register %s',[index_reg]);
  if operand_opt <> OPER_NULL then
    PushOperand(operand_opt,0);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActSetOpIndOff(_parser: TLCGParser): TLCGParserStackEntry;
var index_reg: string;
    expr:      int32;
    operand_opt: TOperandOption;
begin
  Result := EmptyStackEntry;
  // Index+Offset operand like [IX+2], [IY+020H]
  index_reg  := UpperCase(_parser.ParserStack[_parser.ParserSP-4].Buf);
  expr       := ParserAsInt(-2);
  operand_opt := OPER_NULL;
  if index_reg = 'IX' then
    operand_opt := OPER_IXPD_IND
  else if index_reg = 'IY' then
    operand_opt := OPER_IYPD_IND
  else
      Monitor(ltInternal,'Could not cater for index register %s',[index_reg]);
  if operand_opt <> OPER_NULL then
    PushOperand(operand_opt,expr);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActSetOpSimple(_parser: TLCGParser): TLCGParserStackEntry;
var op_reg: string;
    operand_opt: TOperandOption;
begin
  Result := EmptyStackEntry;
  // Simple operand like A, IX, P, etc.
  op_reg  := _parser.ParserStack[_parser.ParserSP-1].Buf;
  op_reg := UpperCase(op_reg);
  operand_opt := FInstructionList.SimpleOpToOperandOption(op_reg);
  if operand_opt = OPER_NULL then
    Monitor(ltInternal,'Operand type %s not valid',[op_reg])
  else
    PushOperand(operand_opt,0);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActSetOpSimpleW(_parser: TLCGParser): TLCGParserStackEntry;
var op_reg: string;
    operand_opt: TOperandOption;
begin
  Result := EmptyStackEntry;
  // Simple operand wrapped in brackets (C), (HL), etc.
  op_reg  := _parser.ParserStack[_parser.ParserSP-2].Buf;
  op_reg := '(' + UpperCase(op_reg) + ')';
  operand_opt := FInstructionList.SimpleOpToOperandOption(op_reg);
  if operand_opt = OPER_NULL then
    Monitor(ltInternal,'Operand type %s not valid',[op_reg])
  else
    PushOperand(operand_opt,0);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActSetOpBracketed(_parser: TLCGParser): TLCGParserStackEntry;
var expr:      int32;
    operand_opt: TOperandOption;
begin
  Result := EmptyStackEntry;
  // Operand like [NN], [0x200], [5]
  expr       := ParserAsInt(-2);
  operand_opt := OPER_U16_IND;
  if operand_opt <> OPER_NULL then
    PushOperand(operand_opt,expr);
  SetStringEntry(Result,'');
end;

function TAssembler80.ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
var expr:      int32;
    operand_opt: TOperandOption;
begin
  Result := EmptyStackEntry;
  // Operand like NN, 0x200, 5
  expr       := ParserAsInt(-1);
  operand_opt := OPER_U16;
  PushOperand(operand_opt,expr);
  SetStringEntry(Result,'');
end;


function TAssembler80.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,StripQuotesAndEscaped(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler80.ActSymbolDef(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-2].Buf;
  if (Length(symbolname) > 1) and (RightStr(symbolname,1) = ':') then
    symbolname := LeftStr(symbolname,Length(symbolname)-1); // Remove trailing colon
  message := FSymbols.Define(FPass,symbolname,FOrg);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler80.ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,FOrg);
end;

function TAssembler80.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,StrToInt(FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf,IntToStr(FOrg),FIfStack)));
end;

procedure TAssembler80.Assemble;
begin
  FilesOpen;
  FAssemblyStart := Now;
  Monitor(ltVerbose,'Assembly started %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',FAssemblyStart)]);
  InitStart;
  try
    AssemblePass(1);
    AssemblePass(2);
    WriteMapFile;
    {
    if FilenameDbg <> '' then
      FDebugList.SaveToFile(FilenameDbg);
    }
    if FilenameLst <> '' then
      FListing.SaveToFile(FilenameLst);
    if FBytesTotal > 0 then
      begin
        if FilenameCom <> '' then
          FOutput.SaveCom(FilenameCom);
        FOutput.SaveHex(FilenameHex);
        {
        FOutput.SaveObject(FilenameObj);
        }
      end;
  finally
    Monitor(ltInfo,'%d lines assembled, %d bytes generated',[FLineCount, FBytesTotal]);
    FAssemblyEnd := Now;
    Monitor(ltVerbose,'Assembly ended %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',FAssemblyEnd)]);
    Monitor(ltVerbose,'Total assembly time %.3f seconds',[(FAssemblyEnd-FAssemblyStart)*86400.0]);
    FilesClose;
  end;
end;

procedure TAssembler80.AssemblePass(_pass: integer);
begin
  FPass := _pass;
  Monitor(ltInfo,'ASSEMBLER PASS %d',[_pass]);
  InitPass;
  ProcessFile(FFilenameSrc);
  if FDefiningMacro then
    Monitor(ltError,'Pass terminated unexpectedly in the middle of a .MACRO block');
end;

function TAssembler80.DoOpcode(_parser: TLCGParser; _operands: integer): TLCGParserStackEntry;
var opcindex: integer;
    opcode:   string;
    instrec:  TInstructionRec;
    displacement: integer;
begin
  Result := EmptyStackEntry;
  // Format in stack SP-4 | SP-3 | SP-2 | SP-1 is
  // T_OPCODE | operand1 | , | operand2 when _operands is 2
  // stack SP-2 | SP-1 when _operands is 1
  // stack SP-1 when _operands is 0
  displacement := 1;
  case _operands of
    0: displacement := 1;
    1: displacement := 2;
    2: displacement := 4;
    otherwise
      Monitor(ltInternal,'Operand count %d not catered for',[_operands]);
  end; // Case
  opcode := _parser.ParserStack[_parser.ParserSP-displacement].Buf;
  opcode := UpperCase(opcode);
  opcindex := -1;
  if not FInstructionList.FindOpcode(opcode,opcindex) then
    Monitor(ltError,'Opcode mnemonic %s not available with processor type %s',[opcode,FProcessor])
  else if FOperandIndex <> _operands then
    Monitor(ltInternal,'Did not find the expected %d operands',[_operands])
  else
    begin
      if not FInstructionList.FindInstruction(opcindex,FOperands[0].oper_opt,FOperands[1].oper_opt,instrec{%H-}) then
        case _operands of
          0: Monitor(ltError,'Could not find instruction to match opcode %s',[opcode]);
          1: Monitor(ltError,'Could not find instruction to match opcode %s with operand %s',[opcode,OperandStrings[FOperands[0].oper_opt]]);
          2: Monitor(ltError,'Could not find instruction to match opcode %s with operands %s, %s',[opcode,OperandStrings[FOperands[0].oper_opt],OperandStrings[FOperands[1].oper_opt]]);
        end // Case
      else
        PumpCode(instrec);
    end;
  SetStringEntry(Result,'');
end;

procedure TAssembler80.FilesClose;
begin
  if Assigned(FStreamLog) then
    begin
      Monitor(ltDebug,'Closing %s',[FilenameLog]);
      FreeAndNil(FStreamLog);
    end;
end;

procedure TAssembler80.FilesOpen;
begin
  try
    Monitor(ltDebug,'Opening %s',[FilenameLog]);
    if FilenameLog <> '' then
      FStreamLog := TFileStream.Create(FilenameLog,fmCreate,fmShareDenyWrite);
  except
    Monitor(ltError,'Unable to create log file %s',[FilenameLog]);
  end;
end;

function TAssembler80.GetSource: string;
begin
  Result := '';
  if FFileStack.Count > 0 then
    Result := FFileStack.Filename;
end;

procedure TAssembler80.InitLine;
begin
//  FAddrMode := ADM_IMPL;
//  FOpCode := OPC_NOP;
  FBytesFromLine := 0;
  FOperandIndex := 0;  // Starts off at 0, will accept 0/1/2 operands in total
  FOperands[0].oper_opt := OPER_NULL;
  FOperands[0].value    := 0;
  FOperands[1].oper_opt := OPER_NULL;
  FOperands[1].value    := 0;
  FExprList.Clear;
end;

procedure TAssembler80.InitPass;
var i: integer;
    symbolstring: string;
    symbolname:   string;
    symbolval:    string;
    symbolvaln:   integer;
    eqpos:        integer;
    message:      string;
begin
  FBytesTotal := 0;
  FDefiningMacro := False;
  FEnded     := False;
  FForceList := False;
  FLineCount := 0;
  FList := True;
  FListNext := True;
  FMacroList.Clear;
  FMacroList.Init;
  FMacroNestLevel := 0;
  FOrg := DEFAULT_ORG;
  FOutput.Clear;
  FTitle := '';
  // Now add the predefined symbols if present on the command line
  for i := 0 to CmdDefines.Count-1 do
    begin
      symbolstring := CmdDefines[i];
      eqpos := Pos('=',symbolstring);
      if eqpos = 0 then
        message := FSymbols.Define(FPass,symbolstring)
      else
        begin // Symbol AND defined value
          symbolname := Copy(symbolstring,1,eqpos-1);
          symbolval  := Copy(symbolstring,eqpos+1,9999);
          try
            symbolvaln := StrToInt(symbolval);
            message := FSymbols.Define(FPass,symbolname,symbolvaln);
          except
            message := FSymbols.Define(FPass,symbolname,symbolval);
          end;
        end;
      if message <> '' then
        Monitor(ltError,message);
    end;
end;

procedure TAssembler80.InitStart;
begin
  Monitor(ltWarAndPeace,'Processor type = %s',[FProcessor]);
  Monitor(ltWarAndPeace,'Defines = %s',[CmdDefines.DelimitedText]);
  Monitor(ltWarAndPeace,'Includes = %s',[CmdIncludes.DelimitedText]);
  Monitor(ltWarAndPeace,'D80 filename = %s',[FilenameDbg]);
  Monitor(ltWarAndPeace,'HEX filename = %s',[FilenameHex]);
  Monitor(ltWarAndPeace,'LST filename = %s',[FilenameLst]);
  Monitor(ltWarAndPeace,'LOG filename = %s',[FilenameLog]);
  Monitor(ltWarAndPeace,'MAP filename = %s',[FilenameMap]);
  Monitor(ltWarAndPeace,'O80 filename = %s',[FilenameObj]);
  Monitor(ltWarAndPeace,'tab value = %d',[TabSize]);
end;

function TAssembler80.Logical0: TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,0);
end;

function TAssembler80.Logical1: TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetIntEntry(Result,1);
end;

procedure TAssembler80.Monitor(LogType: TLCGLogType; const Message: string);
var s: string;
    prefix: string;
    lineinfo: string;
    line,column: integer;
    msg: string;
    raisedin: string;
    i: integer;
begin
  if LogType > FLogLevel then
    Exit;
  // Sort out prefix
  case LogType of
    ltInternal:    prefix := 'INTERNAL';
    ltError:       prefix := '   ERROR';
    ltWarning:     prefix := ' WARNING';
    ltInfo,
    ltVerbose,
    ltWarAndPeace: prefix := '    INFO';
    ltDebug:       prefix := '   DEBUG';
    otherwise      prefix := '????????';
  end;
  line := 0;
  column := 0;
  if FFileStack.Count > 0 then
    line := FFileStack.InputLine;
  if LogType <= ltWarning then
    column := FInputColumnSave;
  lineinfo := '';
  if line > 0 then
    begin
      if column > 0 then
        lineinfo := Format('[%d:%d] ',[line,column])
      else
        lineinfo := Format('[%d] ',[line]);
    end;
  raisedin := Source;
  msg := Format('%s: %s%s',[prefix,lineinfo,message]);
  if (LogType in [ltInternal,ltError,ltWarning]) and (raisedin <> '') then
    msg := msg + ', raised in ' + raisedin;
  if FLogLevel >= ltVerbose then
    for i := FFileStack.Count-2 downto 0 do
      msg := msg + #13 + #10 + '          > ' + FFileStack[i].Filename;

  if Assigned(FStreamLog)  then
    begin
      s := msg + #13 + #10;
      FStreamLog.Write(s[1],Length(s));
    end;
  inherited Monitor(LogType,msg);
end;

procedure TAssembler80.Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const);
begin
  Monitor(LogType,Format(Message,Args));
end;

procedure TAssembler80.OutputDebugLine(const _asmline: string);
var addr:     uint16;
    ent:      TDebugEntry;
begin
  if (FPass = 1) then
    Exit;
  if FBytesFromLine > 0 then
    addr := FOrg
  else
    addr := $FFFF;
  ent := TDebugEntry.Create(addr,FBytesFromLine,FOutputArr,_asmline);
  FDebugList.Add(ent);
end;

procedure TAssembler80.OutputListingLine(const _asmline: string);
const MAX_BYTES_PER_ROW = 4;
      HEX_WIDTH = MAX_BYTES_PER_ROW * 3 + 7;
      MAX_MACRO_INDENTS = 3;
{$DEFINE SHORTEN_OVERSPILL}
var byteindex: integer;
    bytesleft: integer;
    bcount:    integer;
    i:         integer;
    s:         string;
begin
  if (FPass = 1) or
     (not FList) or
     (not FFileStack.IsListing) then
    Exit;
  if (FListing.Count = 0) and (FTitle <> '') then
    begin
      FListing.Add(FTitle);
      FListing.Add('');
    end;
  if FBytesFromLine = 0 then
    FListing.Add(StringOfChar(' ',HEX_WIDTH+MAX_MACRO_INDENTS) + _asmline)
  else
    begin
      byteindex := 0;
      bytesleft := FBytesFromLine;
      bcount := bytesleft;
      if bcount > MAX_BYTES_PER_ROW then
        bcount := MAX_BYTES_PER_ROW;
      bytesleft := bytesleft - bcount;
      s := Format('%4.4X:',[byteindex+FOrg]);
      for i := 0 to bcount-1 do
        s := s + Format(' %2.2X',[FOutputArr[i+byteindex]]);
{$IFDEF SHORTEN_OVERSPILL}
      if bytesleft > 0 then
        s := s + '+';
{$ENDIF}
      s := PadRight(s,HEX_WIDTH);
      if FMacroNestLevel > MAX_MACRO_INDENTS then
        s := s + '>' + StringOfChar('.',MAX_MACRO_INDENTS-2) + '>'
      else if FMacroNestLevel = 0 then
        s := s + Space(MAX_MACRO_INDENTS)
      else
        begin
          s := s + StringOfChar('>',FMacroNestLevel);
          s := s + StringOfChar(' ',MAX_MACRO_INDENTS-FMacroNestLevel);
        end;
      s := s + _asmline;
      FListing.Add(s);
      byteindex := byteindex + bcount;
{$IFNDEF SHORTEN_OVERSPILL}
      // Do any overspill lines if required
      while bytesleft > 0 do
        begin
          bcount := bytesleft;
          if bcount > MAX_BYTES_PER_ROW then
            bcount := MAX_BYTES_PER_ROW;
          bytesleft := bytesleft - bcount;
          s := Format('%4.4X:',[byteindex+FOrg]);
          for i := 0 to bcount-1 do
            s := s + Format(' %2.2X',[FOutputArr[i+byteindex]]);
          FListing.Add(s);
          byteindex := byteindex + bcount;
        end;
{$ENDIF}
    end;
end;
 
function TAssembler80.ParserAsInt(_index: integer): int32;
var entry: TLCGParserStackEntry;
begin
  entry := ParserStack[ParserSP+_index];
  if entry.BufType = pstINT32 then
    Result := entry.BufInt
  else
    begin  // Attempt to convert the string value to an integer
      Result := 0;
      if Length(entry.Buf) = 0 then
        Monitor(ltError,'Attempting to convert null string to a numeric value')
      else
        begin
          if Length(entry.Buf) > 1 then
            Monitor(ltWarning,'Only first character of string used to determine value');
          Result := Ord(entry.Buf[1]);
        end;
    end;
end;

procedure TAssembler80.ProcessFile(const _fn: string; _listing: boolean);
begin
  Monitor(ltWarAndPeace,'Processing file %s',[_fn]);
  FFileStack.Push(_fn,_listing);
  try
    ProcessFileInner;
  finally
    FFileStack.Pop();
  end;
end;

procedure TAssembler80.ProcessFileInner;
var asmline: string;
    incl:    string;
    list:    boolean;
    anew:    integer;
begin
  while not FFileStack.EOF do
    begin
      InitLine;
      asmline := FFileStack.GetLine;
      if LogLevel >= ltDebug then
        Monitor(ltDebug,'> ' + asmline);
      Inc(FLineCount);
      // Check for macro definition
      if FDefiningMacro then
        FMacroCapture.Add(asmline);
      // Assemble the line here
      if asmline <> '' then
        begin
          ProcessLine(asmline);
          // Check for assembly after end
          if FEnded and (FBytesFromLine > 0) then
            Monitor(ltError,'Code generated after END directive');
          // Add to debug
          OutputDebugLine(asmline);
          // Add to listing
          OutputListingLine(asmline);
          // Bump org
          anew := FOrg;
          anew := anew + FBytesFromLine;
          FOrg := anew and $FFFF;
          if (anew > $FFFF) and (FPass = 2) then
            Monitor(ltWarning,'ORG has wrapped back round to zero');
          FBytesTotal := FBytesTotal + FBytesFromLine;
          // Check to see if listing flag has changed
          FList := FListNext;
          // Check to see if there is a macro to expand
          if FProcessMacro <> '' then
            ProcessMacroExpansion;
          // Check to see if we created a new include file to use
          if FIncludeNext <> '' then
            begin
              incl := FIncludeNext;
              list := FForceList;
              FIncludeNext := '';
              FForceList := False;
              ProcessFile(incl,list); // Recursive!!!
            end;
        end
      else
        OutputListingLine(asmline);
    end;
end;

procedure TAssembler80.ProcessMacroExpansion;
var parms:      TStringList;
    index:   integer;
    sl:      TStringList;
    savedprefix: string;
begin
  // Insert the macro definition into the current "stream"
  index := FMacroList.IndexOf(FProcessMacro);
  if index < 0 then
    Monitor(ltError,'Macro %s is not defined',[FProcessMacro]);
  parms := TStringList.Create;
  Inc(FMacroNestLevel);
  try
    savedprefix := FLocalPrefix;
    FLocalPrefix := FMacroList.LocalPrefix;
    parms.Delimiter := ',';
    parms.StrictDelimiter := True;
    parms.DelimitedText := FProcessParms;
    sl := FMacroList.Items[index].FList;
    FFileStack.PushMacro(FProcessMacro,sl,parms);
    FProcessMacro := '';
    FProcessParms := '';
    ProcessFileInner;
    FFileStack.Pop;
    FLocalPrefix := savedprefix;
  finally
    parms.Free;
    Dec(FMacroNestLevel);
  end;
end;

function TAssembler80.ProcessingAllowed: boolean;
begin
  Result := FIfStack.Allowed and (not FDefiningMacro);
end;

procedure TAssembler80.ProcessLine(const _line: string);
var strm: TStringStream;
begin
  if (Length(_line) > 0) and (_line[1] = '*') then
    Exit; // Comment line
  strm := TStringStream.Create(_line);
  try
    // Preparse first
    FPreparser.InitRun;
    FPreparser.Parse(strm);
    Parse(strm);
  finally
    strm.Free;
  end;
end;

procedure TAssembler80.PumpCode(_r: TInstructionRec);
var i: integer;
    o: integer;
    rst_addr: integer;
    rel:      integer;

  procedure PumpByte(_v: integer);
  begin
    if (_v < -128) or (_v > 255) then
      Monitor(ltError,'Instruction byte with value %8.8X cannot be used',[_v])
    else
      begin
        FOutputArr[o] := _v and $FF;
        Inc(o);
      end;
  end;

  procedure PumpWord(_v: integer);
  begin
    if (_v < -32768) or (_v > 65535) then
      Monitor(ltError,'Instruction word with value %8.8X cannot be used',[_v])
    else
      begin
        FOutputArr[o]   := _v and $FF;
        FOutputArr[o+1] := (_v shr 8) and $FF;
        Inc(o,2);
      end;
  end;

begin
  // Pump the code out from the instruction record
  if ProcessingAllowed then
    begin
      FBytesFromLine := _r.CodeElementSize;
      SetLength(FOutputArr,FBytesFromLine);
      o := 0;
      for i := 0 to _r.CodeElementCount - 1 do
        with _r.CodeElements[i] do
          begin
            case ElementType of
              cetB3:  PumpByte(Value or (FOperands[OperandNo-1].value shl Offset));
              cetHex: PumpByte(Value);
              cetIM:  case FOperands[OperandNo-1].value of
                        0: PumpByte($46);
                        1: PumpByte($56);
                        2: PumpByte($5E);
                        otherwise
                          Monitor(ltError,'Operand %d is not valid for IM instruction',[FOperands[OperandNo-1].value]);
                      end;
              cetR8:  begin
                        rel := FOperands[OperandNo-1].value - (FORG + 2);
                        if (rel < -128) or (rel > 127) then
                          Monitor(ltError,'Relative jump value out of range')
                        else
                          PumpByte(rel);
                      end;
              cetRST: begin
                        rst_addr := FOperands[OperandNo-1].value;
                        if (rst_addr >= 8) and (rst_addr < 64) then
                          begin
                            if rst_addr mod 8 = 0 then
                                rst_addr := rst_addr div 8
                            else
                              Monitor(ltError,'Operand %d is not valid for RST instruction',[FOperands[OperandNo-1].value]);
                          end;
                        PumpByte(Value or (rst_addr shl Offset));
                      end;
              cetS8:  begin
                        rel := FOperands[OperandNo-1].value;
                        if (rel < -128) or (rel > 127) then
                          Monitor(ltError,'Signed 8 bit value out of range')
                        else
                          PumpByte(rel);
                      end;
              cetU8:  PumpByte(FOperands[OperandNo-1].value);
              cetU16: PumpWord(FOperands[OperandNo-1].value);
              otherwise
                raise Exception.Create('Code element not catered for');
            end;
          end;
      FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
    end;
end;

procedure TAssembler80.PushOperand(_op: TOperandOption; _v: integer);
begin
  if FOperandIndex >= MAX_OPERANDS then
    Monitor(ltInternal,'Maximum number of operands exceeded')
  else
    begin
      FOperands[FOperandIndex].oper_opt := _op;
      FOperands[FOperandIndex].value    := _v;
      Inc(FOperandIndex);
    end;
end;

function TAssembler80.Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  Result := EmptyStackEntry;
  SetStringEntry(Result,'');
  if Assigned(FProcArray[RuleIndex]) then
    Result := FProcArray[RuleIndex](Parser)
  else
    Monitor(ltInternal,'Code not defined for rule no. %d (%s)',[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TAssembler80.RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
var i: integer;
    done_one: boolean;
begin
  done_one := False;
  for i := 0 to Rules-1 do
    if _procs[i] = _procname then
      begin
        FProcArray[i] := _proc;
        done_one := True;
      end;
  if not done_one then
    Monitor(ltInternal,'Could not find procedure %s in grammar',[_procname]);
end;

procedure TAssembler80.RegisterProcs;
var _procs: TStringArray;
begin
  _procs := RuleProcs;
  RegisterProc('ActBinLiteral',     @ActBinLiteral, _procs);
  RegisterProc('ActCopy1',          @ActCopy1, _procs);
  RegisterProc('ActCompEQ',         @ActCompEQ, _procs);
  RegisterProc('ActCompGE',         @ActCompGE, _procs);
  RegisterProc('ActCompGT',         @ActCompGT, _procs);
  RegisterProc('ActCompLE',         @ActCompLE, _procs);
  RegisterProc('ActCompLT',         @ActCompLT, _procs);
  RegisterProc('ActCompNE',         @ActCompNE, _procs);
  RegisterProc('ActDecLiteral',     @ActDecLiteral, _procs);
  RegisterProc('ActDirCPU',         @ActDirCPU, _procs);
  RegisterProc('ActDirDB',          @ActDirDB, _procs);
  RegisterProc('ActDirDC',          @ActDirDC, _procs);
  RegisterProc('ActDirDefine',      @ActDirDefine, _procs);
  RegisterProc('ActDirDefineExpr',  @ActDirDefineExpr, _procs);
  RegisterProc('ActDirDefineExprC', @ActDirDefineExprC, _procs);
  RegisterProc('ActDirDefMacro',    @ActDirDefMacro, _procs);
  RegisterProc('ActDirDS',          @ActDirDS, _procs);
  RegisterProc('ActDirDS2',         @ActDirDS2, _procs);
  RegisterProc('ActDirDW',          @ActDirDW, _procs);
  RegisterProc('ActDirElse',        @ActDirElse, _procs);
  RegisterProc('ActDirEnd',         @ActDirEnd, _procs);
  RegisterProc('ActDirEndif',       @ActDirEndif, _procs);
  RegisterProc('ActDirEndm',        @ActDirEndm, _procs);
  RegisterProc('ActDirError',       @ActDirError, _procs);
  RegisterProc('ActDirIf',          @ActDirIf, _procs);
  RegisterProc('ActDirIfdef',       @ActDirIfdef, _procs);
  RegisterProc('ActDirIfndef',      @ActDirIfndef, _procs);
  RegisterProc('ActDirInclude',     @ActDirInclude, _procs);
  RegisterProc('ActDirIncludeList', @ActDirIncludeList, _procs);
//RegisterProc('ActDirList',        @ActDirList, _procs);
  RegisterProc('ActDirMacro',       @ActDirMacro, _procs);
  RegisterProc('ActDirMacroNoexpr', @ActDirMacroNoexpr, _procs);
  RegisterProc('ActDirMessage',     @ActDirMessage, _procs);
//RegisterProc('ActDirNolist',      @ActDirNolist, _procs);
  RegisterProc('ActDirOrg',         @ActDirOrg, _procs);
  RegisterProc('ActDirSet',         @ActDirSet, _procs);
  RegisterProc('ActDirTitle',       @ActDirTitle, _procs);
  RegisterProc('ActDirUndefine',    @ActDirUndefine, _procs);
  RegisterProc('ActDirWarning',     @ActDirWarning, _procs);
  RegisterProc('ActExprAdd',        @ActExprAdd, _procs);
  RegisterProc('ActExprAnd',        @ActExprAnd, _procs);
  RegisterProc('ActExprBracket',    @ActExprBracket, _procs);
  RegisterProc('ActExprDiv',        @ActExprDiv, _procs);
  RegisterProc('ActExprListItem',   @ActExprListItem, _procs);
  RegisterProc('ActExprUnaryMinus', @ActExprUnaryMinus, _procs);
  RegisterProc('ActExprMod',        @ActExprMod, _procs);
  RegisterProc('ActExprMul',        @ActExprMul, _procs);
  RegisterProc('ActExprNot',        @ActExprNot, _procs);
  RegisterProc('ActExprOr',         @ActExprOr, _procs);
  RegisterProc('ActExprShl',        @ActExprShl, _procs);
  RegisterProc('ActExprShr',        @ActExprShr, _procs);
  RegisterProc('ActExprSub',        @ActExprSub, _procs);
  RegisterProc('ActExprXor',        @ActExprXor, _procs);
  RegisterProc('ActFuncHigh',       @ActFuncHigh, _procs);
  RegisterProc('ActFuncIif',        @ActFuncIif, _procs);
  RegisterProc('ActFuncLow',        @ActFuncLow, _procs);
  RegisterProc('ActHexLiteral',     @ActHexLiteral, _procs);
  RegisterProc('ActIgnore',         @ActIgnore, _procs);
  RegisterProc('ActLogAnd',         @ActLogAnd, _procs);
  RegisterProc('ActLogNot',         @ActLogNot, _procs);
  RegisterProc('ActLogOr',          @ActLogOr, _procs);
  RegisterProc('ActOctLiteral',     @ActOctLiteral, _procs);
  RegisterProc('ActOpcode0',        @ActOpcode0, _procs);
  RegisterProc('ActOpcode1',        @ActOpcode1, _procs);
  RegisterProc('ActOpcode2',        @ActOpcode2, _procs);
  RegisterProc('ActSetOpBracketed', @ActSetOpBracketed, _procs);
  RegisterProc('ActSetOpInd',       @ActSetOpInd, _procs);
  RegisterProc('ActSetOpIndOff',    @ActSetOpIndOff, _procs);
  RegisterProc('ActSetOpLiteral',   @ActSetOpLiteral, _procs);
  RegisterProc('ActSetOpSimple',    @ActSetOpSimple, _procs);
  RegisterProc('ActSetOpSimpleW',   @ActSetOpSimpleW, _procs);
  RegisterProc('ActStringConstant', @ActStringConstant, _procs);
  RegisterProc('ActSymbolDef',      @ActSymbolDef, _procs);
  RegisterProc('ActValueOrg',       @ActValueOrg, _procs);
  RegisterProc('ActValueSymbol',    @ActValueSymbol, _procs);
end;

procedure TAssembler80.SetFilenameSrc(const _fn: string);
begin
  FFilenameSrc := _fn;
  if FilenameObj  = '' then FilenameObj  := ChangeFileExt(_fn,FILETYPE_OBJECT);
end;

procedure TAssembler80.SetIntEntry(var _e: TLCGParserStackEntry; _intval: int32);
begin
  _e.BufInt  := _intval;
  _e.BufType := pstINT32;
  _e.Buf     := '';
end;

procedure TAssembler80.SetOnMonitor(_monitor: TLCGMonitorProc);
begin
  FOnMonitor := _monitor;
  if Assigned(FFileStack) then
    FFileStack.OnMonitor := _monitor;
end;

procedure TAssembler80.SetStringEntry(var _e: TLCGParserStackEntry; _strval: string);
begin
  _e.BufInt  := 0;
  _e.BufType := pstString;
  _e.Buf     := _strval;
end;

procedure TAssembler80.WriteMapFile;
var sl: TStringList;
    procedure MapTitle;
    begin
      if FTitle <> '' then
        begin
          sl.Add(FTitle);
          sl.Add('');
        end;
    end;
begin
  if FilenameMap = '' then
    Exit;
  sl := TStringList.Create;
  try
    FSymbols.SortByName;
    MapTitle;
    sl.Add('SYMBOLS BY NAME');
    sl.Add('');
    FSymbols.Dump(sl);
    FSymbols.SortByAddr;
    sl.Add('');
    sl.Add('');
    sl.Add('');
    MapTitle;
    sl.Add('SYMBOLS BY VALUE');
    sl.Add('');
    FSymbols.Dump(sl);
    sl.SaveToFile(FilenameMap);
  finally
    sl.Free;
  end;
end;

end.

