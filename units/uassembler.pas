{$WARN 5024 off : Parameter "$1" not used}
unit uassembler;

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
  uinstruction, Generics.Collections;


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

  TAssembler = class(TLCGParser)
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
      FProcArray:      array of TLCGParserProc;
      FProcessMacro:   string;
      FProcessor:      string;
      FProcessParms:   string;
      FStreamLog:      TFileStream;
      FSymbols:        TSymbolTable;
      FVerbose:        boolean;
      function  ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCopy2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirByte(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineString(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefmacro(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSH(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
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
      function  ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprA8(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprA16(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprCL(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprListA8orStr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprListStr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprS8(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprU16(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLExprI(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLExprS(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
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
      function  ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSymbolDef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      function  DoOpcode(_parser: TLCGParser; _operands: integer): TLCGParserStackEntry;
      procedure FilesClose;
      procedure FilesOpen;
      function  GetSource: string;
      procedure InitLine;
      procedure InitPass;
      procedure InitStart;
      procedure OutputDebugLine(const _asmline: string);
      procedure OutputListingLine(const _asmline: string);
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
      procedure SetOnMonitor(_monitor: TLCGMonitorProc);
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
  uexpression, strutils, fileinfo, uutility;

{
const CodeTable: array[TOpCode,TAddrMode] of integer = (
          (109,125,121,-1,-1,113,105,-1,97,101,117,-1,-1),
          (45,61,57,-1,-1,49,41,-1,33,37,53,-1,-1),
          (14,30,-1,-1,-1,-1,-1,-1,-1,6,22,-1,10),
          (-1,-1,-1,-1,-1,-1,-1,144,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,176,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,240,-1,-1,-1,-1,-1),
          (44,-1,-1,-1,-1,-1,-1,-1,-1,36,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,48,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,208,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,16,-1,-1,-1,-1,-1),
          (-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,80,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,112,-1,-1,-1,-1,-1),
          (-1,-1,-1,24,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,216,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,88,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,184,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (205,221,217,-1,-1,209,201,-1,193,197,213,-1,-1),
          (236,-1,-1,-1,-1,-1,224,-1,-1,228,-1,-1,-1),
          (204,-1,-1,-1,-1,-1,192,-1,-1,196,-1,-1,-1),
          (206,222,-1,-1,-1,-1,-1,-1,-1,198,214,-1,-1),
          (-1,-1,-1,202,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,136,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (77,93,89,-1,-1,81,73,-1,65,69,85,-1,-1),
          (238,254,-1,-1,-1,-1,-1,-1,-1,230,246,-1,-1),
          (-1,-1,-1,232,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,200,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (76,-1,-1,-1,108,-1,-1,-1,-1,-1,-1,-1,-1),
          (32,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (173,189,185,-1,-1,177,169,-1,161,165,181,-1,-1),
          (174,-1,190,-1,-1,-1,162,-1,-1,166,-1,182,-1),
          (172,188,-1,-1,-1,-1,160,-1,-1,164,180,-1,-1),
          (78,94,-1,-1,-1,-1,-1,-1,-1,70,86,-1,74),
          (-1,-1,-1,234,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (13,29,25,-1,-1,17,9,-1,1,5,21,-1,-1),
          (-1,-1,-1,72,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,104,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,40,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (46,62,-1,-1,-1,-1,-1,-1,-1,38,54,-1,42),
          (110,126,-1,-1,-1,-1,-1,-1,-1,102,118,-1,106),
          (-1,-1,-1,64,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (237,253,249,-1,-1,241,233,-1,225,229,245,-1,-1),
          (-1,-1,-1,56,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,248,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,120,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (141,157,153,-1,-1,145,-1,-1,129,133,149,-1,-1),
          (142,-1,-1,-1,-1,-1,-1,-1,-1,134,-1,150,-1),
          (140,-1,-1,-1,-1,-1,-1,-1,-1,132,148,-1,-1),
          (-1,-1,-1,170,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,168,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,186,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,138,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,154,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,152,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1));

  InstructionSizes: array[TAddrMode] of integer =
    (
      3,       // ADM_ABS
      3,       // ADM_ABSX
      3,       // ADM_ABSY
      1,       // ADM_IMPL
      3,       // ADM_IND
      2,       // ADM_INDY
      2,       // ADM_LIT
      2,       // ADM_REL
      2,       // ADM_XIND
      2,       // ADM_ZPG
      2,       // ADM_ZPGX
      2,       // ADM_ZPGY
      1        // ADM_ACC
    );
}

{ TAssembler }

constructor TAssembler.Create(const _grammar: string; const _processor: string);
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
  FPass := 0;
  FTabSize := 4;
  FVerbose := False;
  FOnReduce := @Reduce;
  RegisterProcs;
end;

destructor TAssembler.Destroy;
begin
  FreeAndNil(FExprList);
  FreeAndNil(FInstructionList);
  FCmdIncludes.Free;
  FCmdDefines.Free;
  FMacroList.Free;
  FListing.Free;
  FDebugList.Free;
  FOutput.Free;
  FIfStack.Free;
  FFileStack.Free;
  FSymbols.Free;
  inherited Destroy;
end;

function TAssembler.ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromBinLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(Ord(_parser.ParserStack[_parser.ParserSP-1].Buf[2]));
end;

function TAssembler.ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) =
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) >=
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) >
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <=
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
  result.Buf := '1'
else
  result.Buf := '0';
end;

function TAssembler.ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <>
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActCopy2(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-2].Buf;
end;

function TAssembler.ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
var s: string;
begin
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  // Get rid of the trailing D if present
  if (Length(s) > 0) and (UpperCase(RightStr(s,1)) = 'D') then
    s := LeftStr(s,Length(s)-1);
  Result.Buf := IntToStr(StrToInt(s));
end;

function TAssembler.ActDirByte(_parser: TLCGParser): TLCGParserStackEntry;
var bval: integer;
    bcount: integer;
    i:      integer;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  bval   := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  bcount := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if bcount < 1 then
    Monitor(ltError,'Byte count must be greater than zero');
  if (bval > 255) or (bval < -128) then
    Monitor(ltError,'Byte value must be in the range -128 to 255');
  FBytesFromLine := bcount;
  SetLength(FOutputArr,bcount);
  for i := 0 to bcount-1 do
    FOutputArr[i] := bval and $00FF;
end;

function TAssembler.ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    op:  integer;
    bval: integer;
    ch:   char;
begin
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
  Result.Buf := '';
end;

function TAssembler.ActDirDC(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    op:  integer;
    bval: integer;
    ch:   char;
begin
  if not ProcessingAllowed then
    Exit;
  // Get number of bytes in total
  FBytesFromLine := 0;
  for i := 0 to FExprList.Count-1 do
    case FExprList[i].ItemType of
      litString:  FBytesFromLine := FBytesFromLine + Length(FExprList[i].StringValue);
      otherwise
        Monitor(ltInternal,'Expression list type not catered for');
    end; // Case
  // Now output the bytes
  SetLength(FOutputArr,FBytesFromLine);
  op := 0;
  for i := 0 to FExprList.Count-1 do
    begin
      for ch in FExprList[i].StringValue do
        begin
          FOutputArr[op] := Ord(ch);
          Inc(op);
        end;
      FOutputArr[op-1] := FOutputArr[op-1] or $80;
    end; // Case
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  Result.Buf := '';
end;

function TAssembler.ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,symbolname);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler.ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,False,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler.ActDirDefineString(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,True,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
end;
function TAssembler.ActDirDefmacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not FIfStack.Allowed then
    exit;
  if FDefiningMacro then
    Monitor(ltError,'.DEFMACRO cannot be nested');
  FDefiningMacro := True;
  FMacroName := UpperCase(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if FMacroList.IndexOf(FMacroName) >= 0 then
    Monitor(ltError,'Macro %s is already defined',[FMacroName]);
  FMacroCapture := TStringList.Create;
  Monitor(ltWarAndPeace,'Defining macro %s',[FMacroName]);
end;

function TAssembler.ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    bytes: integer;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  bytes := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if (bytes < 1) or (bytes > 16384) then
    begin
      Monitor(ltError,'Number of bytes for storage directive not in the range 1-16384');
      Exit;
    end;
  FBytesFromLine := bytes;
  SetLength(FOutputArr,bytes);
  for i := 1 to bytes do
    FOutputArr[i-1] := 0;
  FOutput.Write(FOutputArr,FOrg,bytes);
end;

function TAssembler.ActDirDS2(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    bytes: integer;
    code:  integer;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  bytes := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  code  := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if (bytes < 1) or (bytes > 255) then
    begin
      Monitor(ltError,'Number of bytes for storage directive not in the range 1-255');
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

function TAssembler.ActDirDSH(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s);
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    if i = Length(s) then
      FOutputArr[i-1] := Ord(s[i]) or $80
    else
      FOutputArr[i-1] := Ord(s[i]);
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
end;

function TAssembler.ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s) + 1;
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    FOutputArr[i-1] := Ord(s[i]);
  FOutputArr[FBytesFromLine-1] := 0;
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
end;

function TAssembler.ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
var sl: TStringList;
    i:   integer;
    bval: integer;
    op: integer;
begin
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
  Result.Buf := '';
end;

function TAssembler.ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FIfStack.ElseSwap;
  Result.Buf := '';
end;

function TAssembler.ActDirEnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // @@@@@ ADD CODE FOR END
  // @@@@@ CHECK NOT IN MACRO ETC. MARK FILE AS ENDED SO FURTHER CODE WILL
  // @@@@@ GENERATE AN ERROR
  Result.Buf := '';
end;

function TAssembler.ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FIfStack.Pop;
  Result.Buf := '';
end;

function TAssembler.ActDirEndm(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not FIfStack.Allowed then
    exit;
  FMacroList.Add(FMacroName,FMacroCapture);
  FDefiningMacro := False;
  FMacroName := '';
  FMacroCapture := nil;
  Monitor(ltWarAndPeace,'End of defining macro %s',[FMacroName]);
end;

function TAssembler.ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  Monitor(ltError,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  succeeded := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0;
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler.ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := FSymbols.ExistsInPass(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler.ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := not FSymbols.ExistsInPass(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler.ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
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
  Result.Buf := '';
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

function TAssembler.ActDirIncludeList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FForceList := True;
  Result := ActDirInclude(_parser);
end;

function TAssembler.ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FList := True;
  FListNext := True;
end;

function TAssembler.ActDirMacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FProcessMacro := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FProcessParms := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActDirMacroNoexpr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FProcessMacro := UpperCase(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FProcessParms := '';
end;

function TAssembler.ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  Monitor(ltInfo,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActDirNoList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FListNext := False;
end;

function TAssembler.ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FOrg := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActDirSet(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,False,symbolname,expression,true);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler.ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Undefine(symbolname);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler.ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  Monitor(ltWarning,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) +
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprA8(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // @@@@@ PUT IN SOME CHECKS TO ENSURE THIS IS WITHIN 8 BIT LIMITS @@@@@
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprA16(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // @@@@@ PUT IN SOME CHECKS TO ENSURE THIS IS WITHIN 16 BIT LIMITS @@@@@
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) and
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-2].Buf;
end;

function TAssembler.ActExprCL(_parser: TLCGParser): TLCGParserStackEntry;
var s: string;
begin
  // @@@@@ ADD CODE TO CHECK THE INPUT AND REMOVE EXTERNAL QUOTES
  // @@@@@ ALSO NEED TO DEAL WITH ESCAPE CHARACTERS
  s := UnEscape(_parser.ParserStack[_parser.ParserSP-1].Buf);
  Result.Buf := '0';
  if Length(s) = 0 then
    Monitor(ltError,'Empty string cannot be used with this directive')
  else
    begin
      if Length(s) > 1 then
        Monitor(ltWarning,'Only first character of string "%s" used',[s]);
      Result.Buf := IntToStr(Ord(s[1]));
    end;
end;

function TAssembler.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) div
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                ',' +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActExprListA8orStr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                ',' +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActExprListStr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                ',' +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(-StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) mod
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) *
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) xor
                         $FFFF);
end;

function TAssembler.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) or
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprS8(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // @@@@@ PUT IN SOME CHECKS TO ENSURE THIS IS WITHIN 16 BIT LIMITS @@@@@
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
var val1,val2: uint32;
begin
  val1 := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  val2 := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  val1 := val1 shl val2;
  Result.Buf := IntToStr(val1 and $FFFF);
end;

function TAssembler.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) shr
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) -
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprU16(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // @@@@@ PUT IN SOME CHECKS TO ENSURE THIS IS WITHIN 16 BIT LIMITS @@@@@
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) xor
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler.ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
var s: string;
begin
  s := _parser.ParserStack[_parser.ParserSP-2].Buf;
  if Length(s) < 1 then
    Monitor(ltError,'No argument provided to ASC() function');
  Result.Buf := IntToStr(Ord(s[1]));
end;

function TAssembler.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf) shr 8);
end;

function TAssembler.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
var expr:     integer;
    trueval:  integer;
    falseval: integer;
    resval:   integer;
begin
  expr     := StrToInt(_parser.ParserStack[_parser.ParserSP-6].Buf);
  trueval  := StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf);
  falseval := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if expr <> 0 then
    resval := trueval
  else
    resval := falseval;
  Result.Buf := IntToStr(resval);
end;

function TAssembler.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf) and $00FF);
end;

function TAssembler.ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(Pos(_parser.ParserStack[_parser.ParserSP-4].Buf,
                             _parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromHexLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
end;

function TAssembler.ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler.ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-2].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler.ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := FLocalPrefix + _parser.ParserStack[_parser.ParserSP-1].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler.ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := FLocalPrefix + _parser.ParserStack[_parser.ParserSP-2].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler.ActLExprI(_parser: TLCGParser): TLCGParserStackEntry;
var r:    TExprListItem;
begin
  r.ItemType := litInteger;
  r.IntegerValue := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FExprList.Add(r);
  result.Buf := '';
end;

function TAssembler.ActLExprS(_parser: TLCGParser): TLCGParserStackEntry;
var r:    TExprListItem;
begin
  r.ItemType := litString;
  r.StringValue := UnEscape(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FExprList.Add(r);
  result.Buf := '';
end;

function TAssembler.ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <> 0) and
     (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) = 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <> 0) or
     (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler.ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // We will only end up here if a macro parameter was not expanded
  if ProcessingAllowed then
    Monitor(ltError,'@ macro placeholder used without a corresponding parameter');
  Result.Buf := IntToStr(FOrg);
end;

function TAssembler.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromOctLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActOpcode0(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := DoOpcode(_parser,0);
end;

function TAssembler.ActOpcode1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := DoOpcode(_parser,1);
end;

function TAssembler.ActOpcode2(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := DoOpcode(_parser,2);
end;

function TAssembler.ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
var index_reg: string;
    operand_opt: TOperandOption;
begin
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
  Result.Buf := '';
end;

function TAssembler.ActSetOpIndOff(_parser: TLCGParser): TLCGParserStackEntry;
var index_reg: string;
    expr:      string;
    operand_opt: TOperandOption;
begin
  // Index+Offset operand like [IX+2], [IY+020H]
  index_reg  := UpperCase(_parser.ParserStack[_parser.ParserSP-4].Buf);
  expr       := _parser.ParserStack[_parser.ParserSP-2].Buf;
  operand_opt := OPER_NULL;
  if index_reg = 'IX' then
    operand_opt := OPER_IXPD_IND
  else if index_reg = 'IY' then
    operand_opt := OPER_IYPD_IND
  else
      Monitor(ltInternal,'Could not cater for index register %s',[index_reg]);
  if operand_opt <> OPER_NULL then
    PushOperand(operand_opt,StrToInt(expr));
  Result.Buf := '';
end;

function TAssembler.ActSetOpSimple(_parser: TLCGParser): TLCGParserStackEntry;
var op_reg: string;
    operand_opt: TOperandOption;
begin
  // Simple operand like A, IX, P, etc.
  op_reg  := _parser.ParserStack[_parser.ParserSP-1].Buf;
  op_reg := UpperCase(op_reg);
  operand_opt := FInstructionList.SimpleOpToOperandOption(op_reg);
  if operand_opt = OPER_NULL then
    Monitor(ltInternal,'Operand type %s not valid',[op_reg])
  else
    PushOperand(operand_opt,0);
  Result.Buf := '';
end;

function TAssembler.ActSetOpSimpleW(_parser: TLCGParser): TLCGParserStackEntry;
var op_reg: string;
    operand_opt: TOperandOption;
begin
  // Simple operand wrapped in brackets (C), (HL), etc.
  op_reg  := _parser.ParserStack[_parser.ParserSP-2].Buf;
  op_reg := '(' + UpperCase(op_reg) + ')';
  operand_opt := FInstructionList.SimpleOpToOperandOption(op_reg);
  if operand_opt = OPER_NULL then
    Monitor(ltInternal,'Operand type %s not valid',[op_reg])
  else
    PushOperand(operand_opt,0);
  Result.Buf := '';
end;

function TAssembler.ActSetOpBracketed(_parser: TLCGParser): TLCGParserStackEntry;
var expr:      string;
    operand_opt: TOperandOption;
begin
  // Operand like [NN], [0x200], [5]
  expr       := _parser.ParserStack[_parser.ParserSP-2].Buf;
  operand_opt := OPER_U16_IND;
  if operand_opt <> OPER_NULL then
    PushOperand(operand_opt,StrToInt(expr));
  Result.Buf := '';
end;

function TAssembler.ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
var expr:      string;
    operand_opt: TOperandOption;
begin
  // Operand like NN, 0x200, 5
  expr       := _parser.ParserStack[_parser.ParserSP-1].Buf;
  operand_opt := OPER_U16;
  PushOperand(operand_opt,StrToInt(expr));
  Result.Buf := '';
end;

function TAssembler.ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Result.Buf := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function TAssembler.ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler.ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := Chr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('yyyy-mm-dd',FAssemblyStart);
end;

function TAssembler.ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := Format('%X',[StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf)]);
end;

function TAssembler.ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
var fmt: string;
    digits: integer;
begin
  digits := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if (digits < 1) or (digits > 4) then
      Monitor(ltError,'Range for number of hex digits is 1 to 4');
  fmt := Format('%%%d.%dX',[digits,digits]);
  Result.Buf := Format(fmt,[StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf)]);
end;

function TAssembler.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := StripQuotesAndEscaped(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler.ActStringSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf,'',FIfStack);
end;

function TAssembler.ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
begin
  len := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if len < 0 then
    Monitor(ltError,'Length for LEFT() cannot use a negative number');
  Result.Buf := LeftStr(_parser.ParserStack[_parser.ParserSP-4].Buf,len);
end;

function TAssembler.ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := LowerCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
end;

function TAssembler.ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
    start: integer;
begin
  start := StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf);
  len   := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if start < 0 then
    Monitor(ltError,'Start for MID() cannot use a negative number');
  if len < 0 then
    Monitor(ltError,'Length for MID() cannot use a negative number');
  Result.Buf := Copy(_parser.ParserStack[_parser.ParserSP-6].Buf,start,len);
end;

function TAssembler.ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
begin
  len := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if len < 0 then
    Monitor(ltError,'Length for RIGHT() cannot use a negative number');
  Result.Buf := RightStr(_parser.ParserStack[_parser.ParserSP-4].Buf,len);
end;

function TAssembler.ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler.ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('hh:nn:ss',FAssemblyStart);
end;

function TAssembler.ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
end;

function TAssembler.ActSymbolDef(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-2].Buf;
  if (Length(symbolname) > 1) and (RightStr(symbolname,1) = ':') then
    symbolname := LeftStr(symbolname,Length(symbolname)-1); // Remove trailing colon
  message := FSymbols.Define(FPass,symbolname,FOrg);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler.ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(FOrg);
end;

function TAssembler.ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not ProcessingAllowed then
    Result.Buf := IntToStr(FOrg)
  else
    Result.Buf := FSymbols.Variable(FPass,FLocalPrefix+_parser.ParserStack[_parser.ParserSP-1].Buf,IntToStr(FOrg),FIfStack);
end;

function TAssembler.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf,IntToStr(FOrg),FIfStack);
end;

procedure TAssembler.Assemble;
var elapsed: double;
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
        FOutput.SaveObject(FilenameObj);
      end;
  finally
    Monitor(ltInfo,'%d lines assembled, %d bytes generated',[FLineCount, FBytesTotal]);
    FAssemblyEnd := Now;
    Monitor(ltVerbose,'Assembly ended %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',FAssemblyEnd)]);
    Monitor(ltVerbose,'Total assembly time %.3f seconds',[(FAssemblyEnd-FAssemblyStart)*86400.0]);
    FilesClose;
  end;
end;

procedure TAssembler.AssemblePass(_pass: integer);
begin
  FPass := _pass;
  Monitor(ltInfo,'ASSEMBLER PASS %d',[_pass]);
  InitPass;
  ProcessFile(FFilenameSrc);
  if FDefiningMacro then
    Monitor(ltError,'Pass terminated unexpectedly in the middle of a .MACRO block');
end;

function TAssembler.DoOpcode(_parser: TLCGParser; _operands: integer): TLCGParserStackEntry;
var opcindex: integer;
    opcode:   string;
    instrec:  TInstructionRec;
    displacement: integer;
begin
  // Format in stack SP-4 | SP-3 | SP-2 | SP-1 is
  // T_OPCODE | operand1 | , | operand2 when _operands is 2
  // stack SP-2 | SP-1 when _operands is 1
  // stack SP-1 when _operands is 0
  case _operands of
    0: displacement := 1;
    1: displacement := 2;
    2: displacement := 4;
    otherwise
      Monitor(ltInternal,'Operand count %d not catered for',[_operands]);
  end; // Case
  opcode := _parser.ParserStack[_parser.ParserSP-displacement].Buf;
  opcode := UpperCase(opcode);
  if not FInstructionList.FindOpcode(opcode,opcindex) then
    Monitor(ltError,'Opcode mnemonic %s not available with processor type %s',[opcode,FProcessor])
  else if FOperandIndex <> _operands then
    Monitor(ltInternal,'Did not find the expected %d operands',[_operands])
  else
    begin
      if not FInstructionList.FindInstruction(opcindex,FOperands[0].oper_opt,FOperands[1].oper_opt,instrec) then
        case _operands of
          0: Monitor(ltError,'Could not find instruction to match opcode %s',[opcode]);
          1: Monitor(ltError,'Could not find instruction to match opcode %s with operand %s',[opcode,OperandStrings[FOperands[0].oper_opt]]);
          2: Monitor(ltError,'Could not find instruction to match opcode %s with operands %s, %s',[opcode,OperandStrings[FOperands[0].oper_opt],OperandStrings[FOperands[1].oper_opt]]);
        end // Case
      else
        PumpCode(instrec);
    end;
  Result.Buf := '';
end;

procedure TAssembler.FilesClose;
begin
  if Assigned(FStreamLog) then
    begin
      Monitor(ltDebug,'Closing %s',[FilenameLog]);
      FreeAndNil(FStreamLog);
    end;
end;

procedure TAssembler.FilesOpen;
begin
  try
    Monitor(ltDebug,'Opening %s',[FilenameLog]);
    if FilenameLog <> '' then
      FStreamLog := TFileStream.Create(FilenameLog,fmCreate,fmShareDenyWrite);
  except
    Monitor(ltError,'Unable to create log file %s',[FilenameLog]);
  end;
end;

function TAssembler.GetSource: string;
begin
  Result := '';
  if FFileStack.Count > 0 then
    Result := FFileStack.Filename;
end;

procedure TAssembler.InitLine;
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

procedure TAssembler.InitPass;
var i: integer;
    symbolstring: string;
    symbolname:   string;
    symbolval:    string;
    symbolvaln:   integer;
    symindex:     integer;
    symentry:     TSymbolEntry;
    eqpos:        integer;
    message:      string;
begin
  FBytesTotal := 0;
  FDefiningMacro := False;
  FForceList := False;
  FLineCount := 0;
  FList := True;
  FListNext := True;
  FMacroList.Clear;
  FMacroList.Init;
  FMacroNestLevel := 0;
  FOrg := DEFAULT_ORG;
  FOutput.Clear;
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

procedure TAssembler.InitStart;
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

procedure TAssembler.Monitor(LogType: TLCGLogType; const Message: string);
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

procedure TAssembler.Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const);
begin
  Monitor(LogType,Format(Message,Args));
end;

procedure TAssembler.OutputDebugLine(const _asmline: string);
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

procedure TAssembler.OutputListingLine(const _asmline: string);
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

procedure TAssembler.ProcessFile(const _fn: string; _listing: boolean);
begin
  Monitor(ltWarAndPeace,'Processing file %s',[_fn]);
  FFileStack.Push(_fn,_listing);
  try
    ProcessFileInner;
  finally
    FFileStack.Pop();
  end;
end;

procedure TAssembler.ProcessFileInner;
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

procedure TAssembler.ProcessMacroExpansion;
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

function TAssembler.ProcessingAllowed: boolean;
begin
  Result := FIfStack.Allowed and (not FDefiningMacro);
end;

procedure TAssembler.ProcessLine(const _line: string);
var strm: TStringStream;
begin
  if (Length(_line) > 0) and (_line[1] = '*') then
    Exit; // Comment line
  strm := TStringStream.Create(_line);
  try
    Parse(strm);
  finally
    strm.Free;
  end;
end;

procedure TAssembler.PumpCode(_r: TInstructionRec);
var i: integer;
    o: integer;
    rst_addr: integer;
    rel:      integer;

  procedure PumpByte(_v: integer);
  begin
    if (_v < 0) or (_v > 255) then
      Monitor(ltError,'Instruction byte with value %8.8X cannot be used',[_v])
    else
      begin
        FOutputArr[o] := _v and $FF;
        Inc(o);
      end;
  end;

  procedure PumpWord(_v: integer);
  begin
    if (_v < 0) or (_v > 65535) then
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
                        PumpByte(Value or (rst_addr shl Offset)); // @@@@@ IS THIS CORRECT WHERE IS VALUE COMING FROM
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

procedure TAssembler.PushOperand(_op: TOperandOption; _v: integer);
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

function TAssembler.Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if Assigned(FProcArray[RuleIndex]) then
    Result := FProcArray[RuleIndex](Parser)
  else
    Monitor(ltInternal,'Code not defined for rule no. %d (%s)',[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TAssembler.RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
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

procedure TAssembler.RegisterProcs;
var _procs: TStringArray;
begin
  _procs := RuleProcs;
  RegisterProc('ActBinLiteral',     @ActBinLiteral, _procs);
//RegisterProc('ActCharLiteral',    @ActCharLiteral, _procs);
  RegisterProc('ActCopy1',          @ActCopy1, _procs);
//RegisterProc('ActCopy2',          @ActCopy2, _procs);
  RegisterProc('ActCompEQ',         @ActCompEQ, _procs);
  RegisterProc('ActCompGE',         @ActCompGE, _procs);
  RegisterProc('ActCompGT',         @ActCompGT, _procs);
  RegisterProc('ActCompLE',         @ActCompLE, _procs);
  RegisterProc('ActCompLT',         @ActCompLT, _procs);
  RegisterProc('ActCompNE',         @ActCompNE, _procs);
  RegisterProc('ActDecLiteral',     @ActDecLiteral, _procs);
//RegisterProc('ActDirByte',        @ActDirByte, _procs);
  RegisterProc('ActDirDB',          @ActDirDB, _procs);
  RegisterProc('ActDirDC',          @ActDirDC, _procs);
//RegisterProc('ActDirDD',          @ActDirDD, _procs);
  RegisterProc('ActDirDefine',      @ActDirDefine, _procs);
  RegisterProc('ActDirDefineExpr',  @ActDirDefineExpr, _procs);
//RegisterProc('ActDirDefineString',@ActDirDefineString, _procs);
//RegisterProc('ActDirDefmacro',    @ActDirDefmacro, _procs);
  RegisterProc('ActDirDS',          @ActDirDS, _procs);
  RegisterProc('ActDirDS2',         @ActDirDS2, _procs);
//RegisterProc('ActDirDSH',         @ActDirDSH, _procs);
//RegisterProc('ActDirDSZ',         @ActDirDSZ, _procs);
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
  RegisterProc('ActDirList',        @ActDirList, _procs);
//RegisterProc('ActDirMacro',       @ActDirMacro, _procs);
//RegisterProc('ActDirMacroNoexpr', @ActDirMacroNoexpr, _procs);
  RegisterProc('ActDirMessage',     @ActDirMessage, _procs);
  RegisterProc('ActDirNolist',      @ActDirNolist, _procs);
  RegisterProc('ActDirOrg',         @ActDirOrg, _procs);
  RegisterProc('ActDirSet',         @ActDirSet, _procs);
  RegisterProc('ActDirUndefine',    @ActDirUndefine, _procs);
  RegisterProc('ActDirWarning',     @ActDirWarning, _procs);
  RegisterProc('ActExprA8',         @ActExprA8, _procs);
//RegisterProc('ActExprA16',        @ActExprA16, _procs);
  RegisterProc('ActExprAdd',        @ActExprAdd, _procs);
  RegisterProc('ActExprAnd',        @ActExprAnd, _procs);
  RegisterProc('ActExprBracket',    @ActExprBracket, _procs);
  RegisterProc('ActExprCL',         @ActExprCL, _procs);
  RegisterProc('ActExprDiv',        @ActExprDiv, _procs);
//RegisterProc('ActExprList',       @ActExprList, _procs);
//RegisterProc('ActExprListA8orStr',@ActExprListA8orStr, _procs);
//RegisterProc('ActExprListStr',    @ActExprListStr, _procs);
  RegisterProc('ActExprUnaryMinus', @ActExprUnaryMinus, _procs);
  RegisterProc('ActExprMod',        @ActExprMod, _procs);
  RegisterProc('ActExprMul',        @ActExprMul, _procs);
  RegisterProc('ActExprNot',        @ActExprNot, _procs);
  RegisterProc('ActExprOr',         @ActExprOr, _procs);
  RegisterProc('ActExprS8',         @ActExprS8, _procs);
  RegisterProc('ActExprShl',        @ActExprShl, _procs);
  RegisterProc('ActExprShr',        @ActExprShr, _procs);
  RegisterProc('ActExprSub',        @ActExprSub, _procs);
  RegisterProc('ActExprU16',        @ActExprU16, _procs);
  RegisterProc('ActExprXor',        @ActExprXor, _procs);
  RegisterProc('ActFuncAsc',        @ActFuncAsc, _procs);
  RegisterProc('ActFuncHigh',       @ActFuncHigh, _procs);
  RegisterProc('ActFuncIif',        @ActFuncIif, _procs);
  RegisterProc('ActFuncLow',        @ActFuncLow, _procs);
  RegisterProc('ActFuncPos',        @ActFuncPos, _procs);
  RegisterProc('ActFuncValue',      @ActFuncValue, _procs);
  RegisterProc('ActHexLiteral',     @ActHexLiteral, _procs);
  RegisterProc('ActIgnore',         @ActIgnore, _procs);
//RegisterProc('ActLabel',          @ActLabel, _procs);
//RegisterProc('ActLabelC',         @ActLabelC, _procs);
//RegisterProc('ActLabelLocal',     @ActLabelLocal, _procs);
//RegisterProc('ActLabelLocalC',    @ActLabelLocalC, _procs);
  RegisterProc('ActLExprI',         @ActLExprI, _procs);
  RegisterProc('ActLExprS',         @ActLExprS, _procs);
  RegisterProc('ActLogAnd',         @ActLogAnd, _procs);
  RegisterProc('ActLogNot',         @ActLogNot, _procs);
  RegisterProc('ActLogOr',          @ActLogOr, _procs);
//RegisterProc('ActMacroPlaceholder', @ActMacroPlaceholder, _procs);
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
  RegisterProc('ActStrBuild',       @ActStrBuild, _procs);
  RegisterProc('ActStrCat',         @ActStrCat, _procs);
  RegisterProc('ActStrChr',         @ActStrChr, _procs);
  RegisterProc('ActStrDate',        @ActStrDate, _procs);
  RegisterProc('ActStrHex1',        @ActStrHex1, _procs);
  RegisterProc('ActStrHex2',        @ActStrHex2, _procs);
  RegisterProc('ActStringConstant', @ActStringConstant, _procs);
  RegisterProc('ActStringSymbol',   @ActStringSymbol, _procs);
  RegisterProc('ActStrLeft',        @ActStrLeft, _procs);
  RegisterProc('ActStrLower',       @ActStrLower, _procs);
  RegisterProc('ActStrMid',         @ActStrMid, _procs);
  RegisterProc('ActStrRight',       @ActStrRight, _procs);
  RegisterProc('ActStrString',      @ActStrString, _procs);
  RegisterProc('ActStrTime',        @ActStrTime, _procs);
  RegisterProc('ActStrUpper',       @ActStrUpper, _procs);
  RegisterProc('ActSymbolDef',      @ActSymbolDef, _procs);
//RegisterProc('ActValueLocal',     @ActValueLocal, _procs);
  RegisterProc('ActValueOrg',       @ActValueOrg, _procs);
  RegisterProc('ActValueSymbol',    @ActValueSymbol, _procs);
end;

procedure TAssembler.SetFilenameSrc(const _fn: string);
begin
  FFilenameSrc := _fn;
  if FilenameObj  = '' then FilenameObj  := ChangeFileExt(_fn,'.o80');
end;

procedure TAssembler.SetOnMonitor(_monitor: TLCGMonitorProc);
begin
  FOnMonitor := _monitor;
  if Assigned(FFileStack) then
    FFileStack.OnMonitor := _monitor;
end;

procedure TAssembler.WriteMapFile;
var sl: TStringList;
begin
  if FilenameMap = '' then
    Exit;
  sl := TStringList.Create;
  try
    FSymbols.SortByName;
    sl.Add('SYMBOLS BY NAME');
    sl.Add('');
    FSymbols.Dump(sl);
    FSymbols.SortByAddr;
    sl.Add('');
    sl.Add('');
    sl.Add('');
    sl.Add('SYMBOLS BY VALUE');
    sl.Add('');
    FSymbols.Dump(sl);
    sl.SaveToFile(FilenameMap);
  finally
    sl.Free;
  end;
end;

end.

