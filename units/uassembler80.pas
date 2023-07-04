unit uassembler80;

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2023 Duncan Munro

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


// Main assembler unit. This handles all the work of assembling a single file


{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, lacogen_types, lacogen_module,
  umessages, uinstruction, usymboltable,
  upreparser3, ucommand, upreparser3_defs, ucodebuffer, ulisting, uinclude,
  uenvironment, ustack, umacro;

type
  TCompareMode = (cmEqual, cmNotEqual, cmLessThan, cmLessEqual, cmGreaterThan,
    cmGreaterEqual);

  TAssembler80 = class(TLCGParser)
  private
    FAsmStack: TAsmStack;
    FCaseSensitive: boolean;
    FCmdList: TCommandList;
    FCodeBuffer: TCodeBuffer;
    FCurrentFile: string;
    FDefiningMacro: boolean;
    FDefMacro:      TMacroEntry;
    FEnded: boolean;
    FFilenameAsm: string;
    FFilenameCom: string;
    FFilenameError: string;
    FFilenameHex: string;
    FFilenameMap: string;
    FInputCol: integer;
    FIncludeStack: TIncludeStack;
    FIncludeList: string;
    FInstructionList: TInstructionList;
    FListing: TListing;
    FMacroList: TMacroList;
    FMacroStack: TMacroStack;
    FMemory: array[word] of byte;
    FMemoryUsed: array[word] of boolean;
    FNextInclude: string;
    FOptionCom: string;
    FOptionDefines: string;
    FOptionError: string;
    FOptionHex: string;
    FOptionListing: string;
    FOptionMap: string;
    FOrg: integer;
    FPass: integer;
    FPreparser: TPreparser;
    FProcArray: array of TLCGParserProc;
    FProcessor: string;
    FSolGenerate: boolean;
    FSymbolTable: TSymbolTable;
    FTitle: string;
    procedure RegisterProc(const _procname: string; _proc: TLCGParserProc;
      _procs: TStringArray);
    procedure RegisterProcs;
    function ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCharConstant(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
    function ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
    function ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprUnaryPlus(_parser: TLCGParser): TLCGParserStackEntry;
    function ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncDefined(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncLength(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
    function ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
    function ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
    function ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
    //    function  ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
    //    function  ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
    //    function  ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
    //    function  ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
    //    function  ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
    function ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
    function ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
    function ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
    function ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
    function ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
    function ActSetOpIndOffIX(_parser: TLCGParser): TLCGParserStackEntry;
    function ActSetOpIndOffIY(_parser: TLCGParser): TLCGParserStackEntry;
    function ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrProcessor(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
    function ActStrVersion(_parser: TLCGParser): TLCGParserStackEntry;
    function ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
    function ActValueParam(_parser: TLCGParser): TLCGParserStackEntry;
    function ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
    procedure ApplyCommandLineDefines;
    procedure AsmProcessLabel(const _label: string; _command_index: integer);
    procedure CheckNoLabel(const _label: string);
    procedure CheckByte(_i: integer);
    procedure CheckInteger(_i, _min, _max: integer);
    procedure CheckLabelsDefined;
    procedure CheckMacroDone;
    procedure CheckOperandByte(_index: integer);
    procedure CheckOperandCount(_minop, _maxop: integer);
    procedure CheckOperandInteger(_index, _min, _max: integer);
    procedure CheckStack;
    procedure CheckStringNotEmpty(const _s: string);
    procedure CmdCODE(const _label: string; _preparser: TPreparserBase);
    procedure CmdCPU(const _label: string; _preparser: TPreparserBase);
    procedure CmdDATA(const _label: string; _preparser: TPreparserBase);
    procedure CmdDB(const _label: string; _preparser: TPreparserBase);
    procedure CmdDC(const _label: string; _preparser: TPreparserBase);
    procedure CmdDS(const _label: string; _preparser: TPreparserBase);
    procedure CmdDW(const _label: string; _preparser: TPreparserBase);
    procedure CmdELSE(const _label: string; _preparser: TPreparserBase);
    procedure CmdEND(const _label: string; _preparser: TPreparserBase);
    procedure CmdENDIF(const _label: string; _preparser: TPreparserBase);
    procedure CmdENDM(const _label: string; _preparser: TPreparserBase);
    procedure CmdENDR(const _label: string; _preparser: TPreparserBase);
    procedure CmdENDW(const _label: string; _preparser: TPreparserBase);
    procedure CmdEQU(const _label: string; _preparser: TPreparserBase);
    procedure CmdEQU2(const _label: string; _preparser: TPreparserBase);
    procedure CmdEXTERN(const _label: string; _preparser: TPreparserBase);
    procedure CmdGLOBAL(const _label: string; _preparser: TPreparserBase);
    procedure CmdIF(const _label: string; _preparser: TPreparserBase);
    procedure CmdIFDEF(const _label: string; _preparser: TPreparserBase);
    procedure CmdIFNDEF(const _label: string; _preparser: TPreparserBase);
    procedure CmdINCLUDE(const _label: string; _preparser: TPreparserBase);
    procedure CmdLISTON(const _label: string; _preparser: TPreparserBase);
    procedure CmdMACRO(const _label: string; _preparser: TPreparserBase);
    procedure CmdMSGERROR(const _label: string; _preparser: TPreparserBase);
    procedure CmdMSGINFO(const _label: string; _preparser: TPreparserBase);
    procedure CmdMSGWARNING(const _label: string; _preparser: TPreparserBase);
    procedure CmdLISTOFF(const _label: string; _preparser: TPreparserBase);
    procedure CmdWARNOFF(const _label: string; _preparser: TPreparserBase);
    procedure CmdORG(const _label: string; _preparser: TPreparserBase);
    procedure CmdREPEAT(const _label: string; _preparser: TPreparserBase);
    procedure CmdTITLE(const _label: string; _preparser: TPreparserBase);
    procedure CmdUDATA(const _label: string; _preparser: TPreparserBase);
    procedure CmdWARNON(const _label: string; _preparser: TPreparserBase);
    procedure CmdWHILE(const _label: string; _preparser: TPreparserBase);
    procedure CommonDB(_preparser: TPreparserBase; _highbit: boolean = False);
    function CompareGeneric(_comparer: TCompareMode): TLCGParserStackEntry;
    procedure ConvertOperandToOrd(_index: integer);
    procedure EQUCore(const _label: string; _preparser: TPreparserBase; _allow_redefine: boolean);
    procedure ExpandMacro(macro_entry: TMacroEntry);
    function GetDFA: TLCGDFA;
    function GetFilenameListing: string;
    function MakeFilename(const _base_asm, _option, _ext: string): string;
    procedure NeedNumber(_index: integer; const _msg: string);
    procedure NeedNumberCompare;
    procedure NeedPosNumber(_index: integer; const _msg: string; _min: integer = 0);
    procedure NeedString(_index: integer; const _msg: string);
    procedure OutputMemoryToCom(_filename: string);
    procedure OutputMemoryToHex(_filename: string);
    procedure Parse(const _s: string; _cmd_only: boolean);
    procedure Parse(_strm: TStream; _firstcol: integer);
    function ParserM1: TLCGParserStackEntry;
    function ParserM2: TLCGParserStackEntry;
    function ParserM3: TLCGParserStackEntry;
    function ParserM4: TLCGParserStackEntry;
    function ParserM5: TLCGParserStackEntry;
    function ParserM6: TLCGParserStackEntry;
    procedure PostReduce(Parser: TLCGParser);
    procedure Preparse(const _s: string);
    procedure ProcessFile(const filename: string);
    procedure ProcessInclude;
    function Reduce(Parser: TLCGParser; RuleIndex: uint32): TLCGParserStackEntry;
    procedure RegisterCommands;
    procedure ResetMemory;
    procedure SetCaseSensitive(_v: boolean);
    procedure SetDefiningMacro(_v: boolean);
    procedure SetFilenameAsm(const _filename: string);
    procedure SetFilenameError(const _filename: string);
    procedure SetFilenameListing(const _filename: string);
    procedure SetOrg(_neworg: integer);
    procedure SetTitle(_title: string);
    function SourceCombine1(_a: integer): TParserStackSource;
    function SourceCombine2(_a, _b: integer): TParserStackSource;
    function SourceCombine3(_a, _b, _c: integer): TParserStackSource;
  public
    FinalVal: TLCGParserStackEntry;
    ParsedOperandOption: TOperandOption;
    constructor Create(const _processor: string);
    destructor Destroy; override;
    procedure Assemble(const filename: string);
    procedure AssembleLine(const _s: string);
    procedure AssemblePass(_pass: integer; const filename: string);
    procedure DumpInstructions;
    procedure DumpReserved;
    procedure ShowError(_colno: integer; _logtype: TLCGLogType; _msgno: TMessageNumbers);
    procedure ShowError(_colno: integer; _logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
    procedure ShowErrorToken(_token: TToken; _logtype: TLCGLogType; _msgno: TMessageNumbers);
    procedure ShowErrorToken(_token: TToken; _logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
    property CaseSensitive:   boolean read FCaseSensitive     write SetCaseSensitive;
    property CurrentFile:     string  read FCurrentFile;
    property DefiningMacro:   boolean read FDefiningMacro     write SetDefiningMacro;
    property DFA:             TLCGDFA read GetDFA;
    property FilenameAsm:     string  read FFilenameAsm       write SetFilenameAsm;
    property FilenameCom:     string  read FFilenameCom       write FFilenameCom;
    property FilenameError:   string  read FFilenameError     write SetFilenameError;
    property FilenameHex:     string  read FFilenameHex       write FFilenameHex;
    property FilenameMap:     string  read FFilenameMap       write FFilenameMap;
    property FilenameListing: string  read GetFilenameListing write SetFilenameListing;
    property IncludeList:     string  read FIncludeList       write FIncludeList;
    property InputLine:       integer read FInputLine;
    property InputCol:        integer read FInputCol;
    property OptionCom:       string  read FOptionCom         write FOptionCom;
    property OptionDefines:   string  read FOptionDefines     write FOptionDefines;
    property OptionError:     string  read FOptionError       write FOptionError;
    property OptionHex:       string  read FOptionHex         write FOptionHex;
    property OptionListing:   string  read FOptionListing     write FOptionListing;
    property OptionMap:       string  read FOptionMap         write FOptionMap;
    property Org:             integer read FOrg               write SetOrg;
    property Pass:            integer read FPass;
    property Processor:       string read FProcessor;
    property Title:           string read FTitle              write SetTitle;
  end;

var
  Asm80: TAssembler80;

implementation

uses
  uutility, typinfo, uasmglobals;

constructor TAssembler80.Create(const _processor: string);
begin
  inherited Create;
  ResetMemory;
  FProcessor := _processor;
  FListing := TListing.Create;
  FListing.Listing := True;
  FCodeBuffer := TCodeBuffer.Create;
  FInstructionList := TInstructionList.Create(_processor);
  FCmdList := TCommandList.Create;
  RegisterCommands;
  FPreparser := TPreparser.Create(FCmdList, FInstructionList);
  FPreparser.ForceColon := False;
  FSymbolTable := TSymbolTable.Create;
  FSymbolTable.MixedCase := False;
  FOptionDefines := '';
  FIncludeStack := TIncludeStack.Create;
  FIncludeList := EnvObject.GetValue('Includes');
  FAsmStack := TAsmStack.Create;
  FMacroList  := TMacroList.Create;
  FMacroStack := TMacroStack.Create;
  FDefMacro   := TMacroEntry.Create;

  FCmdList.SymbolTable := FSymbolTable;
  FPreparser.MacroList := FMacroList;
  LoadFromResource('XA80OPER');
  SetLength(FProcArray, Rules);
  RegisterProcs;
  FEnded := False;
  FOrg := 0;
  FPass := 0;
  Title := '';
  OnPostReduce := @PostReduce;
  OnReduce := @Reduce;
end;

destructor TAssembler80.Destroy;
begin
  FreeAndNil(FDefMacro);
  FreeAndNil(FMacroStack);
  FreeAndNil(FMacroList);
  FreeAndNil(FAsmStack);
  FreeAndNil(FIncludeStack);
  FreeAndNil(FSymbolTable);
  FreeAndNil(FPreparser);
  FreeAndNil(FCmdList);
  FreeAndNil(FInstructionList);
  FreeAndNil(FCodeBuffer);
  FreeAndNil(FListing);
  inherited Destroy;
end;

function TAssembler80.ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt := VariableFromBinLiteral(ParserM1.Buf);
  Result.BufType := pstINT32;
  Result.Source := pssConstant;
end;

function TAssembler80.ActCharConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := StripQuotesAndEscaped(ParserM1.Buf);
  Result.BufType := pstString;
  Result.Source := pssConstant;
  {
  Result.BufInt := Ord(ParserM1.Buf[2]);
  Result.BufType := pstINT32;
  Result.Source  := pssConstant;
  }
end;

function TAssembler80.ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := CompareGeneric(cmEqual);
end;

function TAssembler80.ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := CompareGeneric(cmGreaterEqual);
end;

function TAssembler80.ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := CompareGeneric(cmGreaterThan);
end;

function TAssembler80.ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := CompareGeneric(cmLessEqual);
end;

function TAssembler80.ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := CompareGeneric(cmLessThan);
end;

function TAssembler80.ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := CompareGeneric(cmNotEqual);
end;

function TAssembler80.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
var
  buf: string;
begin
  buf := UpperCase(ParserM1.Buf);
  if (buf <> '') and (RightStr(buf, 1) = 'D') then
    Result.BufInt := StrToInt(LeftStr(buf, Length(buf) - 1))
  else
    Result.BufInt := StrToInt(buf);
  Result.Buf := IntToStr(Result.BufInt);
  Result.BufType := pstINT32;
  Result.Source := pssConstant;
end;

function TAssembler80.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (ParserM3.BufType = pstString) and (ParserM1.BufType = pstString) then
  begin
    // String concatenation
    Result.Buf := ParserM3.Buf + ParserM1.Buf;
    Result.BufInt := 0;
    Result.BufType := pstString;
  end
  else
  begin
    // Normal numeric add
    NeedNumber(-3, 'on left hand side of add');
    NeedNumber(-1, 'on right hand side of add');
    Result.BufInt := ParserM3.BufInt + ParserM1.BufInt;
    if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
      ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
    Result.BufType := pstINT32;
  end;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of binary and');
  NeedNumber(-1, 'on right hand side of binary and');
  Result.BufInt := (ParserM3.BufInt and $FFFF) and
    (ParserM1.BufInt and $FFFF);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM2;
end;

function TAssembler80.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of divide');
  NeedNumber(-1, 'on right hand side of divide');
  if ParserM1.BufInt = 0 then
    ShowErrorToken(ParserM1.Token, ltError, E2009_DIVIDE_BY_ZERO);
  Result.BufInt := ParserM3.BufInt div ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of modulo');
  NeedNumber(-1, 'on right hand side of modulo');
  Result.BufInt := ParserM3.BufInt mod ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of multiply');
  NeedNumber(-1, 'on right hand side of multiply');
  Result.BufInt := ParserM3.BufInt * ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1, 'for unary not');
  Result.BufInt := (ParserM1.BufInt and $FFFF) xor $FFFF;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-1);
end;

function TAssembler80.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of binary or');
  NeedNumber(-1, 'on right hand side of binary or');
  Result.BufInt := (ParserM3.BufInt and $FFFF) or
    (ParserM1.BufInt and $FFFF);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of shl');
  NeedPosNumber(-1, 'on right hand side of shl');
  Result.BufInt := (ParserM3.BufInt and $FFFF) shl ParserM1.BufInt;
  Result.BufInt := Result.BufInt and $FFFF;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of shr');
  NeedPosNumber(-1, 'on right hand side of shr');
  Result.BufInt := (ParserM3.BufInt and $FFFF) shr ParserM1.BufInt;
  Result.BufInt := Result.BufInt and $FFFF;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of subtract');
  NeedNumber(-1, 'on right hand side of subtract');
  Result.BufInt := ParserM3.BufInt - ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1, 'for unary minus');
  Result.BufInt := -ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-1);
end;

function TAssembler80.ActExprUnaryPlus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1, 'for unary plus');
  Result.BufInt := ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token, ltError, E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-1);
end;

function TAssembler80.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of binary xor');
  NeedNumber(-1, 'on right hand side of binary xor');
  Result.BufInt := (ParserM3.BufInt and $FFFF) xor
    (ParserM1.BufInt and $FFFF);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
var
  ascval: integer;
begin
  NeedString(-2, 'for ASC() function');
  ascval := 0;
  if Length(ParserM2.Buf) > 0 then
    ascval := Ord(ParserM2.Buf[1]);
  Result.BufInt := ascval;
  Result.BufType := pstINT32;
  Result.Buf := IntToStr(Result.BufInt);
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActFuncDefined(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt   := Ord(FSymbolTable.Defined(ParserM2.Buf));
  Result.BufType  := pstINT32;
  Result.Buf      := IntToStr(Result.BufInt);
  Result.Source   := SourceCombine1(-2);
end;

function TAssembler80.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2, 'for HIGH() function');
  Result.BufInt := (ParserM2.BufInt and $FFFF) shr 8;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-6, 'for IIF() function conditional expression');
  if ParserM6.BufInt <> 0 then
  begin
    Result := ParserM4;
    Result.Source := SourceCombine2(-6, -4);
  end
  else
  begin
    Result := ParserM2;
    Result.Source := SourceCombine2(-6, -2);
  end;
end;

function TAssembler80.ActFuncLength(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-2, 'for LENGTH() function');
  Result.BufInt := Length(ParserM2.Buf);
  Result.BufType := pstINT32;
  Result.Buf := IntToStr(Result.BufInt);
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2, 'for LOW() function');
  Result.BufInt := ParserM2.BufInt and $00FF;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-4, 'for POS() function search');
  NeedString(-2, 'for POS() function target');
  Result.BufInt := Pos(ParserM4.Buf, ParserM2.Buf);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-4, -2);
end;

function TAssembler80.ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
var
  code: integer;
begin
  NeedString(-2, 'for VALUE() function');
  Val(ParserM2.Buf, Result.BufInt, code);
  if code <> 0 then
    ShowErrorToken(ParserM2.Token, ltError, E2012_CONVERSION_ERROR, [ParserM2.Buf]);
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt := VariableFromHexLiteral(ParserM1.Buf);
  Result.BufType := pstINT32;
  Result.Source := pssConstant;
end;

function TAssembler80.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

{
function TAssembler80.ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;
}

function TAssembler80.ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of logical and');
  NeedNumber(-1, 'on right hand side of logical and');
  if (ParserM3.BufInt <> 0) and (ParserM1.BufInt <> 0) then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

function TAssembler80.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1, 'on right hand side of logical not');
  if (ParserM1.BufInt = 0) then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine1(-1);
end;

function TAssembler80.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3, 'on left hand side of logical or');
  NeedNumber(-1, 'on right hand side of logical or');
  if (ParserM3.BufInt <> 0) or (ParserM1.BufInt <> 0) then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

{
function TAssembler80.ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActMandateInt(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;
}

function TAssembler80.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt := VariableFromOctLiteral(ParserM1.Buf);
  Result.BufType := pstINT32;
  Result.Source := pssConstant;
end;

{
function TAssembler80.ActParamList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;
}

function TAssembler80.ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FinalVal := ParserM2;
  ParsedOperandOption := OPER_U16_IND;
  Result := ParserM2;
end;

function TAssembler80.ActSetOpIndOffIX(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FinalVal := ParserM2;
  ParsedOperandOption := OPER_IXPD_IND;
  Result := ParserM2;
end;

function TAssembler80.ActSetOpIndOffIY(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FinalVal := ParserM2;
  ParsedOperandOption := OPER_IYPD_IND;
  Result := ParserM2;
end;

function TAssembler80.ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FinalVal := ParserM1;
  ParsedOperandOption := OPER_U16;
  Result := ParserM1;
end;

function TAssembler80.ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := EnvObject.Build;
  Result.BufType := pstString;
  Result.Source := pssConstant;
end;

function TAssembler80.ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedPosNumber(-2, 'for CHR() function');
  Result.Buf := Chr(ParserM2.BufInt);
  Result.BufType := pstString;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('yyyy-mm-dd', StartTime);
  Result.BufType := pstString;
  Result.Source := pssConstant;
end;

function TAssembler80.ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2, 'for HEX() function');
  Result.Buf := IntToHex(ParserM2.BufInt and $FFFF, 1);
  Result.BufType := pstString;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-4, 'for HEX() function value');
  NeedPosNumber(-4, 'for HEX() function digits', 1);
  Result.Buf := IntToHex(ParserM4.BufInt and $FFFF, ParserM2.BufInt);
  Result.BufType := pstString;
  Result.Source := SourceCombine2(-4, -2);
end;

function TAssembler80.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := StripQuotesAndEscaped(ParserM1.Buf);
  Result.BufType := pstString;
  Result.Source := pssConstant;
end;

function TAssembler80.ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-4, 'for LEFT() source');
  NeedPosNumber(-2, 'for LEFT() count');
  Result.Buf := LeftStr(ParserM4.Buf, ParserM2.BufInt);
  Result.BufType := pstString;
  Result.Source := SourceCombine2(-4, -2);
end;

function TAssembler80.ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-2, 'for LOWER() function');
  Result.Buf := LowerCase(ParserM2.Buf);
  Result.BufInt := 0;
  Result.BufType := pstString;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-6, 'for MID() source');
  NeedPosNumber(-4, 'for MID() start', 1);
  NeedPosNumber(-2, 'for MID() count');
  Result.Buf := Copy(ParserM6.Buf, ParserM4.BufInt, ParserM2.BufInt);
  Result.BufType := pstString;
  Result.Source := SourceCombine3(-6, -4, -2);
end;

function TAssembler80.ActStrProcessor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FProcessor;
  Result.BufType := pstString;
  Result.Source := pssConstant;
end;

function TAssembler80.ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-4, 'for RIGHT() source');
  NeedPosNumber(-2, 'for RIGHT() count');
  Result.Buf := RightStr(ParserM4.Buf, ParserM2.BufInt);
  Result.BufType := pstString;
  Result.Source := SourceCombine2(-4, -2);
end;

function TAssembler80.ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2, 'for STRING() function');
  Result.Buf := IntToStr(ParserM2.BufInt);
  Result.BufType := pstString;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('hh:nn:ss', StartTime);
  Result.BufType := pstString;
  Result.Source := pssConstant;
end;

function TAssembler80.ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-2, 'for UPPER() function');
  Result.Buf := UpperCase(ParserM2.Buf);
  Result.BufType := pstString;
  Result.Source := SourceCombine1(-2);
end;

function TAssembler80.ActStrVersion(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := EnvObject.Version;
  Result.BufType := pstString;
  Result.Source := pssConstant;
end;

{
function TAssembler80.ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;
}

function TAssembler80.ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
  Result.BufInt := FOrg;
  Result.Source := pssConstant;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActValueParam(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // @@@@@ Add code to put the expanded parameter value in here
  Result := ParserM1;
end;

function TAssembler80.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
var
  Name: string;
  idx: integer;
  sym: TSymbol;

  function SetSource: TParserStackSource;
  begin
    if sym.Defined then
      SetSource := pssConstant
    else
      SetSource := pssUndefined;
  end;

begin
  Result := ParserM1;
  Name := ParserM1.Buf;
  idx := FSymbolTable.IndexOf(Name);
  if idx < 0 then
    // Symbol not found, we should add a placeholder for it
    // But not if we're in a failed IF statement etc.
    // Or if preparser CmdFlags contains cfNoPlaceHolder
    begin
      if FPreparser.DFA.IsReserved(Name) then
        ErrorObj.Show(ltError, E2030_USING_RESERVED_AS_LABEL, [Name]);
      Result.BufInt := 0; // Assume a forward address for now
      Result.BufType := pstINT32;
      Result.Source := pssUndefined;
      if FSolGenerate and (not (cfNoPlaceholder in FPreparser.CmdFlags)) then
        FSymbolTable.Add(Name, stAddress, 0, '', False, True);
    end
  else
    begin
      sym := FSymbolTable[idx];
      sym.Referenced := True;
      FSymbolTable[idx] := sym;
      // @@@@@ Set up the variable/constant thing from the symbol table
      case sym.SymType of
        stUnknown: begin
          Result.BufType := pstINT32;
          Result.BufInt := 0;
          Result.Source := pssUndefined;
        end;
        stAddress: begin
          Result.BufType := pstINT32;
          Result.BufInt := sym.IValue;
          Result.Source := SetSource;
        end;
        stWord: begin
          Result.BufType := pstINT32;
          Result.BufInt := sym.IValue;
          Result.Source := SetSource;
        end;
        stString: begin
          Result.BufType := pstString;
          Result.Buf := sym.SValue;
          Result.Source := SetSource;
        end;
      end;
    end;
end;

procedure TAssembler80.ApplyCommandLineDefines;
var sl: TStringList;
    sl2: TStringList;
    i:  integer;
    s:  string;
begin
  if FOptionDefines <> '' then
    begin
      sl := TStringList.Create;
      try
        sl.Delimiter := ';';
        sl.DelimitedText := FOptionDefines;
        for i := 0 to sl.Count-1 do
          begin
            s := sl[i];
            if Pos('=',s) = 0 then
              begin // Simple define with no assigment
                FSymbolTable.Add(s,stWord,0,'',True,False);
              end
            else
              begin
                sl2 := TStringList.Create;
                try
                  sl2.Delimiter := '=';
                  sl2.DelimitedText := s;
                  if sl2.Count <> 2 then
                    ErrorObj.Show(ltError,E2059_COMMAND_LINE_DEFINE,[s]);
                  if (sl2[1] <> '') and (not (sl2[1][1] in ['0'..'9'])) then
                    FSymbolTable.Add(sl2[0],stString,0,StripQuotes(sl2[1]),True,False)
                  else
                    FSymbolTable.Add(sl2[0],stWord,StrToInt(sl2[1]),'',True,False);
                finally
                  FreeAndNil(sl2);
                end;
              end;
          end;
      finally
        FreeAndNil(sl);
      end;
    end;
end;

procedure TAssembler80.AsmProcessLabel(const _label: string; _command_index: integer);
var
  equate_or_macro: boolean;
  _index: integer;
  _symbol: TSymbol;
begin
  equate_or_macro := (_command_index >= 0) and
    ((FCmdList[_command_index].CommandName = '=') or
    (FCmdList[_command_index].CommandName = 'EQU') or
    (FCmdList[_command_index].CommandName = 'MACRO') or
    (FCmdList[_command_index].CommandName = '.EQU') or
    (FCmdList[_command_index].CommandName = '.MACRO'));
  if (_label <> '') and not equate_or_macro then
  begin // Must be an ordinary program label, cannot already be assigned
    if FPreparser.ForceColon and (not HasColon(_label)) then
    begin
      ErrorObj.ColNumber := 1; // Must be in column 1
      ErrorObj.Show(ltError, E2016_COLON_NOT_PRESENT, [_label]);
    end;
    _index := FSymbolTable.IndexOf(StripColon(_label));
    if (_index >= 0) then
      begin
        _symbol := FSymbolTable[_index];
        if (FPass = 1) then
          // Check for forward reference already defined
          if (FSymbolTable[_index].SymType = stAddress) and
            (FSymbolTable[_index].Defined = False) then
          begin
            _symbol.IValue := FOrg;
            _symbol.Defined := True;
            ;
          end
          else
            ErrorObj.Show(ltError, E2015_CODE_SYMBOL_DEFINED, [_label]);
        _symbol.DefinedPass := FPass;
        FSymbolTable[_index] := _symbol;
      end
    else
      // Add the symbol at the current address
      FSymbolTable.Add(StripColon(_label), stAddress, FOrg, '', True, False);
  end;
end;

// Key routine - assemble a whole file

procedure TAssembler80.Assemble(const filename: string);
begin
  if not FileExists(filename) then
    ErrorObj.Show(ltError, E2031_FILE_NOT_FOUND, [filename]);
  ResetMemory;
  FListing.Reset;
  FSymbolTable.Clear;
  FilenameAsm := filename;
  FilenameCom := MakeFilename(FilenameAsm, OptionCom, '.com');
  if FilenameCom <> '' then
    ErrorObj.Show(ltVerbose, I0004_FILENAME_ASSIGNMENT, ['com', FilenameCom]);
  FilenameListing := MakeFilename(FilenameAsm, OptionListing, '.lst');
  if FilenameListing <> '' then
    ErrorObj.Show(ltVerbose, I0004_FILENAME_ASSIGNMENT, ['listing', FilenameListing]);
  FilenameHex := MakeFilename(FilenameAsm, OptionHex, '.hex');
  if FilenameHex <> '' then
    ErrorObj.Show(ltVerbose, I0004_FILENAME_ASSIGNMENT, ['hex', FilenameHex]);
  FilenameMap := MakeFilename(FilenameAsm, OptionMap, '.map');
  if FilenameMap <> '' then
    ErrorObj.Show(ltVerbose, I0004_FILENAME_ASSIGNMENT, ['map', FilenameMap]);
  FilenameError := MakeFilename(FilenameAsm, OptionError, '.log');
  if FilenameError <> '' then
    ErrorObj.Show(ltVerbose, I0004_FILENAME_ASSIGNMENT, ['error log', FilenameError]);
  ApplyCommandLineDefines;
  ErrorObj.SetLogFilename(FilenameError);
  ErrorObj.Show(ltInfo, I0003_ASSEMBLING_FILE, [filename]);
  FPreparser.CaseSensitive := CaseSensitive;
  AssemblePass(1, filename);
  AssemblePass(2, filename);
  CheckStack;
  CheckMacroDone;
  ErrorObj.ColNumber  := 0;
  ErrorObj.LineNumber := 0;
  ErrorObj.Filename   := '';
  CheckLabelsDefined;
  OutputMemoryToCom(FilenameCom);
  OutputMemoryToHex(FilenameHex);
  FSymbolTable.DumpByBoth(FFilenameMap);
  ErrorObj.SetLogFilename('');
end;

// Key routine - assemble a single line of text from the input

procedure TAssembler80.AssembleLine(const _s: string);
var
  labelx: string;
  command_index: integer;
  opcode_index: integer;
  macro_index:  integer;
  inst_rec: TInstructionRec;
  oper1, oper2: TOperandOption;
  s: string;
  indent_str: string;
  elem: TCodeElement;
  i: integer;
  opval: integer;
  newaddr: integer;
  macro_entry: TMacroEntry;

  function GetOpVal(_idx: integer): integer;
  begin
    CheckOperandInteger(_idx, -32768, 65535);
    GetOpVal := FPreparser[_idx].IntValue;
  end;

begin
  FSolGenerate := FAsmStack.CanGenerate;
  ErrorObj.SourceLine := _s;
  FCodeBuffer.Init;
  Parse(_s,FDefiningMacro); // Do the pre-parsing and main parsing
  labelx := FPreparser.LabelX;
  opcode_index := FPreparser.OpcodeIndex;
  command_index := FPreparser.CommandIndex;
  macro_index   := FPreparser.MacroIndex;
  if (not FDefiningMacro) and FSolGenerate then
    AsmProcessLabel(labelx, command_index);
  // Check for end
  if FEnded and ((command_index > 0) or (opcode_index > 0)) then
    ErrorObj.Show(ltError, E2037_CODE_AFTER_END);
  // Process commands if available
  if command_index >= 0 then
    if FSolGenerate or (cfBypass in FCmdList[command_index].CommandFlags) then
      if (cfDuringMD in FCmdList[command_index].CommandFlags) or (not FDefiningMacro) then
        FCmdList[command_index].CommandExec(labelx, FPreparser);
  // Assemble opcodes if available
  if (opcode_index >= 0) and
     FSolGenerate and
     (not FDefiningMacro) and
     (macro_index < 0) then
  begin
    oper1 := OPER_NULL;
    oper2 := OPER_NULL;
    inst_rec.CodeElementCount := 0;
    if FPreparser.Count > 0 then
      oper1 := TOperandOption(FPreparser[0].Index);
    if FPreparser.Count > 1 then
      oper2 := TOperandOption(FPreparser[1].Index);
    if not FInstructionList.FindInstruction(opcode_index, oper1, oper2, inst_rec) then
    begin // Instruction not found
      s := FInstructionList.OpcodeAtIndex(opcode_index);
      if oper1 <> OPER_NULL then
        s := s + ' ' + OperandSanitised[oper1];
      if oper2 <> OPER_NULL then
        s := s + ',' + OperandSanitised[oper2];
      ErrorObj.ColNumber := FPreparser.OpcodeCol;
      ErrorObj.Show(ltError, E2021_INSTRUCTION_UNAVAILABLE, [s]);
    end;
    // Add the code to populate the output buffer using inst_rec
    for i := 0 to inst_rec.CodeElementCount - 1 do
    begin
      elem := inst_rec.CodeElements[i];
      opval := 0;
      case elem.OperandNo of
        1:
        begin
          ErrorObj.ColNumber := FPreparser[0].Column;
          opval := GetOpVal(0);
        end;
        2:
        begin
          ErrorObj.ColNumber := FPreparser[1].Column;
          opval := GetOpVal(1);
        end;
      end;
      case elem.ElementType of
        cetNull:
          ErrorObj.Show(ltInternal, X3007_INVALID_ELEMENT_TYPE);
        cetB3:
        begin
          if (opval < 0) or (opval > 7) then
            ErrorObj.Show(ltError, E2028_BIT_NUMBER);
          FCodeBuffer.Push(elem.Value or (opval shl elem.Offset));
        end;
        cetHex:
          FCodeBuffer.Push(elem.Value);
        cetIM:
        begin
          if (opval < 0) or (opval > 2) then
            ErrorObj.Show(ltError, E2029_IM_NUMBER);
          case opval of
            0: FCodeBuffer.Push($46);
            1: FCodeBuffer.Push($56);
            2: FCodeBuffer.Push($5E);
          end;
        end;
        cetR8:
          if FPass = 1 then
            FCodeBuffer.Push(0)
          else
          begin
            newaddr := opval - (FOrg + 2);
            if (newaddr < -128) or (newaddr > 127) then
              ErrorObj.Show(ltError, E2027_RELATIVE_DISTANCE, [newaddr]);
            FCodeBuffer.Push(newaddr and $FF);
          end;
        cetS8:
          FCodeBuffer.Push(opval and $FF);
        cetRST:
        begin
          if (opval > 7) and ((opval mod 8) = 0) then
            opval := opval div 8;
          if (opval < 0) or (opval > 7) then
            ErrorObj.Show(ltError, E2028_BIT_NUMBER);
          FCodeBuffer.Push(elem.Value or (opval shl elem.Offset));
        end;
        cetU8:
        begin
          CheckByte(opval);
          FCodeBuffer.Push(opval and $FF);
        end;
        cetU16:
        begin
          FCodeBuffer.Push(opval and $FF);
          FCodeBuffer.Push((opval shr 8) and $FF);
        end;
      end;
    end;
  end;
  // Add macro line if required
  if (FPass = 1) and FDefiningMacro then
    FDefMacro.Content.Add(_s);
  // Do the listing and increase FOrg
  if (FPass = 2) then
  begin
    // Listing
    // @@@@@ Set macro indent here if required
    indent_str := Format('%5d', [FInputLine]);
    if FSolGenerate then
      indent_str := indent_str + ' |'
    else
      indent_str := indent_str + ' :';
    if FSolGenerate and (FCodeBuffer.Contains > 0) then
      s := Format('%4.4X: %s %s %s', [FOrg, FCodeBuffer.AsString, indent_str, _s])
    else
      s := Format('%s %s %s', [Space(6 + MAX_HEX_WIDTH), indent_str, _s]);
    FListing.Output(s);
    // Code
    for i := 0 to FCodeBuffer.Contains - 1 do
      begin
        if FSolGenerate then
          begin
            FMemory[Org] := FCodeBuffer.Buffer[i];
            FMemoryUsed[Org] := True;
          end;
        Org := Org + 1;
      end;
  end
  else
  if FSolGenerate then
    Org := Org + FCodeBuffer.Contains;
  // Process the include file if required
  ProcessInclude;
  // Push the macro details if available
  if macro_index >= 0 then
    begin
      macro_entry := FMacroList.Items[macro_index];
      macro_entry.Params.Clear;
      for i := 0 to FPreparser.Count-1 do
        macro_entry.Params.Add(FPreparser.Items[i].Payload);
      // Run the macro expansion here
      ExpandMacro(macro_entry);
    end;
end;

procedure TAssembler80.AssemblePass(_pass: integer; const filename: string);
begin
  FPass := _pass;
  FPreparser.Pass := _pass;
  FSymbolTable.Pass := _pass;
  FAsmStack.Clear;
  FListing.Listing := True;

  ErrorObj.Filename := filename;
  DefiningMacro := False;
  FEnded := False;
  FNextInclude := '';
  FOrg := 0;
  FMacroList.Init;
  FCmdList.InitPass;
  ProcessFile(filename);
end;

procedure TAssembler80.CheckByte(_i: integer);
begin
  if (_i > 255) then
    _i := _i - 65536;
  if (_i < -128) or (_i > 255) then
    ErrorObj.Show(ltError, E2023_BYTE_RANGE_ERROR);
end;

procedure TAssembler80.CheckNoLabel(const _label: string);
begin
  if (_label <> '') and (Pass = 1) then
    ErrorObj.Show(ltError, E2020_UNEXPECTED_LABEL, [_label]);
end;

procedure TAssembler80.CheckInteger(_i, _min, _max: integer);
begin
  if (_i < _min) or (_i > _max) then
    ErrorObj.Show(ltError, E2026_INTEGER_RANGE_ERROR, [_min, _max]);
end;

procedure TAssembler80.CheckLabelsDefined;
var sym: TSymbol;
begin
  for sym in FSymbolTable do
    if (not sym.Defined) and (sym.Area in [saInternal,saExported]) then
      ErrorObj.Show(ltWarning,W1005_SYMBOL_UNDEFINED,[sym.Name]);
end;

procedure TAssembler80.CheckMacroDone;
var _msg: string;
begin
  if FDefiningMacro then
    begin
      _msg := Format('No ENDM command for MACRO defined on line %d of %s',
              [FDefMacro.LineNumber, FDefMacro.Filename]);
      ErrorObj.Show(ltError, E2047_UNEXPECTED_END, [_msg]);
    end;
end;

procedure TAssembler80.CheckOperandByte(_index: integer);
begin
  CheckOperandInteger(_index, -128, 255);
end;

procedure TAssembler80.CheckOperandCount(_minop, _maxop: integer);
begin
  if FPreparser.Count < _minop then
    ErrorObj.Show(ltError, E2022_OPERANDS_EXPECTED);
  if FPreparser.Count > _maxop then
  begin
    ErrorObj.ColNumber := FPreparser[_maxop].Column;
    ErrorObj.Show(ltError, E2017_UNEXPECTED_OPERANDS);
  end;
end;

procedure TAssembler80.CheckOperandInteger(_index, _min, _max: integer);
var
  itm: TParserProp;
begin
  if _index >= FPreparser.Count then
    ErrorObj.Show(ltError, E2022_OPERANDS_EXPECTED);
  itm := FPreparser[_index];
  if itm.DataType <> pstINT32 then
  begin
    if (itm.DataType = pstString) and (Length(itm.Payload) = 3) and
      (itm.Payload[1] = '''') then
    begin
      itm.DataType := pstInt32;
      itm.IntValue := Ord(itm.Payload[2]);
      FPreparser[_index] := itm;
    end
    else
      ErrorObj.Show(ltError, E2019_EXPECTED_INTEGER);
  end;
  CheckInteger(FPreparser[_index].IntValue, _min, _max);
end;

procedure TAssembler80.CheckStack;
var
  _tos: TAsmStackEntry;
  _msg: string;

  procedure CreateMessage(const _missing, _for: string);
  begin
    _msg := Format('No %s command for %s defined on line %d of %s',
      [_missing, _for, _tos.LineNumber, _tos.Filename]);
  end;

begin
  if FAsmStack.Count > 0 then
  begin // Shouldn't be anything left on the stack at the end of assembly
    _tos := FAsmStack.TOS;
    case _tos.EntryType of
      setNone: ErrorObj.Show(ltInternal, X3001_UNHANDLED_CASE_OPTION,
          ['TAssembler80.CheckStack']);
      setIf: CreateMessage('ENDIF', 'IF');
      setWhile: CreateMessage('ENDW', 'WHILE');
      setRepeat: CreateMessage('ENDR', 'REPEAT');
    end;
    ErrorObj.Show(ltError, E2047_UNEXPECTED_END, [_msg]);
  end;
end;

procedure TAssembler80.CheckStringNotEmpty(const _s: string);
begin
  if _s = '' then
    ErrorObj.Show(ltError, E2025_EMPTY_STRING_NOT_ALLOWED);
end;

procedure TAssembler80.CmdCODE(const _label: string; _preparser: TPreparserBase);
begin
end;

procedure TAssembler80.CmdCPU(const _label: string; _preparser: TPreparserBase);
begin
  if FPass = 1 then
    ErrorObj.Show(ltWarning, W1002_DIRECTIVE_IGNORED, ['CPU']);
end;

procedure TAssembler80.CmdDATA(const _label: string; _preparser: TPreparserBase);
begin
end;

procedure TAssembler80.CmdDB(const _label: string; _preparser: TPreparserBase);
begin
  CommonDB(_preparser,False);
end;

procedure TAssembler80.CmdDC(const _label: string; _preparser: TPreparserBase);
begin
  CommonDB(_preparser,True);
end;

procedure TAssembler80.CmdDS(const _label: string; _preparser: TPreparserBase);
var
  bcount: integer;
  bval: integer;
begin
  // Define storage
  // Two forms of this command:
  //    DS <storagesize>
  //    DS <storagesize>,<bytetofill>
  CheckOperandCount(1, 2);
  case _preparser.Count of
    1: begin
      CheckOperandInteger(0, 0, MAX_BYTES_PER_CODE_RECORD - 1);
      bcount := _preparser[0].IntValue;
      bval := 0;
    end;
    2: begin
      CheckOperandInteger(0, 0, MAX_BYTES_PER_CODE_RECORD - 1);
      CheckOperandByte(1);
      bcount := _preparser[0].IntValue;
      bval := _preparser[1].IntValue;
    end;
  end;
  // Go through the operands populating the code buffer
  FCodeBuffer.PushMany(bcount, (bval and $FF), (_preparser.Count = 2));
end;

procedure TAssembler80.CmdDW(const _label: string; _preparser: TPreparserBase);
var
  itm: TParserProp;
  i: integer;
begin
  // Define words
  // There should be one or more operands
  CheckOperandCount(1, 9999);
  // Go through the operands populating the code buffer
  for i := 0 to _preparser.Count - 1 do
  begin
    itm := _preparser[i];
    ErrorObj.ColNumber := itm.Column;
    case itm.DataType of
      pstNone: ErrorObj.Show(ltError, E2018_OPERAND_NO_DATA_TYPE, [i + 1]);
      pstINT32: begin
        CheckOperandInteger(i, -32768, 65535);
        FCodeBuffer.Push((itm.IntValue and $FF));
        FCodeBuffer.Push((itm.IntValue shr 8) and $FF);
      end;
      pstString: ErrorObj.Show(ltError, E2019_EXPECTED_INTEGER);
    end;
  end;
end;

procedure TAssembler80.CmdELSE(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should be no operands
  CheckOperandCount(0, 0);
  if FAsmStack.TOStype <> setIf then
    ErrorObj.Show(ltError, E2049_UNEXPECTED_ELSE);
  entry := FAsmStack.TOS;
  if entry.ElseDoneAlready then
    ErrorObj.Show(ltError, E2050_ELSE_ALREADY_USED);
  entry.EvalResult := not entry.EvalResult;
  entry.ElseDoneAlready := True;
  FAsmStack.Items[FAsmStack.Count - 1] := entry;
end;

procedure TAssembler80.CmdEND(const _label: string; _preparser: TPreparserBase);
begin
  // There should be no operands, if there are we produce a message
  CheckOperandCount(0, 0);
  CheckMacroDone;
  CheckStack;
  FEnded := True;
end;

procedure TAssembler80.CmdENDIF(const _label: string; _preparser: TPreparserBase);
begin
  // Should only be no operands
  CheckOperandCount(0, 0);
  if FAsmStack.TOStype <> setIf then
    ErrorObj.Show(ltError, E2048_UNEXPECTED_ENDIF);
  FAsmStack.Pop;
end;

procedure TAssembler80.CmdENDM(const _label: string; _preparser: TPreparserBase);
var newobj: TMacroEntry;
begin
  if not FDefiningMacro then
    ErrorObj.Show(ltError,E2055_UNEXPECTED_ENDM);
  if FPass = 1 then
    begin
      if FPreparser.LabelX <> '' then
        begin
          ErrorObj.ColNumber := 1;
          ErrorObj.Show(ltError,E2058_NO_LABEL_ON_ENDM);
        end;
      // Set up a new macro object
      newobj := TMacroEntry.Create;
      // Clone the master object
      newobj.Name       := FDefMacro.Name;
      newobj.LineNumber := FDefMacro.LineNumber;
      newobj.Filename   := FDefMacro.Filename;
      newobj.Headings.Assign(FDefMacro.Headings);
      newobj.Params.Assign(FDefMacro.Params);
      newobj.Content.Assign(FDefMacro.Content);
      // Delete first row from the macro content as it's the MACRO command!
      if newobj.Content.Count > 0 then
        newobj.Content.Delete(0);
      FMacroList.Add(newobj);
    end;
  // Finally
  DefiningMacro := False;
end;

procedure TAssembler80.CmdENDR(const _label: string; _preparser: TPreparserBase);
var entry: TAsmStackEntry;
begin
  // Should only be no operands
  CheckOperandCount(0, 0);
  if FAsmStack.TOStype <> setRepeat then
    ErrorObj.Show(ltError, E2053_UNEXPECTED_ENDR);
  // Check it's in the same file as the REPEAT statement
  entry := FAsmStack.TOS;
  if entry.Filename <> FCurrentFile then
    ErrorObj.Show(ltError, E2054_ENDR_IN_DIFFERENT_FILE, [entry.Filename]);
  entry.RepeatRemain := entry.RepeatRemain - 1;
  entry.EvalResult := entry.RepeatRemain > 0;
  FAsmStack[FAsmStack.Count-1] := entry;
  if entry.EvalResult then
    FInputLine := entry.LineNumber - 1  // Go round again
  else
    FAsmStack.Pop;                      // or end the repeat statement loop
end;

procedure TAssembler80.CmdENDW(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should only be no operands
  CheckOperandCount(0, 0);
  if FAsmStack.TOStype <> setWhile then
    ErrorObj.Show(ltError, E2051_UNEXPECTED_ENDW);
  // Check it's in the same file as the WHILE statement
  entry := FAsmStack.TOS;
  if entry.Filename <> FCurrentFile then
    ErrorObj.Show(ltError, E2052_ENDW_IN_DIFFERENT_FILE, [entry.Filename]);
  if entry.EvalResult then
    FInputLine := entry.LineNumber - 1  // Go round again
  else
    FAsmStack.Pop;                      // or end the while statement loop
end;

procedure TAssembler80.CmdEQU(const _label: string; _preparser: TPreparserBase);
begin
  EQUCore(_label, _preparser, False);
end;

procedure TAssembler80.CmdEQU2(const _label: string; _preparser: TPreparserBase);
begin
  EQUCore(_label, _preparser, True);
end;

procedure TAssembler80.CmdEXTERN(const _label: string; _preparser: TPreparserBase);
begin
end;

procedure TAssembler80.CmdGLOBAL(const _label: string; _preparser: TPreparserBase);
begin
end;

procedure TAssembler80.CmdIF(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should only be one operand
  CheckOperandCount(1, 1);
  CheckOperandInteger(0, -32767, 65535);
  entry.EntryType := setIf;
  entry.ElseDoneAlready := False;
  entry.EvalResult := (FPreparser[0].IntValue <> 0);
  entry.ParentGen := FSolGenerate;
  entry.Filename := FCurrentFile;
  entry.LineNumber := FInputLine;
  entry.RepeatRemain := 0;
  FAsmStack.Add(entry);
end;

procedure TAssembler80.CmdIFDEF(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should only be one operand
  CheckOperandCount(1, 1);
  entry.EntryType := setIf;
  entry.ElseDoneAlready := False;
  entry.EvalResult := FSymbolTable.Defined(FPreparser[0].Payload);
  entry.ParentGen := FSolGenerate;
  entry.Filename := FCurrentFile;
  entry.LineNumber := FInputLine;
  entry.RepeatRemain := 0;
  FAsmStack.Add(entry);
end;

procedure TAssembler80.CmdIFNDEF(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should only be one operand
  CheckOperandCount(1, 1);
  entry.EntryType := setIf;
  entry.ElseDoneAlready := False;
  entry.EvalResult := not FSymbolTable.Defined(FPreparser[0].Payload);
  entry.ParentGen := FSolGenerate;
  entry.Filename := FCurrentFile;
  entry.LineNumber := FInputLine;
  entry.RepeatRemain := 0;
  FAsmStack.Add(entry);
end;

procedure TAssembler80.CmdINCLUDE(const _label: string; _preparser: TPreparserBase);
begin
  // Should only be one operand
  CheckOperandCount(1, 1);
  if FPreparser[0].DataType <> pstString then
    ErrorObj.Show(ltError, E2010_EXPECTED_STRING, ['for INCLUDE command']);
  FNextInclude := FPreparser[0].StrValue;
end;

procedure TAssembler80.CmdLISTON(const _label: string; _preparser: TPreparserBase);
begin
  // Turn listings on
  // There should be no operands
  CheckOperandCount(0, 0);
  FListing.Listing := True;
end;

procedure TAssembler80.CmdMACRO(const _label: string; _preparser: TPreparserBase);
var i: integer;
    payload: string;
begin
  if DefiningMacro then
    ErrorObj.Show(ltError,E2056_MACRO_IN_MACRO_DEFINE);
  DefiningMacro := True;
  if FPass = 1 then
    begin
      FDefMacro.Name       := StripColon(_label);
      FDefMacro.Filename   := FCurrentFile;
      FDefMacro.LineNumber := FInputLine;
      FDefMacro.Headings.Clear;
      FDefMacro.Params.Clear;
      FDefMacro.Content.Clear;
      if FDefMacro.Name = '' then
        begin
          ErrorObj.ColNumber := 1;
          ErrorObj.Show(ltError,E2046_EXPECTED_LABEL,['nothing']);
        end;
      if not FCaseSensitive then
        FDefMacro.Name := UpperCase(FDefMacro.Name);
      for i := 0 to _preparser.Count-1 do
        begin
          payload := _preparser.Items[i].Payload;
          if not FCaseSensitive then
            payload := UpperCase(payload);
          FDefMacro.Headings.Add(payload);
        end;
    end;
end;

procedure TAssembler80.CmdMSGERROR(const _label: string; _preparser: TPreparserBase);
begin
  // Issue an error message
  // There should be 1 operand (numeric or string)
  CheckOperandCount(1, 1);
  if FPass = 1 then
  begin
    ErrorObj.ColNumber := 0;
    ErrorObj.Show(ltError, E2000_USER_ERROR, [FPreparser[0].StrValue]);
  end;
end;

procedure TAssembler80.CmdMSGINFO(const _label: string; _preparser: TPreparserBase);
begin
  // Issue an info message
  // There should be 1 operand (numeric or string)
  CheckOperandCount(1, 1);
  if FPass = 1 then
  begin
    ErrorObj.ColNumber := 0;
    ErrorObj.Show(ltInfo, I0000_USER_INFO, [FPreparser[0].StrValue]);
  end;
end;

procedure TAssembler80.CmdMSGWARNING(const _label: string; _preparser: TPreparserBase);
begin
  // Issue a warning message
  // There should be 1 operand (numeric or string)
  CheckOperandCount(1, 1);
  if FPass = 1 then
  begin
    ErrorObj.ColNumber := 0;
    ErrorObj.Show(ltWarning, W1000_USER_WARNING, [FPreparser[0].StrValue]);
  end;
end;

procedure TAssembler80.CmdLISTOFF(const _label: string; _preparser: TPreparserBase);
begin
  // Turn listings off
  // There should be no operands
  CheckOperandCount(0, 0);
  FListing.Listing := False;
end;

procedure TAssembler80.CmdWARNOFF(const _label: string; _preparser: TPreparserBase);
begin
  // Turn warnings off
  // There should be no operands
  CheckOperandCount(0, 0);
  ErrorObj.Warnings := False;
end;

procedure TAssembler80.CmdORG(const _label: string; _preparser: TPreparserBase);
begin
  // Set the origin for the assembly
  // There should be one and only one operand and no label
  CheckOperandCount(1, 1);
  CheckNoLabel(_label);
  case _preparser[0].DataType of
    pstNone,
    pstString: ErrorObj.Show(ltError, E2019_EXPECTED_INTEGER);
    pstINT32: begin
      CheckOperandInteger(0, 0, $FFFF);
      FOrg := _preparser[0].IntValue;
    end;
  end; // case
end;

procedure TAssembler80.CmdREPEAT(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should only be one operand
  CheckOperandCount(1, 1);
  CheckOperandInteger(0, 0, 65535);
  // Check if the top of stack is already this repeat loop in progress
  if (FAsmStack.TOStype = setRepeat) then
  begin
    entry := FAsmStack.TOS;
    if (entry.Filename = FCurrentFile) and (entry.LineNumber = FInputLine) then
      Exit;
  end;
  // Must be a new REPEAT statement
  entry.EntryType := setRepeat;
  entry.ElseDoneAlready := False;
  entry.EvalResult := FPreparser[0].IntValue > 0;
  entry.ParentGen := FSolGenerate;
  entry.Filename := FCurrentFile;
  entry.LineNumber := FInputLine;
  entry.RepeatRemain := FPreparser[0].IntValue;
  FAsmStack.Add(entry);
end;

procedure TAssembler80.CmdTITLE(const _label: string; _preparser: TPreparserBase);
begin
  // Set the title
  // There should one string operand containing the title
  CheckOperandCount(1, 1);
  if FPreparser[0].DataType <> pstString then
  begin
    ErrorObj.ColNumber := FPreparser[0].Column;
    ErrorObj.Show(ltError, E2010_EXPECTED_STRING, ['']);
  end;
  Title := FPreparser[0].StrValue;
end;

procedure TAssembler80.CmdUDATA(const _label: string; _preparser: TPreparserBase);
begin
end;

procedure TAssembler80.CmdWARNON(const _label: string; _preparser: TPreparserBase);
begin
  // Turn warnings back on
  // There should be no operands
  CheckOperandCount(0, 0);
  ErrorObj.Warnings := True;
end;

procedure TAssembler80.CmdWHILE(const _label: string; _preparser: TPreparserBase);
var
  entry: TAsmStackEntry;
begin
  // Should only be one operand
  CheckOperandCount(1, 1);
  CheckOperandInteger(0, -32767, 65535);
  // Check if the top of stack is already this while loop in progress
  if (FAsmStack.TOStype = setWhile) then
  begin
    entry := FAsmStack.TOS;
    if (entry.Filename = FCurrentFile) and (entry.LineNumber = FInputLine) then
    begin // This is our while loop going round again
      entry.EvalResult := (FPreparser[0].IntValue <> 0);
      FAsmStack[FAsmStack.Count - 1] := entry;
      Exit;
    end;
  end;
  // Must be a new WHILE statement
  entry.EntryType := setWhile;
  entry.ElseDoneAlready := False;
  entry.EvalResult := (FPreparser[0].IntValue <> 0) and FSolGenerate;
  entry.ParentGen := FSolGenerate;
  entry.Filename := FCurrentFile;
  entry.LineNumber := FInputLine;
  entry.RepeatRemain := 0;
  FAsmStack.Add(entry);
end;

procedure TAssembler80.CommonDB(_preparser: TPreparserBase; _highbit: boolean);
var
  itm: TParserProp;
  i: integer;
  j: integer;
  s: string;
begin
  // Define bytes
  // There should be one or more operands
  CheckOperandCount(1, 9999);
  // Go through the operands populating the code buffer
  for i := 0 to _preparser.Count - 1 do
  begin
    itm := _preparser[i];
    ErrorObj.ColNumber := itm.Column;
    case itm.DataType of
      pstNone: ErrorObj.Show(ltError, E2018_OPERAND_NO_DATA_TYPE, [i + 1]);
      pstINT32: begin
        CheckByte(itm.IntValue);
        FCodeBuffer.Push((itm.IntValue and $FF));
      end;
      pstString:
        begin
          s := _preparser[i].StrValue;
          CheckStringNotEmpty(s);
          for j := 1 to Length(s) do
            if (j < Length(s)) or (not _highbit) then
              FCodeBuffer.Push(Ord(s[j]))
            else
              FCodeBuffer.Push(Ord(s[j]) or $80);
        end;
    end;
  end;
end;

function TAssembler80.CompareGeneric(_comparer: TCompareMode): TLCGParserStackEntry;
var
  compare: boolean;
  s1, s2: string;
  i1, i2: integer;
begin
  compare := False;
  if (ParserM3.BufType = pstString) and (ParserM1.BufType = pstString) then
  begin
    // String comparison
    s1 := ParserM3.Buf;
    s2 := ParserM1.Buf;
    case _comparer of
      cmEqual: compare := (s1 = s2);
      cmNotEqual: compare := (s1 <> s2);
      cmLessThan: compare := (s1 < s2);
      cmLessEqual: compare := (s1 <= s2);
      cmGreaterThan: compare := (s1 > s2);
      cmGreaterEqual: compare := (s1 >= s2);
    end; // case
  end
  else
  begin
    // Numeric comparison
    NeedNumberCompare;
    i1 := ParserM3.BufInt;
    i2 := ParserM1.BufInt;
    case _comparer of
      cmEqual: compare := (i1 = i2);
      cmNotEqual: compare := (i1 <> i2);
      cmLessThan: compare := (i1 < i2);
      cmLessEqual: compare := (i1 <= i2);
      cmGreaterThan: compare := (i1 > i2);
      cmGreaterEqual: compare := (i1 >= i2);
    end; // case
  end;
  if compare then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
  Result.Source := SourceCombine2(-3, -1);
end;

procedure TAssembler80.ConvertOperandToOrd(_index: integer);
begin
  ParserStack[ParserSP + _index].BufType := pstInt32;
  ParserStack[ParserSP + _index].BufInt := Ord(ParserStack[ParserSP + _index].Buf[1]);
end;

procedure TAssembler80.DumpInstructions;
begin
  FInstructionList.Dump(True);
end;

procedure TAssembler80.DumpReserved;
var sl: TStringList;
    i: integer;
begin
  sl := TStringList.Create;
  try
    FCmdList.PopulateStringList(sl);
    FInstructionList.GetInstructions(sl);

    for i := sl.Count-1 downto 0 do
      if (sl[i] = '') or (sl[i][1] < 'A') then
        sl.Delete(i);
    sl.Sort;
    WriteLn(Format('RESERVED WORDS (%d ITEMS IN TOTAL)',[sl.Count]));
    for i := 0 to sl.Count-1 do
      WriteLn(sl[i]);
    WriteLn;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TAssembler80.EQUCore(const _label: string; _preparser: TPreparserBase;
  _allow_redefine: boolean);
var
  _index: integer;
  sym: TSymbol;
begin
  // Make an assignment to the label
  // EQU is allowed to change value during assembly but is not allowed to
  // change type. e.g. cannot define as integer then re-define as string
  // If _allow_redefine is false, a warning will be issued if you try a
  // (valid) redefine

  // There should be one and only one operand
  CheckOperandCount(1, 1);
  // If label exists get it, if not create it
  ErrorObj.ColNumber := _preparser[0].Column;
  _index := FSymbolTable.IndexOf(_label);
  if _index < 0 then
  begin  // Label doesn't exist, create it
    if FPreparser.DFA.IsReserved(_preparser[0].Payload) then
      ErrorObj.Show(ltError, E2030_USING_RESERVED_AS_LABEL, [_label]);
    case _preparser[0].DataType of
      pstNone: ErrorObj.Show(ltError, E2018_OPERAND_NO_DATA_TYPE, [1]);
      pstINT32: FSymbolTable.Add(_label, stWord, _preparser[0].IntValue, '', True, False);
      pstString: FSymbolTable.Add(_label, stString, 0, _preparser[0].StrValue, True, False);
    end; // case
  end
  else
  begin  // Label exists
    sym := FSymbolTable[_index];
    if (Pass = 1) and (not _allow_redefine) and (sym.Defined = True) then
      ErrorObj.Show(ltWarning, W1003_LABEL_REDEFINED, [_label]);
    sym.Defined := True;
    case _preparser[0].DataType of
      pstNone: ErrorObj.Show(ltError, E2018_OPERAND_NO_DATA_TYPE, [1]);
      pstINT32:
        begin
          sym.IValue := _preparser[0].IntValue;
          sym.DefinedPass := FPass;
        end;
      pstString:
        begin
          sym.SValue := _preparser[0].StrValue;
          sym.DefinedPass := FPass;
        end;
    end; // case
    FSymbolTable[_index] := sym;
  end;
end;

procedure TAssembler80.ExpandMacro(macro_entry: TMacroEntry);
var i,j: integer;
    s: string;
    p: integer;
    serial: integer;
    param:  string;
    repl_flags: TReplaceFlags;
begin
  serial := FMacroList.AllocateSerial;
  repl_flags := [rfReplaceAll];
  if not FCaseSensitive then
    repl_flags := repl_flags + [rfIgnoreCase];
  for i := 0 to macro_entry.Content.Count-1 do
    begin
      s := macro_entry.Content[i];

      p := Pos('{#}',s);
      if p > 0 then
        s := StringReplace(s,'{#}',IntToStr(serial),[]);

      for j := 0 to macro_entry.Headings.Count-1 do
        begin
          if j >= macro_entry.Params.Count then
            Exit;
          param := '{' + macro_entry.Headings[j] + '}';
          if FCaseSensitive then
            p := Pos(param,s)
          else
            p := Pos(param,UpperCase(s));
          if p > 0 then
            s := StringReplace(s,param,macro_entry.Params[j],repl_flags);
        end;
      AssembleLine(s);
    end;
end;

function TAssembler80.GetDFA: TLCGDFA;
begin
  Result := FDFA;
end;

function TAssembler80.GetFilenameListing: string;
begin
  GetFilenameListing := FListing.Filename;
end;

function TAssembler80.MakeFilename(const _base_asm, _option, _ext: string): string;
var
  filename_base: string;
  mfn: string;
begin
  mfn := '';
  filename_base := ExtractFilename(_base_asm);
  if RevPos('.', filename_base) > 0 then
    filename_base := LeftStr(filename_base, RevPos('.', filename_base) - 1);
  if _option = '' then
    mfn := ''
  else if _option = '*' then
    mfn := ExtractFilePath(_base_asm) + filename_base + _ext
  else if ExtractFilename(_option) = '' then
    mfn := ExpandFilename(_option) + filename_base + _ext
  else if LeftStr(_option,2) = '*.' then
    mfn := ExtractFilePath(_base_asm) + filename_base + '.' + RightStr(_option,Length(_option)-2)
  else
    mfn := ExpandFilename(_option);
  MakeFilename := mfn;
end;

procedure TAssembler80.NeedNumber(_index: integer; const _msg: string);
begin
  // Requests a number in a certain position
  // If it's a string _and_ it's exactly 1 character in length, we can
  // take the ordinal value of the first character.
  // We use the for things like:   LD A,'0'
  if ParserStack[ParserSP + _index].BufType <> pstINT32 then
  begin
    if (ParserStack[ParserSP + _index].BufType = pstString) and
      (Length(ParserStack[ParserSP + _index].Buf) = 1) then
      ConvertOperandToOrd(_index)
    else
      ShowErrorToken(ParserStack[ParserSP + _index].Token, ltError,
        E2004_EXPECTED_NUMBER, [_msg]);
  end;
end;

procedure TAssembler80.NeedNumberCompare;
begin
  NeedNumber(-3, 'on left hand side of compare');
  NeedNumber(-1, 'on right hand side of compare');
end;

procedure TAssembler80.NeedPosNumber(_index: integer; const _msg: string; _min: integer);
begin
  if (ParserStack[ParserSP + _index].BufType <> pstINT32) or
    (ParserStack[ParserSP + _index].BufInt < _min) then
    ShowErrorToken(ParserStack[ParserSP + _index].Token, ltError,
      E2011_EXPECTED_POS_NUMBER, [_msg]);
end;

procedure TAssembler80.NeedString(_index: integer; const _msg: string);
begin
  if ParserStack[ParserSP + _index].BufType <> pstString then
    ShowErrorToken(ParserStack[ParserSP + _index].Token, ltError,
      E2010_EXPECTED_STRING, [_msg]);
end;

procedure TAssembler80.OutputMemoryToCom(_filename: string);
var
  lowest, highest: integer;
  bcount: integer;
  strm: TFileStream;
begin
  if _filename = '' then
    Exit;
  lowest := $0000;
  highest := $FFFF;
  while (highest > 0) and (not FMemoryUsed[highest]) do
    Dec(highest);
  while (lowest < 65536) and (not FMemoryUsed[lowest]) do
    Inc(lowest);
  strm := TFileStream.Create(_filename, fmCreate);
  try
    bcount := highest - lowest + 1;
    if bcount > 0 then
      strm.Write(FMemory[lowest], bcount);
  finally
    FreeAndNil(strm);
  end;
end;

procedure TAssembler80.OutputMemoryToHex(_filename: string);
var
  sl: TStringList;
  i: integer;
  addr: integer;
  remaining: integer;
  checksum: integer;
  s: string;
  lowest, highest: integer;
begin
  if _filename = '' then
    Exit;
  lowest := $0000;
  highest := $FFFF;
  while (highest > 0) and (not FMemoryUsed[highest]) do
    Dec(highest);
  while (lowest < 65536) and (not FMemoryUsed[lowest]) do
    Inc(lowest);
  sl := TStringList.Create;
  try
    addr := lowest;
    while addr < Highest do
    begin
      // Do a line
      s := ':';
      remaining := Highest + 1 - addr;
      if remaining > 16 then
        remaining := 16;
      s := s + Format('%2.2X%4.4X00', [remaining, addr]);
      checksum := remaining + (addr and $ff) + ((addr shr 8) and $ff);
      for i := 0 to remaining - 1 do
      begin
        s := s + Format('%2.2X', [FMemory[addr + i]]);
        checksum := checksum + FMemory[addr + i];
      end;
      s := s + Format('%2.2X', [(-checksum) and $ff]);
      sl.Add(s);
      addr := addr + remaining;
    end;
    // Final line
    s := ':';
    remaining := 0;
    s := s + Format('%2.2X%4.4X01', [remaining, addr]);
    checksum := remaining + (addr and $ff) + ((addr shr 8) and $ff) + 1;
    s := s + Format('%2.2X', [(-checksum) and $ff]);
    sl.Add(s);
    sl.SaveToFile(_filename);
  finally
    sl.Free;
  end;
end;


// Key routine - parse a single line of text from the input

procedure TAssembler80.Parse(const _s: string; _cmd_only: boolean);
var
  i: integer;
  itm: TParserProp;
  strm: TStringStream;
  s: string;
begin
  FPreparser.Parse(_s);
  if FPreparser.MacroReference then
    exit;
  // Now parse all the operands where needed
  if (not FDefiningMacro) and (FSolGenerate or (cfBypass in FPreparser.CmdFlags)) then
    for i := 0 to FPreparser.Count - 1 do
      begin
        itm := FPreparser.Items[i];
        if itm.Index < 0 then
        begin // We need to use the main parser to sort this out
          s := itm.Payload;
          if FInstructionList.IndirectAllowed and
             (s <> '') and
             (Indirected(s, DEFAULT_ESCAPE, DEFAULT_ESCAPED)) then
          begin
            s[1] := '[';
            s[Length(s)] := ']';
          end;
          strm := TStringStream.Create(s);
          try
            Parse(strm, itm.Column);
            case ParsedOperandOption of
              OPER_U16,
              OPER_U16_IND,
              OPER_IXPD_IND,
              OPER_IYPD_IND: begin
                itm.Index := Ord(ParsedOperandOption);
                itm.DataType := FinalVal.BufType;
                itm.IntValue := (FinalVal.BufInt and $FFFF);
                itm.StrValue := FinalVal.Buf;
                itm.Source := FinalVal.Source;
                FPreparser.Items[i] := itm;
              end;
              otherwise
                ErrorObj.Show(ltError, E2014_UNABLE_TO_PARSE, [itm.Payload]);
            end;  // case
          finally
            FreeAndNil(strm);
          end;
        end
        else
        begin
          itm.DataType := pstNone;
          itm.IntValue := 0;
          itm.StrValue := '';
          itm.Source := pssUndefined;
          FPreparser.Items[i] := itm;
        end;
    end;
{$IFDEF DEBUG_LOG}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Label   = ' + FPreparser.LabelX]);
  if FPreparser.CommandIndex >= 0 then
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Command = ' + IntToStr(FPreparser.CommandIndex) + ' (' + FPreparser.CommandList[FPreparser.CommandIndex].CommandName + ')'])
  else
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Command = ' + IntToStr(FPreparser.CommandIndex)]);
  if FPreparser.OpcodeIndex >= 0 then
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Opcode  = ' + IntToStr(FPreparser.OpcodeIndex) + ' (' + FPreparser.OpcodeList.OpcodeAtIndex(FPreparser.OpcodeIndex) + ')'])
  else
    ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['Opcode  = ' + IntToStr(FPreparser.OpcodeIndex)]);
  for i := 0 to FPreparser.Count-1 do
    begin
      itm := FPreparser.Items[i];
      if itm.index >= 0 then
        ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[
              Format('    %d: %s [%s] Idx=%d %s, Lev=%d DataType=%s StrVal=%s IntVal=%d Src=%s',
                    [itm.Column,
                     GetEnumName(TypeInfo(TParserState),Ord(itm.State)),
                     itm.Payload,
                     itm.Index,
                     OperandSanitised[TOperandOption(itm.Index)],
                     itm.Level,
                     GetEnumName(TypeInfo(TLCGParserStackType),Ord(itm.DataType)),
                     itm.StrValue,
                     itm.IntValue,
                     GetEnumName(TypeInfo(TParserStackSource),Ord(itm.Source))]
                     )])
      else
        ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[
              Format('    %d: %s [%s] Idx=%d, Lev=%d  ',
                    [itm.Column,
                     GetEnumName(TypeInfo(TParserState),Ord(itm.State)),
                     itm.Payload,
                     itm.Index,
                     itm.Level]
                     )]);
    end;
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,[StringOfChar('=',80)]);
{$ENDIF}
end;

procedure TAssembler80.Parse(_strm: TStream; _firstcol: integer);
begin
  FInputCol := _firstcol;
  ParsedOperandOption := OPER_NULL;
  FinalVal.Buf := '';
  FinalVal.BufInt := 0;
  FinalVal.BufType := pstNone;
  FinalVal.State := 0;
  FinalVal.Token.Buf := '';
  FinalVal.Token.Col := 0;
  FinalVal.Token.ID := 0;
  FinalVal.Token.Row := 0;
  if not FDefiningMacro then
    inherited Parse(_strm);
end;

function TAssembler80.ParserM1: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP - 1];
end;

function TAssembler80.ParserM2: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP - 2];
end;

function TAssembler80.ParserM3: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP - 3];
end;

function TAssembler80.ParserM4: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP - 4];
end;

function TAssembler80.ParserM5: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP - 5];
end;

function TAssembler80.ParserM6: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP - 6];
end;

procedure TAssembler80.PostReduce(Parser: TLCGParser);
{$IFDEF DEBUG_LOG}
var i: integer;
    s: string;

  function M1: TLCGParserStackEntry;
  begin
    Result := Parser.ParserStack[Parser.ParserSP-1];
  end;

{$ENDIF}
begin
{$IFDEF DEBUG_LOG}
      Monitor(ltDebug,'    Buffer type: %s',[GetEnumName(TypeInfo(TLCGParserStackType),Ord(M1.BufType))]);
      Monitor(ltDebug,'    Buffer str:  %s',[M1.Buf]);
      Monitor(ltDebug,'    Buffer int:  %d',[M1.BufInt]);
      Monitor(ltDebug,'    Source:      %s',[GetEnumName(TypeInfo(TParserStackSource),Ord(M1.Source))]);
      s := '';
      for i := 0 to Parser.ParserSP-1 do
        s := s + Parser.ParserStack[i].Buf + ' ';
      Monitor(ltDebug,'    Stack:       %s',[s]);
{$ENDIF}
end;

procedure TAssembler80.Preparse(const _s: string);
begin
  FPreparser.Parse(_s);
end;

procedure TAssembler80.ProcessFile(const filename: string);
var
  sl: TStringList;
  s: string;
begin
  FCurrentFile := filename;
  sl := TStringList.Create;
  sl.LoadFromFile(FCurrentFile);
  ErrorObj.Filename := FCurrentFile;
  try
    FInputLine := 0;
    while (FInputLine < sl.Count) do
    begin
      FInputLine := FInputLine + 1;
      ErrorObj.LineNumber := FInputLine;
      ErrorObj.ColNumber := 0;
      s := sl[FInputLine - 1];
      s := ExpandTabs(s, DEFAULT_TAB_SIZE);
      AssembleLine(s);
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TAssembler80.ProcessInclude;
var
  fn: string;
  rec: TIncludeEntry;

  function TryIncludeNameUnchanged(const _fn, _next: string): string;
  begin  // If source file has path info, we should leave as is!
    if (_fn <> '') then
      Result := _fn
    else if FileExists(ExpandFilename(_next)) then
      Result := _next
    else if ExtractFilePath(_next) <> '' then
      Result := _next
    else
      Result := '';  // Will only happen if file doesn't exist and no path info
  end;

  function TryUsingBasePath(const _fn, _next: string): string;
  var
    path: string;
  begin
    if (_fn <> '') then
      Result := _fn
    else
    begin
      path := ExtractFilePath(FilenameAsm);
      ErrorObj.Show(ltWarAndPeace, I0006_SEARCHING_FOR_INCLUDE, [_next, path]);
      if FileExists(path + _next) then
        Result := path + _next
      else
        Result := '';
    end;
  end;

  function TryUsingIncludeOptions(const _fn, _next: string): string;
  var
    _sl: TStringList;
    i: integer;
    path: string;
  begin
    Result := '';
    if _fn <> '' then
      Result := _fn;
    _sl := TStringList.Create;
    try
      _sl.Delimiter := ';';
      _sl.DelimitedText := FIncludeList;
      i := 0;
      while (i < _sl.Count) and (Result = '') do
      begin
        path := ExtractFilePath(_sl[i]);
        ErrorObj.Show(ltWarAndPeace, I0006_SEARCHING_FOR_INCLUDE, [_next, path]);
        if FileExists(path + _next) then
          Result := path + _next;
        Inc(i);
      end;
    finally
      FreeAndNil(_sl);
    end;
  end;

begin
  // Check if the command was INCLUDE
  if FNextInclude <> '' then
  begin
    fn := '';
    fn := TryIncludeNameUnchanged(fn, FNextInclude);
    fn := TryUsingBasePath(fn, FNextInclude);
    fn := TryUsingIncludeOptions(fn, FNextInclude);
    if (fn <> '') and (FileExists(fn)) then
    begin
      FNextInclude := '';
      fn := ExpandFilename(fn);
      ErrorObj.Show(ltWarAndPeace, I0007_PROCESSING_INCLUDE, [fn]);
      // Stack the include
      if FIncludeStack.Count >= MAX_NESTED_INCLUDES then
        ErrorObj.Show(ltError, E2045_MAXIMUM_INCLUDES_EXCEEDED, [MAX_NESTED_INCLUDES]);
      rec.Filename   := FCurrentFile;
      rec.LineNumber := FInputLine;
      rec.Warnings := ErrorObj.Warnings;
      FIncludeStack.Push(rec);
      // and process the file
      ProcessFile(fn);
      // Unstack the include
      rec := FIncludeStack.Pop;
      ErrorObj.Filename   := rec.Filename;
      FCurrentFile        := rec.Filename;
      ErrorObj.LineNumber := rec.LineNumber;
      FInputLine          := rec.LineNumber;
      ErrorObj.Warnings   := rec.Warnings;
    end
    else
    begin
      ErrorObj.ColNumber := FPreparser[0].Column;
      ErrorObj.Show(ltError, E2044_INCLUDE_FILE_NOT_FOUND, [FNextInclude]);
    end;
  end;
end;

procedure TAssembler80.RegisterProc(const _procname: string;
  _proc: TLCGParserProc; _procs: TStringArray);
var
  i: integer;
  done_one: boolean;
begin
  done_one := False;
  for i := 0 to Rules - 1 do
    if _procs[i] = _procname then
    begin
      FProcArray[i] := _proc;
      done_one := True;
    end;
  if not done_one then
    ErrorObj.Show(ltInternal, X3003_PROCEDURE_NOT_IN_GRAMMAR, [_procname]);
end;

function TAssembler80.Reduce(Parser: TLCGParser;
  RuleIndex: uint32): TLCGParserStackEntry;
begin
  Result.Buf := '';
  Result.BufType := pstNone;
  if Assigned(FProcArray[RuleIndex]) then
  begin
{$IFDEF DEBUG_LOG}
      Monitor(ltDebug,'Performing reduction on rule %d: %s',[RuleIndex,RuleProcs[RuleIndex]]);
{$ENDIF}
    Result := FProcArray[RuleIndex](Parser);
  end
  else
    ErrorObj.Show(ltInternal, X3004_REDUCTION_NOT_DEFINED,
      [RuleIndex, RuleProcs[RuleIndex]]);
end;


procedure TAssembler80.RegisterCommands;
var ival: integer;
begin
  ival := 0;
  FCmdList.RegisterCommand('.BYTE',      [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('.CPU',       [],                        @CmdCPU);
  FCmdList.RegisterCommand('.DB',        [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('.DC',        [cfLabel],                 @CmdDC);
  FCmdList.RegisterCommand('.DEFB',      [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('.DEFC',      [cfLabel],                 @CmdDC);
  FCmdList.RegisterCommand('.DEFS',      [cfLabel],                 @CmdDS);
  FCmdList.RegisterCommand('.DEFW',      [cfLabel],                 @CmdDW);
  FCmdList.RegisterCommand('.DS',        [cfLabel],                 @CmdDS);
  FCmdList.RegisterCommand('.DW',        [cfLabel],                 @CmdDW);
  FCmdList.RegisterCommand('.ELSE',      [cfBypass],                @CmdELSE);
  FCmdList.RegisterCommand('.END',       [],                        @CmdEND);
  FCmdList.RegisterCommand('.ENDIF',     [cfBypass],                @CmdENDIF);
  FCmdList.RegisterCommand('.ENDM',      [cfDuringMD],              @CmdENDM);
  FCmdList.RegisterCommand('.ENDR',      [cfBypass],                @CmdENDR);
  FCmdList.RegisterCommand('.ENDW',      [cfBypass],                @CmdENDW);
  FCmdList.RegisterCommand('.EQU',       [cfLabel,cfEQU],           @CmdEQU);
  FCmdList.RegisterCommand('.EXTERN',    [],                        @CmdEXTERN);
  FCmdList.RegisterCommand('.GLOBAL',    [],                        @CmdGLOBAL);
  FCmdList.RegisterCommand('.IF',        [cfNoPlaceholder,cfBypass],@CmdIF);
  FCmdList.RegisterCommand('.IFDEF',     [cfNoPlaceholder,cfBypass],@CmdIFDEF);
  FCmdList.RegisterCommand('.IFNDEF',    [cfNoPlaceholder,cfBypass],@CmdIFNDEF);
  FCmdList.RegisterCommand('.INCLUDE',   [],                        @CmdINCLUDE);
  FCmdList.RegisterCommand('.LISTOFF',   [],                        @CmdLISTOFF);
  FCmdList.RegisterCommand('.LISTON',    [],                        @CmdLISTON);
  FCmdList.RegisterCommand('.MACRO',     [cfNoPlaceholder,cfLabel,cfDuringMD], @CmdMACRO);
  FCmdList.RegisterCommand('.MSGINFO',   [],                        @CmdMSGINFO);
  FCmdList.RegisterCommand('.MSGERROR',  [],                        @CmdMSGERROR);
  FCmdList.RegisterCommand('.MSGWARNING',[],                        @CmdMSGWARNING);
  FCmdList.RegisterCommand('.ORG',       [],                        @CmdORG);
  FCmdList.RegisterCommand('.REPEAT',    [cfNoPlaceholder,cfBypass],@CmdREPEAT);
  FCmdList.RegisterCommand('.TEXT',      [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('.TITLE',     [],                        @CmdTITLE);
  FCmdList.RegisterCommand('.WARNOFF',   [],                        @CmdWARNOFF);
  FCmdList.RegisterCommand('.WARNON',    [],                        @CmdWARNON);
  FCmdList.RegisterCommand('.WHILE',     [cfNoPlaceholder,cfBypass],@CmdWHILE);
  FCmdList.RegisterCommand('.WORD',      [cfLabel],                 @CmdDW);
  FCmdList.RegisterCommand('=',          [cfLabel,cfEQU],           @CmdEQU2);
  FCmdList.RegisterCommand('CPU',        [],                        @CmdCPU);
  FCmdList.RegisterCommand('DB',         [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('DC',         [cfLabel],                 @CmdDC);
  FCmdList.RegisterCommand('DEFB',       [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('DEFC',       [cfLabel],                 @CmdDC);
  FCmdList.RegisterCommand('DEFS',       [cfLabel],                 @CmdDS);
  FCmdList.RegisterCommand('DEFW',       [cfLabel],                 @CmdDW);
  FCmdList.RegisterCommand('DS',         [cfLabel],                 @CmdDS);
  FCmdList.RegisterCommand('DW',         [cfLabel],                 @CmdDW);
  FCmdList.RegisterCommand('ELSE',       [cfBypass],                @CmdELSE);
  FCmdList.RegisterCommand('END',        [],                        @CmdEND);
  FCmdList.RegisterCommand('ENDIF',      [cfBypass],                @CmdENDIF);
  FCmdList.RegisterCommand('ENDM',       [cfDuringMD],              @CmdENDM);
  FCmdList.RegisterCommand('ENDR',       [cfBypass],                @CmdENDR);
  FCmdList.RegisterCommand('ENDW',       [cfBypass],                @CmdENDW);
  FCmdList.RegisterCommand('EQU',        [cfLabel,cfEQU],           @CmdEQU);
  FCmdList.RegisterCommand('EXTERN',     [],                        @CmdEXTERN);
  FCmdList.RegisterCommand('GLOBAL',     [],                        @CmdGLOBAL);
  FCmdList.RegisterCommand('IF',         [cfNoPlaceholder,cfBypass],@CmdIF);
  FCmdList.RegisterCommand('IFDEF',      [cfNoPlaceholder,cfBypass],@CmdIFDEF);
  FCmdList.RegisterCommand('IFNDEF',     [cfNoPlaceholder,cfBypass],@CmdIFNDEF);
  FCmdList.RegisterCommand('INCLUDE',    [],                        @CmdINCLUDE);
  FCmdList.RegisterCommand('LISTOFF',    [],                        @CmdLISTOFF);
  FCmdList.RegisterCommand('LISTON',     [],                        @CmdLISTON);
  FCmdList.RegisterCommand('MACRO',      [cfNoPlaceholder,cfLabel,cfDuringMD], @CmdMACRO);
  FCmdList.RegisterCommand('MSGINFO',    [],                        @CmdMSGINFO);
  FCmdList.RegisterCommand('MSGERROR',   [],                        @CmdMSGERROR);
  FCmdList.RegisterCommand('MSGWARNING', [],                        @CmdMSGWARNING);
  FCmdList.RegisterCommand('ORG',        [],                        @CmdORG);
  FCmdList.RegisterCommand('REPEAT',     [cfNoPlaceholder,cfBypass],@CmdREPEAT);
  FCmdList.RegisterCommand('TEXT',       [cfLabel],                 @CmdDB);
  FCmdList.RegisterCommand('TITLE',      [],                        @CmdTITLE);
  FCmdList.RegisterCommand('WARNOFF',    [],                        @CmdWARNOFF);
  FCmdList.RegisterCommand('WARNON',     [],                        @CmdWARNON);
  FCmdList.RegisterCommand('WHILE',      [cfNoPlaceholder,cfBypass],@CmdWHILE);
  FCmdList.RegisterCommand('WORD',       [cfLabel],                 @CmdDW);
  if not FInstructionList.FindOpcode('SET',ival) then
    begin
      FCmdList.RegisterCommand('.SET',   [cfLabel,cfEQU],           @CmdEQU2);
      FCmdList.RegisterCommand('SET',    [cfLabel,cfEQU],           @CmdEQU2);
    end;
end;

procedure TAssembler80.RegisterProcs;
var
  _procs: TStringArray;
begin
  _procs := RuleProcs;
  RegisterProc('ActBinLiteral', @ActBinLiteral, _procs);
  RegisterProc('ActCharConstant', @ActCharConstant, _procs);
  RegisterProc('ActCompEQ', @ActCompEQ, _procs);
  RegisterProc('ActCompGE', @ActCompGE, _procs);
  RegisterProc('ActCompGT', @ActCompGT, _procs);
  RegisterProc('ActCompLE', @ActCompLE, _procs);
  RegisterProc('ActCompLT', @ActCompLT, _procs);
  RegisterProc('ActCompNE', @ActCompNE, _procs);
  RegisterProc('ActCopy1', @ActCopy1, _procs);
  RegisterProc('ActDecLiteral', @ActDecLiteral, _procs);
  RegisterProc('ActExprAdd', @ActExprAdd, _procs);
  RegisterProc('ActExprAnd', @ActExprAnd, _procs);
  RegisterProc('ActExprBracket', @ActExprBracket, _procs);
  RegisterProc('ActExprDiv', @ActExprDiv, _procs);
  RegisterProc('ActExprMod', @ActExprMod, _procs);
  RegisterProc('ActExprMul', @ActExprMul, _procs);
  RegisterProc('ActExprNot', @ActExprNot, _procs);
  RegisterProc('ActExprOr', @ActExprOr, _procs);
  RegisterProc('ActExprShl', @ActExprShl, _procs);
  RegisterProc('ActExprShr', @ActExprShr, _procs);
  RegisterProc('ActExprSub', @ActExprSub, _procs);
  RegisterProc('ActExprUnaryMinus', @ActExprUnaryMinus, _procs);
  RegisterProc('ActExprUnaryPlus', @ActExprUnaryPlus, _procs);
  RegisterProc('ActExprXor', @ActExprXor, _procs);
  RegisterProc('ActFuncAsc', @ActFuncAsc, _procs);
  RegisterProc('ActFuncDefined', @ActFuncDefined, _procs);
  RegisterProc('ActFuncHigh', @ActFuncHigh, _procs);
  RegisterProc('ActFuncIif', @ActFuncIif, _procs);
  RegisterProc('ActFuncLength', @ActFuncLength, _procs);
  RegisterProc('ActFuncLow', @ActFuncLow, _procs);
  RegisterProc('ActFuncPos', @ActFuncPos, _procs);
  RegisterProc('ActFuncValue', @ActFuncValue, _procs);
  RegisterProc('ActHexLiteral', @ActHexLiteral, _procs);
  RegisterProc('ActLogAnd', @ActLogAnd, _procs);
  RegisterProc('ActLogNot', @ActLogNot, _procs);
  RegisterProc('ActLogOr', @ActLogOr, _procs);
  RegisterProc('ActOctLiteral', @ActOctLiteral, _procs);
  RegisterProc('ActSetOpInd', @ActSetOpInd, _procs);
  RegisterProc('ActSetOpIndOffIX', @ActSetOpIndOffIX, _procs);
  RegisterProc('ActSetOpIndOffIY', @ActSetOpIndOffIY, _procs);
  RegisterProc('ActSetOpLiteral', @ActSetOpLiteral, _procs);
  RegisterProc('ActStrBuild', @ActStrBuild, _procs);
  RegisterProc('ActStrChr', @ActStrChr, _procs);
  RegisterProc('ActStrDate', @ActStrDate, _procs);
  RegisterProc('ActStrHex1', @ActStrHex1, _procs);
  RegisterProc('ActStrHex2', @ActStrHex2, _procs);
  RegisterProc('ActStrLeft', @ActStrLeft, _procs);
  RegisterProc('ActStrLower', @ActStrLower, _procs);
  RegisterProc('ActStrMid', @ActStrMid, _procs);
  RegisterProc('ActStrProcessor', @ActStrProcessor, _procs);
  RegisterProc('ActStrRight', @ActStrRight, _procs);
  RegisterProc('ActStrString', @ActStrString, _procs);
  RegisterProc('ActStrTime', @ActStrTime, _procs);
  RegisterProc('ActStrUpper', @ActStrUpper, _procs);
  RegisterProc('ActStrVersion', @ActStrVersion, _procs);
  RegisterProc('ActStringConstant', @ActStringConstant, _procs);
  RegisterProc('ActValueOrg', @ActValueOrg, _procs);
  RegisterProc('ActValueParam', @ActValueParam, _procs);
  RegisterProc('ActValueSymbol', @ActValueSymbol, _procs);
end;

procedure TAssembler80.ResetMemory;
var
  w: word;
begin
  for w in word do FMemory[w] := 0;
  for w in word do FMemoryUsed[w] := False;
end;

procedure TAssembler80.SetCaseSensitive(_v: boolean);
begin
  FCaseSensitive := _v;
  FSymbolTable.MixedCase := _v;
end;

procedure TAssembler80.SetDefiningMacro(_v: boolean);
begin
  FDefiningMacro := _v;
  FPreparser.DefiningMacro := _v;
end;

procedure TAssembler80.SetFilenameAsm(const _filename: string);
begin
  FFilenameAsm := _filename;
end;

procedure TAssembler80.SetFilenameError(const _filename: string);
begin
  FFilenameError := _filename;
  ErrorObj.Filename := _filename;
end;

procedure TAssembler80.SetFilenameListing(const _filename: string);
begin
  FListing.Filename := _filename;
end;

procedure TAssembler80.SetOrg(_neworg: integer);
begin
  if (_neworg > $FFFF) or (_neworg < 0) then
  begin
    if Pass = 2 then
      ErrorObj.Show(ltWarning, W1001_CODE_WRAPPED_ROUND);
    _neworg := _neworg and $FFFF;
  end;
  FOrg := _neworg;
end;

procedure TAssembler80.SetTitle(_title: string);
begin
  FTitle := _title;
  FSymbolTable.Title := _title;
  FListing.Title := _title;
end;

procedure TAssembler80.ShowError(_colno: integer; _logtype: TLCGLogType;
  _msgno: TMessageNumbers);
begin
  ShowError(_colno, _logtype, _msgno, []);
end;

procedure TAssembler80.ShowError(_colno: integer; _logtype: TLCGLogType;
  _msgno: TMessageNumbers; _args: array of const);
begin
  ErrorObj.ColNumber := _colno + FInputCol - 1;
  ErrorObj.Show(_logtype, _msgno, _args);
end;

procedure TAssembler80.ShowErrorToken(_token: TToken; _logtype: TLCGLogType;
  _msgno: TMessageNumbers);
begin
  ShowErrorToken(_token, _logtype, _msgno, []);
end;

procedure TAssembler80.ShowErrorToken(_token: TToken; _logtype: TLCGLogType;
  _msgno: TMessageNumbers; _args: array of const);
begin
  ShowError(_token.Col, _logtype, _msgno, _args);
end;

function TAssembler80.SourceCombine1(_a: integer): TParserStackSource;
begin
  // Simple copy
  Result := ParserStack[ParserSP + _a].Source;
end;

function TAssembler80.SourceCombine2(_a, _b: integer): TParserStackSource;
begin
  if ParserStack[ParserSP + _a].Source < ParserStack[ParserSP + _b].Source then
    Result := ParserStack[ParserSP + _a].Source
  else
    Result := ParserStack[ParserSP + _b].Source;
end;

function TAssembler80.SourceCombine3(_a, _b, _c: integer): TParserStackSource;
begin
  if SourceCombine2(_a, _b) < SourceCombine1(_c) then
    Result := SourceCombine2(_a, _b)
  else
    Result := SourceCombine1(_c);
end;

end.
