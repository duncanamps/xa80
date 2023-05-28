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

//
// Main assembler unit. This handles all the work of assembling a single file
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, umonitor, deployment_parser_types_12,
  deployment_parser_module_12, umessages, uinstruction, usymboltable;

type
  TCompareMode = (cmEqual,cmNotEqual,cmLessThan,cmLessEqual,cmGreaterThan,cmGreaterEqual);

  TAssembler80 = class(TLCGParser)
    private
      FCurrentFile: string;
      FInputCol:    integer;
      FPass:        integer;
      FProcArray:      array of TLCGParserProc;
      FSymbolTable:    TSymbolTable;
      procedure RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
      procedure RegisterProcs;
      function  ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCharConstant(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprUnaryPlus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActMandateInt(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActParamList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndOffIX(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndOffIY(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
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
      function  ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
//    function  ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      function  CompareGeneric(_comparer: TCompareMode): TLCGParserStackEntry;
      procedure NeedNumber(_index: integer; const _msg: string);
      procedure NeedNumberCompare;
      procedure NeedPosNumber(_index: integer; const _msg: string; _min: integer = 0);
      procedure NeedString(_index: integer; const _msg: string);
      function  ParserM1: TLCGParserStackEntry;
      function  ParserM2: TLCGParserStackEntry;
      function  ParserM3: TLCGParserStackEntry;
      function  ParserM4: TLCGParserStackEntry;
      function  ParserM5: TLCGParserStackEntry;
      function  ParserM6: TLCGParserStackEntry;
      function  Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
    public
      FinalVal: TLCGParserStackEntry;
      ParsedOperandOption: TOperandOption;
      constructor Create;
      destructor Destroy; override;
      procedure Assemble(const filename: string);
      procedure Parse(_strm: TStream; _firstcol: integer);
      procedure ShowError(_colno: integer; _logtype: TLCGLogType; _msgno: TMessageNumbers);
      procedure ShowError(_colno: integer; _logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
      procedure ShowErrorToken(_token: TToken; _logtype: TLCGLogType; _msgno: TMessageNumbers);
      procedure ShowErrorToken(_token: TToken; _logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
      property CurrentFile: string  read FCurrentFile;
      property InputLine:   integer read FInputLine;
      property InputCol:    integer read FInputCol;
      property Pass:        integer read FPass;
  end;

var
  Asm80: TAssembler80;

implementation

uses
  uutility;

constructor TAssembler80.Create;
begin
  inherited Create;
  FSymbolTable := TSymbolTable.Create;
  LoadFromResource('XA80OPER');
  SetLength(FProcArray,Rules);
  RegisterProcs;
  OnReduce := @Reduce;
  // Test stuff
  FSymbolTable.AddPlaceholder('PlaceHold');
  FSymbolTable.AddWord('cassette_buf',$033A);
  FSymbolTable.AddString('myname','Duncan Munro');
end;

destructor TAssembler80.Destroy;
begin
  FreeAndNil(FSymbolTable);
  inherited Destroy;
end;

function TAssembler80.ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt := VariableFromBinLiteral(ParserM1.Buf);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActCharConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt := Ord(ParserM1.Buf[2]);
  Result.BufType := pstINT32;
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
begin
  Result.BufInt  := StrToInt(ParserM1.Buf);
  Result.Buf     := IntToStr(Result.BufInt);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (ParserM3.BufType = pstString) and
     (ParserM1.BufType = pstString) then
    begin
      // String concatenation
      Result.Buf := ParserM3.Buf + ParserM1.Buf;
      Result.BufInt := 0;
      Result.BufType := pstString;
    end
  else
    begin
      // Normal numeric add
      NeedNumber(-3,'on left hand side of add');
      NeedNumber(-1,'on right hand side of add');
      Result.BufInt := ParserM3.BufInt +
                       ParserM1.BufInt;
      if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
        ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
      Result.BufType := pstINT32;
    end;
end;

function TAssembler80.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of binary and');
  NeedNumber(-1,'on right hand side of binary and');
  Result.BufInt := (ParserM3.BufInt and $FFFF) and
                   (ParserM1.BufInt and $FFFF);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM2;
end;

function TAssembler80.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of divide');
  NeedNumber(-1,'on right hand side of divide');
  if ParserM1.BufInt = 0 then
    ShowErrorToken(ParserM1.Token,ltError,E2009_DIVIDE_BY_ZERO);
  Result.BufInt := ParserM3.BufInt div
                   ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of modulo');
  NeedNumber(-1,'on right hand side of modulo');
  Result.BufInt := ParserM3.BufInt mod
                   ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of multiply');
  NeedNumber(-1,'on right hand side of multiply');
  Result.BufInt := ParserM3.BufInt *
                   ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1,'for unary not');
  Result.BufInt := (ParserM1.BufInt and $FFFF) xor $FFFF;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of binary or');
  NeedNumber(-1,'on right hand side of binary or');
  Result.BufInt := (ParserM3.BufInt and $FFFF) or
                   (ParserM1.BufInt and $FFFF);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of shl');
  NeedPosNumber(-1,'on right hand side of shl');
  Result.BufInt := (ParserM3.BufInt and $FFFF) shl
                   ParserM1.BufInt;
  Result.BufInt := Result.BufInt and $FFFF;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of shr');
  NeedPosNumber(-1,'on right hand side of shr');
  Result.BufInt := (ParserM3.BufInt and $FFFF) shr
                   ParserM1.BufInt;
  Result.BufInt := Result.BufInt and $FFFF;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of subtract');
  NeedNumber(-1,'on right hand side of subtract');
  Result.BufInt := ParserM3.BufInt -
                   ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1,'for unary minus');
  Result.BufInt := - ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprUnaryPlus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1,'for unary plus');
  Result.BufInt := ParserM1.BufInt;
  if (Result.BufInt > 65535) or (Result.BufInt < -32767) then
    ShowErrorToken(ParserM1.Token,ltError,E2005_INTEGER_OVERFLOW);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of binary xor');
  NeedNumber(-1,'on right hand side of binary xor');
  Result.BufInt := (ParserM3.BufInt and $FFFF) xor
                   (ParserM1.BufInt and $FFFF);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
var ascval: integer;
begin
  NeedString(-2,'for ASC() function');
  ascval := 0;
  if Length(ParserM2.Buf) > 0 then
    ascval := Ord(ParserM2.Buf[1]);
  Result.BufInt := ascval;
  Result.BufType := pstINT32;
  Result.Buf     := IntToStr(Result.BufInt);
end;

function TAssembler80.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2,'for HIGH() function');
  Result.BufInt := (ParserM2.BufInt and $FFFF) shr 8;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-6,'for IIF() function conditional expression');
  if ParserM6.BufInt <> 0 then
    Result := ParserM4
  else
    Result := ParserM2
end;

function TAssembler80.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2,'for LOW() function');
  Result.BufInt := ParserM2.BufInt and $00FF;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-4,'for POS() function search');
  NeedString(-2,'for POS() function target');
  Result.BufInt := Pos(ParserM4.Buf,
                       ParserM2.Buf);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
var code: integer;
begin
  NeedString(-2,'for VALUE() function');
  Val(ParserM2.Buf,Result.BufInt,code);
  if code <> 0 then
    ShowErrorToken(ParserM2.Token,ltError,E2012_CONVERSION_ERROR,[ParserM2.Buf]);
  Result.BufType := pstINT32;
end;

function TAssembler80.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.BufInt := VariableFromHexLiteral(ParserM1.Buf);
  Result.BufType := pstINT32;
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
  NeedNumber(-3,'on left hand side of logical and');
  NeedNumber(-1,'on right hand side of logical and');
  if (ParserM3.BufInt <> 0) and (ParserM1.BufInt <> 0) then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-1,'on right hand side of logical not');
  if (ParserM1.BufInt = 0) then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
end;

function TAssembler80.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-3,'on left hand side of logical or');
  NeedNumber(-1,'on right hand side of logical or');
  if (ParserM3.BufInt <> 0) or (ParserM1.BufInt <> 0) then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
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

function TAssembler80.ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedPosNumber(-2,'for CHR() function');
  Result.Buf := Chr(ParserM2.BufInt);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('yyyy-mm-dd',StartTime);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-2,'for HEX() function');
  Result.Buf := IntToHex(ParserM2.BufInt and $FFFF,1);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedNumber(-4,'for HEX() function value');
  NeedPosNumber(-4,'for HEX() function digits',1);
  Result.Buf := IntToHex(ParserM4.BufInt and $FFFF,ParserM2.BufInt);
  Result.BufType := pstString;
end;

function TAssembler80.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := StripQuotesAndEscaped(ParserM1.Buf);
  Result.BufType := pstString;
end;

function TAssembler80.ActStringSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := ParserM1;
end;

function TAssembler80.ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-4,'for LEFT() source');
  NeedPosNumber(-2,'for LEFT() count');
  Result.Buf := LeftStr(ParserM4.Buf,ParserM2.BufInt);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-2,'for LOWER() function');
  Result.Buf := LowerCase(ParserM2.Buf);
  Result.BufInt := 0;
  Result.BufType := pstString;
end;

function TAssembler80.ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-6,'for MID() source');
  NeedPosNumber(-4,'for MID() start',1);
  NeedPosNumber(-2,'for MID() count');
  Result.Buf := Copy(ParserM6.Buf,ParserM4.BufInt,ParserM2.BufInt);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-4,'for RIGHT() source');
  NeedPosNumber(-2,'for RIGHT() count');
  Result.Buf := RightStr(ParserM4.Buf,ParserM2.BufInt);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('hh:nn:ss',StartTime);
  Result.BufType := pstString;
end;

function TAssembler80.ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
begin
  NeedString(-2,'for UPPER() function');
  Result.Buf := UpperCase(ParserM2.Buf);
  Result.BufType := pstString;
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
end;

function TAssembler80.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
var name: string;
    idx:  integer;
    sym:  TSymbol;
begin
  name := ParserM1.Buf;
  idx := FSymbolTable.IndexOf(name);
  if idx < 0 then
    // Symbol not found, we should add a placeholder for it
    begin
      FSymbolTable.AddPlaceholder(name);
      Result.BufInt  := 0; // Assume an integer for now
      Result.BufType := pstINT32;
    end
  else
    begin
      sym := FSymbolTable[idx];
      sym.Referenced := True;
      FSymbolTable[idx] := sym;
      case sym.SymType of
        stUnknown:  begin
                      Result.BufType := pstINT32;
                      Result.BufInt  := 0;
                    end;
        stAddress:  begin
                      Result.BufType := pstINT32;
                      Result.BufInt  := sym.IValue;
                    end;
        stWord:     begin
                      Result.BufType := pstINT32;
                      Result.BufInt  := sym.IValue;
                    end;
        stString:   begin
                      Result.BufType := pstString;
                      Result.Buf     := sym.SValue;
                    end;
      end;
    end;

end;

procedure TAssembler80.Assemble(const filename: string);
begin
  umonitor.Monitor(mtInfo,'Assembling %s',[filename]);
end;

function TAssembler80.CompareGeneric(_comparer: TCompareMode): TLCGParserStackEntry;
var compare: boolean;
    s1,s2:   string;
    i1,i2:   integer;
begin
  if (ParserM3.BufType = pstString) and
     (ParserM1.BufType = pstString) then
    begin
      // String comparison
      s1 := ParserM3.Buf;
      s2 := ParserM1.Buf;
      case _comparer of
        cmEqual:        compare := (s1 = s2);
        cmNotEqual:     compare := (s1 <> s2);
        cmLessThan:     compare := (s1 < s2);
        cmLessEqual:    compare := (s1 <= s2);
        cmGreaterThan:  compare := (s1 > s2);
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
        cmEqual:        compare := (i1 = i2);
        cmNotEqual:     compare := (i1 <> i2);
        cmLessThan:     compare := (i1 < i2);
        cmLessEqual:    compare := (i1 <= i2);
        cmGreaterThan:  compare := (i1 > i2);
        cmGreaterEqual: compare := (i1 >= i2);
      end; // case
    end;
  if compare then
    Result.BufInt := 1
  else
    Result.BufInt := 0;
  Result.BufType := pstINT32;
end;

procedure TAssembler80.NeedNumber(_index: integer; const _msg: string);
begin
  if ParserStack[ParserSP+_index].BufType <> pstINT32 then
    ShowErrorToken(ParserStack[ParserSP+_index].Token,ltError,E2004_EXPECTED_NUMBER,[_msg])
end;

procedure TAssembler80.NeedNumberCompare;
begin
  NeedNumber(-3,'on left hand side of compare');
  NeedNumber(-1,'on right hand side of compare');
end;

procedure TAssembler80.NeedPosNumber(_index: integer; const _msg: string; _min: integer);
begin
  if (ParserStack[ParserSP+_index].BufType <> pstINT32) or
     (ParserStack[ParserSP+_index].BufInt < _min) then
    ShowErrorToken(ParserStack[ParserSP+_index].Token,ltError,E2011_EXPECTED_POS_NUMBER,[_msg])
end;

procedure TAssembler80.NeedString(_index: integer; const _msg: string);
begin
  if ParserStack[ParserSP+_index].BufType <> pstString then
    ShowErrorToken(ParserStack[ParserSP+_index].Token,ltError,E2010_EXPECTED_STRING,[_msg])
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
  FinalVal.Token.ID  := 0;
  FinalVal.Token.Row := 0;
  inherited Parse(_strm);
end;

function TAssembler80.ParserM1: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP-1];
end;

function TAssembler80.ParserM2: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP-2];
end;

function TAssembler80.ParserM3: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP-3];
end;

function TAssembler80.ParserM4: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP-4];
end;

function TAssembler80.ParserM5: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP-5];
end;

function TAssembler80.ParserM6: TLCGParserStackEntry;
begin
  Result := ParserStack[ParserSP-6];
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
    ErrorObj.Show(ltInternal,X3003_PROCEDURE_NOT_IN_GRAMMAR,[_procname]);
end;

function TAssembler80.Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  Result.Buf := '';
  Result.BufType := pstNone;
  if Assigned(FProcArray[RuleIndex]) then
    Result := FProcArray[RuleIndex](Parser)
  else
    ErrorObj.Show(ltInternal,X3004_REDUCTION_NOT_DEFINED,[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TAssembler80.RegisterProcs;
var _procs: TStringArray;
begin
  _procs := RuleProcs;
  RegisterProc('ActBinLiteral',		@ActBinLiteral, _procs);
  RegisterProc('ActCharConstant',	@ActCharConstant, _procs);
  RegisterProc('ActCompEQ',		@ActCompEQ, _procs);
  RegisterProc('ActCompGE',		@ActCompGE, _procs);
  RegisterProc('ActCompGT',		@ActCompGT, _procs);
  RegisterProc('ActCompLE',		@ActCompLE, _procs);
  RegisterProc('ActCompLT',		@ActCompLT, _procs);
  RegisterProc('ActCompNE',		@ActCompNE, _procs);
  RegisterProc('ActCopy1',		@ActCopy1, _procs);
  RegisterProc('ActDecLiteral',		@ActDecLiteral, _procs);
  RegisterProc('ActExprAdd',		@ActExprAdd, _procs);
  RegisterProc('ActExprAnd',		@ActExprAnd, _procs);
  RegisterProc('ActExprBracket',	@ActExprBracket, _procs);
  RegisterProc('ActExprDiv',		@ActExprDiv, _procs);
  RegisterProc('ActExprMod',		@ActExprMod, _procs);
  RegisterProc('ActExprMul',		@ActExprMul, _procs);
  RegisterProc('ActExprNot',		@ActExprNot, _procs);
  RegisterProc('ActExprOr',		@ActExprOr, _procs);
  RegisterProc('ActExprShl',		@ActExprShl, _procs);
  RegisterProc('ActExprShr',		@ActExprShr, _procs);
  RegisterProc('ActExprSub',		@ActExprSub, _procs);
  RegisterProc('ActExprUnaryMinus',     @ActExprUnaryMinus, _procs);
  RegisterProc('ActExprUnaryPlus',	@ActExprUnaryPlus, _procs);
  RegisterProc('ActExprXor',		@ActExprXor, _procs);
  RegisterProc('ActFuncAsc',		@ActFuncAsc, _procs);
  RegisterProc('ActFuncHigh',		@ActFuncHigh, _procs);
  RegisterProc('ActFuncIif',		@ActFuncIif, _procs);
  RegisterProc('ActFuncLow',		@ActFuncLow, _procs);
  RegisterProc('ActFuncPos',		@ActFuncPos, _procs);
  RegisterProc('ActFuncValue',		@ActFuncValue, _procs);
  RegisterProc('ActHexLiteral',		@ActHexLiteral, _procs);
  RegisterProc('ActLogAnd',		@ActLogAnd, _procs);
  RegisterProc('ActLogNot',		@ActLogNot, _procs);
  RegisterProc('ActLogOr',		@ActLogOr, _procs);
  RegisterProc('ActOctLiteral',		@ActOctLiteral, _procs);
  RegisterProc('ActSetOpInd',           @ActSetOpInd, _procs);
  RegisterProc('ActSetOpIndOffIX',      @ActSetOpIndOffIX, _procs);
  RegisterProc('ActSetOpIndOffIY',      @ActSetOpIndOffIY, _procs);
  RegisterProc('ActSetOpLiteral',	@ActSetOpLiteral, _procs);
  RegisterProc('ActStrChr',		@ActStrChr, _procs);
  RegisterProc('ActStrDate',		@ActStrDate, _procs);
  RegisterProc('ActStrHex1',		@ActStrHex1, _procs);
  RegisterProc('ActStrHex2',		@ActStrHex2, _procs);
  RegisterProc('ActStrLeft',		@ActStrLeft, _procs);
  RegisterProc('ActStrLower',		@ActStrLower, _procs);
  RegisterProc('ActStrMid',		@ActStrMid, _procs);
  RegisterProc('ActStrRight',		@ActStrRight, _procs);
  RegisterProc('ActStrTime',		@ActStrTime, _procs);
  RegisterProc('ActStrUpper',		@ActStrUpper, _procs);
  RegisterProc('ActStringConstant',     @ActStringConstant, _procs);
//RegisterProc('ActValueOrg',		@ActValueOrg, _procs);
  RegisterProc('ActValueSymbol',	@ActValueSymbol, _procs);
end;

procedure TAssembler80.ShowError(_colno: integer; _logtype: TLCGLogType; _msgno: TMessageNumbers);
begin
  ShowError(_colno,_logtype,_msgno,[]);
end;

procedure TAssembler80.ShowError(_colno: integer; _logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
begin
  ErrorObj.ColNumber := _colno + FInputCol - 1;
  ErrorObj.Show(_logtype,_msgno,_args);
end;

procedure TAssembler80.ShowErrorToken(_token: TToken; _logtype: TLCGLogType; _msgno: TMessageNumbers);
begin
  ShowErrorToken(_token,_logtype,_msgno,[]);
end;

procedure TAssembler80.ShowErrorToken(_token: TToken; _logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
begin
  ShowError(_token.Col,_logtype,_msgno,_args);
end;

end.

