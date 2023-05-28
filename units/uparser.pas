unit uparser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, deployment_parser_types_12, deployment_parser_module_12;

type
  TXA80Parser = class(TLCGParser)
    private
      FProcArray:      array of TLCGParserProc;
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
      function  ActDirByte(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDD(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineString(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefMacro(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSH(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
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
      function  ActFuncIifs(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActMandateInt(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActParamList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpBracketed(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndOff(_parser: TLCGParser): TLCGParserStackEntry;
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
      function  ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
    public
      constructor Create;
  end;

implementation

constructor TXA80Parser.Create;
begin
  inherited Create;
  LoadFromResource('XA80OPER');
  SetLength(FProcArray,Rules);
  RegisterProcs;
end;

function TXA80Parser.ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCharConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirByte(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDD(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDefineString(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDefMacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDSH(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirEndm(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirIncludeList(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirMacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirMacroNoexpr(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirNolist(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirSet(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprUnaryMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprUnaryPlus(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncIifs(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActMandateInt(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActParamList(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActSetOpBracketed(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActSetOpIndOff(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActSetOpLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStringSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

function TXA80Parser.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
end;

procedure TXA80Parser.RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
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

procedure TXA80Parser.RegisterProcs;
var _procs: TStringArray;
begin
  _procs := RuleProcs;
   RegisterProc('ActBinLiteral',     @ActBinLiteral, _procs);
   RegisterProc('ActCharConstant',   @ActCharConstant, _procs);
   RegisterProc('ActCompEQ',         @ActCompEQ, _procs);
   RegisterProc('ActCompGE',         @ActCompGE, _procs);
   RegisterProc('ActCompGT',         @ActCompGT, _procs);
   RegisterProc('ActCompLE',         @ActCompLE, _procs);
   RegisterProc('ActCompLT',         @ActCompLT, _procs);
   RegisterProc('ActCompNE',         @ActCompNE, _procs);
   RegisterProc('ActCopy1',          @ActCopy1, _procs);
   RegisterProc('ActDecLiteral',     @ActDecLiteral, _procs);
   RegisterProc('ActExprAdd',        @ActExprAdd, _procs);
   RegisterProc('ActExprAnd',        @ActExprAnd, _procs);
   RegisterProc('ActExprBracket',    @ActExprBracket, _procs);
   RegisterProc('ActExprDiv',        @ActExprDiv, _procs);
   RegisterProc('ActExprMod',        @ActExprMod, _procs);
   RegisterProc('ActExprMul',        @ActExprMul, _procs);
   RegisterProc('ActExprNot',        @ActExprNot, _procs);
   RegisterProc('ActExprOr',         @ActExprOr, _procs);
   RegisterProc('ActExprShl',        @ActExprShl, _procs);
   RegisterProc('ActExprShr',        @ActExprShr, _procs);
   RegisterProc('ActExprSub',        @ActExprSub, _procs);
   RegisterProc('ActExprUnaryMinus', @ActExprUnaryMinus, _procs);
   RegisterProc('ActExprUnaryPlus',  @ActExprUnaryPlus, _procs);
   RegisterProc('ActExprXor',        @ActExprXor, _procs);
   RegisterProc('ActFuncAsc',        @ActFuncAsc, _procs);
   RegisterProc('ActFuncHigh',       @ActFuncHigh, _procs);
   RegisterProc('ActFuncIif',        @ActFuncIif, _procs);
   RegisterProc('ActFuncIifs',       @ActFuncIifs, _procs);
   RegisterProc('ActFuncLow',        @ActFuncLow, _procs);
   RegisterProc('ActFuncPos',        @ActFuncPos, _procs);
   RegisterProc('ActFuncValue',      @ActFuncValue, _procs);
   RegisterProc('ActHexLiteral',     @ActHexLiteral, _procs);
   RegisterProc('ActLogAnd',         @ActLogAnd, _procs);
   RegisterProc('ActLogNot',         @ActLogNot, _procs);
   RegisterProc('ActLogOr',          @ActLogOr, _procs);
   RegisterProc('ActMandateInt',     @ActMandateInt, _procs);
   RegisterProc('ActOctLiteral',     @ActOctLiteral, _procs);
   RegisterProc('ActSetOpBracketed', @ActSetOpBracketed, _procs);
   RegisterProc('ActSetOpIndOff',    @ActSetOpIndOff, _procs);
   RegisterProc('ActSetOpLiteral',   @ActSetOpLiteral, _procs);
   RegisterProc('ActStrBuild',       @ActStrBuild, _procs);
   RegisterProc('ActStrChr',         @ActStrChr, _procs);
   RegisterProc('ActStrDate',        @ActStrDate, _procs);
   RegisterProc('ActStrHex1',        @ActStrHex1, _procs);
   RegisterProc('ActStrHex2',        @ActStrHex2, _procs);
   RegisterProc('ActStrLeft',        @ActStrLeft, _procs);
   RegisterProc('ActStrLower',       @ActStrLower, _procs);
   RegisterProc('ActStrMid',         @ActStrMid, _procs);
   RegisterProc('ActStrRight',       @ActStrRight, _procs);
   RegisterProc('ActStrString',      @ActStrString, _procs);
   RegisterProc('ActStrTime',        @ActStrTime, _procs);
   RegisterProc('ActStrUpper',       @ActStrUpper, _procs);
   RegisterProc('ActStringConstant', @ActStringConstant, _procs);
   RegisterProc('ActValueOrg',       @ActValueOrg, _procs);
   RegisterProc('ActValueOrg ',      @ActValueOrg , _procs);
   RegisterProc('ActValueSymbol',    @ActValueSymbol, _procs);
end;

end.

