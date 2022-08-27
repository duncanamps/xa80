unit uexprparser;

{$mode ObjFPC}{$H+}

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

interface

// Expression evaluator. Has a set of operators, functions and other tokens
// which can be defined at runtime
//
// The general data type is WORD (16 bit). It's possible for the information
// to overflow, for example:
//
//   0x0002 - 0x0004 -> 0xfffe
//   0x8000 + 0x9000 -> 0x1000
//
// It assumes that all tokens containing expressions / numbers are already in
// WORD type. So strings or characters like '3' need to be turned into 0x0033
// first.

uses
  Classes, SysUtils, Generics.Collections;

const
  U32_MAX = $FFFFFFFF;

type
  TEPDataType = integer;

  TEPTokenType = (
                   ttNone,
                   ttEOF,
                   ttComma,
                   ttExpr,
                   ttLbrack,
                   ttRbrack,
                   ttUnary,
                   ttUnaryOrBinary,
                   ttBinary,
                   ttFunc1,
                   ttFunc2
                 );

  TEPReductionType = (
                       rtBrackets,
                       rtUnary,
                       rtBinary,
                       rtFunc1,
                       rtFunc2
                     );

  TEPToken = (
               tokNone,
               tokLbrack,
               tokFuncHigh,
               tokFuncLow,
               tokFuncWord,
               tokLogicalNot,
               tokBitwiseNot,
               tokModulo,
               tokMultiply,
               tokDivide,
               tokMinus,
               tokPlus,
               tokLShift,
               tokRShift,
               tokLessThan,
               tokLessEqual,
               tokGreaterThan,
               tokGreaterEqual,
               tokNotEqual,
               tokEqual,
               tokBitwiseAnd,
               tokBitwiseXor,
               tokBitwiseOr,
               tokLogicalAnd,
               tokLogicalOr,
               tokComma,
               tokRbrack,
               tokEOF,
               tokExprLabel,
               tokExprOrg,
               tokExprI32,
               tokExprStr
             );

  TEPStackEntry = record
    FToken: TEPToken;
    FPayload: TEPDataType;
    FOriginal: string;
    FColumn:   integer;
  end;

  TEPStack = class; // Preliminary declaration

  TEPReduceFunc = function (_stack: TEPStack; _index: integer): TEPStackEntry of object;
  TEPOnReduceProc = procedure (_stack: TEPStack; _rt: TEPReductionType; _index: integer) of object;

const
  EPTokenTypes: array[TEPToken] of TEPTokenType =
    (
      ttNone,            // tokNone,
      ttLbrack,          // tokLbrack,
      ttFunc1,           // tokFuncHigh,
      ttFunc1,           // tokFuncLow,
      ttFunc1,           // tokFuncWord,
      ttUnary,           // tokLogicalNot,
      ttUnary,           // tokBitwiseNot,
      ttBinary,          // tokModulo,
      ttBinary,          // tokMultiply,
      ttBinary,          // tokDivide,
      ttUnaryOrBinary,   // tokMinus,
      ttUnaryOrBinary,   // tokPlus,
      ttBinary,          // tokLShift,
      ttBinary,          // tokRShift,
      ttBinary,          // tokLessThan,
      ttBinary,          // tokLessEqual,
      ttBinary,          // tokGreaterThan,
      ttBinary,          // tokGreaterEqual,
      ttBinary,          // tokNotEqual,
      ttBinary,          // tokEqual,
      ttBinary,          // tokBitwiseAnd,
      ttBinary,          // tokBitwiseXor,
      ttBinary,          // tokBitwiseOr,
      ttBinary,          // tokLogicalAnd,
      ttBinary,          // tokLogicalOr,
      ttComma,           // tokComma,
      ttRbrack,          // tokRbrack,
      ttEOF,             // tokEOF,
      ttExpr,            // tokExprLabel
      ttExpr,            // tokExprOrg
      ttExpr,            // tokExprI32
      ttExpr             // tokExprStr
    );

  EPReductionLengths: array[TEPReductionType] of integer =
    (
      3,  // rtBrackets
      2,  // rtUnary
      3,  // rtBinary
      3,  // rtFunc1FP
      5   // rtFunc2FP
    );

type
  TEPStack = class(specialize TList<TEPStackEntry>)
    protected
      FAfterReduce:     TEPOnReduceProc;
      FBeforeReduce:    TEPOnReduceProc;
    public
      ReduceFuncs:     array[TEPReductionType] of TEPReduceFunc;
      procedure Dump(_strm: TStream);
      procedure Need(_start: integer; _count: integer);
      procedure PartialParse(_start: integer);
      procedure Parse(_downto: integer);
      procedure Push(_e: TEPStackEntry);
      procedure Push(_t: TEPToken; _str: string = ''; _col: integer = 0; _v: TEPDataType = 0);
      procedure Push(_p: TEPDataType);
      procedure Reduce(_rt: TEPReductionType; _index: integer);
      function  StackEntryAsHuman(_e: TEPStackEntry): string;
      function  StackEntryAsText(_e: TEPStackEntry): string;
      procedure Tokenise(const s: string; _startcol: integer);
      property  AfterReduce: TEPOnReduceProc read FAfterReduce write FAfterReduce;
      property  BeforeReduce: TEPOnReduceProc read FBeforeReduce write FBeforeReduce;
  end;

// Forward declarations

procedure ClearEntry(var _e: TEPStackEntry);
procedure SetI32(var _e: TEPStackEntry; _v: TEPDataType);

var
  EPTokenLabels: array[TEPToken] of string =
    (
      '<none>',
      '(',
      'HIGH(',
      'LOW(',
      'WORD(',
      '!',
      '~',
      '%',
      '*',
      '/',
      '-',
      '+',
      '<<',
      '>>',
      '<',
      '<=',
      '>',
      '>=',
      '!=',
      '==',
      '&',
      '^',
      '|',
      '&&',
      '||',
      ',',
      ')',
      '<eof>',
      '<label>',
      '$',
      '<exprU16>',
      '<exprStr>'
    );

  EPAllowed: array[TEPToken] of boolean =
    (
      False,  // tokNone
      True,   // tokLbrack
      True,   // tokFuncHigh
      True,   // tokFuncLow
      True,   // tokFuncWord
      True,   // tokLogicalNot
      True,   // tokBitwiseNot
      True,   // tokModulo
      True,   // tokMultiply
      True,   // tokDivide
      True,   // tokMinus
      True,   // tokPlus
      True,   // tokLShift
      True,   // tokRShift
      True,   // tokLessThan
      True,   // tokLessEqual
      True,   // tokGreaterThan
      True,   // tokGreaterEqual
      True,   // tokNotEqual
      True,   // tokEqual
      True,   // tokBitwiseAnd
      True,   // tokBitwiseXor
      True,   // tokBitwiseOr
      True,   // tokLogicalAnd
      True,   // tokLogicalOr
      True,   // tokComma
      True,   // tokRbrack
      False,  // tokEOF
      False,  // tokExprLabel
      True,   // tokExprOrg
      False,  // tokExprI32
      False   // tokExprStr
    );

  EPPrecedence: array[TEPToken] of integer =
    (
      0,   // tokNone
      1,   // tokLbrack
      1,   // tokFuncHigh
      1,   // tokFuncLow
      1,   // tokFuncWord
      2,   // tokLogicalNot
      2,   // tokBitwiseNot
      3,   // tokModulo
      3,   // tokMultiply
      3,   // tokDivide
      4,   // tokMinus
      4,   // tokPlus
      5,   // tokLShift
      5,   // tokRShift
      6,   // tokLessThan
      6,   // tokLessEqual
      6,   // tokGreaterThan
      6,   // tokGreaterEqual
      7,   // tokNotEqual
      7,   // tokEqual
      8,   // tokBitwiseAnd
      9,   // tokBitwiseXor
      10,  // tokBitwiseOr
      11,  // tokLogicalAnd
      12,  // tokLogicalOr
      15,  // tokComma
      99,  // tokRbrack
      99,  // tokEOF
      99,  // tokExprLabel
      99,  // tokExprOrg
      99,  // tokExprI32
      99   // tokExprStr
    );



implementation

uses
  typinfo;

procedure ClearEntry(var _e: TEPStackEntry);
begin
  _e.FPayload    := 0;
  _e.FToken      := tokNone;
end;

procedure SetI32(var _e: TEPStackEntry; _v: TEPDataType);
begin
  _e.FPayload := _v;
  _e.FToken   := tokExprI32;
end;

//==============================================================================
//
//  TEPStack code
//
//==============================================================================

procedure TEPStack.Dump(_strm: TStream);
var _e: TEPStackEntry;
    s:  string;
begin
  for _e in Self do
    begin
      s := StackEntryAsText(_e) + #13 + #10;
      _strm.Write(s[1],Length(s));
    end;
  s := #13 + #10;
  _strm.Write(s[1],Length(s));
end;

procedure TEPStack.Need(_start: integer; _count: integer);
begin
  if (_start + _count) > Count then
    raise Exception.Create('Attempting to parse beyond end of stack');
end;

// Parse the stack entries from _start onwards

procedure TEPStack.PartialParse(_start: integer);
var op: TEPToken;
begin
  // Stack position could be:
  //   expression which may be followed by binary op
  //   function
  //   unary op such as + or -
  //   open bracket (
  // Unary op and open bracket are higher priority than binary ops
  Need(_start,1);
  case EPTokenTypes[Items[_start].FToken] of
    ttEOF,
    ttComma,
    ttRbrack,
    ttBinary: raise Exception.Create(Format('Unexpected token %s',[GetEnumName(TypeInfo(TEPToken),Ord(Items[_start].FToken))]));
    ttExpr:
      begin
        // Could be: <expr> <op> <expr> <something...>
        //       or: <expr> <op> <func>
        //       or: <expr> <op> "("
        //       or: <expr> <op> <unaryop>
        //       or: <expr> <EOF>
        //       or: a fail
        Need(_start,2);
        // Check second field
        case EPTokenTypes[Items[_start+1].FToken] of
          ttBinary,
          ttUnaryOrBinary:
            // We know is <expr> <op> <something...>
            // Check the third field
            begin
              Need(_start,3);
              op := Items[_start+1].FToken;
              case EPTokenTypes[Items[_start+2].FToken] of
                ttLbrack,
                ttFunc1,
                ttFunc2,
                ttUnary,
                ttUnaryOrBinary: PartialParse(_start+2);
                ttExpr:
                  if EPPrecedence[Items[_start+3].FToken] < EPPrecedence[op] then
                    PartialParse(_start+2)
                  else
                    Reduce(rtBinary,_start);
                otherwise
                  raise Exception.Create(Format('Token type %s not catered for in Parse()',
                                                [GetEnumName(TypeInfo(TEPTokenType),Ord(EPTokenTypes[Items[_start+2].FToken]))]));
              end;
            end;
          otherwise
            raise Exception.Create(Format('Unexpected token %s',[GetEnumName(TypeInfo(TEPToken),Ord(Items[_start+1].FToken))]));
        end;
      end;
    ttLbrack:
      begin
        Need(_start,3);
        if (EPTokenTypes[Items[_start+1].FToken] = ttExpr) and
           (EPTokenTypes[Items[_start+2].FToken] = ttRbrack) then
          Reduce(rtBrackets,_start)
        else
          PartialParse(_start+1);
      end;
    ttFunc1:
      begin
        Need(_start,3);
        if (EPTokenTypes[Items[_start+1].FToken] = ttExpr) and
           (EPTokenTypes[Items[_start+2].FToken] = ttRbrack) then
          Reduce(rtFunc1,_start)
        else
          PartialParse(_start+1);
      end;
    ttFunc2:
      begin
        Need(_start,5);
        if (EPTokenTypes[Items[_start+1].FToken] <> ttExpr) or
           (EPTokenTypes[Items[_start+2].FToken] <> ttComma) then
          PartialParse(_start+1)
        else if (EPTokenTypes[Items[_start+3].FToken] <> ttExpr) or
                (EPTokenTypes[Items[_start+4].FToken] <> ttRbrack) then
          PartialParse(_start+3)
        else
          Reduce(rtFunc2,_start);
      end;
    ttUnary,
    ttUnaryOrBinary:
      begin
        Need(_start,2);
        if (EPTokenTypes[Items[_start+1].FToken] <> ttExpr) then
          PartialParse(_start+1)
        else
          Reduce(rtUnary,_start);
      end;
    otherwise
      raise Exception.Create(Format('Token type %s not catered for in Parse()',
                                    [GetEnumName(TypeInfo(TEPTokenType),Ord(EPTokenTypes[Items[_start].FToken]))]));
  end;
end;

procedure TEPStack.Parse(_downto: integer);
begin
  while Count > _downto do
    PartialParse(0);
end;

procedure TEPStack.Push(_e: TEPStackEntry);
begin
  Add(_e);
end;

procedure TEPStack.Push(_t: TEPToken; _str: string; _col: integer; _v: TEPdataType);
var _e: TEPStackEntry;
begin
  ClearEntry(_e{%H-});
  _e.FToken      := _t;
  _e.FOriginal   := _str;
  _e.FPayload    := _v;
  _e.FColumn     := _col;
  Push(_e);
end;

procedure TEPStack.Push(_p: TEPDataType);
var _e: TEPStackEntry;
begin
  ClearEntry(_e{%H-});
  SetI32(_e,_p);
  Push(_e);
end;

procedure TEPStack.Reduce(_rt: TEPReductionType; _index: integer);
var e:        TEPStackEntry;
    i:        integer;
begin
  Need(_index,EPReductionLengths[_rt]);
  ClearEntry(e{%H-});
  if Assigned(FBeforeReduce) then
    FBeforeReduce(Self,_rt,_index);
  if Assigned(ReduceFuncs[_rt]) then
    e := ReduceFuncs[_rt](Self,_index)
  else
    raise Exception.Create(Format('Reduction type %s has no function defined',
             [GetEnumName(TypeInfo(TEPReductionType),Ord(_rt))]));
  // Get rid of the extra items
  for i := 2 to EPReductionLengths[_rt] do
    Delete(_index+1);
  // Now set the reduction result
  Items[_index] := e;
  if Assigned(FAfterReduce) then
    FAfterReduce(Self,_rt,_index);
end;

function TEPStack.StackEntryAsHuman(_e: TEPStackEntry): string;
var s: string;
begin
  if EPTokenTypes[_e.FToken] = ttExpr then
    s := Format('$%8.8X',[_e.FPayload])
  else
    s := EPTokenLabels[_e.FToken];
  Result := s;
end;

function TEPStack.StackEntryAsText(_e: TEPStackEntry): string;
var s: string;
begin
  s := GetEnumName(TypeInfo(TEPToken),Ord(_e.FToken));
  if _e.FToken in [tokExprOrg,tokExprLabel,tokExprI32] then
    s := s + ' I32: ' + Format('$%8.8X',[_e.FPayload]);
  s := s + Format(' "%s" [%d]',[_e.FOriginal,_e.FColumn]);
  Result := s;
end;


// Tokeniser. Take an input string with its nominal starting column and break
// it up into the tokens that the parser can process. Tries to tokenise in the
// following order:
//
// 1. Whitespace - put this into a tokNone token
// 2. String - put this into a tokExprStr token
// 3. Keywords - like LOW( >= + % &&
// 4. Anything else goes into an overflow. If we hit whitespace, string, EOF
//    keyword or EOF, we try and convert the input segment into a tokExpr32 or
//    tokExprLabel ready for further processing

procedure TEPStack.Tokenise(const s: string; _startcol: integer);
var buf:    string;
    t:      string;
    i:      integer;
    of_buf: string;
    of_col: integer;
    newtok: TEPToken;

  procedure ExtractKeyword(var _index: integer; var _dest: string; var _newtok: TEPToken);
  var match_len: integer;
      match_tok: TEPToken;
      match_str: string;
      i:         TEPToken;
      partial:   string;
      toklen:    integer;
  begin
    match_len := 0;
    match_tok := tokNone;
    match_str := '';
    for i in TEPToken do
      if EPAllowed[i] then
        begin
          toklen := Length(EPTokenLabels[i]);
          partial := UpperCase(Copy(buf,_index,toklen));
          if (partial = EPTokenLabels[i]) and (toklen > match_len) then
            begin
              match_len := toklen;
              match_tok := i;
              match_str := EPTokenLabels[i];
            end;
        end;
    if match_tok <> tokNone then
      _index  := _index + Length(match_str);
    _dest   := match_str;
    _newtok := match_tok;
  end;

  procedure ExtractString(var _index: integer; var _dest: string; _delim: char);
  var startidx: integer;
  begin
    _dest := '';
    startidx := _index;
    Inc(_index);
    while not (buf[_index] in [_delim,#26]) do
      begin
        _dest := _dest + buf[_index];
        Inc(_index);
      end;
    if buf[_index] = #26 then
      raise Exception.Create(Format('String not terminated starting column %d [%s]',[startidx+_startcol-1,_dest]));
    Inc(_index);
  end;

  procedure ExtractWhiteSpace(var _index: integer; var _dest: string);
  begin
    _dest := '';
    while buf[_index] = ' ' do
      begin
        _dest := _dest + buf[_index];
        Inc(_index);
      end;
  end;

  procedure DealWithOverflow;
  begin
    if of_buf <> '' then
      begin
        // Figure out if number or label
        if of_buf[1] in ['0'..'9'] then
          Push(tokExprI32,of_buf,of_col, StrToInt(of_buf))
        else
          Push(tokExprLabel,of_buf,of_col);
        of_buf := '';
        of_col := -1;
      end;
  end;

begin
  Clear; // Make sure stack is empty
  buf := s + #26; // Add <eof> character to the end of the string
  of_buf := '';
  of_col := -1;
  i := 1;
  while i <= Length(buf) do
    begin
      case buf[i] of
        ' ': begin
               DealWithOverflow;
               ExtractWhiteSpace(i,t{%H-});
//             Push(tokNone,t,i-Length(t));
             end;
        #34,
        #39: begin
               DealWithOverflow;
               ExtractString(i,t,buf[i]);
               Push(tokExprStr,t,i-Length(t)-2);
             end;
        #26: begin
               DealWithOverflow;
               Push(tokEOF,buf[i],i);
               Inc(i);
             end;
        otherwise
          begin
            // Check for keywords etc.
            ExtractKeyword(i,t,newtok{%H-});
            if t <> '' then
              begin
                DealWithOverflow;
                if newtok = tokExprOrg then
                  Push(newtok,t,i-Length(t),$0200) // @@@@@@ Need to set ORG address properly
                else
                  Push(newtok,t,i-Length(t));
              end
            else
              begin
                if of_buf = '' then
                  of_col := i;
                of_buf := of_buf + buf[i];
                Inc(i);
              end;
          end;
      end;
    end;
end;

end.

