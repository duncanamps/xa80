unit ugrammar;

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

//
// Grammar unit. Holds the defined rules for a particular grammar. Can load/save
// grammars, and has some utility functions to parse numbers etc.
//
// RegisterObjects allows each grammar item to be registered.
//

interface

uses
{$IFDEF GRAMMAR_EDITOR}
  Forms,
{$ENDIF}
  Classes, SysUtils, Generics.Collections;

const
  GRAMMAR_FIELD_SEPARATOR = '|';
  GRAMMAR_FIELD_SEPARATOR_EXPANDED = ' | ';

type
  TNOM = (tnNever,tnOptional,tnMandatory);

  TMacroLabelRule = (mlrNeverLocal,mlrLocalIfPrefixed,mlrAlwaysLocal);

  TGrammarCharSet = set of char;

  TGrammarDataType = (
                        gdtBoolean,
                        gdtCharList,
                        gdtCharSet,
                        gdtFuncDef,
                        gdtMacroLabelRule,
                        gdtNOM,
                        gdtOperatorDef,
                        gdtString,
                        gdtStringList,
                        gdtText,
                        gdtU16
                      );

  TGrammarObj = class; // Forward definition

  TGrammarObjValidator = function(_obj: TGrammarObj; var _msg: string): boolean;

  TGrammarObj = class(TObject)
    public
      Title:       string;
      // The data type and variables V  V  V   cannot use variants or variant records
      // for a variety of reasons
      boolVar:     boolean;
      csetVar:     TGrammarCharSet;
      DataType:    TGrammarDataType;
      mlrVar:      TMacroLabelRule;
      nomVar:      TNOM;
      strlistVar:  TStringList;
      strVar:      string;
      wordVar:     word;
      // The data type and variables ^  ^  ^
      Validator: TGrammarObjValidator;
      Default:   string;
      destructor Destroy; override;
      function  AsText: string;
      function  CharsetToStr(_cs: TGrammarCharSet): string;
      function  DecodeStr(_v: string): string;
      function  EncodeStr(_v: string): string;
      function  SetVal(const _v: string; var _msg: string): boolean;
      function  StrToCharset(const _v: string): TGrammarCharSet;
  end;

{$IFDEF GRAMMAR_EDITOR}
  TGrammarEditor = class(TForm)
    protected
      FObj: TGrammarObj;
    public
      constructor Create(AOwner: TComponent; _obj: TGrammarObj); virtual;
  end;
{$ENDIF}

  TGrammarList = class(specialize TObjectList<TGrammarObj>)

  end;

  TGrammar = class(TObject)
    private
      // Other variables
      FGrammarList: TGrammarList;
      // Functions / procedures
      procedure RegisterObjects;
      procedure RegisterSingleObject(const _title: string; _datatype: TGrammarDataType; _validator: TGrammarObjValidator; const _default: string);
  public
      // Other properties
      property GrammarList: TGrammarList read FGrammarList;
      // Procedures, functions, ...
      constructor Create;
      destructor Destroy; override;
      procedure New;
  end;

function ValidateEscapeChar(_obj: TGrammarObj; var _msg: string): boolean;

implementation

uses
  typinfo;

type
  TInitRec = record
    Title:     string;
    DataType:  TGrammarDataType;
    Validator: TGrammarObjValidator;
    Default:   string
  end;

const
  InitArray: array[0..96] of TInitRec =
  (
    (Title: 'Author';                 DataType: gdtText;           Validator: nil;                 Default: 'Duncan Munro'),
    (Title: 'CmdCPU';                 DataType: gdtStringList;     Validator: nil;                 Default: ''),
    (Title: 'CmdDefineBytes';         DataType: gdtStringList;     Validator: nil;                 Default: 'DB|DEFB'),
    (Title: 'CmdDefineStorage';       DataType: gdtStringList;     Validator: nil;                 Default: 'DS|DEFS'),
    (Title: 'CmdDefineString';        DataType: gdtStringList;     Validator: nil;                 Default: 'DM|DEFM'),
    (Title: 'CmdDefineStringH';       DataType: gdtStringList;     Validator: nil;                 Default: 'DC|DEFC'),
    (Title: 'CmdDefineStringZ';       DataType: gdtStringList;     Validator: nil;                 Default: 'DZ|DEFZ'),
    (Title: 'CmdDefineWord16';        DataType: gdtStringList;     Validator: nil;                 Default: 'DW|DEFW'),
    (Title: 'CmdDefineWord32';        DataType: gdtStringList;     Validator: nil;                 Default: 'DD|DEFD'),
    (Title: 'CmdElse';                DataType: gdtStringList;     Validator: nil;                 Default: 'ELSE'),
    (Title: 'CmdEnd';                 DataType: gdtStringList;     Validator: nil;                 Default: 'END'),
    (Title: 'CmdEndif';               DataType: gdtStringList;     Validator: nil;                 Default: 'ENDIF'),
    (Title: 'CmdEndMacro';            DataType: gdtStringList;     Validator: nil;                 Default: 'ENDM'),
    (Title: 'CmdEndRepeat';           DataType: gdtStringList;     Validator: nil;                 Default: 'ENDR'),
    (Title: 'CmdEquate';              DataType: gdtStringList;     Validator: nil;                 Default: 'EQU|='),
    (Title: 'CmdError';               DataType: gdtStringList;     Validator: nil;                 Default: 'ERROR'),
    (Title: 'CmdExtern';              DataType: gdtStringList;     Validator: nil;                 Default: 'EXTERN|EXTERNAL'),
    (Title: 'CmdGlobal';              DataType: gdtStringList;     Validator: nil;                 Default: 'GLOBAL'),
    (Title: 'CmdIf';                  DataType: gdtStringList;     Validator: nil;                 Default: 'IF'),
    (Title: 'CmdIfdef';               DataType: gdtStringList;     Validator: nil;                 Default: 'IFDEF'),
    (Title: 'CmdIfndef';              DataType: gdtStringList;     Validator: nil;                 Default: 'IFNDEF'),
    (Title: 'CmdInclude';             DataType: gdtStringList;     Validator: nil;                 Default: 'INCLUDE'),
    (Title: 'CmdMacroDefine';         DataType: gdtStringList;     Validator: nil;                 Default: 'MACRO'),
    (Title: 'CmdMessage';             DataType: gdtStringList;     Validator: nil;                 Default: 'MESSAGE'),
    (Title: 'CmdOrg';                 DataType: gdtStringList;     Validator: nil;                 Default: 'ORG|ORIGIN'),
    (Title: 'CmdRepeat';              DataType: gdtStringList;     Validator: nil;                 Default: 'REPEAT'),
    (Title: 'CmdSEGC';                DataType: gdtStringList;     Validator: nil;                 Default: 'CSEG|CODE'),
    (Title: 'CmdSEGD';                DataType: gdtStringList;     Validator: nil;                 Default: 'DSEG|DATA'),
    (Title: 'CmdSEGU';                DataType: gdtStringList;     Validator: nil;                 Default: 'USEG|UDATA'),
    (Title: 'CmdSet';                 DataType: gdtStringList;     Validator: nil;                 Default: 'SET'),
    (Title: 'CmdTitle';               DataType: gdtStringList;     Validator: nil;                 Default: 'TITLE'),
    (Title: 'CmdWarning';             DataType: gdtStringList;     Validator: nil;                 Default: 'WARNING'),
    (Title: 'CommentAnywhere';        DataType: gdtStringList;     Validator: nil;                 Default: ';|//'),
    (Title: 'CommentStart';           DataType: gdtStringList;     Validator: nil;                 Default: '*|;|//'),
    (Title: 'DefaultOrg';             DataType: gdtU16;            Validator: nil;                 Default: '0'),
    (Title: 'DefaultProcessor';       DataType: gdtText;           Validator: nil;                 Default: 'Z80'),
    (Title: 'EndBaggage';             DataType: gdtNOM;            Validator: nil;                 Default: 'Optional'),
    (Title: 'EndRule';                DataType: gdtNOM;            Validator: nil;                 Default: 'Optional'),
    (Title: 'EscapeCharacter';        DataType: gdtString;         Validator: @ValidateEscapeChar; Default: '\'),
    (Title: 'EscapeNumbers';          DataType: gdtStringList;     Validator: nil;                 Default: 'octal|Xhex|xhex'),
    (Title: 'EscapeSet';              DataType: gdtCharset;        Validator: nil;                 Default: '[?\\"''abefnrtv]'),
    (Title: 'ExprOrg';                DataType: gdtString;         Validator: nil;                 Default: '$'),
    (Title: 'FuncHigh';               DataType: gdtStringList;     Validator: nil;                 Default: 'HIGH('),
    (Title: 'FuncLow';                DataType: gdtStringList;     Validator: nil;                 Default: 'LOW('),
    (Title: 'LabelCaseSensitive';     DataType: gdtBoolean;        Validator: nil;                 Default: 'False'),
    (Title: 'LabelCharactersMid';     DataType: gdtCharset;        Validator: nil;                 Default: '[0-9A-Za-z]'),
    (Title: 'LabelCharactersStart';   DataType: gdtCharset;        Validator: nil;                 Default: '[A-Za-z]'),
    (Title: 'LabelColonRuleEqu';      DataType: gdtNOM;            Validator: nil;                 Default: 'Never'),
    (Title: 'LabelColonRuleNormal';   DataType: gdtNOM;            Validator: nil;                 Default: 'Optional'),
    (Title: 'LabelLocalPrefix';       DataType: gdtString;         Validator: nil;                 Default: '@'),
    (Title: 'LabelLocalSuffix';       DataType: gdtString;         Validator: nil;                 Default: ''),
    (Title: 'LabelMaximumLimit';      DataType: gdtU16;            Validator: nil;                 Default: '128'),
    (Title: 'LabelMaximumUsed';       DataType: gdtU16;            Validator: nil;                 Default: '128'),
    (Title: 'LabelPredefineReg';      DataType: gdtBoolean;        Validator: nil;                 Default: 'False'),
    (Title: 'LiteralASCIIquote';      DataType: gdtStringList;     Validator: nil;                 Default: ''',"'),
    (Title: 'LiteralBinaryFormat';    DataType: gdtStringList;     Validator: nil;                 Default: '0bnnnn|0Bnnnn|%nnnn|nnnnb|nnnnB'),
    (Title: 'LiteralDecimalFormat';   DataType: gdtStringList;     Validator: nil;                 Default: 'nnnn|nnnnd|nnnnD'),
    (Title: 'LiteralHexFormat';       DataType: gdtStringList;     Validator: nil;                 Default: '0xnnnn|0Xnnnn|$nnnn|#nnnn|nnnnh|nnnnH'),
    (Title: 'LiteralOctalFormat';     DataType: gdtStringList;     Validator: nil;                 Default: 'nnnnO|nnnno|nnnnQ|nnnnq|0onnnn|0Onnnn'),
    (Title: 'MacroLabelPrefixG';      DataType: gdtString;         Validator: nil;                 Default: '%G%'),
    (Title: 'MacroLabelPrefixL';      DataType: gdtString;         Validator: nil;                 Default: ''),
    (Title: 'MacroLabelRule';         DataType: gdtMacroLabelRule; Validator: nil;                 Default: 'Global if prefixed'),
    (Title: 'MacroParamPrefix';       DataType: gdtString;         Validator: nil;                 Default: ''),
    (Title: 'MacroParamUse';          DataType: gdtString;         Validator: nil;                 Default: ''),
    (Title: 'OpBinaryAdd';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '+|[5]'),
    (Title: 'OpBinaryDivide';         DataType: gdtOperatorDef;    Validator: nil;                 Default: '/|DIV|[3]'),
    (Title: 'OpBinaryMod';            DataType: gdtOperatorDef;    Validator: nil;                 Default: 'MOD|[3]'),
    (Title: 'OpBinaryMultiply';       DataType: gdtOperatorDef;    Validator: nil;                 Default: '*|[3]'),
    (Title: 'OpBinaryShl';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '<<|[4]'),
    (Title: 'OpBinaryShr';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '>>|[4]'),
    (Title: 'OpBinarySubtract';       DataType: gdtOperatorDef;    Validator: nil;                 Default: '-|[5]'),
    (Title: 'OpBitwiseAnd';           DataType: gdtOperatorDef;    Validator: nil;                 Default: '&|[3]'),
    (Title: 'OpBitwiseOr';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '{rule}|[5]'),
    (Title: 'OpBitwiseXor';           DataType: gdtOperatorDef;    Validator: nil;                 Default: '^|[3]'),
    (Title: 'OpBracketClose';         DataType: gdtOperatorDef;    Validator: nil;                 Default: ')|[1]'),
    (Title: 'OpBracketOpen';          DataType: gdtOperatorDef;    Validator: nil;                 Default: '(|[1]'),
    (Title: 'OpcodeSquareRule';       DataType: gdtNOM;            Validator: nil;                 Default: 'Optional'),
    (Title: 'OpCompEqual';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '==|[6]'),
    (Title: 'OpCompGreater';          DataType: gdtOperatorDef;    Validator: nil;                 Default: '>|[6]'),
    (Title: 'OpCompGreaterEqual';     DataType: gdtOperatorDef;    Validator: nil;                 Default: '>=|[6]'),
    (Title: 'OpCompLess';             DataType: gdtOperatorDef;    Validator: nil;                 Default: '<|[6]'),
    (Title: 'OpCompLessEqual';        DataType: gdtOperatorDef;    Validator: nil;                 Default: '<=|[6]'),
    (Title: 'OpCompUnequal';          DataType: gdtOperatorDef;    Validator: nil;                 Default: '<>|!=|[6]'),
    (Title: 'OpLogicalAnd';           DataType: gdtOperatorDef;    Validator: nil;                 Default: '&&|AND|[7]'),
    (Title: 'OpLogicalOr';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '{rule}{rule}|OR|[8]'),
    (Title: 'OpLogicalXor';           DataType: gdtOperatorDef;    Validator: nil;                 Default: '^^|XOR|[7]'),
    (Title: 'OpUnaryMinus';           DataType: gdtOperatorDef;    Validator: nil;                 Default: '-|[2]'),
    (Title: 'OpUnaryNot';             DataType: gdtOperatorDef;    Validator: nil;                 Default: '!|NOT|[9]'),
    (Title: 'OpUnaryPlus';            DataType: gdtOperatorDef;    Validator: nil;                 Default: '+|[2]'),
    (Title: 'OpUnaryResult';          DataType: gdtOperatorDef;    Validator: nil;                 Default: ''),
    (Title: 'ParserInterfldAllowed';  DataType: gdtCharSet;        Validator: nil;                 Default: '[]'),
    (Title: 'ParserInterfldMandated'; DataType: gdtCharSet;        Validator: nil;                 Default: '[\t ]'),
    (Title: 'ParserInteropAllowed';   DataType: gdtCharSet;        Validator: nil;                 Default: '[\t ]'),
    (Title: 'ParserInteropMandated';  DataType: gdtCharSet;        Validator: nil;                 Default: '[,]'),
    (Title: 'StringTerminator';       DataType: gdtCharSet;        Validator: nil;                 Default: '[''"]'),
    (Title: 'Title';                  DataType: gdtText;           Validator: nil;                 Default: 'Default XA80 Grammar'),
    (Title: 'TokeniserTabSize';       DataType: gdtU16;            Validator: nil;                 Default: '4')
  );

function ValidateEscapeChar(_obj: TGrammarObj; var _msg: string): boolean;
begin
  Result := False;
  if Length(_obj.strVar) > 1 then
    _msg := 'Escape character should blank or 1 character'
  else
    Result := True;
end;

//==============================================================================
//
//  TGrammarEditor code
//
//==============================================================================

{$IFDEF GRAMMAR_EDITOR}
constructor TGrammarEditor.Create(AOwner: TComponent; _obj: TGrammarObj);
begin
  inherited Create(AOwner);
  FObj := _obj;
end;
{$ENDIF}

//==============================================================================
//
//  TGrammarObj code
//
//==============================================================================

destructor TGrammarObj.Destroy;
begin
  // Check if a TStringList was made and clear it if required
  if DataType in [gdtStringList,gdtFuncDef,gdtOperatorDef,gdtCharList] then
    FreeAndNil(strlistVar);
  // Finally, destroy this object
  inherited Destroy;
end;

function TGrammarObj.AsText: string;
var sl: TStringList;
    i:  integer;
begin
  case DataType of
    gdtString:
      Result := EncodeStr(strVar);
    gdtStringList,
    gdtFuncDef,
    gdtOperatorDef,
    gdtCharList:
      begin
        // Make a copy of the string list and use that to create the result
        sl := TStringList.Create;
        try
          sl.Delimiter := #13;
          sl.AlwaysQuote := False;
          sl.QuoteChar := #0;
          sl.DelimitedText := strlistVar.DelimitedText;
          for i := 0 to sl.Count-1 do
            sl[i] := EncodeStr(sl[i]);
          sl.Delimiter := GRAMMAR_FIELD_SEPARATOR;
          Result := sl.DelimitedText;
          Result := StringReplace(Result,GRAMMAR_FIELD_SEPARATOR,GRAMMAR_FIELD_SEPARATOR_EXPANDED,[rfReplaceAll]);
        finally
          FreeAndNil(sl);
        end;
      end;
    gdtU16:
      Result := IntToStr(wordVar);
    gdtNOM:
      case nomVar of
        tnNever:     Result := 'Never';
        tnOptional:  Result := 'Optional';
        tnMandatory: Result := 'Mandatory';
      end;
    gdtCharSet:
      Result := CharSetToStr(csetVar);
    gdtBoolean:
      case boolVar of
        True:  Result := 'True';
        False: Result := 'False';
      end;
    gdtText:
      Result := strVar;
    otherwise
      raise Exception.Create(Format('Internal error, data type %s not catered for in TGrammarObj.AsText',[GetEnumName(TypeInfo(TGrammarDataType),Ord(DataType))]));
  end;
end;

function TGrammarObj.CharsetToStr(_cs: TGrammarCharSet): string;
var c: char;
    done: boolean;
    run_length: integer;

  function Beautify(_c:char): string;
  begin
    case _c of
      #9:  Result := '\t';
      #10: Result := '\n';
      #13: Result := '\r';
      '-',
      '\',
      ']': Result := '\' + _c;
      ' '..',',
      '.'..'[',
      '^'..'~': Result := _c;
      otherwise
        Result := Format('\0%2.2X',[Ord(_c)]);
    end; // case
  end;

  function Bump(_c: char; _amt: integer): char;
  begin
    if Ord(c) + _amt > 255 then
      Result := #255
    else
      Result := Char(Ord(c) + _amt);
  end;

  function BumpChkDone(_c: char; _amt: integer): char;
  begin
    if Ord(c) + _amt > 255 then
      begin
        Result := #255;
        done := True;
      end
    else
      Result := Char(Ord(c) + _amt);
  end;

  function CalcRL(_c: char): integer;
  var i: integer;
  begin
    Result := 1;
    i := Ord(_c) + 1;
    while (i <= 255) and (Char(i) in _cs) do
      begin
        Inc(Result);
        Inc(i);
      end;
  end;

begin
  Result := '[';
  c := #0;
  done := false;
  while not done do
    begin
      if c in _cs then
        begin
          run_length := CalcRL(c);
          if run_length < 4 then
            begin
              Result := Result + Beautify(c);
              c := BumpChkDone(c,1);
            end
          else
            begin
              Result := Result + Beautify(c) + '-' + Beautify(Chr(Ord(c)+run_length-1));
              c := BumpChkDone(c,run_length);
            end;
        end
      else
        c := BumpChkDone(c,1);
    end;
  Result := Result + ']';
end;

function TGrammarObj.DecodeStr(_v: string): string;
begin
  _v := StringReplace(_v,'{closebrace}', '}',[rfReplaceAll]);
  _v := StringReplace(_v,'{comma}',      ',',[rfReplaceAll]);
  _v := StringReplace(_v,'{comma}',      ',',[rfReplaceAll]);
  _v := StringReplace(_v,'{openbrace}',  '{',[rfReplaceAll]);
  _v := StringReplace(_v,'{rule}',       '|',[rfReplaceAll]);
  _v := StringReplace(_v,'{spc}',        ' ',[rfReplaceAll]);
  _v := StringReplace(_v,'{tab}',        #9, [rfReplaceAll]);
  Result := _v;
end;

function TGrammarObj.EncodeStr(_v: string): string;
begin
  _v := StringReplace(_v,'}','{closebrace}',[rfReplaceAll]);
  _v := StringReplace(_v,',','{comma}',     [rfReplaceAll]);
  _v := StringReplace(_v,'{','{openbrace}', [rfReplaceAll]);
  _v := StringReplace(_v,'|','{rule}',      [rfReplaceAll]);
  _v := StringReplace(_v,' ','{spc}',       [rfReplaceAll]);
  _v := StringReplace(_v,#9, '{tab}',       [rfReplaceAll]);
  Result := _v;
end;

function TGrammarObj.SetVal(const _v: string; var _msg: string): boolean;
var i: integer;
    precedence_str: string;
    precedence_val: integer;
begin
  Result := True;  // Assume all good for now
  _msg := '';
  try
  case DataType of
    gdtBoolean:
      boolVar := StrToBool(_v);
    {
    gdtCharList:
    }
    gdtCharSet:
      csetVar := StrToCharset(_v);
    {
    gdtFuncDef:
    }
    gdtMacroLabelRule:
      case _v of
        'Never Local':        mlrVar := mlrNeverLocal;
        'Local if Prefixed':  mlrVar := mlrLocalIfPrefixed;
        'Always Local':       mlrVar := mlrAlwaysLocal;
        otherwise
          raise Exception.Create('Internal error, default value of ' + _v + ' not catered for in TGrammar.RegisterSingleObject');
      end;
    gdtNOM:
      case _v of
        'Never':     nomVar := tnNever;
        'Optional':  nomVar := tnOptional;
        'Mandatory': nomVar := tnMandatory;
        otherwise
          raise Exception.Create('Internal error, default value of ' + _v + ' not catered for in TGrammar.RegisterSingleObject');
      end;
    gdtOperatorDef:
      begin
        strListVar.Delimiter := GRAMMAR_FIELD_SEPARATOR;
        try
          strListVar.DelimitedText := _v;
        finally
          strListVar.Delimiter := #13;
        end;
        for i := 0 to strListVar.Count-1 do
          strListVar[i] := Trim(DecodeStr(strListVar[i]));
        // Ensure the last field is [n] or [nn]
        // If our entry is not blank!
        if strlistVar.Count > 0 then
          begin
            if strlistVar.Count < 2 then
              raise Exception.Create('Insufficient number of parameters for operator definition ' + _v);
            precedence_str := strListVar[strListVar.Count-1];
            if (Length(precedence_str) < 3) or
               (Length(precedence_str) > 4) or
               (precedence_str[1] <> '[') or
               (precedence_str[Length(precedence_str)] <> ']') then
              raise Exception.Create('Operator precedence should be [n] or [nn] with nn being 0 to 99');
            precedence_val := StrToInt(Copy(precedence_str,2,Length(precedence_str)-2));
            if (precedence_val < 0) or
               (precedence_val > 99) then
              raise Exception.Create('Operator precedence should be [n] or [nn] with nn being 0 to 99');
          end;
      end;
    gdtString:
      strVar := DecodeStr(_v);
    gdtStringList:
      begin
        strListVar.Delimiter := GRAMMAR_FIELD_SEPARATOR;
        try
          strListVar.DelimitedText := _v;
        finally
          strListVar.Delimiter := #13;
        end;
        for i := 0 to strListVar.Count-1 do
          strListVar[i] := Trim(DecodeStr(strListVar[i]));
      end;
    gdtText:
      strVar := _v;
    gdtU16:
      wordVar := StrToInt(_v);
    otherwise
      raise Exception.Create(Format('Internal error, data type %s not catered for in TGrammarObj.AsText',[GetEnumName(TypeInfo(TGrammarDataType),Ord(DataType))]));
  end; // case
  except // Catch all exception
    on E:Exception do
      begin
        _msg := E.Message;
        Result := False;
      end;
  end;
end;

function TGrammarObj.StrToCharset(const _v: string): TGrammarCharSet;
var s: string;
    c: char;
    lastc: char;
    i: char;
    escaping: boolean;
    inrange:  boolean;
    haschar:  boolean;
  function NextChar: char;
  begin
    if s = '' then
      Result := #0
    else
      Result := s[1];
  end;
  function EatChar: char;
  begin
    Result := NextChar;
    Delete(s,1,1);
  end;
  function EatHex: char;
  var ival: integer;
  begin
    if Length(s) < 3 then
      raise Exception.Create('Incorrect hex escape character in ' + _v + ', should be \0xx where xx is a pair of hex digits');
    ival := StrToInt('$' + Copy(s,2,2));
    Result := Chr(ival);
    Delete(s,1,3);
  end;
begin
  //
  // The string rep is:
  //   [  element*  ]
  //
  // Element is:
  //   char            single character
  //   char - char     range of characters
  //
  // Char is:
  //   ascii_rep       for example A
  //   \ escape_char     escape, e.g. \t for tab (#9)
  //
  // Escape chars are:
  //   t   tab
  //   r   carriage return
  //   n   newline
  //   ]
  //   \
  //   -
  //   0xx hex value
  Result := [];
  s := _v;
  c := #0;
  lastc := #255;
  escaping := False;
  inrange  := False;
  if Length(_v) < 2 then
    raise Exception.Create('Character set badly formed, should be a set of characters or character ranges enclosed by [ ]');
  EatChar; // Get rid of the opening [
  while s <> '' do
    begin
      haschar := false;
      if escaping then
        begin
          case NextChar of
            't': begin
                   c := #9;
                   EatChar;
                 end;
            'r': begin
                   c := #13;
                   EatChar;
                 end;
            'n': begin
                   c := #10;
                   EatChar;
                 end;
            ']',
            '\',
            '-': c := EatChar;
            '0': c := EatHex;
            otherwise
              raise Exception.Create(Format('Illegal escape character %s in %s',[NextChar,_v]));;
          end;
          escaping := False;
          haschar := True; // Escape sequence will always yield a character
        end
      else
        case NextChar of
          ']' : begin // End of input
                  if inrange then
                    raise Exception.Create('Premature end of character set in ' + _v);
                  EatChar;
                end;
          '\' : begin
                  escaping := True;
                  EatChar;
                end;
          '-' : begin
                  inrange := True;
                  EatChar;
                end;
          otherwise
            begin
              c := EatChar;
              haschar := True;
            end;
        end;
      if haschar then
        begin
          if inrange then
            begin
              for i := lastc to c do
                Result := Result + [i];
              inrange := False;
            end
          else
            begin
              Result := Result + [c];
              lastc := c;
            end;
        end;
    end;
end;


//==============================================================================
//
//  TGrammar code
//
//==============================================================================

constructor TGrammar.Create;
begin
  inherited Create;
  FGrammarList := TGrammarList.Create;
end;

destructor TGrammar.Destroy;
begin
  FreeAndNil(FGrammarList);
  inherited Destroy;
end;

procedure TGrammar.New;
begin
  FGrammarList.Clear;
  RegisterObjects;
end;

procedure TGrammar.RegisterObjects;
var i: integer;
begin
  for i := Low(InitArray) to High(InitArray) do
    RegisterSingleObject(InitArray[i].Title,
                         InitArray[i].DataType,
                         InitArray[i].Validator,
                         InitArray[i].Default);
  {
  RegisterSingleObject('Author',                 gdtText,           nil,                'Duncan Munro');
  RegisterSingleObject('CmdCPU',                 gdtStringList,     nil,                '');
  RegisterSingleObject('CmdDefineBytes',         gdtStringList,     nil,                'DB|DEFB');
  RegisterSingleObject('CmdDefineStorage',       gdtStringList,     nil,                'DS|DEFS');
  RegisterSingleObject('CmdDefineString',        gdtStringList,     nil,                'DM|DEFM');
  RegisterSingleObject('CmdDefineStringH',       gdtStringList,     nil,                'DC|DEFC');
  RegisterSingleObject('CmdDefineStringZ',       gdtStringList,     nil,                'DZ|DEFZ');
  RegisterSingleObject('CmdDefineWord16',        gdtStringList,     nil,                'DW|DEFW');
  RegisterSingleObject('CmdDefineWord32',        gdtStringList,     nil,                'DD|DEFD');
  RegisterSingleObject('CmdElse',                gdtStringList,     nil,                'ELSE');
  RegisterSingleObject('CmdEnd',                 gdtStringList,     nil,                'END');
  RegisterSingleObject('CmdEndif',               gdtStringList,     nil,                'ENDIF');
  RegisterSingleObject('CmdEndMacro',            gdtStringList,     nil,                'ENDM');
  RegisterSingleObject('CmdEndRepeat',           gdtStringList,     nil,                'ENDR');
  RegisterSingleObject('CmdEquate',              gdtStringList,     nil,                'EQU|=');
  RegisterSingleObject('CmdError',               gdtStringList,     nil,                'ERROR');
  RegisterSingleObject('CmdExtern',              gdtStringList,     nil,                'EXTERN|EXTERNAL');
  RegisterSingleObject('CmdGlobal',              gdtStringList,     nil,                'GLOBAL');
  RegisterSingleObject('CmdIf',                  gdtStringList,     nil,                'IF');
  RegisterSingleObject('CmdIfdef',               gdtStringList,     nil,                'IFDEF');
  RegisterSingleObject('CmdIfndef',              gdtStringList,     nil,                'IFNDEF');
  RegisterSingleObject('CmdInclude',             gdtStringList,     nil,                'INCLUDE');
  RegisterSingleObject('CmdMacroDefine',         gdtStringList,     nil,                'MACRO');
  RegisterSingleObject('CmdMessage',             gdtStringList,     nil,                'MESSAGE');
  RegisterSingleObject('CmdOrg',                 gdtStringList,     nil,                'ORG|ORIGIN');
  RegisterSingleObject('CmdRepeat',              gdtStringList,     nil,                'REPEAT');
  RegisterSingleObject('CmdSEGC',                gdtStringList,     nil,                'CSEG|CODE');
  RegisterSingleObject('CmdSEGD',                gdtStringList,     nil,                'DSEG|DATA');
  RegisterSingleObject('CmdSEGU',                gdtStringList,     nil,                'USEG|UDATA');
  RegisterSingleObject('CmdSet',                 gdtStringList,     nil,                'SET');
  RegisterSingleObject('CmdTitle',               gdtStringList,     nil,                'TITLE');
  RegisterSingleObject('CmdWarning',             gdtStringList,     nil,                'WARNING');
  RegisterSingleObject('CommentAnywhere',        gdtStringList,     nil,                ';|//');
  RegisterSingleObject('CommentStart',           gdtStringList,     nil,                '*|;|//');
  RegisterSingleObject('DefaultOrg',             gdtU16,            nil,                '0');
  RegisterSingleObject('DefaultProcessor',       gdtText,           nil,                'Z80');
  RegisterSingleObject('EndBaggage',             gdtNOM,            nil,                'Optional');
  RegisterSingleObject('EndRule',                gdtNOM,            nil,                'Optional');
  RegisterSingleObject('EscapeCharacter',        gdtString,         @ValidateEscapeChar,'\');
  RegisterSingleObject('EscapeNumbers',          gdtStringList,     nil,                'octal|Xhex|xhex');
  RegisterSingleObject('EscapeSet',              gdtCharset,        nil,                '[?\\"''abefnrtv]');
  RegisterSingleObject('ExprOrg',                gdtString,         nil,                '$');
  RegisterSingleObject('FuncHigh',               gdtStringList,     nil,                'HIGH(');
  RegisterSingleObject('FuncLow',                gdtStringList,     nil,                'LOW(');
  RegisterSingleObject('LabelCaseSensitive',     gdtBoolean,        nil,                'False');
  RegisterSingleObject('LabelCharactersMid',     gdtCharset,        nil,                '[0-9A-Za-z]');
  RegisterSingleObject('LabelCharactersStart',   gdtCharset,        nil,                '[A-Za-z]');
  RegisterSingleObject('LabelColonRuleEqu',      gdtNOM,            nil,                'Never');
  RegisterSingleObject('LabelColonRuleNormal',   gdtNOM,            nil,                'Optional');
  RegisterSingleObject('LabelLocalPrefix',       gdtString,         nil,                '@');
  RegisterSingleObject('LabelLocalSuffix',       gdtString,         nil,                '');
  RegisterSingleObject('LabelMaximumLimit',      gdtU16,            nil,                '128');
  RegisterSingleObject('LabelMaximumUsed',       gdtU16,            nil,                '128');
  RegisterSingleObject('LabelPredefineReg',      gdtBoolean,        nil,                'False');
  RegisterSingleObject('LiteralASCIIquote',      gdtStringList,     nil,                ''',"');
  RegisterSingleObject('LiteralBinaryFormat',    gdtStringList,     nil,                '0bnnnn|0Bnnnn|%nnnn|nnnnb|nnnnB');
  RegisterSingleObject('LiteralDecimalFormat',   gdtStringList,     nil,                'nnnn|nnnnd|nnnnD');
  RegisterSingleObject('LiteralHexFormat',       gdtStringList,     nil,                '0xnnnn|0Xnnnn|$nnnn|#nnnn|nnnnh|nnnnH');
  RegisterSingleObject('LiteralOctalFormat',     gdtStringList,     nil,                'nnnnO|nnnno|nnnnQ|nnnnq|0onnnn|0Onnnn');
  RegisterSingleObject('MacroLabelPrefixG',      gdtString,         nil,                '%G%');
  RegisterSingleObject('MacroLabelPrefixL',      gdtString,         nil,                '');
  RegisterSingleObject('MacroLabelRule',         gdtMacroLabelRule, nil,                'Global if prefixed');
  RegisterSingleObject('MacroParamPrefix',       gdtString,         nil,                '');
  RegisterSingleObject('MacroParamUse',          gdtString,         nil,                '');
  RegisterSingleObject('OpBinaryAdd',            gdtOperatorDef,    nil,                '+|[5]');
  RegisterSingleObject('OpBinaryDivide',         gdtOperatorDef,    nil,                '/|DIV|[3]');
  RegisterSingleObject('OpBinaryMod',            gdtOperatorDef,    nil,                'MOD|[3]');
  RegisterSingleObject('OpBinaryMultiply',       gdtOperatorDef,    nil,                '*|[3]');
  RegisterSingleObject('OpBinaryShl',            gdtOperatorDef,    nil,                '<<|[4]');
  RegisterSingleObject('OpBinaryShr',            gdtOperatorDef,    nil,                '>>|[4]');
  RegisterSingleObject('OpBinarySubtract',       gdtOperatorDef,    nil,                '-|[5]');
  RegisterSingleObject('OpBitwiseAnd',           gdtOperatorDef,    nil,                '&|[3]');
  RegisterSingleObject('OpBitwiseOr',            gdtOperatorDef,    nil,                '{rule}|[5]');
  RegisterSingleObject('OpBitwiseXor',           gdtOperatorDef,    nil,                '^|[3]');
  RegisterSingleObject('OpBracketClose',         gdtOperatorDef,    nil,                ')|[1]');
  RegisterSingleObject('OpBracketOpen',          gdtOperatorDef,    nil,                '(|[1]');
  RegisterSingleObject('OpcodeSquareRule',       gdtNOM,            nil,                'Optional');
  RegisterSingleObject('OpCompEqual',            gdtOperatorDef,    nil,                '==|[6]');
  RegisterSingleObject('OpCompGreater',          gdtOperatorDef,    nil,                '>|[6]');
  RegisterSingleObject('OpCompGreaterEqual',     gdtOperatorDef,    nil,                '>=|[6]');
  RegisterSingleObject('OpCompLess',             gdtOperatorDef,    nil,                '<|[6]');
  RegisterSingleObject('OpCompLessEqual',        gdtOperatorDef,    nil,                '<=|[6]');
  RegisterSingleObject('OpCompUnequal',          gdtOperatorDef,    nil,                '<>|!=|[6]');
  RegisterSingleObject('OpLogicalAnd',           gdtOperatorDef,    nil,                '&&|AND|[7]');
  RegisterSingleObject('OpLogicalOr',            gdtOperatorDef,    nil,                '{rule}{rule}|OR|[8]');
  RegisterSingleObject('OpLogicalXor',           gdtOperatorDef,    nil,                '^^|XOR|[7]');
  RegisterSingleObject('OpUnaryMinus',           gdtOperatorDef,    nil,                '-|[2]');
  RegisterSingleObject('OpUnaryNot',             gdtOperatorDef,    nil,                '!|NOT|[9]');
  RegisterSingleObject('OpUnaryPlus',            gdtOperatorDef,    nil,                '+|[2]');
  RegisterSingleObject('OpUnaryResult',          gdtOperatorDef,    nil,                '');
  RegisterSingleObject('ParserInterfldAllowed',  gdtCharSet,        nil,                '[]');
  RegisterSingleObject('ParserInterfldMandated', gdtCharSet,        nil,                '[\t ]');
  RegisterSingleObject('ParserInteropAllowed',   gdtCharSet,        nil,                '[\t ]');
  RegisterSingleObject('ParserInteropMandated',  gdtCharSet,        nil,                '[,]');
  RegisterSingleObject('StringTerminator',       gdtCharSet,        nil,                '[''"]');
  RegisterSingleObject('Title',                  gdtText,           nil,                'Default XA80 Grammar');
  RegisterSingleObject('TokeniserTabSize',       gdtU16,            nil,                '4');
  }
end;

procedure TGrammar.RegisterSingleObject(const _title: string; _datatype: TGrammarDataType; _validator: TGrammarObjValidator; const _default: string);
var obj: TGrammarObj;
    _msg: string;
begin
  obj := TGrammarObj.Create;
  try
  obj.Title     := _title;
  obj.DataType  := _datatype;
  obj.Default   := _default;
  obj.Validator := _validator;
  // If TStringList then create the list
  if obj.DataType in [gdtStringList,gdtFuncDef,gdtOperatorDef,gdtCharList] then
    begin
      obj.strlistVar := TStringList.Create;
      obj.strlistVar.Delimiter := #13;
      obj.strlistVar.AlwaysQuote := False;
      obj.strlistVar.QuoteChar := #0;
    end;
  // Set the default values
  if not obj.SetVal(_default,_msg) then
    raise Exception.Create(Format('Unable to set default value "%s" in object "%s". %s',[_default,_title,_msg]));
  {
  case _datatype of
    gdtString:
      obj.strVar := _default;
    gdtStringList,
    gdtFuncDef,
    gdtOperatorDef,
    gdtCharList:
      obj.strlistVar.DelimitedText := _default;
    gdtU16:
      obj.wordVar := StrToInt(_default);
    gdtNOM:
      case _default of
        'Never':     obj.nomVar := tnNever;
        'Optional':  obj.nomVar := tnOptional;
        'Mandatory': obj.nomVar := tnMandatory;
        otherwise
          raise Exception.Create('Internal error, default value of ' + _default + ' not catered for in TGrammar.RegisterSingleObject');
      end;
    gdtCharSet:
      ; // Needs to be coded
    gdtBoolean:
      case _default of
        'True':  obj.boolVar := True;
        'False': obj.boolVar := False;
      end;
    otherwise
      raise Exception.Create(Format('Internal error, data type %s not catered for in TGrammarObj.AsText',[GetEnumName(TypeInfo(TGrammarDataType),Ord(_datatype))]));
  end;
  }
  except
    FreeAndNil(obj);
  end;
  // Finally, add the object to the list
  if Assigned(obj) then
    FGrammarList.Add(obj);
end;

end.

