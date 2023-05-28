unit uevaluator;

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

uses
  Classes, SysUtils, uasmglobals, Generics.Collections;

type
  TGrammarType = (gtUnassigned,gtXA80);

  TGrammarSet = set of TGrammarType;

  TTokenType = (ttSpecial,ttVariable,ttFunction,ttOperator);

  TTokenID = (FUNC_LOW,
              FUNC_HIGH,
              OP_NE,
              OP_LAND,
              OP_LXOR,
              OP_LOR,
              OP_SHL,
              OP_LE,
              OP_EQ,
              OP_GE,
              OP_SHR,
              OP_MINUS,
              OP_NOT,
              OP_BAND,
              OP_MULTIPLY,
              OP_DIVIDE,
              OP_BXOR,
              OP_BOR,
              OP_PLUS,
              OP_LT,
              OP_ASSIGN,
              OP_GT,
              OP_BRKO,
              OP_BRKC,
              VAL_VAR,
              VAL_NUMBER,
              VAL_STRING,
              TOK_EOF,
              TOK_ACCEPT,
              TOK_EXPR,
              TOK_ERROR);

  TTokenDefinition = class(TObject)
    protected
      FTokenID:     TTokenID;
      FTokenName:   string;
      FTokenPrec:   integer;
      FTokenText:   string;
      FTokenType:   TTokenType;
    public
      constructor Create(_tokid: TTokenID; _toktype: TTokenType; const _toktext: string; const _tokname: string; _tokprec: integer);
    published
      property TokenID:     TTokenID     read FTokenID;
      property TokenName:   string       read FTokenName;
      property TokenPrec:   integer      read FTokenPrec;
      property TokenText:   string       read FTokenText;
      property TokenType:   TTokenType   read FTokenType;
  end;

  TTokenDefinitionList = class(specialize TObjectList<TTokenDefinition>)
    protected
      FGrammarType: TGrammarType;
    public
      constructor Create;
      constructor Create(_grammartype: TGrammarType);
      procedure RegisterToken(_tokid: TTokenID; _toktype: TTokenType; const _toktext: string; const _tokname: string; _tokprec: integer; _grammarset: TGrammarSet);
  end;


implementation

//------------------------------------------------------------------------------
//
//  TTokenDefinition code
//
//------------------------------------------------------------------------------

constructor TTokenDefinition.Create(_tokid: TTokenID; _toktype: TTokenType; const _toktext: string; const _tokname: string; _tokprec: integer);
begin
  inherited Create;
  FTokenID    := _tokid;
  FTokenName  := _tokname;
  FTokenPrec  := _tokprec;
  FTokenText  := _toktext;
  FTokenType  := _toktype;
end;


//------------------------------------------------------------------------------
//
//  TTokenList code
//
//------------------------------------------------------------------------------

constructor TTokenDefinitionList.Create;
begin
  Create(gtUnassigned);
end;

constructor TTokenDefinitionList.Create(_grammartype: TGrammarType);
begin
  inherited Create;
  FGrammarType := _grammartype;
end;

procedure TTokenDefinitionList.RegisterToken(_tokid: TTokenID; _toktype: TTokenType; const _toktext: string; const _tokname: string; _tokprec: integer; _grammarset: TGrammarSet);
begin
  if FGrammarType = gtUnassigned then
    raise Exception.Create('Grammar type not defined in TTokenDefinitionList');
  if FGrammarType in _grammarset then
    Add(TTokenDefinition.Create(_tokid,_toktype,_toktext,_tokname,_tokprec));
end;

end.

