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

unit ucommand;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Generics.Collections, upreparser3_defs, usymboltable;

type
  TOperand = class(TObject)

  end;

  TOperandList = class(specialize TObjectList<TOperand>)

  end;

  TCommandExec = procedure (const _label: string; _operandlist: TPreparserBase)  of object;

  TCommandObj = class(TObject)
    public
      CommandExec: TCommandExec;
      CommandName: string;
  end;

  TCommandList = class(specialize TObjectList<TCommandObj>)
    private
      FSymbolTable: TSymbolTable;

    public
      constructor Create;
      procedure PopulateStringList(_sl: TStringList);
      function RegisterCommand(const _cmdname: string; _cmdexec: TCommandExec): integer;
      property SymbolTable: TSymbolTable read FSymbolTable write FSymbolTable;
  end;


implementation

uses
  umessages, lacogen_types;

constructor TCommandList.Create;
begin
  inherited Create;
end;


procedure TCommandList.PopulateStringList(_sl: TStringList);
var obj: TCommandObj;
begin
  _sl.Clear;
  for obj in Self do
    _sl.Add(obj.CommandName);
end;

function TCommandList.RegisterCommand(const _cmdname: string; _cmdexec: TCommandExec): integer;
var obj: TCommandObj;
begin
  obj := TCommandObj.Create;
  obj.CommandName := _cmdname;
  obj.CommandExec := _cmdexec;
  Result := Count;
  Add(obj);
end;

end.

