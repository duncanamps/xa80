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

  //  Command flags:
  //
  //    cfNoPlaceholder used for commands which shouldn't allow a placeholder
  //                    to be created. e.g. MACRO as these are params, not
  //                    labels; and IF/WHILE/REPEAT as the contents should be
  //                    known on pass 1 anyway
  //    cfBypass        During conditional assembly, the cfBypass flag allows
  //                    a "non processing" situation to be overridden - we need
  //                    to know when an ELSE or ENDIF appears
  //    cfLabel         Specifies if a label is mandatory for this command,
  //                    otherwise it's completely unwelcome
  //    cfDuringMD      Execute command during a Macro Definition, everything
  //                    else is blocked from being run in that phase

  TCommandFlag = (cfNoPlaceholder,cfEQU,cfBypass,cfLabel,cfDuringMD);

  TCommandFlags = set of TCommandFlag;

  TCommandExec = procedure (const _label: string; _operandlist: TPreparserBase)  of object;

  TCommandObj = class(TObject)
    public
      CommandName:  string;
      CommandFlags: TCommandFlags;
      CommandExec:  TCommandExec;
  end;

  TCommandList = class(specialize TObjectList<TCommandObj>)
    private
      FSymbolTable: TSymbolTable;

    public
      constructor Create;
      procedure PopulateStringList(_sl: TStringList);
      function RegisterCommand(const _cmdname: string; _cmdflags: TCommandFlags; _cmdexec: TCommandExec): integer;
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

function TCommandList.RegisterCommand(const _cmdname: string; _cmdflags: TCommandFlags; _cmdexec: TCommandExec): integer;
var obj: TCommandObj;
begin
  obj := TCommandObj.Create;
  obj.CommandName  := _cmdname;
  obj.CommandFlags := _cmdflags;
  obj.CommandExec  := _cmdexec;
  Result := Count;
  Add(obj);
end;

end.

