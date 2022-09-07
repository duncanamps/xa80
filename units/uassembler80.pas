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

//
// Main assembler unit. This handles all the work of assembling a single file
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, umonitor;

type
  TAssembler80 = class(TObject)
    private
      FCurrentFile: string;
      FInputLine:   integer;
      FInputCol:    integer;
      FPass:        integer;
    public
      procedure Assemble(const filename: string);
      procedure Monitor(_mt: TMonitorType; const _msg: string);
      procedure Monitor(_mt: TMonitorType; const _fmt: string; const _args: array of const);
      property CurrentFile: string  read FCurrentFile;
      property InputLine:   integer read FInputLine;
      property InputCol:    integer read FInputCol;
      property Pass:        integer read FPass;
  end;

var
  Asm80: TAssembler80;

implementation

procedure TAssembler80.Assemble(const filename: string);
begin
  umonitor.Monitor(mtInfo,'Assembling %s',[filename]);
end;

procedure TAssembler80.Monitor(_mt: TMonitorType; const _msg: string);
begin
  umonitor.Monitor(Pass,CurrentFile,InputLine,InputCol,_mt,_msg);
end;

procedure TAssembler80.Monitor(_mt: TMonitorType; const _fmt: string; const _args: array of const);
begin
  Monitor(_mt,Format(_fmt,_args));
end;

end.

