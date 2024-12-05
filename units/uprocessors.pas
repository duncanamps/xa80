unit uprocessors;

{$mode ObjFPC}{$H+}

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2024 Duncan Munro

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
  Classes, SysUtils, Generics.Collections;

type
  TProcessor = record
    ProcessorName: string;
    ProcessorDescription: string;
  end;

  TProcessorList = class(specialize TList<TProcessor>)
    public
      procedure Add(const _procname, _procdesc: string);
  end;

var
  ProcessorList: TProcessorList;

implementation

procedure TProcessorList.Add(const _procname, _procdesc: string);
var newrec: TProcessor;
begin
  newrec.ProcessorName        := _procname;
  newrec.ProcessorDescription := _procdesc;
  inherited Add(newrec);
end;

initialization
  ProcessorList := TProcessorList.Create;
  ProcessorList.Add('8080', 'Intel 8080 processor');
  ProcessorList.Add('8085', 'Intel 8085 processor');
  ProcessorList.Add('Z80',  'Zilog Z80 processor and compatibles');
  ProcessorList.Add('Z80X', 'Zilog Z80 processor and compatibles (+undocumented instructions)');
  ProcessorList.Add('Z180', 'Zilog Z180 processor');

finalization
  FreeAndNil(ProcessorList);

end.

