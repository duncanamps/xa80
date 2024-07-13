unit uxlib80;

{
    XLIB80 - Cross Assembler for x80 processors
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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uobject, generics.collections;

type
  TLib80 = class(specialize TObjectList<TObjectFile>)
    private
      FFilename: string;
    public
      constructor Create(const _filename: string);
      destructor Destroy; override;
      procedure AddFile(const _objfilename: string);
      procedure Load;
      procedure Save;
  end;

var
  Lib80: TLib80;

implementation

constructor TLib80.Create(const _filename: string);
begin
  inherited Create;
  FFilename := _filename;
  // Load from disk or create new and save
  if FileExists(FFilename) then
    Load
  else
    begin
      Clear;
      Save;
    end;
end;

destructor TLib80.Destroy;
begin
  inherited Destroy;
end;

procedure TLib80.AddFile(const _objfilename: string);
var obj: TObjectFile;
begin
  obj := TObjectFile.Create(_objfilename);
  Add(obj);
  Save;
end;

procedure TLib80.Load;
begin

end;

procedure TLib80.Save;
begin

end;

end.

