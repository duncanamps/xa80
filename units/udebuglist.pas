unit udebuglist;

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


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TDebugEntry = class(TObject)
    private
      FAddress:   uint16;
      FCodeLen:   uint16;
      FCodeObj:   array of byte;
      FStringLen: uint16;
      FStringObj: string;
    public
      constructor Create(_addr: uint16; _codesize: uint16; _code: array of Byte; _str: string);
      procedure WriteStream(Strm: TStream);
  end;

  TDebugList = class(specialize TObjectList<TDebugEntry>)
    public
      constructor Create;
      destructor Destroy; override;
      procedure SaveToFile(const Filename: string);
      procedure SaveToStream(Strm: TStream);
  end;

implementation


{ TDebugEntry }

constructor TDebugEntry.Create(_addr: uint16; _codesize: uint16; _code: array of Byte; _str: string);
var i: integer;
begin
  inherited Create;
  FAddress   := _addr;
  FCodeLen   := _codesize;
  if FCodeLen > 0 then
    begin
      SetLength(FCodeObj,_codesize);
      for i := 0 to _codesize-1 do
        FCodeObj[i] := _code[i];
    end;
  FStringLen := Length(_str);
  FStringObj := _str;
end;

procedure TDebugEntry.WriteStream(Strm: TStream);
begin
  Strm.Write(FAddress,Sizeof(FAddress));
  Strm.Write(FCodeLen,SizeOf(FCodeLen));
  if FCodeLen > 0 then
    Strm.Write(FCodeObj[0],FCodeLen);
  Strm.Write(FStringLen,SizeOf(FStringLen));
  Strm.Write(FStringObj[1],FStringLen);
end;


{ TDebugList }

constructor TDebugList.Create;
begin
  inherited Create;
end;

destructor TDebugList.Destroy;
begin
  inherited Destroy;
end;

procedure TDebugList.SaveToFile(const Filename: string);
var strm: TFileStream;
begin
  strm := TFileStream.Create(Filename,fmCreate);
  try
    SaveToStream(strm);
  finally
    strm.Free;
  end;
end;

procedure TDebugList.SaveToStream(Strm: TStream);
var i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].WriteStream(Strm);
end;

end.

