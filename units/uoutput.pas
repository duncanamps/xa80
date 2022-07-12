unit uoutput;

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


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOutputSequence = array of byte;

  TOutput = class(TObject)
    private
      FBytes: array[UINT16] of byte;
      FUsed:  array[UINT16] of boolean;
      function Highest: integer;
      function Lowest: integer;
    public
      procedure Clear;
      procedure Write(const _array: TOutputSequence; _addr: UINT16; _len: integer);
      procedure SaveHex(const _filename: string);
      procedure SaveObject(const _filename: string);
  end;

implementation

{ TOutput }

procedure TOutput.Clear;
var i: UINT16;
begin
  for i in UINT16 do
    FUsed[i] := False;
  for i in UINT16 do
    FBytes[i] := $00;
end;

function TOutput.Highest: integer;
var i: integer;
begin
  for i := High(UINT16) downto Low(UINT16) do
    if FUsed[i] then
      break;
  Result := i;
end;

function TOutput.Lowest: integer;
var i: integer;
begin
  for i in UINT16 do
    if FUsed[i] then
      break;
  Result := i;
end;

{ Save as Intel Hex format }

procedure TOutput.SaveHex(const _filename: string);
var sl: TStringList;
    i:  integer;
    addr: integer;
    remaining: integer;
    column: integer;
    checksum: integer;
    s:      string;
begin
  if _filename = '' then
    Exit;
  sl := TStringList.Create;
  try
    column := 0;
    addr := Lowest;
    while addr < Highest do
      begin
        // Do a line
        s := ':';
        remaining := Highest + 1 - addr;
        if remaining > 16 then
          remaining := 16;
        s := s + Format('%2.2X%4.4X00',[remaining,addr]);
        checksum := remaining + (addr and $ff) + ((addr shr 8) and $ff);
        for i := 0 to remaining-1 do
          begin
            s := s + Format('%2.2X',[FBytes[addr+i]]);
            checksum := checksum + FBytes[addr+i];
          end;
        s := s + Format('%2.2X',[(-checksum) and $ff]);
        sl.Add(s);
        addr := addr + remaining;
      end;
    // Final line
    s := ':';
    remaining := 0;
    s := s + Format('%2.2X%4.4X01',[remaining,addr]);
    checksum := remaining + (addr and $ff) + ((addr shr 8) and $ff) + 1;
    s := s + Format('%2.2X',[(-checksum) and $ff]);
    sl.Add(s);
    sl.SaveToFile(_filename);
  finally
    sl.Free;
  end;
end;

procedure TOutput.SaveObject(const _filename: string);
var addr: UINT16;
    size: UINT16;
    strm: TFileStream;
begin
  if _filename = '' then
    Exit;
  size := Highest-Lowest+1;
  addr := Lowest;
  strm := TFileStream.Create(_filename,fmCreate,fmShareDenyWrite);
  try
    strm.Write(addr,sizeof(addr));
    strm.Write(size,sizeof(addr));
    strm.Write(FBytes[addr],size);
  finally
    strm.Free;
  end;
end;

procedure TOutput.Write(const _array: TOutputSequence; _addr: UINT16; _len: integer);
var i: integer;
    anew: integer;
begin
  for i := 1 to _len do
    if FUsed[_addr] then
      raise Exception.Create(Format('Attempt to write to an address %4.4X which is already written to',[_addr]))
    else
      begin
        FUsed[_addr] := True;
        FBytes[_addr] := _array[i-1];
        // Split out to a 32 bit value in case we are assembling right at
        // the end of memory ($FFFF)
        anew := _addr;
        Inc(anew);
        _addr := anew and $FFFF;
      end;
end;

end.

