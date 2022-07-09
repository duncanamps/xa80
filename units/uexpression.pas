unit uexpression;

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

// Forward declarations

function StripQuotes(const _s: string): string;
function StripQuotesAndEscaped(const _s: string): string;
function VariableFromBinLiteral(s: string): string;
function VariableFromHexLiteral(s: string): string;
function VariableFromOctLiteral(s: string): string;

implementation

uses
  strutils;


function StripQuotes(const _s: string): string;
var l: integer;
begin
  Result := _s;
  l := Length(_s);
  if (l > 0) and (LeftStr(_s,1) = CHR(34)) then
    begin
      if l < 2 then
        raise Exception.Create('Trying to strip quotes from string which is too short ' + _s);
      if (LeftStr(_s,1) <> Chr(34)) or
         (RightStr(_s,1) <> Chr(34)) then
        raise Exception.Create('Trying to strip quotes which are not present ' + _s);
      Result := Copy(_s,2,Length(_s)-2);
    end;
end;

function StripQuotesAndEscaped(const _s: string): string;
begin
  Result := StripQuotes(_s);
  Result := StringReplace(Result,'\"','"',[rfReplaceAll]);
  Result := StringReplace(Result,'\\','\',[rfReplaceAll]);
  Result := StringReplace(Result,'\t',#9, [rfReplaceAll]);
  Result := StringReplace(Result,'\n',#10,[rfReplaceAll]);
  Result := StringReplace(Result,'\r',#13,[rfReplaceAll]);
end;

function VariableFromBinLiteral(s: string): string;
var decval: int64;
    i:      integer;
begin
  decval := 0;
  // Get rid of 0B prefix if present
  if (Length(s) > 2) and (UpperCase(LeftStr(s,2)) = '0B') then
    s := RightStr(s,Length(s)-2);
  // Get rid of % prefix if present
  if (Length(s) > 1) and (LeftStr(s,1) = '%') then
    s := RightStr(s,Length(s)-1);
  // Get rid of B suffix if present
  if (Length(s) > 1) and (UpperCase(RightStr(s,1)) = 'B') then
    s := LeftStr(s,Length(s)-1);
  if Length(s) < 1 then
    raise Exception.Create('BinLiteral is too short');
  for i := 1 to Length(s) do
    decval := decval * 2 + (Ord(s[i])-Ord('0'));
  Result := IntToStr(decval);
end;

function VariableFromHexLiteral(s: string): string;
var decval: int64;
begin
  // Check for #nnnn and change to $nnnn
  if (Length(s) > 0) and (s[1] = '#') then
    s[1] := '$';
  // Check for nnnnH and change to $nnnn
  if (Length(s) > 0) and (UpperCase(RightStr(s,1)) = 'H') then
    s := '$' + LeftStr(s,Length(s)-1);
  decval := StrToInt(s);
  Result := IntToStr(decval);
end;

function VariableFromOctLiteral(s: string): string;
var decval: int64;
    i:      integer;
begin
  // Get rid of the trailing O or Q if present
  if (Length(s) > 0) and
     ((UpperCase(RightStr(s,1)) = 'O') or
      (UpperCase(RightStr(s,1)) = 'Q')) then
    s := LeftStr(s,Length(s)-1);
  decval := 0;
  for i := 1 to Length(s) do
    decval := decval * 8 + (Ord(s[i])-Ord('0'));
  Result := IntToStr(decval);
end;

end.

