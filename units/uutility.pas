unit uutility;

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
  Classes, SysUtils, CustApp;

procedure AugmentIncludes(s: string; list: TStringList);
procedure CmdOptionToList(app: TCustomApplication; shortopt: char; longopt: string; list: TStringList; delim: boolean = False);
function ExpandTabs(const _s: string; tabsize: integer): string;
function IsPrime(_value: integer): boolean;
function NextPrime(_value: integer): integer;

implementation

procedure AugmentIncludes(s: string; list: TStringList);
begin
  {%H-}s := IncludeTrailingPathDelimiter(s);
  if list.IndexOf(s) < 0 then
    list.Insert(0,s);
end;

procedure CmdOptionToList(app: TCustomApplication; shortopt: char; longopt: string; list: TStringList; delim: boolean = False);
var i: integer;
begin
  if app.HasOption(shortopt,longopt) then
    begin
      list.Delimiter := ';';
      list.DelimitedText := app.GetOptionValue(shortopt,longopt);
      if delim then for i := 0 to list.Count-1 do
        list[i] := IncludeTrailingPathDelimiter(ExpandFilename(list[i]));
    end;
end;

function ExpandTabs(const _s: string; tabsize: integer): string;
var i: integer;
    amt: integer;
begin
  Result := '';
  for i := 1 to Length(_s) do
    begin
      if _s[i] <> #9 then
        Result := Result + _s[i]
      else
        begin
          amt := Length(Result) mod tabsize;
          amt := tabsize - amt;
          Result := Result + Space(amt);
        end;
    end;
end;

function IsPrime(_value: integer): boolean;
var divisor: integer;
begin
  divisor := 3;
  Result := True;
  if (_value > 3) and ((_value mod 2) = 0) then
    Exit(False);  // Even numbers > 2 aren't prime
  while divisor*divisor < _value do
    if (_value mod divisor) = 0 then
      Exit(False)
    else
      divisor := divisor + 2;
end;

function NextPrime(_value: integer): integer;
begin
  if (_value mod 2) = 0 then
    Inc(_value);
  while not IsPrime(_value) do
    _value := _value + 2;
  Result := _value;
end;

end.

