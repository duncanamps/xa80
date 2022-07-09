unit uifstack;

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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections, deployment_parser_module_12,
  deployment_parser_types_12;

type

TIfStackEntry = record
  Index:     integer;
  Succeeded: boolean;
  ElseMode:  boolean;
  class operator = (ise1, ise2: TIfStackEntry): boolean;
end;

TIfStack = class(specialize TList<TIfStackEntry>)
  private
    FParser:    TLCGParser;
    function  GetAllowed: boolean;
    function  GetTop: TIfStackEntry;
  public
    constructor Create(_parser: TLCGParser);
    procedure ElseSwap;
    procedure Pop;
    procedure Push(_succeeded: boolean);
    property Allowed: boolean read GetAllowed;
    property Top:     TIfStackEntry  read GetTop;
end;


implementation

{ TIfStackEntry }

class operator TIfStackEntry.= (ise1, ise2: TIfStackEntry): boolean;
begin
  Result := (ise1.Index = ise2.Index);
end;

{ TIfStack }

constructor TIfStack.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
end;

function TIfStack.GetAllowed: boolean;
var i:       integer;
begin
  if Count = 0 then
    Exit(True); // Not in any kind of If construct
  Result := True;
  for i := 0 to Count-1 do
    if Items[i].Succeeded = Items[i].ElseMode then
      Result := False;
end;

procedure TIfStack.ElseSwap;
var entry: TIfStackEntry;
begin
  if Count = 0 then
    FParser.Monitor(ltError,'.ELSE without corresponding .IF / .IFDEF / .IFNDEF');
  entry := Top;
  if entry.ElseMode then
    FParser.Monitor(ltError,'.ELSE has already been specified for this block');
  entry.ElseMode := True;
  Items[Count-1] := entry;
end;

function TIfStack.GetTop: TIfStackEntry;
begin
  if Count = 0 then
    FParser.Monitor(ltInternal,'Attempting to take the top of an empty if stack');
  Result := Items[Count-1];
end;

procedure TIfStack.Pop;
begin
  if Count = 0 then
    FParser.Monitor(ltError,'.ENDIF without corresponding .IF / .IFDEF / .IFNDEF');
  Delete(Count-1);
end;

procedure TIfStack.Push(_succeeded: boolean);
var entry: TIfStackEntry;
begin
  entry.Index     := Count;
  entry.Succeeded := _succeeded;
  entry.ElseMode  := False;
  Add(entry);
end;

end.

