unit ufilestack;

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
  Classes, SysUtils, Generics.Collections, deployment_parser_module_12, deployment_parser_types_12;

type

  TFileStackEntry = class(TObject)
    public
      Filename:  string;
      List:      TStringList;
      InputLine: integer;
      Listing:   boolean;
      constructor Create;
      constructor Create(const _filename: string; _listing: boolean = False);
      destructor Destroy; override;
  end;

  TFileStack = class(specialize TObjectList<TFileStackEntry>)
    private
      FOnMonitor: TLCGMonitorProc;
      FParser:    TLCGParser;
      function  Exists(const _fn: string): boolean;
      function  GetEOF: boolean;
      function  GetFilename: string;
      function  GetInputLine: integer;
      function  GetIsListing: boolean;
      function  GetTop: TFileStackEntry;
    public
      constructor Create(_parser: TLCGParser);
      function  GetLine: string;
      procedure InsertLine(const s: string);
      procedure Pop;
      procedure Push(filename: string; _listing: boolean = True);
      procedure PushMacro(macroname: string; sl: TStringList; parms: TStringList);
      procedure Monitor(LogType: TLCGLogType; const Message: string);
      property EOF:       boolean          read GetEOF;
      property Filename:  string           read GetFilename;
      property InputLine: integer          read GetInputLine;
      property IsListing: boolean          read GetIsListing;
      property OnMonitor: TLCGMonitorProc  read FOnMonitor   write FOnMonitor;
      property Top:       TFileStackEntry  read GetTop;
  end;

// operator= (const fse1, fse2: TFileStackEntry): boolean;

implementation

uses
  uutility, uassembler, uasmglobals;

{ TFileStackEntry }

{
class operator TFileStackEntry.= (fse1, fse2: TFileStackEntry): boolean;
begin
  Result := (fse1.Filename = fse2.Filename);
end;
}

constructor TFileStackEntry.Create;
begin
  inherited Create;
  Filename  := '';
  InputLine := 0;
  Listing   := False;
  List := TStringList.Create;
end;

constructor TFileStackEntry.Create(const _filename: string; _listing: boolean);
begin
  Create;
  Filename := _filename;
  Listing := _listing;
  List.LoadFromFile(_filename);
end;

destructor TFileStackEntry.Destroy;
begin
  FreeAndNil(List);
  inherited Destroy;
end;

{ TFileStack }

constructor TFileStack.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
end;

function TFileStack.Exists(const _fn: string): boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
    if SameFilename(Items[i].Filename,_fn) then
      Exit(True);
end;

function TFileStack.GetEOF: boolean;
begin
  Result := (Top.InputLine >= Top.List.Count);
end;

function TFileStack.GetFilename: string;
begin
  Result := Top.Filename;
end;

function TFileStack.GetInputLine: integer;
begin
  Result := Top.InputLine;
end;

function TFileStack.GetIsListing: boolean;
begin
  Result := False;
  if Count > 0 then
    Result := Items[Count-1].Listing;
end;

function TFileStack.GetLine: string;
var rec: TFileStackEntry;
begin
  if EOF then
    Monitor(ltInternal,'Attempt to read past end of file on ' + Filename);
  Result := Top.List[InputLine];
  rec := Top;
  Inc(rec.InputLine);
 // Items[Count-1] := rec;
end;

procedure TFileStack.InsertLine(const s: string);
begin
  Top.List.Insert(InputLine,s);
end;

function TFileStack.GetTop: TFileStackEntry;
begin
  if Count = 0 then
    Monitor(ltInternal,'Attempting to take the top of an empty file stack');
  Result := Items[Count-1];
end;

procedure TFileStack.Monitor(LogType: TLCGLogType; const Message: string);
begin
  FParser.Monitor(LogType,Message);
end;

procedure TFileStack.Pop;
begin
  Monitor(ltDebug,'Closing file ' + Top.Filename);
  Delete(Count-1);
end;

procedure TFileStack.Push(filename: string; _listing: boolean);
var rec: TFileStackEntry;
    i:   integer;
    tabsize: integer;
begin
  {%H-}filename := ExpandFilename(filename);
  if Exists(filename) then
    Monitor(ltError,'Circular file reference in file ' + filename);
  Monitor(ltDebug,'Opening file ' + filename);
  tabsize := TAssembler(FParser).TabSize;
  rec := TFileStackEntry.Create(filename,_listing);
  if Count > 0 then
    if Items[Count-1].Listing = False then
      rec.Listing := False;;
  for i := 0 to rec.List.Count-1 do
    if Length(rec.List[i]) > MAX_LINE_LENGTH then
      FParser.Monitor(ltError,'Input line exceeds maximum line length of %s',[MAX_LINE_LENGTH])
    else
      rec.List[i] := ExpandTabs(rec.List[i],tabsize);
  rec.InputLine := 0;
  Add(rec);
end;

procedure TFileStack.PushMacro(macroname: string; sl: TStringList; parms: TStringList);
var rec: TFileStackEntry;
    i,j: integer;
begin
  {%H-}macroname := UpperCase(macroname);
  rec.Filename := 'MACRO '+ macroname;
  if Exists(rec.Filename) then
    Monitor(ltError,'Circular macro reference in macro ' + macroname);
  rec.List := TStringList.Create;
  rec.List.Assign(sl);
  // Delete last record (the .ENDM)
  rec.List.Delete(rec.List.Count-1);
  // Now go and change all the @n into parameters
  for i := 0 to rec.List.Count-1 do
    for j := 0 to parms.Count-1 do
      rec.List[i] := StringReplace(rec.List[i],'@'+IntToStr(j),parms[j],[rfReplaceAll]);
  rec.InputLine := 0;
  rec.Listing := True;
  if Count > 0 then
    if Items[Count-1].Listing = False then
      rec.Listing := False;;
  Add(rec);
end;

end.

