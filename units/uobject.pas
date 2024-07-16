unit uobject;

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

//
// Deal with object (.obj80) files
//
// Format is:
//
//   Header
//   Global definitions
//   Local definitions
//   Segment 1 (includes
//   Segment 2
//     :  :  :
//   Segment N
//
// Duncan Munro 16-Jan-2024
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ucodesegment, usymboltable, fpjson;

type
  TObjectFile = class(TObject)
    private
      FDebugList:       TDebugList;
      FDestroyElements: boolean;
      FFilename:        string;
      FFixupList:       TFixupList;
      FSegments:        TSegments;
      FSymbolTable:     TSymbolTable;
      jData:            TJSONdata;
      procedure CreateJSONfromParams;
      procedure CreateParamsFromJSON;
    public
      property DebugList:   TDebugList read FDebugList;
      property Filename:    string     read FFilename;
      property FixupList:   TFixupList read FFixupList;
      property Segments:    TSegments  read FSegments;
      property SymbolTable: TSymbolTable read FSymbolTable;
      constructor Create;
      constructor Create(const _filename: string);
      constructor Create(const _filename: string; _symboltable: TSymbolTable; _segmentlist: TSegments; _fixups: TFixupList; _debuglist: TDebugList);
      destructor Destroy; override;
      procedure Clear;
      procedure Load;
      procedure Load(_stream: TStream);
      procedure Load(const _filename: string);
      procedure Save;
      procedure Save(_stream: TStream);
  end;

implementation

uses
  uutility, uenvironment, jsonparser, uasmglobals;


constructor TObjectFile.Create;
begin
  inherited Create;
  FDestroyElements := True;
  FDebugList := TDebugList.Create;
  FSymbolTable := TSymbolTable.Create;
  FSegments := TSegments.Create;
  FFixupList := TFixupList.Create;
end;

constructor TObjectFile.Create(const _filename: string);
begin
  inherited Create;
  Create;
  FFilename := _filename;
  Load;
end;

constructor TObjectFile.Create(const _filename: string; _symboltable: TSymbolTable; _segmentlist: TSegments; _fixups: TFixupList; _debuglist: TDebugList);
begin
  inherited Create;
  FDestroyElements := False;
  FFilename    := _filename;
  FDebugList   := _debuglist;
  FSymbolTable := _symboltable;
  FSegments    := _segmentlist;
  FFixupList   := _fixups;
//  CreateJSONfromParams;
end;

destructor TObjectFile.Destroy;
begin
  if FDestroyElements then
    begin
      FreeAndNil(FFixupList);
      FreeAndNil(FSegments);
      FreeAndNil(FSymbolTable);
      FreeAndNil(FDebugList);
    end;
  if Assigned(jData) then
    FreeAndNil(jData);
  // Finally...
  inherited Destroy;
end;

procedure TObjectFile.Clear;
begin
  if Assigned(jData) then
    FreeAndNil(jData); // Clear the JSON if it already exists
  FDebugList.Clear;
  FSymbolTable.Clear;
  FSegments.Clear;
  FFixupList.Clear;
end;

procedure TObjectFile.CreateJSONfromParams;
var jObject: TJSONObject;
    jSub:    TJSONObject;
    jArray:  TJSONArray;
    i:       integer;
begin
  if Assigned(jData) then
    FreeAndNil(jData); // Clear the JSON if it already exists
  jData := GetJSON('{"Header":{},"Globals":{},"Locals":{},"DebugFilenames":[],"Segments":{}}');
  // Do header items
  EnvObject.ToJSONobject(jData,FFilename);
  // Do Globals and Locals
  FSymbolTable.ToJSONobject(jData,ssGlobal);
  FSymbolTable.ToJSONobject(jData,ssLocal);
  // Do debug filenames
  FDebugList.ToJSONobject(jData);
  // Do segments
  FSegments.ToJSONobject(jData,FFixupList,FDebugList);
end;

procedure TObjectFile.CreateParamsFromJSON;
begin
  // Do segments, fixup list and debug list first of all
  FSegments.FromJSONobject(jData,FFixupList,FDebugList);
end;

procedure TObjectFile.Load;
var s: string;
    filelen: int64;
    fstream: TFileStream;
begin
  if not FileExists(FFilename) then
    raise Exception.Create('Cannot find object file ' + FFilename);
  fstream := TFileStream.Create(FFilename,fmOpenRead);
  try
    Load(fstream);
  finally
    FreeAndNil(fstream);
  end;
end;

procedure TObjectFile.Load(_stream: TStream);
var s: string;
    filelen: int64;

begin
  filelen := _stream.Size;
  if filelen > MAX_OBJECT_SIZE then
    raise Exception.Create(Format('Attempt to load object file exceeding %d bytes',[MAX_OBJECT_SIZE]));
  SetLength(s,filelen);
  _stream.Read(s[1],filelen);
  // @@@@@
  // Process the string into the bits and pieces here
  // @@@@@
  if Assigned(jData) then
    FreeAndNil(jData); // Clear the JSON if it already exists
  jData := GetJSON(s);
  CreateParamsFromJSON;
end;

procedure TObjectFile.Load(const _filename: string);
begin
  FFilename := _filename;
  Load;
end;

procedure TObjectFile.Save;
var fstream: TFileStream;
begin
  if FFilename = '' then
    raise Exception.Create('Attempting to save object file with no filename');
  fstream := TFileStream.Create(FFilename,fmCreate);
  try
    Save(fstream);
  finally
    FreeAndNil(fstream);
  end;
end;

procedure TObjectFile.Save(_stream: TStream);
var s: string;
begin
  CreateJSONfromParams;
  s := jData.FormatJSON;
  if Length(s) > MAX_OBJECT_SIZE then
    raise Exception.Create(Format('Attempt to save object file exceeding %d bytes',[MAX_OBJECT_SIZE]));
  _stream.Write(s[1],Length(s));
end;

end.

