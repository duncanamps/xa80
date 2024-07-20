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
  TObjectHeader = class(TObject)
    private
      FFileName:       string;
      FFileType:       string;
      FFileCreated:    TDateTime;
      FHostOS:         string;
      FHostAppName:    string;
      FHostAppVersion: string;
    public
      constructor Create;
      procedure FromJSONobject(_parent: TJSONdata);
      procedure ToJSONobject(_parent: TJSONdata);
  end;

  TObjectFile = class(TObject)
    private
      FDebugList:       TDebugList;
      FDestroyElements: boolean;
//    FFilename:        string;
      FFixupList:       TFixupList;
      FHeader:          TObjectHeader;
      FSegments:        TSegments;
      FSymbolTable:     TSymbolTable;
      jData:            TJSONdata;
      procedure CreateJSONfromParams;
      procedure CreateParamsFromJSON;
      function  GetFilename: string;
      procedure SetFilename(const _fn: string);
    public
      property DebugList:   TDebugList read FDebugList;
      property FileName:    string     read GetFilename write SetFilename;
      property FixupList:   TFixupList read FFixupList;
      property Segments:    TSegments  read FSegments;
      property SymbolTable: TSymbolTable read FSymbolTable;
      constructor Create;
      constructor Create(const _filename: string);
      constructor Create(const _filename: string; _symboltable: TSymbolTable; _segmentlist: TSegments; _fixups: TFixupList; _debuglist: TDebugList);
      destructor Destroy; override;
      function  AsText: string;
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


//------------------------------------------------------------------------------
//
//  TObjectHeader code
//
//------------------------------------------------------------------------------

constructor TObjectHeader.Create;
begin
  inherited Create;
  FFileCreated := Now;
end;

procedure TObjectHeader.FromJSONobject(_parent: TJSONdata);
var jObject:  TJSONobject;
    fmt:      TFormatSettings;
    dtstring: string;
begin
  fmt.ShortDateFormat := CONST_JSON_HEADER_DATE_FORMAT;
  fmt.LongTimeFormat  := CONST_JSON_HEADER_TIME_FORMAT;
  fmt.DateSeparator   := CONST_JSON_HEADER_DATESEP_FORMAT;
  fmt.TimeSeparator   := CONST_JSON_HEADER_TIMESEP_FORMAT;
  jObject := _parent.FindPath(CONST_JSON_HEADER_TITLE) as TJSONobject;
  if Assigned(jObject) then
    begin
      FFileType       := jObject.Get(CONST_JSON_HEADER_FILETYPE);
      dtstring        := jObject.Get(CONST_JSON_HEADER_FILECREATED);
      FFileCreated    := StrToDateTime(dtstring,fmt);
      FHostOS         := jObject.Get(CONST_JSON_HEADER_HOSTOS);
      FHostAppName    := jObject.Get(CONST_JSON_HEADER_HOSTAPPNAME);
      FHostAppVersion := jObject.Get(CONST_JSON_HEADER_HOSTAPPVERSION);
    end;
end;

procedure TObjectHeader.ToJSONobject(_parent: TJSONdata);
var jObject: TJSONobject;
    tmp:     TJSONdata;
    jArray:  TJSONarray;
begin
  jObject := _parent.FindPath(CONST_JSON_HEADER_TITLE) as TJSONobject;
  if Assigned(jObject) then
    begin
      jObject.Clear;
      jObject.Add(CONST_JSON_HEADER_FILENAME,       FFileName);
      jObject.Add(CONST_JSON_HEADER_FILETYPE,       'xa80 Object File V1');
      jObject.Add(CONST_JSON_HEADER_FILECREATED,    FormatDateTime(CONST_JSON_HEADER_DATETIME_FORMAT,FFileCreated));
      jObject.Add(CONST_JSON_HEADER_HOSTOS,         {$I %FPCTARGETOS%});
      jObject.Add(CONST_JSON_HEADER_HOSTAPPNAME,    'xa80');
      jObject.Add(CONST_JSON_HEADER_HOSTAPPVERSION, 'V' + EnvObject.Version + ' build ' + EnvObject.Build);
    end;
end;



//------------------------------------------------------------------------------
//
//  TObjectFile code
//
//------------------------------------------------------------------------------

constructor TObjectFile.Create;
begin
  inherited Create;
  FDestroyElements := True;
  FHeader := TObjectHeader.Create;
  FDebugList := TDebugList.Create;
  FSymbolTable := TSymbolTable.Create;
  FSegments := TSegments.Create;
  FFixupList := TFixupList.Create;
end;

constructor TObjectFile.Create(const _filename: string);
begin
  inherited Create;
  Create;
  FHeader.FFileName := _filename;
  Load;
end;

constructor TObjectFile.Create(const _filename: string; _symboltable: TSymbolTable; _segmentlist: TSegments; _fixups: TFixupList; _debuglist: TDebugList);
begin
  inherited Create;
  FDestroyElements := False;
  FHeader := TObjectHeader.Create;
  FHeader.FFileName := _filename;
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
  FreeAndNil(FHeader);
  inherited Destroy;
end;

function TObjectFile.AsText: string;
begin
  CreateJSONfromParams;
  AsText := jData.FormatJSON;
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
  FHeader.ToJSONobject(jData);
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
  // Read the header first
  FHeader.FromJSONobject(jData);
  // Do segments, fixup list and debug list first of all
  FSegments.FromJSONobject(jData,FFixupList,FDebugList);
  // Do GLobals and Locals
  // Do debug filenames

end;

function TObjectFile.GetFilename: string;
begin
  GetFilename := '';
  if Assigned(FHeader) then
    GetFilename := FHeader.FFileName;
end;

procedure TObjectFile.Load;
var s: string;
    filelen: int64;
    fstream: TFileStream;
begin
  Clear; // Wipe it first
  if not FileExists(FileName) then
    raise Exception.Create('Cannot find object file ' + FileName);
  fstream := TFileStream.Create(FileName,fmOpenRead);
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
  if Assigned(jData) then
    FreeAndNil(jData); // Clear the JSON if it already exists
  jData := GetJSON(s);
  CreateParamsFromJSON;
end;

procedure TObjectFile.Load(const _filename: string);
begin
  FHeader.FFileName := _filename;
  Load;
end;

procedure TObjectFile.Save;
var fstream: TFileStream;
begin
  if FileName = '' then
    raise Exception.Create('Attempting to save object file with no filename');
  fstream := TFileStream.Create(FileName,fmCreate);
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

procedure TObjectFile.SetFilename(const _fn: string);
begin
  FHeader.FFilename := _fn;
end;

end.

