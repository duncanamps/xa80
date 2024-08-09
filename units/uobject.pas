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
      procedure FromJSONobject(_parent: TJSONdata; _extractfilename: boolean = False);
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
      procedure CreateParamsFromJSON(_extractfilename: boolean = False);
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
      constructor Create(_obj: TJSONobject);
      destructor Destroy; override;
      function  AsText: string;
      procedure Clear;
      procedure Load;
      procedure Load(_stream: TStream);
      procedure Load(const _filename: string);
      procedure Load(_obj: TJSONobject);
      procedure Save;
      procedure Save(_stream: TStream);
  end;

implementation

uses
  uutility, uenvironment, jsonparser, uasmglobals, lacogen_types, umessages,
  ujsonsupport;


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

procedure TObjectHeader.FromJSONobject(_parent: TJSONdata; _extractfilename: boolean);
var jObject:  TJSONobject;
    fmt:      TFormatSettings;
    dtstring: string;
begin
  fmt.ShortDateFormat := CONST_JSON_HEADER_DATE_FORMAT;
  fmt.LongTimeFormat  := CONST_JSON_HEADER_TIME_FORMAT;
  fmt.DateSeparator   := CONST_JSON_HEADER_DATESEP_FORMAT;
  fmt.TimeSeparator   := CONST_JSON_HEADER_TIMESEP_FORMAT;
  jObject := _parent.FindPath(CONST_JSON_HEADER_TITLE) as TJSONobject;
  if not Assigned(jObject) then
    ErrorObj.Show(ltError,E2077_OBJECT_NO_HEADER);
  FFilename       := jObject.Get(CONST_JSON_HEADER_FILENAME);
  FFileType       := jObject.Get(CONST_JSON_HEADER_FILETYPE);
  dtstring        := jObject.Get(CONST_JSON_HEADER_FILECREATED);
  FFileCreated    := StrToDateTime(dtstring,fmt);
  FHostOS         := jObject.Get(CONST_JSON_HEADER_HOSTOS);
  FHostAppName    := jObject.Get(CONST_JSON_HEADER_HOSTAPPNAME);
  FHostAppVersion := jObject.Get(CONST_JSON_HEADER_HOSTAPPVERSION);
end;

procedure TObjectHeader.ToJSONobject(_parent: TJSONdata);
var jObject: TJSONobject;
    tmp:     TJSONdata;
    jArray:  TJSONarray;
begin
  jObject := FindOrMakeJSON(_parent,CONST_JSON_HEADER_TITLE) as TJSONobject;
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

// Create a blank object file with no filename and set up skeleton contents
// The object file owns its contents and will destroy them automatically

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

// Create an object file from a file. Specify a filename and the object file
// will be loaded from disk. The object file owns its contents and will
// destroy them automatically

constructor TObjectFile.Create(const _filename: string);
begin
  inherited Create;
  Create;
  FHeader.FFileName := _filename;
  Load;
end;

// Create an object file from symbols, segments, etc. A filename is specified
// but the object file is not automatically written out to disk. Call Save() to
// do this after creation. The object file does not own its contents as these
// would have been created elsewhere

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

// Create an object file from a JSONobject. The filename is extracted from the
// JSON content. The object file owns its contents and will destroy them
// automatically

constructor TObjectFile.Create(_obj: TJSONobject);
begin
  inherited Create;
  Create;
  Load(_obj);
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
//if Assigned(jData) then
//  FreeAndNil(jData); // Clear the JSON if it already exists
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
//  jData := GetJSON('{"' + CONST_JSON_HEADER_TITLE + '":{},"' + CONST_JSON_SYMBOLS_TITLE + '":{},"' + CONST_JSON_DEBUGNAMES_TITLE + '":[],"' + CONST_JSON_SEGMENTS_TITLE + '":{}}');
  jData := GetJSON('{}');
  // Do header items
  FHeader.ToJSONobject(jData);
  // Do Globals and Locals
  FSymbolTable.ToJSONobject(jData);
  // Do debug filenames
  FDebugList.ToJSONobject(jData);
  // Do segments
  FSegments.ToJSONobject(jData,FFixupList,FDebugList);
end;

procedure TObjectFile.CreateParamsFromJSON(_extractfilename: boolean);
begin
  Clear;
  // Read the header first
  FHeader.FromJSONobject(jData,_extractfilename);
  // Do segments, fixup list and debug list first of all
  FSegments.FromJSONobject(jData,FFixupList,FDebugList,Filename);
  // Do GLobals and Locals
  FSymbolTable.FromJSONobject(jData,FSegments);
  // Do debug filenames
  FDebugList.FromJSONobject(jData);
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
    ErrorObj.Show(ltError,E2078_OBJECT_NOT_FOUND,[Filename]);
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
    ErrorObj.Show(ltError,E2079_OBJECT_TOO_LARGE_LOAD,[filelen,MAX_OBJECT_SIZE]);
  try
    SetLength(s,filelen);
    _stream.Read(s[1],filelen);
    if Assigned(jData) then
      FreeAndNil(jData); // Clear the JSON if it already exists
    jData := GetJSON(s);
    CreateParamsFromJSON;
  except
    on E : LCGErrorException do ; // Nothing
    on E : LCGInternalException do ; // Nothing
    on E : Exception do ErrorObj.Show(ltError,E2076_OBJECT_LOAD_ERROR,[E.Message]);
  end;
end;

procedure TObjectFile.Load(const _filename: string);
begin
  FHeader.FFileName := _filename;
  Load;
end;

procedure TObjectFile.Load(_obj: TJSONobject);
begin
  jData := _obj;
  CreateParamsFromJSON(True); // Create object and get filename from JSON
end;

procedure TObjectFile.Save;
var fstream: TFileStream;
begin
  if FileName = '' then
    ErrorObj.Show(ltInternal,X3019_OBJECT_NO_FILENAME);
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
    ErrorObj.Show(ltError,E2080_OBJECT_TOO_LARGE_SAVE,[Length(s),MAX_OBJECT_SIZE]);
  _stream.Write(s[1],Length(s));
end;

procedure TObjectFile.SetFilename(const _fn: string);
begin
  FHeader.FFilename := _fn;
end;

end.

