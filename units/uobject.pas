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
  Classes, SysUtils, ucodesegment, usymboltable, ufixups, fpjson;

type
  TObjectFile = class(TObject)
    private
      FFilename:    string;
      FFixupList:   TFixupList;
      FSegments:    TSegments;
      FSymbolTable: TSymbolTable;
      jData:        TJSONData;
      procedure CreateJSONfromParams;
    public
      constructor Create(const _filename: string; _symboltable: TSymbolTable; _segmentlist: TSegments; _fixups: TFixupList);
      destructor Destroy; override;
      procedure Save;
  end;

implementation

uses
  uutility, uenvironment, jsonparser;

constructor TObjectFile.Create(const _filename: string; _symboltable: TSymbolTable; _segmentlist: TSegments; _fixups: TFixupList);
begin
  inherited Create;
  FFilename    := _filename;
  FSymbolTable := _symboltable;
  FSegments    := _segmentlist;
  FFixupList   := _fixups;
  CreateJSONfromParams;
end;

destructor TObjectFile.Destroy;
begin
  FreeAndNil(jData);
  // Finally...
  inherited Destroy;
end;

procedure TObjectFile.CreateJSONfromParams;
var jObject: TJSONObject;
    jSub:    TJSONObject;
    i:       integer;
begin
  if Assigned(jData) then
    FreeAndNil(jData); // Clear the JSON if it already exists
  jData := GetJSON('{"Header":{},"Globals":{},"Locals":{},"Segments":{}}');
  // Do header items
  jObject := jData.FindPath('Header') as TJSONObject;
  if Assigned(jObject) then
    begin
      jObject.Add('FileName',FFilename);
      jObject.Add('FileType','xa80 Object File V1');
      jObject.Add('FileCreated',FormatDateTime('yyyy-mm-dd hh:nn:ss',Now));
      jObject.Add('HostOS',{$I %FPCTARGETOS%});
      jObject.Add('HostAppName','xa80');
      jObject.Add('HostAppVersion','V' + EnvObject.Version + ' build ' + EnvObject.Build);
    end;
  // Do Globals
  jObject := jData.FindPath('Globals') as TJSONObject;
  if Assigned(jObject) then
    begin
      for i := 0 to FSymbolTable.Count-1 do
        if FSymbolTable.Items[i].Scope = ssGlobal then
          with FSymbolTable.Items[i] do
            begin
              if Assigned(Seg) then
                jSub := GetJSON(Format('{"Segment":"%s","Offset":"%4.4X"}',[Seg.Segname,IValue])) as TJSONObject
              else
                jSub := GetJSON(Format('{"Segment":"%s","Offset":"%4.4X"}',['<nil>',IValue])) as TJSONObject;
              jObject.Add(Name,jSub);
            end;
    end;
  // Do Locals
  jObject := jData.FindPath('Locals') as TJSONObject;
  if Assigned(jObject) then
    begin
      for i := 0 to FSymbolTable.Count-1 do
        if FSymbolTable.Items[i].Scope = ssLocal then
          with FSymbolTable.Items[i] do
            begin
              if Assigned(Seg) then
                jSub := GetJSON(Format('{"Segment":"%s","Offset":"%4.4X"}',[Seg.Segname,IValue])) as TJSONObject
              else
                jSub := GetJSON(Format('{"Segment":"%s","Offset":"%4.4X"}',['<nil>',IValue])) as TJSONObject;
              jObject.Add(Name,jSub);
            end;
    end;
  // Do segments
  jObject := jData.FindPath('Segments') as TJSONObject;
  if Assigned(jObject) then
    for i := 0 to FSegments.Count-1 do
      with FSegments.Items[i] do
        begin
          jSub := GetJSON(Format('{"Address":"%4.4X","Length":"%4.4X","IsFixed":"%s","IsReadOnly":"%s","IsUninitialised":"%s","Code":%s,"Fixups":%s}',
                              [FirstAddress,
                               Bytes,
                               BooleanToYN(smFixed in Modifiers),
                               BooleanToYN(smReadOnly in Modifiers),
                               BooleanToYN(smUninitialised in Modifiers),
                               CodeAsJSONArray,
                               FFixupList.SegmentFixupsAsJSONArray(SegName)
                               ])) as TJSONObject;
          jObject.Add(Segname,jSub);
        end;
end;

procedure TObjectFile.Save;
var s: string;
    fstream: TFileStream;
begin
  s := jData.FormatJSON;
  fstream := TFileStream.Create(FFilename,fmCreate);
  try
    fstream.Write(s[1],Length(s));
  finally
    FreeAndNil(fstream);
  end;
end;

end.

