unit udebuglist;

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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ucodesegment;

type

  TDebugLine = class(TObject)
    private
      FFileIndex: integer;
      FLine:      integer;
      FSeg:       TSegment;
      FOffset:    word;
    public
      constructor Create(_fileindex: integer; _line: integer; _seg: TSegment; _offset: word);
  end;

  TDebugList = class(specialize TObjectList<TDebugLine>)
    private
      FFilenameList: TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddRec(const _filename: string; _line: integer; _seg: TSegment; _offset: word);
      function  DebugDataAsJSONArray(_seg: TSegment): string;
      function  FilenamesAsJSONArray: string;
      property FilenameList: TStringList read FFilenameList;
  end;


implementation



//=============================================================================
//
//  TDebugLine code
//
//=============================================================================

constructor TDebugLine.Create(_fileindex: integer; _line: integer; _seg: TSegment; _offset: word);
begin
  inherited Create;
  FFileIndex := _fileindex;
  FLine      := _line;
  FSeg       := _seg;
  FOffset    := _offset;
end;



//=============================================================================
//
//  TDebugList code
//
//=============================================================================

constructor TDebugList.Create;
begin
  inherited Create;
  FFilenameList := TStringList.Create;
end;

destructor TDebugList.Destroy;
begin
  FreeAndNil(FFilenameList);
  inherited Destroy;
end;

procedure TDebugList.AddRec(const _filename: string; _line: integer; _seg: TSegment; _offset: word);
var _index: integer;
begin
  _index := FFilenameList.IndexOf(_filename);
  if _index < 0 then
    _index := FFilenameList.Add(_filename);
  inherited Add(TDebugLine.Create(_index,_line,_seg,_offset));
end;

function TDebugList.DebugDataAsJSONArray(_seg: TSegment): string;
var obj: TDebugLine;
    s:   string;
begin
  s := '[';
  for obj in Self do
    if obj.FSeg = _seg then
      begin
        if s <> '[' then
          s := s + ',';
        s := s + Format('"%4.4X%4.4X%4.4X"',[obj.FFileIndex,obj.FLine,obj.FOffset]);
      end;
  s := s + ']';
  DebugDataAsJSONArray := s;
end;

function TDebugList.FilenamesAsJSONArray: string;
var s: string;
    i: integer;
begin
  s := '[';
  for i := 0 to FFilenameList.Count-1 do
    begin
      if i > 0 then
        s := s + ',';
//      s := s + '"' + FFilenameList[i] + '"';
      s := s + '"BlahBlah"';
    end;
  s := s + ']';
  FilenamesAsJSONArray := s;
end;

end.

