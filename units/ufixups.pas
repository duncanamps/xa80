unit ufixups;

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
  TFixup = record
    Reference: string;   // Name of external symbol to link to
    Seg:       TSegment; // Segment object where fixup is required
    Offset:    word;     // Offset within the segment
  end;

  TFixupList = class(specialize TList<TFixup>)
    public
      procedure Add(const _ref: string; _seg: TSegment; _offset: word); reintroduce;
      procedure Dump(_strm: TFileStream; var _printpage: integer);
  end;


implementation

uses
  uasmglobals, Generics.Defaults;

function CompareFixup(constref Left,Right: TFixup): integer;
begin
  if Left.Seg.Segname > Right.Seg.Segname then
    CompareFixup := 1
  else if Left.Seg.Segname < Right.Seg.Segname then
    CompareFixup := -1
  else if Left.Reference > Right.Reference then
    CompareFixup := 1
  else if Left.Reference < Right.Reference then
    CompareFixup := -1
  else if Left.Offset > Right.Offset then
    CompareFixup := 1
  else if Left.Offset < Right.Offset then
    CompareFixup := -1
  else
    CompareFixup := 0;
end;

procedure TFixupList.Add(const _ref: string; _seg: TSegment; _offset: word);
var _fixup: TFixup;
begin
  _fixup.Reference := _ref;
  _fixup.Seg       := _seg;
  _fixup.Offset    := _offset;
  inherited Add(_fixup);
end;

procedure TFixupList.Dump(_strm: TFileStream; var _printpage: integer);
const PAGE_WIDTH = 78;
      PAGE_DEPTH = 60;
      _caption = 'FIXUP LIST';
var i: integer;
    s: string;
    t_ch: char;
    line: integer;
    pagestr: string;
    spc:     integer;
    fixup:   TFixup;
    source:  string;
    lastseg:   string;
    lastlabel: string;

  procedure MyWrite(const _buf: string);
  begin
    _strm.Write(_buf[1],Length(_buf));
  end;

  procedure Header;
  begin
    Inc(_PrintPage);
    pagestr := 'Page: ' + IntToStr(_PrintPage);
    spc := PAGE_WIDTH - Length(_caption) - Length(pagestr);
    MyWrite(LINE_TERMINATOR);
    MyWrite(_caption + Space(spc div 2) + Space(spc - spc div 2) + pagestr + LINE_TERMINATOR);
    MyWrite(StringOfChar('-',PAGE_WIDTH) + LINE_TERMINATOR);
    MyWrite(LINE_TERMINATOR);
    MyWrite('TARGET SEGMENT' + LINE_TERMINATOR);
    MyWrite('        LABEL: FIXUP OFFSET(S)' + LINE_TERMINATOR);
    MyWrite(StringOfChar('-',PAGE_WIDTH) + LINE_TERMINATOR);
    line := 7;
  end;

  procedure FormFeed;
  begin
    MyWrite(FF);
  end;

  procedure CheckLine;
  begin
    if line >= PAGE_DEPTH then
      begin
        FormFeed;
        Header;
      end;
  end;

  procedure Purge;
  begin
    if s = '' then
      Exit;
    CheckLine;
    MyWrite(s + LINE_TERMINATOR);
    Inc(line);
    s := '';
  end;

begin
  line := 0;
  Header;
  // Sort into segment, name, offset
  Sort(specialize TComparer<TFixup>.Construct(@CompareFixup));
  // Now do the printing
  lastseg := '';
  lastlabel := '';
  s := '';
  for i := 0 to Count-1 do
    begin
      fixup := Items[i];
      if fixup.Seg.Segname <> lastseg then
        begin
          Purge;
          lastseg := fixup.Seg.Segname;
          CheckLine;
          MyWrite(lastseg + LINE_TERMINATOR);
          Inc(line);
          lastlabel := '';
        end;
      if fixup.Reference <> lastlabel then
        begin
          Purge;
          lastlabel := fixup.Reference;
          s := '        ' + LastLabel + ':';
        end;
      if Length(s) > (PAGE_WIDTH-5) then
        begin
          Purge;
          s := Space(9+Length(LastLabel));
        end;
      s := s + Format(' %4.4X',[fixup.Offset]);
    end;
  Purge;
  FormFeed;
end;

end.

