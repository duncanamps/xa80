unit ucodesegment;

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
// Replacement for ucodebuffer
//
// Duncan Munro 26-Dec-2023
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ucodebuffer, uasmglobals, fpjson;

type
  TSegment = class;

  TFixup = record
    Reference: string;   // Name of external symbol to link to
    Seg:       TSegment; // Segment object where fixup is required
    Offset:    word;     // Offset within the segment
  end;

  TFixupList = class(specialize TList<TFixup>)
    public
      procedure Add(const _ref: string; _seg: TSegment; _offset: word); reintroduce;
      procedure AddJSON(jFixups: TJSONData; _segment: TSegment);
      procedure Dump(_strm: TFileStream; var _printpage: integer);
      function FixupsAsJSONArray: string;
      function SegmentFixupsAsJSONArray(const _reqseg: string): string;
  end;

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
      procedure AddJSON(jDebugs: TJSONData; _segment: TSegment);
      procedure AddRec(const _filename: string; _line: integer; _seg: TSegment; _offset: word);
      procedure Clear;
      function  DebugDataAsJSONArray(_seg: TSegment): string;
      function  FilenamesAsJSONArray: string;
      procedure FromJSONobject(_parent: TJSONdata);
      procedure ToJSONobject(_parent: TJSONdata);
      property FilenameList: TStringList read FFilenameList;
  end;

  TSegmentModifier = (smFixed,smReadOnly,smUninitialised);

  TSegmentModifiers = set of TSegmentModifier;

  TSegment = class(TObject)
    private
      FAddress:    word;
      FBuf:        TBlock64K;
      FDefined:    boolean;
      FModifiers:  TSegmentModifiers;
      FSegName:    string;
      FUsed:       array[word] of boolean;
    public
      constructor Create(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
      procedure AddBuf(_buf: TCodeBuffer; _pass: integer);
      function  Bytes: integer;
      function  CodeAsJSONArray: string;
      function  CodeAsText: string;
      function  FirstAddress: word;
      function  IsEmpty: boolean;
      function  LastAddress: word;
      function  ModifiersAsText: string;
      property Address:   word              read FAddress    write FAddress;
      property Buf:       TBlock64K         read FBuf;
      property Defined:   boolean           read FDefined    write FDefined;
      property Modifiers: TSegmentModifiers read FModifiers  write FModifiers;
      property Segname:   string            read FSegname;
  end;

  TSegments = class(specialize TObjectList<TSegment>)
    private
      FCurrentSegment: TSegment;
    public
      procedure AddBuf(_buf: TCodeBuffer; _pass: integer); reintroduce;
      procedure ClearDefined;
      procedure CreateSegment(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
      function  CurrentSegmentName: string;
      procedure Dump(_strm: TStream; var _printpage: integer);
      procedure EnsureCurrentSegment;
      function  FindByName(const _segname: string; _casesensitive: boolean = False): TSegment;
      procedure FromJSONobject(_object: TJSONdata; _fixuplist: TFixupList; _debuglist: TDebugList; const _filename: string);
      function  GetOrg: word;
      procedure Init;
      procedure SetOrg(_neworg: word);
      procedure SortSegments;
      procedure ToJSONobject(_parent: TJSONdata; _fixuplist: TFixupList; _debuglist: TDebugList);
      property CurrentSegment: TSegment read FCurrentSegment write FCurrentSegment;
  end;



implementation

uses
  lacogen_types, umessages, Generics.Defaults, uutility;


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

procedure TFixupList.AddJSON(jFixups: TJSONData; _segment: TSegment);
var j,k: integer;
    fixuptitle: string;
    hexstring:  string;
    hexval:     word;
    jFixup:     TJSONData;
begin
  for j := 0 to jFixups.Count-1 do
    begin
      fixuptitle := TJSONObject(jFixups).Names[j];
      jFixup := jFixups.FindPath(fixuptitle);
      for k := 0 to jFixup.Count-1 do
        begin
          hexstring := jFixup.Items[k].AsString;
          hexval := StrToInt('$' + hexstring);
          Add(fixuptitle,_segment,hexval);
        end;
    end;
end;

procedure TFixupList.Dump(_strm: TFileStream; var _printpage: integer);
const PAGE_WIDTH = 78;
      PAGE_DEPTH = 60;
      _caption = 'FIXUP LIST';
var i: integer;
    s: string;
    line: integer;
    pagestr: string;
    spc:     integer;
    fixup:   TFixup;
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

function TFixupList.FixupsAsJSONArray: string;
var i: integer;
    fixup:   TFixup;
    lastseg:   string;
    lastlabel: string;
    outstr:    string;

begin
  outstr := '{';
  // Sort into segment, name, offset
  Sort(specialize TComparer<TFixup>.Construct(@CompareFixup));
  // Now do the string compilation
  lastseg := '';
  lastlabel := '';
  for i := 0 to Count-1 do
    begin
      fixup := Items[i];
      if fixup.Seg.Segname <> lastseg then
        begin
          if lastseg <> '' then
            outstr := outstr + ']},';
          outstr := outstr + '"' + fixup.Seg.Segname + '":{"' + fixup.Reference + '":[';
          lastlabel := fixup.Reference;
          lastseg := fixup.Seg.Segname;
        end;
      if fixup.Reference <> lastlabel then
        begin
          outstr := outstr + '],"' + fixup.Reference + '":[';
          lastlabel := fixup.Reference;
        end;
      if RightStr(outstr,1) = '"' then
        outstr := outstr + ',';
      outstr := outstr + '"' + Format('%4.4X',[fixup.Offset]) + '"';
    end;
  outstr := outstr + ']}}';
  FixupsAsJSONArray := outstr;
end;

function TFixupList.SegmentFixupsAsJSONArray(const _reqseg: string): string;
var i: integer;
    fixup:   TFixup;
    lastlabel: string;
    outstr:    string;

begin
  outstr := '{';
  // Sort into segment, name, offset
  Sort(specialize TComparer<TFixup>.Construct(@CompareFixup));
  // Now do the string compilation
  lastlabel := '';
  for i := 0 to Count-1 do
    begin
      fixup := Items[i];
      if fixup.Seg.Segname = _reqseg then
        begin
          if lastlabel = '' then
            begin
              lastlabel := fixup.Reference;
              outstr := outstr + '"' + fixup.Reference + '":[';
            end;
          if fixup.Reference <> lastlabel then
            begin
              outstr := outstr + '],"' + fixup.Reference + '":[';
              lastlabel := fixup.Reference;
            end;
          if RightStr(outstr,1) = '"' then
            outstr := outstr + ',';
          outstr := outstr + '"' + Format('%4.4X',[fixup.Offset]) + '"';
        end;
    end;
  if RightStr(outstr,1) = '"' then
    outstr := outstr + ']';
  outstr := outstr + '}';
  SegmentFixupsAsJSONArray := outstr;
end;


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

procedure TDebugList.AddJSON(jDebugs: TJSONData; _segment: TSegment);
var j,k: integer;
    debugline:  string;
    fixuptitle: string;
    hexstring:  string;
    hexval:     word;
    jFixup:     TJSONData;
    obj:        TDebugLine;

  function GetHex(_offs: integer): word;
  var portion: string;
  begin
    portion := Copy(debugline,_offs,4);
    GetHex := HexToDec16(portion);
  end;

begin
  for j := 0 to jDebugs.Count-1 do
    begin
      debugline := TJSONArray(jDebugs).Items[j].AsString;
      if Length(debugline) <> 12 then
        ErrorObj.Show(ltError,E2074_OBJECT_DEBUG_CORRUPT);
      obj := TDebugLine.Create(GetHex(1),GetHex(5),_segment,GetHex(9));
      Add(obj);
    end;
end;

procedure TDebugList.AddRec(const _filename: string; _line: integer; _seg: TSegment; _offset: word);
var _index: integer;
begin
  _index := FFilenameList.IndexOf(_filename);
  if _index < 0 then
    _index := FFilenameList.Add(_filename);
  inherited Add(TDebugLine.Create(_index,_line,_seg,_offset));
end;

procedure TDebugList.Clear;
begin
  inherited Clear;
  FFilenameList.Clear;
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

procedure TDebugList.FromJSONobject(_parent: TJSONdata);
var jObject:  TJSONarray;
    i:        integer;
begin
  jObject := _parent.FindPath(CONST_JSON_DEBUGNAMES_TITLE) as TJSONarray;
  if Assigned(jObject) then
    for i := 0 to jObject.Count-1 do
      FFilenameList.Add(jObject.Items[i].AsString);
end;

procedure TDebugList.ToJSONobject(_parent: TJSONdata);
var jArray: TJSONarray;
    i:      integer;
begin
  jArray := _parent.FindPath(CONST_JSON_DEBUGNAMES_TITLE) as TJSONArray;
  if Assigned(jArray) then
    begin
      for i := 0 to FilenameList.Count-1 do
        jArray.Add(FilenameList[i]);
    end;
end;

//==============================================================================

function CompareSegment(constref Left,Right: TSegment): integer;

  function IsFixed(_seg: TSegment): boolean;
  begin
    IsFixed := smFixed in _seg.Modifiers;
  end;

  function BothFixed: boolean;
  begin
    BothFixed := IsFixed(Left) and IsFixed(Right);
  end;

  function IsUninitialised(_seg: TSegment): boolean;
  begin
    IsUninitialised := (smUninitialised in _seg.Modifiers);
  end;

  function IsInitialised(_seg: TSegment): boolean;
  begin
    IsInitialised := not IsUninitialised(_seg);
  end;

  function IsReadOnly(_seg: TSegment): boolean;
  begin
    IsReadOnly := smReadOnly in _seg.Modifiers;
  end;

  function IsReadWrite(_seg: TSegment): boolean;
  begin
    IsReadWrite := not IsReadOnly(_seg);
  end;

begin
  // Order so fixed segments go at start
  // Fixed segments are ordered by address, lowest first
  // Followed by relocatable with initialised data (read only)
  // Followed by relocatable with initialised data (read write)
  // Then relocatable with uninitialised data
  if IsFixed(Left) and (not IsFixed(Right)) then
    CompareSegment := -1
  else if (not IsFixed(Left)) and IsFixed(Right) then
    CompareSegment := 1
  // Both segments are fixed or both are not fixed
  else if BothFixed then
    begin
      if Left.FirstAddress > Right.FirstAddress then
        CompareSegment := 1
      else if Left.FirstAddress < Right.FirstAddress then
        CompareSegment := -1
      else
        CompareSegment := 0;
    end
  // Both are relocatable if we got to here
  // Initialised before uninitialised
  else if IsInitialised(Left) and IsUninitialised(Right) then
    CompareSegment := -1
  else if IsUninitialised(Left) and IsInitialised(Right) then
    CompareSegment := 1
  // Last check, read only comes before read/write
  else if IsReadOnly(Left) and IsReadWrite(Right) then
    CompareSegment := -1
  else if IsReadWrite(Left) and IsReadOnly(Right) then
    CompareSegment := 1
  else
    CompareSegment := 0;
end;


//----------------------------------------------------------------------
//
//  TSegment code
//
//----------------------------------------------------------------------

constructor TSegment.Create(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
var w: word;
begin
  inherited Create;
  FSegName    := _segname;
  FModifiers  := _modifiers;
  FAddress    := _address;
  FDefined    := True;
  for w in word do
    begin
      FBuf[w]  := 0;
      FUsed[w] := False;
    end;
end;

procedure TSegment.AddBuf(_buf: TCodeBuffer; _pass: integer);
var i: integer;
begin
  for i := 0 to _buf.Contains-1 do
    begin
      FBuf[FAddress] := _buf.Buffer[i];
      FUsed[FAddress] := True;
      if (FAddress = $FFFF) then
        begin
          FAddress := 0;
          if _pass = 2 then
            ErrorObj.Show(ltWarning,W1001_CODE_WRAPPED_ROUND);
        end
      else
        Inc(FAddress);
    end;
end;

function TSegment.Bytes: integer;
begin
  if IsEmpty then
    Bytes := 0
  else
    Bytes := LastAddress - FirstAddress + 1;
end;

function TSegment.CodeAsJSONArray: string;
var s: string;
    cnt: integer;
    i:   integer;
    a:   word;
    slen: integer;
begin
  a := FirstAddress;
  cnt := Bytes;
  s := '[]';
  if not (smUninitialised in FModifiers) then
    begin
      s := '[';
      while cnt > 0 do
        begin
          slen := cnt;
          if slen > 32 then
            slen := 32;
          if s <> '[' then
            s := s + ',';
          s := s + '"';
          for i := 0 to slen-1 do
            begin
              if FUsed[a] then
                s := s + Format('%2.2X',[FBuf[a]])
              else
                s := s + '--';
              if a = $FFFF then
                a := 0
              else
                a := a + 1;
            end;
          s := s + '"';
          cnt := cnt - slen;
        end;
      s := s + ']';
    end;
  CodeAsJSONArray := s;
end;

function TSegment.CodeAsText: string;
var s: string;
    cnt: integer;
    i:   integer;
    a:   word;
begin
  a := FirstAddress;
  cnt := Bytes;
  s := '';
  if not (smUninitialised in FModifiers) then
    begin
      s := '';
      for i := 0 to cnt-1 do
        begin
          if FUsed[a] then
            s := s + Format('%2.2X',[FBuf[a]])
          else
            s := s + '--';
          if a = $FFFF then
            a := 0
          else
            a := a + 1;
        end;
    end;
  CodeAsText := s;
end;

function TSegment.FirstAddress: word;
var w: word;
begin
  if IsEmpty then
    FirstAddress := 0
  else
    begin
      for w in word do
        if FUsed[w] then
          begin
            FirstAddress := w;
            break;
          end;
    end;
end;

function TSegment.IsEmpty: boolean;
var w: word;
begin
  IsEmpty := True;
  for w in word do
    if FUsed[w] then
      begin
        IsEmpty := False;
        break;
      end;
end;

function TSegment.LastAddress: word;
var i: integer;
begin
  if IsEmpty then
    LastAddress := 0
  else
    begin
      for i := High(Word) downto 0 do
        if FUsed[i] then
          begin
            LastAddress := word(i);
            break;
          end;
    end;
end;

function TSegment.ModifiersAsText: string;
var s: string;
begin
  if smFixed in FModifiers then
    s := 'Fixed'
  else
    s := 'Relocatable';
  if smReadOnly in FModifiers then
    s := s + ', ' + 'Read only'
  else
    s := s + ', ' + 'Read/write';
  if smUninitialised in FModifiers then
    s := s + ', ' + 'Uninitialised'
  else
    s := s + ', ' + 'Initialised';
  ModifiersAsText := s;
end;


//----------------------------------------------------------------------
//
//  TSegments code
//
//----------------------------------------------------------------------

procedure TSegments.AddBuf(_buf: TCodeBuffer; _pass: integer);
begin
  EnsureCurrentSegment;
  FCurrentSegment.AddBuf(_buf,_pass);
end;

procedure TSegments.ClearDefined;
var _seg: TSegment;
begin
  for _seg in Self do
    begin
      _seg.FDefined := False;
      _seg.Address := 0;
    end;
end;

procedure TSegments.CreateSegment(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
var _seg: TSegment;
begin
  _seg := TSegment.Create(_segname,_modifiers,_address);
  inherited Add(_seg);
  FCurrentSegment := _seg;
end;

function TSegments.CurrentSegmentName: string;
begin
  EnsureCurrentSegment;
  CurrentSegmentName := FCurrentSegment.FSegName;
end;

procedure TSegments.Dump(_strm: TStream; var _printpage: integer);
const PAGE_WIDTH = 78;
      PAGE_DEPTH = 60;
      _caption = 'SEGMENT LIST';
var i: integer;
    s: string;
    line: integer;
    pagestr: string;
    spc:     integer;
    segment: TSegment;

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
    MyWrite('SEGMENT              FROM TO   LENX  LEND ATTRIBUTES' + LINE_TERMINATOR);
    MyWrite('-------------------- ---- ---- ---- ----- ------------------------------------' + LINE_TERMINATOR);
    line := 7;
  end;

  procedure FormFeed;
  begin
    MyWrite(FF);
  end;

begin
  line := 0;
  Header;
  for i := 0 to Count-1 do
    begin
      if line >= PAGE_DEPTH then
        begin
          FormFeed;
          Header;
        end;
      segment := Items[i];
      s := Format('%-20s %4.4X %4.4X %4.4X %5d %s',[segment.FSegName,segment.FirstAddress,segment.LastAddress,segment.Bytes,segment.Bytes,segment.ModifiersAsText]);
      MyWrite(s + LINE_TERMINATOR);
      Inc(line);
    end;
  FormFeed;
end;

procedure TSegments.EnsureCurrentSegment;
begin
  if not Assigned(FCurrentSegment) then
    begin // Segment doesn't exist, create a default
//    ErrorObj.Show(ltWarning,W1008_NO_DEFAULT_SEGMENT); // Turned off for now
      CreateSegment(DEFAULT_CODE_SEGMENT,[smFixed]);
    end;
end;

function TSegments.FindByName(const _segname: string; _casesensitive: boolean = False): TSegment;
var _seg: TSegment;
begin
  FindByName := nil;
  for _seg in Self do
    if (_casesensitive and (_segname = _seg.FSegName)) or
       ((not _casesensitive) and (UpperCase(_segname) = UpperCase(_seg.FSegName))) then
      begin
        FindByName := _seg;
        Exit;
      end;
end;

procedure TSegments.FromJSONobject(_object: TJSONdata; _fixuplist: TFixupList; _debuglist: TDebugList; const _filename: string);
var jObject:   TJSONdata;
    jSub:      TJSONdata;
    jCode:     TJSONarray;
    jFixups:   TJSONdata;
    jDebugs:   TJSONdata;
    i,j:       integer;
    codelines: integer;
    segname:   string;
    segment:   TSegment;
    modifiers: TSegmentModifiers;
    seg_address:   Word;
    seg_length:    Word;

 function GetHex(_sub: TJSONdata; const _title: string): Word;
 var _hexstr: string;
 begin
   _hexstr := TJSONObject(_sub).Get(_title);
   GetHex := HexToDec16(_hexstr);
 end;

 procedure ProcessModifier(_sub: TJSONObject; const _modstring: string; _modifier: TSegmentModifier);
 var response: string;
 begin
   response := _sub.Get(_modstring);
   if response = 'Y' then
     modifiers := modifiers + [_modifier];
 end;

 procedure ProcessCodeLine(_s: string);
 var _byte: string;
     _bval: byte;
     _n,_l: integer;
 begin
   _n := 1;
   _l := Length(_s);
   while _n <= _l do
     begin
       _byte := Copy(_s,_n,2);
       if _byte <> '--' then
         begin
           _bval := StrToInt('$' + _byte);
           segment.FBuf[seg_address] := _bval;
         end;
       _n := _n + 2;
       Inc(seg_address);
     end;
 end;

begin
  // Reset to blank objects
  Clear;
  _fixuplist.Clear;
  _debuglist.Clear;

  jObject := _object.FindPath(CONST_JSON_SEGMENTS_TITLE) as TJSONData;
  if not Assigned(jObject) then
    ErrorObj.Show(ltWarning,W1016_OBJECT_NO_SEGMENTS)
  else
    for i := 0 to jObject.Count-1 do
      begin
        segname := TJSONObject(jObject).Names[i];
        jSub := jObject.FindPath(segname);
        // Process segment header
        modifiers := [];
        if not Assigned(jSub) then
          ErrorObj.Show(ltInternal,X3006_OBJECT_SEGMENT_ERROR)
        else
          begin
            ProcessModifier(TJSONObject(jSub),CONST_JSON_SEGMENT_ISFIXED,        smFixed);
            ProcessModifier(TJSONObject(jSub),CONST_JSON_SEGMENT_ISREADONLY,     smReadOnly);
            ProcessModifier(TJSONObject(jSub),CONST_JSON_SEGMENT_ISUNINITIALISED,smUninitialised);
            seg_address := GetHex(jSub,CONST_JSON_SEGMENTS_ADDRESS);
            seg_length  := GetHex(jSub,CONST_JSON_SEGMENTS_LENGTH);
            segment := TSegment.Create(segname,modifiers,seg_address);
            for j := 0 to seg_length-1 do
              segment.FUsed[j+seg_address] := True;
            // Extract the code and fill the segment with it
            jCode := jSub.FindPath(CONST_JSON_SEGMENT_CODE) as TJSONArray;
            if Assigned(jCode) then
              begin
                codelines := jCode.Count;
                for j := 0 to codelines - 1 do
                  ProcessCodeLine(jCode.Items[j].AsString);
              end;
            // Get the fixups
            jFixups := jSub.FindPath(CONST_JSON_SEGMENT_FIXUPS);
            if Assigned(jFixups) then
              _fixuplist.AddJSON(jFixups,segment);
            // Get the debug lines
            jDebugs := jSub.FindPath(CONST_JSON_SEGMENT_DEBUGLIST);
            if Assigned(jDebugs) then
              _debuglist.AddJSON(jDebugs,segment);
            // Finally add to list
            Add(segment);
          end;
      end;
end;

function TSegments.GetOrg: word;
begin
  EnsureCurrentSegment;
  GetOrg := FCurrentSegment.FAddress;
end;

procedure TSegments.Init;
begin
  Clear;
  FCurrentSegment := nil;
end;

procedure TSegments.SetOrg(_neworg: word);
begin
  EnsureCurrentSegment;
  FCurrentSegment.FAddress := _neworg;
end;

procedure TSegments.SortSegments;
begin
  Sort(specialize TComparer<TSegment>.Construct(@CompareSegment));
end;

procedure TSegments.ToJSONobject(_parent: TJSONdata; _fixuplist: TFixupList; _debuglist: TDebugList);
var jObject: TJSONobject;
    jSub:    TJSONobject;
    i:       integer;
    tmpstr:  string;
begin
  jObject := _parent.FindPath(CONST_JSON_SEGMENTS_TITLE) as TJSONObject;
  if Assigned(jObject) then
    for i := 0 to Count-1 do
      with Items[i] do
        begin
          tmpstr := Format('{"Address":"%4.4X","Length":"%4.4X","' + CONST_JSON_SEGMENT_ISFIXED + '":"%s","' + CONST_JSON_SEGMENT_ISREADONLY + '":"%s","' + CONST_JSON_SEGMENT_ISUNINITIALISED + '":"%s","' + CONST_JSON_SEGMENT_CODE + '":%s,"' + CONST_JSON_SEGMENT_FIXUPS + '":%s,"' + CONST_JSON_SEGMENT_DEBUGLIST + '":%s}',
                              [FirstAddress,
                               Bytes,
                               BooleanToYN(smFixed in Modifiers),
                               BooleanToYN(smReadOnly in Modifiers),
                               BooleanToYN(smUninitialised in Modifiers),
                               CodeAsJSONArray,
                               _fixuplist.SegmentFixupsAsJSONArray(SegName),
                               _debuglist.DebugDataAsJSONArray(Items[i])
                               ]);
          jSub := GetJSON(tmpstr) as TJSONObject;
          jObject.Add(Segname,jSub);
        end;
end;

end.

