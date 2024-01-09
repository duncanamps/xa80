unit ucodesegment;

//
// Replacement for ucodebuffer
//
// Duncan Munro 26-Dec-2023
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ucodebuffer, uasmglobals;

type
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
      procedure AddBuf(_buf: TCodeBuffer);
      function  Bytes: integer;
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
      procedure AddBuf(_buf: TCodeBuffer); reintroduce;
      procedure ClearDefined;
      procedure CreateSegment(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
      function  CurrentSegmentName: string;
      procedure Dump(_strm: TFileStream; var _printpage: integer);
      procedure EnsureCurrentSegment;
      function  FindByName(const _segname: string; _casesensitive: boolean = False): TSegment;
      function  GetOrg: word;
      procedure Init;
      procedure SetOrg(_neworg: word);
      property CurrentSegment: TSegment read FCurrentSegment write FCurrentSegment;
  end;



implementation

uses
  lacogen_types, umessages;

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

procedure TSegment.AddBuf(_buf: TCodeBuffer);
var i: integer;
begin
  for i := 0 to _buf.Contains-1 do
    begin
      FBuf[FAddress] := _buf.Buffer[i];
      FUsed[FAddress] := True;
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

procedure TSegments.AddBuf(_buf: TCodeBuffer);
begin
  EnsureCurrentSegment;
  FCurrentSegment.AddBuf(_buf);
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

procedure TSegments.Dump(_strm: TFileStream; var _printpage: integer);
const PAGE_WIDTH = 78;
      PAGE_DEPTH = 60;
      _caption = 'SEGMENT LIST';
var i: integer;
    s: string;
    t_ch: char;
    line: integer;
    pagestr: string;
    spc:     integer;
    segname: string;
    segment: TSegment;
    source:  string;

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
    MyWrite('-------------------- ---- ---- ---- ----- ----------------------------------' + LINE_TERMINATOR);
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
      ErrorObj.Show(ltWarning,W1008_NO_DEFAULT_SEGMENT);
      CreateSegment(DEFAULT_CODE_SEGMENT,[]);
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

end.

