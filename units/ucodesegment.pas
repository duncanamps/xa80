unit ucodesegment;

//
// Replacement for ucodebuffer
//
// Duncan Munro 26-Dec-2023
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, ucodebuffer;

type
  TSegmentModifier = (smFixed,smReadOnly,smUninitialised);

  TSegmentModifiers = set of TSegmentModifier;

  TSegment = class(TObject)
    private
      FAddress:    word;
      FBuf:        array[word] of byte;
      FIntAddress: word;
      FModifiers:  TSegmentModifiers;
      FSegName:    string;
      FUsed:       array[word] of boolean;
    public
      constructor Create(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
      procedure AddBuf(_buf: TCodeArray);
  end;

  TSegments = class(specialize TObjectList<TSegment>)
    private
      FCurrentSegment: TSegment;
    public
      procedure AddBuf(_buf: TCodeArray); reintroduce;
      procedure CreateSegment(const _segname: string; _modifiers: TSegmentModifiers; _address: word = 0);
      function  CurrentSegmentName: string;
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
  FIntAddress := _address;
  for w in word do
    begin
      FBuf[w]  := 0;
      FUsed[w] := False;
    end;
end;

procedure TSegment.AddBuf(_buf: TCodeArray);
begin
end;


//----------------------------------------------------------------------
//
//  TSegments code
//
//----------------------------------------------------------------------

procedure TSegments.AddBuf(_buf: TCodeArray);
begin
  EnsureCurrentSegment;
  FCurrentSegment.AddBuf(_buf);
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

procedure TSegments.EnsureCurrentSegment;
begin
  if not Assigned(FCurrentSegment) then
    begin // Segment doesn't exist, create a default
      ErrorObj.Show(ltWarning,W1008_NO_DEFAULT_SEGMENT);
      CreateSegment('CSEG',[]);
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

