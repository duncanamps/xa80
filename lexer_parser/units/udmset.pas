unit udmset;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TDmSet<T> = class(TSortedList<T>)
    public
      procedure Add(_index: T); reintroduce;
      function  AsString(_width: integer = -1): string;
      function  Empty: boolean;
      function  Equal(_set: TDmSet<T>): boolean;
      procedure Remove(_index: T);
      procedure Subtract(_set: TDmSet<T>);
      procedure Union(_set: TDmSet<T>);
  end;

implementation

uses
  strutils;

//==============================================================================
//
//  TSet code
//
//==============================================================================

procedure TDmSet<T>.Add(_index: T); // Override add to ensure unique values
begin
  if IndexOf(_index) < 0 then
    inherited Add(_index);
end;

function TDmSet<T>.AsString(_width: integer): string;
var idx: T;
begin
  Result := '';
  for idx in Self do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + IntToStr(idx);
    end;
  if _width > 3 then
    begin
      if Length(Result) > _width then
        Result := Copy(Result,1,_width-3) + '...';
      Result := PadRight(Result,_width);
    end;
end;

function TDmSet<T>.Empty: boolean;
begin
  Result := (Count = 0);
end;

function TDmSet<T>.Equal(_set: TDmSet<T>): boolean;
var i: integer;
begin
  Result := True; // Assume good for now
  if Count <> _set.Count then
    Result := False // Different number of elements - cannot possibly be equal
  else
    for i := 0 to Count-1 do
      if Items[i] <> _set[i] then
        begin
          Result := False;
          break;
        end;
end;

procedure TDmSet<T>.Remove(_index: T);
var _pos: integer;
begin
  _pos := IndexOf(_index);
  if _pos >= 0 then
    Delete(_pos);
end;

procedure TDmSet<T>.Subtract(_set: TDmSet<T>);
var i: T;
begin
  for i in _set do
    Remove(i);
end;

procedure TDmSet<T>.Union(_set: TDmSet<T>);
var i: T;
begin
  for i in _set do
    Add(i);
end;

end.

