
{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2023 Duncan Munro

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

unit usymboltable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  // Addresses are relocatable, integers are not
  TSymbolType = (stUnknown,stAddress,stWord,stString);

  TSymbolArea = (saInternal,saExported,saExternal);

  TSymbolSeg  = (ssCSEG,ssDSEG,ssBSS);

  TSymbol = record
    Name:       string;
    SymType:    TSymbolType;
    Area:       TSymbolArea;
    Seg:        TSymbolSeg;
    IValue:     Word;
    SValue:     string;
    Referenced: boolean;
    Defined:    boolean;
  end;

  TSymbolTable = class(specialize TList<TSymbol>)
    private
      HashSize:  integer;
      HashTable: array of integer;
      procedure AddHash(const _txt: string; _rec: integer);
      procedure ReHash;
      procedure SetHashSize(_sz: integer);
    public
      MixedCase: boolean;
      constructor Create;
      destructor Destroy; override;
      function  Add(_name: string; _datatype: TSymbolType; _ival: Word; const _sval: string): integer; reintroduce;
      function  AddPlaceholder(_name: string): integer;
      function  AddString(_name: string; const _sval: string): integer;
      function  AddWord(_name: string; _ival: Word): integer;
      function  CalcHash(const _txt:string): integer;
      procedure Dump(_strm: TFileStream; const _title: string);
      procedure DumpByAddress(_strm: TFileStream);
      procedure DumpByName(_strm: TFileStream);
      function  IndexOf(_name: string): integer; reintroduce;
  end;


implementation

uses
  Generics.Defaults, uasmglobals, uutility;

const
  HASH_RATIO = 3;
  HASH_EXPANSION = 3;
  INITIAL_HASH_BASE = 1000;

function CompareName(constref Left,Right: TSymbol): integer;
begin
  if Left.Name = Right.Name then
    Result := 0
  else if Left.Name > Right.Name then
    Result := 1
  else
    Result := -1;
end;

function CompareAddress(constref Left,Right: TSymbol): integer;
begin
  if Left.IValue = Right.IValue then
    Result := CompareName(Left,Right)
  else if Left.IValue > Right.IValue then
    Result := 1
  else
    Result := -1;
end;


constructor TSymbolTable.Create;
begin
  inherited Create;
  MixedCase := False;
  SetHashSize(NextPrime(INITIAL_HASH_BASE * HASH_RATIO));
end;

destructor TSymbolTable.Destroy;
begin
  inherited Destroy;
end;

procedure TSymbolTable.AddHash(const _txt: string; _rec: integer);
var hashval: uint32;
    divval:  integer;
begin
  // Check for empty slot
  hashval := CalcHash(_txt);
  while HashTable[hashval] <> -1 do
    begin
      Inc(hashval);
      if hashval >= HashSize then
        hashval := 0;
    end;
  HashTable[hashval] := _rec;
  if Count * HASH_RATIO > HashSize then
    SetHashSize(NextPrime(HashSize * HASH_EXPANSION));
end;

function TSymbolTable.Add(_name: string; _datatype: TSymbolType; _ival: Word; const _sval: string): integer;
var idx: integer;
    sym: TSymbol;
begin
  Add := -1;
  if not MixedCase then
    _name := UpperCase(_name);
  // Check for duplicates
  idx := IndexOf(_name);
  if idx >= 0 then
    Exit;
  // Finally use the base Add() procedure
  sym.Name       := _name;
  sym.SymType    := _datatype;
  sym.IValue     := _ival;
  sym.SValue     := _sval;
  sym.Defined    := (_datatype <> stUnknown);
  sym.Referenced := (_datatype = stUnknown);
  Add := inherited Add(sym);
  AddHash(sym.Name,Count-1);
end;

function TSymbolTable.AddPlaceholder(_name: string): integer;
begin
  AddPlaceholder := Add(_name,stUnknown,0,'');
end;

function TSymbolTable.AddString(_name: string; const _sval: string): integer;
begin
  AddString := Add(_name,stString,0,_sval);
end;

function TSymbolTable.AddWord(_name: string; _ival: Word): integer;
begin
  AddWord := Add(_name,stWord,_ival,'');
end;

function TSymbolTable.CalcHash(const _txt:string): integer;
var hashval: uint32;
    i: integer;
begin
  hashval := 123;
  for i := 1 to Length(_txt) do
    begin
      hashval := (hashval * 53) xor Ord(_txt[i]);
      hashval := ((hashval and $FE000000) shr 25) xor (hashval and $01FFFFFF);
    end;
  CalcHash := hashval mod HashSize;
end;

procedure TSymbolTable.Dump(_strm: TFileStream; const _title: string);
const PAGE_WIDTH = 78;
      PAGE_DEPTH = 60;
var i: integer;
    s: string;
    t_ch: char;
    page: integer;
    line: integer;
    pagestr: string;

  procedure MyWrite(const _buf: string);
  begin
    _strm.Write(_buf[1],Length(_buf));
  end;

  procedure Header;
  begin
    MyWrite(LINE_TERMINATOR);
    pagestr := 'Page: ' + IntToStr(page);
    MyWrite(_title + StringOfChar(' ',PAGE_WIDTH-Length(_title)-Length(pagestr)) + pagestr + LINE_TERMINATOR);
    MyWrite(StringOfChar('-',PAGE_WIDTH) + LINE_TERMINATOR);
    MyWrite(LINE_TERMINATOR);
    MyWrite('  HEX   DEC T NAME' + LINE_TERMINATOR);
    MyWrite('----- ----- - ----' + LINE_TERMINATOR);
    line := 7;
  end;

  procedure FormFeed;
  begin
    MyWrite(FF);
    page := page + 1;
  end;

begin
  page := 1;
  line := 0;
  Header;
  for i := 0 to Count-1 do
    begin
      if line >= PAGE_DEPTH then
        begin
          FormFeed;
          Header;
        end;
      case Items[i].SymType of
        stAddress: t_ch := 'A';
        stWord:    t_ch := 'I';
        stString:  t_ch := 'S';
        otherwise
          t_ch := '?';
      end;
      s := Format('$%4.4X %5d %s %s',[Items[i].IValue,Items[i].IValue,t_ch,Items[i].Name]);
      MyWrite(s + LINE_TERMINATOR);
      Inc(line);
    end;
  FormFeed;
end;

procedure TSymbolTable.DumpByAddress(_strm: TFileStream);
begin
  // This destroys the hashing but we can assume it will only be called
  // after all the assembly is complete
  Sort(specialize TComparer<TSymbol>.Construct(@CompareAddress));
  Dump(_strm,'SYMBOL DUMP BY ADDRESS');
end;

procedure TSymbolTable.DumpByName(_strm: TFileStream);
begin
  // This destroys the hashing but we can assume it will only be called
  // after all the assembly is complete
  Sort(specialize TComparer<TSymbol>.Construct(@CompareName));
  Dump(_strm,'SYMBOL DUMP BY NAME');
end;

function TSymbolTable.IndexOf(_name: string): integer;
var i: integer;
    hash: integer;
begin
  {
  IndexOf := -1;
  if not MixedCase then
    _name := UpperCase(_name);
  i := 0;
  while (i < Count) and (Items[i].Name <> _name) do
    Inc(i);
  if (i < Count) then
    IndexOf := i;
  }
  if not MixedCase then
    _name := UpperCase(_name);
  hash := CalcHash(_name);
  i := HashTable[hash];
  while (i <> -1) and (Items[i].Name <> _name) do
    begin
      Inc(hash);
      if hash >= HashSize then
        hash := 0;
      i := HashTable[hash];
    end;
  IndexOf := i;
end;

procedure TSymbolTable.ReHash;
var i: integer;
begin
  for i := 0 to HashSize-1 do
    HashTable[i] := -1;
  for i := 0 to Count-1 do
    AddHash(Items[i].Name,i);
end;

procedure TSymbolTable.SetHashSize(_sz: integer);
begin
  HashSize := _sz;
  SetLength(HashTable,HashSize);
  ReHash;
end;

end.

