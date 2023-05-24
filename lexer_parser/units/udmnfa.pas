unit udmnfa;

{$mode ObjFPC}{$H+}

{
    NFA - Construct a Non-deterministic Finite Automata

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

interface

//
// This is the NFA unit which defines the TSet type for large
// sets, and the TNFA type to host NFA tables.
//
// The TSetIndex type defines the set index, normally Word which allows up
// to 65535 set entries. This is going to be adequate for most real world
// applications.
//

uses
  Classes, SysUtils, Generics.Collections, udmset;

const
  NULL_INDEX = $FFFF;
  NULL_NFA_TOKEN = $FFFF;
  TOKEN_EOF = 0;
  TOKEN_ERROR = 1;
  TOKEN_COMMENT = 2;
  TOKEN_WHITESPACE = 3;

type

  TNFAToken = Word; // 0=EOF, 1=Error, 2 onwards = valid tokens
                    // Suggest 2 for comment and 3 for whitespace

  TTokenAcceptType = (atNormal,atSymbol,atKeyword);

  TCharSet = set of char;

  TSetIndex = Word;

  TDmSetWord = class(specialize TDmSet<TSetIndex>);

  TNFAFragment = record
    FirstNode: TSetIndex;
    LastNode:  TSetIndex;
  end;

  TNFARecord = class(TObject)
    public
      NodeNumber:   TSetIndex;
      Accepting:    TNFAToken;
      AcceptType:   TTokenAcceptType;
      Destinations: array[char] of TSetIndex;
      Epsilons:     TDmSetWord;
      constructor Create;
      destructor  Destroy; override;
      procedure Dump(_strm: TStream);
  end;

  TNFA = class(specialize TObjectList<TNFARecord>)
    public
      function  AllocateNode: TNFARecord;
      function  AllocateNode(_cs: TCharSet; _idx: TSetIndex): TNFARecord;
      function  Dictionary: TCharSet;
      procedure DumpList(_strm: TStream);
      procedure DumpTable(_strm: TStream);
      function  MakeFragCSet(_cs: TCharSet): TNFAFragment;
      function  MakeFragText(_s: string; _mixedcase: boolean = False): TNFAFragment;
      function  OperatorJoin(const _frag1, _frag2: TNFAFragment): TNFAFragment;
      function  OperatorOr(const _frag1, _frag2: TNFAFragment): TNFAFragment;
      function  OperatorPlus(const _frag: TNFAFragment): TNFAFragment;
      function  OperatorQuestion(const _frag: TNFAFragment): TNFAFragment;
      function  OperatorStar(const _frag: TNFAFragment): TNFAFragment;
      procedure SpreadEpsilons;
  end;


function SanitisedChar(_c: char): string; // Forward declaration


implementation

uses
  strutils;


function SanitisedChar(_c: char): string;
begin
  if _c <= #32 then
    case _c of
     #9:  SanitisedChar := 'HT';
     #10: SanitisedChar := 'LF';
     #12: SanitisedChar := 'FF';
     #13: SanitisedChar := 'CR';
     #27: SanitisedChar := 'ES';
     #32: SanitisedChar := 'SP';
     otherwise
       SanitisedChar := '??';
    end // case
  else
    SanitisedChar := _c;
end;


//==============================================================================
//
//  TNFARecord code
//
//==============================================================================

constructor TNFARecord.Create;
var c: char;
begin
  inherited Create;
  Accepting := NULL_NFA_TOKEN; // Assume not an accepting condition for now
  AcceptType := atNormal;      // Assume not a symbol or keyword for now
  for c in char do
    Destinations[c] := NULL_INDEX;
  Epsilons := TDmSetWord.Create;
end;

destructor TNFARecord.Destroy;
begin
  FreeAndNil(Epsilons);
  inherited Destroy;
end;

procedure TNFARecord.Dump(_strm: TStream);
const CRLF = #13 + #10;
var c: char;
    e: TSetIndex;
  procedure Line(const _s: string);
  begin
    _strm.WriteAnsiString(_s + CRLF);
  end;
  procedure Line(const _fmt: string; _args: array of const);
  begin
    Line(Format(_fmt,_args));
  end;
begin
  Line('Dumping node number %d',[NodeNumber]);
  // Do character sets
  for c in char do
    if Destinations[c] <> NULL_INDEX then
      Line('  Character %s --> state %d',[c,Destinations[c]]);
  // Do epsilons
  for e in Epsilons do
    Line('  Epsilon --> state %d',[e]);
  // Do accepting condition
  if Accepting <> NULL_NFA_TOKEN then
    Line('  ACCEPTING CONDITION WITH TOKEN INDEX %d',[Accepting]);
  Line('');
end;


//==============================================================================
//
//  TNFA code
//
//==============================================================================

function TNFA.AllocateNode: TNFARecord;
var _r: TNFARecord;
begin
  _r := TNFARecord.Create;
  _r.NodeNumber := Count;
  _r.Epsilons.Add(_r.NodeNumber);
  Add(_r);
  Result := _r;
end;

function TNFA.AllocateNode(_cs: TCharSet; _idx: TSetIndex): TNFARecord;
var c: char;
    _r: TNFARecord;
begin
  _r := AllocateNode{%H-};
  for c in _cs do
    _r.Destinations[c] := _idx;
  Result := _r;
end;

function TNFA.Dictionary: TCharSet;
var i: integer;
    c: char;
begin
  Result := [];
  for i := 0 to Count-1 do
    for c in char do
      if Items[i].Destinations[c] <> NULL_INDEX then
        Result := Result + [c];
end;

procedure TNFA.DumpList(_strm: TStream);
var _r: TNFARecord;
begin
  for _r in Self do
    _r.Dump(_strm);
end;

procedure TNFA.DumpTable(_strm: TStream);
const CRLF = #13 + #10;
var _cs: TCharSet;
    i:   TSetIndex;
    c:   char;
    s:   string;
    maxdigits: integer;
  procedure Line(const _s: string);
  begin
    _strm.WriteAnsiString(_s + CRLF);
  end;
  procedure Line(const _fmt: string; _args: array of const);
  begin
    Line(Format(_fmt,_args));
  end;
begin
  // Work out how many digits could be used
  maxdigits := 1;
  i := Count-1;
  while i >= 10 do
    begin
      i := i div 10;
      Inc(maxdigits);
    end;
  // First make a composite of all the character sets used
  _cs := Dictionary;
  // Do the table headings
  s := 'State Accept';
  for c in _cs do
    s := s + PadLeft(SanitisedChar(c),maxdigits+1);
  s := s + ' Epsilons';
  Line(s);
  s := '----- ------';
  for c in _cs do
    s := s + ' ' + StringOfChar('-',maxdigits);
  s := s + ' --------';
  Line(s);
  // Do the table data
  for i := 0 to Count-1 do
    begin
      s := PadLeft(IntToStr(i),5) + ' ';
      if Items[i].Accepting <> NULL_NFA_TOKEN then
        begin
          s := s + PadLeft(IntToStr(Items[i].Accepting),5);
          case Items[i].AcceptType of
            atNormal:  s := s + ' ';
            atSymbol:  s := s + 'S';
            atKeyword: s := s + 'K';
          end;
        end
      else
        s := s + StringOfChar(' ',6);
      for c in _cs do
        if Items[i].Destinations[c] <> NULL_INDEX then
          s := s + PadLeft(IntToStr(Items[i].Destinations[c]),maxdigits+1)
        else
          s := s + StringOfChar(' ',maxdigits+1);
      s := s + ' ';
      s := s + Items[i].Epsilons.AsString;
      {
      for j := 0 to Items[i].Epsilons.Count-1 do
        if j <> 0 then
          s := s + ',' + IntToStr(Items[i].Epsilons[j])
        else
          s := s + IntToStr(Items[i].Epsilons[j]);
      }
      Line(s);
    end;
end;

function TNFA.MakeFragCSet(_cs: TCharSet): TNFAFragment;
var node1, node2: TNFARecord;
    c: char;
begin
  // Allocate the nodes
  node1 := AllocateNode;
  node2 := AllocateNode;
  // Now set up the text and link the nodes
  for c in _cs do
    node1.Destinations[c] := node2.NodeNumber;
  Result.FirstNode := node1.NodeNumber;
  Result.LastNode  := node2.NodeNumber;
end;

function TNFA.MakeFragText(_s: string; _mixedcase: boolean = False): TNFAFragment;
var i: integer;
    a: array of TNFARecord;
    nxtindex: TSetIndex;
    c: char;
begin
  if not _mixedcase then
    _s := UpperCase(_s);
  // Allocate the nodes
  SetLength(a{%H-},Length(_s)+1);
  for i := 0 to Length(_s) do
    a[i] := AllocateNode;
  // Now set up the text and link the nodes
  for i := 0 to Length(_s)-1 do
    begin
      nxtindex := a[i+1].NodeNumber;
      c := _s[i+1];
      a[i].Destinations[c] := nxtindex;
      if (not _mixedcase) and (c in ['A'..'Z']) then
        a[i].Destinations[Chr(Ord(c)+Ord('a')-Ord('A'))] := nxtindex;
    end;
  Result.FirstNode := a[0].NodeNumber;
  Result.LastNode  := a[Length(_s)].NodeNumber;
end;

function TNFA.OperatorJoin(const _frag1, _frag2: TNFAFragment): TNFAFragment;
begin
  Items[_frag1.LastNode].Epsilons.Add(Items[_frag2.FirstNode].NodeNumber);
  Result.FirstNode := _frag1.FirstNode;
  Result.LastNode  := _frag2.LastNode;
end;

function TNFA.OperatorOr(const _frag1, _frag2: TNFAFragment): TNFAFragment;
var node1, node4: TNFARecord;
begin
  // Allocate the nodes
  node1 := AllocateNode;
  node4 := AllocateNode;
  // Set up the links
  node1.Epsilons.Add(_frag1.FirstNode);
  node1.Epsilons.Add(_frag2.FirstNode);
  Items[_frag1.LastNode].Epsilons.Add(node4.NodeNumber);
  Items[_frag2.LastNode].Epsilons.Add(node4.NodeNumber);
  // and return results
  Result.FirstNode := node1.NodeNumber;
  Result.LastNode  := node4.NodeNumber;
end;

function TNFA.OperatorPlus(const _frag: TNFAFragment): TNFAFragment;
var node1, node4: TNFARecord;
begin
  // Allocate the nodes
  node1 := AllocateNode;
  node4 := AllocateNode;
  // Set up the links
  node1.Epsilons.Add(_frag.FirstNode);
  Items[_frag.LastNode].Epsilons.Add(node4.NodeNumber);
  node4.Epsilons.Add(_frag.FirstNode);
  // and return results
  Result.FirstNode := node1.NodeNumber;
  Result.LastNode  := node4.NodeNumber;
end;

function TNFA.OperatorQuestion(const _frag: TNFAFragment): TNFAFragment;
var node1: TNFARecord;
begin
  // Allocate the extra node
  node1 := AllocateNode;
  // Set up the links
  node1.Epsilons.Add(_frag.FirstNode);
  node1.Epsilons.Add(_frag.LastNode);
  // and return results
  Result.FirstNode := node1.NodeNumber;
  Result.LastNode  := _frag.LastNode;
end;

function TNFA.OperatorStar(const _frag: TNFAFragment): TNFAFragment;
var node1, node4: TNFARecord;
begin
  // Allocate the nodes
  node1 := AllocateNode;
  node4 := AllocateNode;
  // Set up the links
  node1.Epsilons.Add(_frag.FirstNode);
  node1.Epsilons.Add(node4.NodeNumber);
  Items[_frag.LastNode].Epsilons.Add(node4.NodeNumber);
  node4.Epsilons.Add(_frag.FirstNode);
  // and return results
  Result.FirstNode := node1.NodeNumber;
  Result.LastNode  := node4.NodeNumber;
end;

procedure TNFA.SpreadEpsilons;
var checked: array of boolean;
    i,j:     integer;
  procedure CheckEpsilon(_index: integer);
  var k: integer;
      ec:      integer;
  begin
    if not checked[_index] then
      begin
        Items[i].Epsilons.Union(Items[_index].Epsilons);
        checked[_index] := True;
        ec := Items[_index].Epsilons.Count;
        for k := 0 to ec-1 do
          CheckEpsilon(Items[_index].Epsilons[k]);
      end;
  end;
begin
  // If  node 0 contains epsilons 0,1
  // and node 1 contains epsilons 1,3,5
  // then it follows that node 0 should contain 0,1,3,5
  // So we spread them!
  SetLength(checked{%H-},Count);
  for i := 0 to Count-1 do
    begin
      // First clear each marker
      for j := 0 to Count-1 do
        checked[j] := False;
      // Now check the epsilons
      CheckEpsilon(i);
    end;
end;

end.

