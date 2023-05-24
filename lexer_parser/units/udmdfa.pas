unit udmdfa;

{$mode ObjFPC}{$H+}

{
    DFA - Determinisitic Finite Automata module
    Constructs a DFA by creating from an NFA (relies on unfa.pas)

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

uses
  Classes, SysUtils, udmnfa, Generics.Collections;

const
  DFA_VERSION_HIGH = 0;
  DFA_VERSION_LOW  = 0;
  DFA_HEADER_MAGIC1 = $616E6942;
  DFA_HEADER_MAGIC2 = $44207972;
  DFA_HEADER_MAGIC3 = $1A0A4146;
  DFA_HEADER_ID     = $30384158;


type
  TDFARecord = class(TObject)
    public
      Index: TSetIndex;
      Title: TDmSetWord;
      Expanded:     boolean;
      Accepting:    TNFAToken;
      AcceptType:   TTokenAcceptType;
      Destinations: array[char] of TSetIndex;
      constructor Create;
      destructor Destroy; override;
  end;

  TDFA = class(specialize TObjectList<TDFARecord>)
    public
      function  AddTitle(_set: TDmSetWord): TSetIndex;
      procedure Combine(_a, _b: TSetIndex);
      procedure CreateFromNFA(_nfa: TNFA);
      function  Dictionary: TCharSet;
      function  DictionaryCount: integer;
      procedure DumpTable(_strm: TStream; _showstate: boolean = True);
      function  FindTitle(_set: TDmSetWord): TSetIndex;
      procedure Optimise;
      procedure PopulateRecord(_index: integer; _nfa: TNFA; _dictionary: TCharSet);
      procedure ReadBinary(const _filename: string);
      procedure ReadBinary(_strm: TStream);
      procedure Rename;
      procedure RenameItem(_from, _to: TSetIndex);
      procedure WriteBinary(const _filename: string);
      procedure WriteBinary(_strm: TStream);
      procedure SetAccept(_index: TSetIndex; _nfa: TNFA);
  end;

implementation

uses
  StrUtils;

//==============================================================================
//
//  TDFARecord code
//
//==============================================================================

constructor TDFARecord.Create;
var c: char;
begin
  inherited Create;
  Title := TDmSetWord.Create;
  Accepting := NULL_NFA_TOKEN;
  AcceptType := atNormal;
  for c in char do
    Destinations[c] := NULL_INDEX;
end;

destructor TDFARecord.Destroy;
begin
  FreeAndNil(Title);
  inherited Destroy;
end;



//==============================================================================
//
//  TDFA code
//
//==============================================================================

function TDFA.AddTitle(_set: TDmSetWord): TSetIndex;
var _dr: TDFARecord;
begin
  Result := Count;
  _dr := TDFARecord.Create;
  _dr.Title.Union(_set);
  _dr.Index := Count;
  // Finally add to the list
  Add(_dr);
end;

procedure TDFA.Combine(_a, _b: TSetIndex);
var i: TSetIndex;
    c: char;
begin
  // Anything pointing to _b must now point to _a
  for i := 0 to Count-1 do
    if Items[i].Index <> _b then  // skip _b as we are deleting anyway
      for c in char do
        if Items[i].Destinations[c] = Items[_b].Index then
          Items[i].Destinations[c] := Items[_a].Index;
  // Make a "super union" of the title sets for _a and _b
  Items[_a].Title.Union(Items[_b].Title);
  // Finally... delete the record we don't need
  Delete(_b);
end;


procedure TDFA.CreateFromNFA(_nfa: TNFA);
var c: char;
    j: integer;
    _dict: TCharSet;
    _set: TDmSetWord;
    isempty: boolean;
    chk:     TSetIndex;
begin
  Clear; // Make sure DFA is empty
  _dict := _nfa.Dictionary;
  // Create first record
  _set := TDmSetWord.Create;
  try
    _set.Add(0);
    AddTitle(_set);
    SetAccept(0,_nfa);
    // Check if whole row is empty
    isempty := True;
    for c in _dict do
      if Items[0].Destinations[c] <> NULL_INDEX then
         isempty := False;
    // Action if it is empty...
    if isempty then
      begin
        // Construct the union of sets which the titles point to
        _set.Clear;
        for j := 0 to Items[0].Title.Count-1 do
          begin
            chk := Items[0].Title[j];
            _set.Union(_nfa.Items[chk].Epsilons);
          end;
        Items[0].Title.Clear;
        Items[0].Title.Union(_set);
      end;
  finally
    _set.Free;
  end;
  PopulateRecord(0,_nfa,_dict);
  // Finally, optimise the DFA and rename if required
  Optimise;
  Rename;
end;

function TDFA.Dictionary: TCharSet;
var i: integer;
    c: char;
begin
  Result := [];
  for i := 0 to Count-1 do
    for c in char do
      if Items[i].Destinations[c] <> NULL_INDEX then
        Result := Result + [c];
end;

function TDFA.DictionaryCount: integer;
var c: char;
    cs: TCharSet;
begin
  cs := Dictionary;
  Result := 0;
  for c in char do
    if c in cs then
      Inc(Result);
end;

procedure TDFA.DumpTable(_strm: TStream; _showstate: boolean);
const CRLF = #13 + #10;
      STATE_WIDTH = 32;
var c: char;
    s: string;
    maxdigits: integer;
    i: TSetIndex;
    _cs: TCharSet;
  function IndexToLetters(_index: TSetIndex): string;
  begin
    if _index < 26 then
      Result := Chr(_index + Ord('A'))
    else
      Result := Chr((_index div 26) + Ord('A') - 1) + Chr((_index mod 26) + Ord('A'))
  end;
  procedure Line(const _s: string);
  begin
                                  ;
    _strm.WriteAnsiString(_s + CRLF);
  end;
  procedure Line(const _fmt: string; _args: array of const);
  begin
    Line(Format(_fmt,_args));
  end;

begin
  // Work out how many digits could be used
  maxdigits := 2;
  if Count > 26*27 then
    maxdigits := 3;
  // First make a composite of all the character sets used
  _cs := Dictionary;
  // Do the table headings
  s := 'LT ';
  if _showstate then
    s := s + PadRight('State',STATE_WIDTH+1);
  s := s + 'Accept';
  for c in _cs do
    s := s + PadLeft(SanitisedChar(c),maxdigits+1);
  Line(s);
  s := '-- ';
  if _showstate then
    s := s + StringOfChar('-',STATE_WIDTH) + ' ';
  s := s + '------';
  for c in _cs do
    s := s + ' ' + StringOfChar('-',maxdigits);
  Line(s);
  // Do the table data
  for i := 0 to Count-1 do
    begin
      s := PadLeft(IndexToLetters(Items[i].Index),2) + ' ';
      if _showstate then
        s := s + Items[i].Title.AsString(STATE_WIDTH) + ' ';
      if Items[i].Accepting <> NULL_NFA_TOKEN then
        begin
          s := s + PadLeft(IntToStr(Items[i].Accepting),5);
          case Items[i].AcceptType of
           atNormal  : s := s + ' ';
           atSymbol  : s := s + 'S';
           atKeyword : s := s + 'K';
          end;
        end
      else
        s := s + StringOfChar(' ',6);
      for c in _cs do
        if Items[i].Destinations[c] = NULL_INDEX then
          s := s + PadLeft('',maxdigits+1)
        else
          s := s + PadLeft(IndexToLetters(Items[i].Destinations[c]),maxdigits+1);
      Line(s);
    end;
end;

function TDFA.FindTitle(_set: TDmSetWord): TSetIndex;
var found: boolean;
    index: TSetIndex;
begin
  index := 0;
  found := False;
  Result := NULL_INDEX;
  while (not found) and (index < Count) do
    begin
      if _set.Equal(Items[index].Title) then
        found := True
      else
        Inc(index);
    end;
  if found then
    Result := index;
end;

procedure TDFA.Optimise;
var i: TSetIndex;
    j: TSetIndex;
    c: char;
    matching: boolean;
begin
  if Count >= 2 then
    for i := 0 to Count-2 do
      for j := Count-1 downto i+1 do
        begin
          // Check records i and j to see if the destinations are the same
          // If so, we can combine them to make the DFA a bit smaller
          matching := True; // Assume matched for now
          if Items[i].Accepting <> Items[j].Accepting then
            matching := False
          else
            for c in char do
              if Items[i].Destinations[c] <> Items[j].Destinations[c] then
                matching := False;
          if matching then
            Combine(i,j);
        end;
end;

procedure TDFA.PopulateRecord(_index: integer; _nfa: TNFA; _dictionary: TCharSet);
var c: char;
    i: TSetIndex;
    j: integer;
    tc: integer;
    _set: TDmSetWord;
    chk:  TSetIndex;
    dest: TSetIndex;
begin
  if Items[_index].Expanded then
    raise Exception.Create('Attempting to populate record which is already populated');
  _set := TDmSetWord.Create;
  try
    for c in _dictionary do
      begin
        _set.Clear;
        tc := Items[_index].Title.Count;
        for j := 0 to tc-1 do
          begin
            chk := Items[_index].Title[j];
            if _nfa.Items[chk].Destinations[c] <> NULL_INDEX then
               _set.Union(_nfa.Items[_nfa.Items[chk].Destinations[c]].Epsilons);
          end;
        if _set.Empty then
           dest := NULL_INDEX
        else
          begin
            dest := FindTitle(_set);
            if dest = NULL_INDEX then
              begin
               dest := AddTitle(_set);
               SetAccept(dest,_nfa);
              end;
          end;
        Items[_index].Destinations[c] := dest;
      end;
  finally
    FreeAndNil(_set);
  end;
  Items[_index].Expanded := True;
  for i := 0 to Count-1 do
    if not Items[i].Expanded then
      PopulateRecord(i,_nfa,_dictionary); // Recursive!
end;

procedure TDFA.ReadBinary(const _filename: string);
var _strm: TFileStream;
begin
  _strm := TFileStream.Create(_filename,fmOpenRead);
  try
    ReadBinary(_strm);
  finally
    FreeAndNil(_strm);
  end;
end;

procedure TDFA.ReadBinary(_strm: TStream);
var checksum:  WORD;
    _dictsize: WORD;
    _rcount:   WORD;
    _index:    WORD;
    _accepting: WORD;
    _acctype:   WORD;
    _dests:     array of WORD;
    _rec:       TDFARecord;
    _csum:      WORD;
    c:         char;
    i,j:       integer;
    masks:     array[0..31] of byte;
    magic1,
    magic2,
    magic3,
    id:        DWORD;
    version:   WORD;
  procedure BumpChecksum(_w: word);
  begin
    checksum := (integer(checksum) + integer(_w)) and $ffff;
  end;
  function ReadByte: byte;
  begin
    Result := _strm.ReadByte;
    BumpChecksum(Result);
  end;
  function ReadDWord: cardinal;
  begin
    Result := _strm.ReadDWord;
    BumpChecksum(Result and $FFFF);
    BumpChecksum(Result shr 16);
  end;
  function ReadWord: word;
  begin
    Result := _strm.ReadWord;
    BumpChecksum(Result);
  end;
begin
  // Format is:
  //    DWORD      magic_marker1    contains 61 6E 69 42   ("Bina")
  //    DWORD      magic_marker2    contains 44 20 59 52   ("ry D")
  //    DWORD      magic_marker3    contains 1A 0A 41 46   ("FA" + <lf> + <eof>)
  //    DWORD      identifier       contains 30 38 41 58   ("XA80")
  //    WORD       version          Version no. low followed by high
  //    WORD       dictionary_count Number of elements in dictionary
  //    BYTE[32]   dictionary       Bit mask to show which letters are used
  //    WORD       record_count     Number of records in this file
  //    WORD       checksum         Word checksum of all the above
  //
  // Each record is:
  //    WORD    state_no        Is sequential 0..n-1
  //    WORD    token_id        Token ID this produces or $FFFF if none
  //    WORD    token_style     0=Normal, 1=Symbol, 2=keyword
  //    WORD[n] destinations    Each destination if used or $FFFF if none
  //    WORD    checksum        Word checksum of all of the above

  // Initialise
  Clear;
  // Read the header
  checksum := 0;
  magic1  := ReadDWord;
  magic2  := ReadDWord;
  magic3  := ReadDWord;
  id      := ReadDWord;
  if (magic1 <> DFA_HEADER_MAGIC1) or
     (magic2 <> DFA_HEADER_MAGIC2) or
     (magic3 <> DFA_HEADER_MAGIC3) or
     (id     <> DFA_HEADER_ID) then
    raise Exception.Create('Corrupt header in DFA file');
  version := ReadWord;
  _dictsize := ReadWord;
  for i := 0 to 31 do
    masks[i] := ReadByte;
  _rcount := ReadWord;
  _csum := _strm.ReadWord;
  if checksum <> _csum then
    raise Exception.Create('Header checksum error in DFA file');
  // Read each record
  SetLength(_dests{%H-},_dictsize);
  for i := 0 to _rcount-1 do
    begin
      checksum := 0;
      _index := ReadWord;
      if _index <> i then
        raise Exception.Create(Format('Corrupt record number %d in DFA file',[i])) ;
      _accepting := ReadWord;
      _acctype   := ReadWord;
      for j := 0 to _dictsize-1 do
        _dests[j] := ReadWord;
      j := 0;
      _rec := TDFARecord.Create;
      Add(_rec);
      _rec.Accepting  := _accepting;
      _rec.AcceptType := TTokenAcceptType(_acctype);
      _rec.Expanded   := True;
      _rec.Index      := i;
      for c in char do
        if (masks[Ord(c) div 8] and (1 shl (Ord(c) mod 8))) <> 0 then
          begin
           _rec.Destinations[c] := _dests[j];
           Inc(j);
          end;
      _csum := _strm.ReadWord;
      if checksum <> _csum then
        raise Exception.Create(Format('Checksum error reading record %d of DFA',[i]));
    end;
end;

procedure TDFA.Rename;
var i: TSetIndex;
begin
  for i := 0 to Count-1 do
    if Items[i].Index <> i then
      RenameItem(Items[i].Index,i);
end;

procedure TDFA.RenameItem(_from, _to: TSetIndex);
var i: TSetIndex;
    c: char;
begin
  for i := 0 to Count-1 do
    begin
      if Items[i].Index = _from then
        Items[i].Index := _to;
      for c in char do
        if Items[i].Destinations[c] = _from then
          Items[i].Destinations[c] := _to;
    end;
end;

procedure TDFA.SetAccept(_index: TSetIndex; _nfa: TNFA);
var _newaccept: TNFAToken;
    _newtype:   TTokenAcceptType;
    i:          TSetIndex;
    idx:        TSetIndex;
begin
  for i := 0 to Items[_index].Title.Count-1 do
    begin
      idx := Items[_index].Title[i];
      _newaccept := _nfa.Items[idx].Accepting;
      _newtype   := _nfa.Items[idx].AcceptType;
      if _newaccept <> NULL_NFA_TOKEN then
        begin
          // Check if no accept type set up yet
          if Items[_index].Accepting = NULL_NFA_TOKEN then
            begin
              Items[_index].Accepting  := _newaccept;
              Items[_index].AcceptType := _newtype;
            end
          // Check if we already have a token set WITH THE SAME ACCEPTANCE TYPE (normal/symbol/keyword)
          else if Items[_index].AcceptType = _newtype then
            raise Exception.Create(Format('Multiple tokens %d and %d for the same state',[Items[_index].Accepting,_newaccept]))
          // We already have a token there, but it's a different priority
          // If the priority is higher, we will replace. If lower we will skip the
          // item we are reviewing, the existing token will tay
          else if Items[_index].AcceptType < _newtype then
            begin
              Items[_index].Accepting  := _newaccept;
              Items[_index].AcceptType := _newtype;
            end;
        end;
    end;
end;

procedure TDFA.WriteBinary(const _filename: string);
var _strm: TFileStream;
begin
  _strm := TFileStream.Create(_filename,fmCreate);
  try
    WriteBinary(_strm);
  finally
    FreeAndNil(_strm);
  end;
end;

procedure TDFA.WriteBinary(_strm: TStream);
var checksum:  WORD;
    _cs:       TCharSet;
    _dictsize: WORD;
    c:         char;
    i:         integer;
    masks:     array[0..31] of byte;
  procedure BumpChecksum(_w: word);
  begin
    checksum := (integer(checksum) + integer(_w)) and $ffff;
  end;
  procedure WriteByte(_b: byte);
  begin
    _strm.WriteByte(_b);
    BumpChecksum(_b);
  end;
  procedure WriteDWord(_dw: cardinal);
  begin
    _strm.WriteDWord(_dw);
    BumpChecksum(_dw and $FFFF);
    BumpChecksum(_dw shr 16);
  end;
  procedure WriteWord(_w: word);
  begin
    _strm.WriteWord(_w);
    BumpChecksum(_w);
  end;
begin
  // Format is:
  //    DWORD      magic_marker1    contains 61 6E 69 42   ("Bina")
  //    DWORD      magic_marker2    contains 44 20 59 52   ("ry D")
  //    DWORD      magic_marker3    contains 1A 0A 41 46   ("FA" + <lf> + <eof>)
  //    DWORD      identifier       contains 30 38 41 58   ("XA80")
  //    WORD       version          Version no. low followed by high
  //    WORD       dictionary_count Number of elements in dictionary
  //    BYTE[32]   dictionary       Bit mask to show which letters are used
  //    WORD       record_count     Number of records in this file
  //    WORD       checksum         Word checksum of all the above
  //
  // Each record is:
  //    WORD    state_no        Is sequential 0..n-1
  //    WORD    token_id        Token ID this produces or $FFFF if none
  //    WORD    token_style     0=Normal, 1=Symbol, 2=keyword
  //    WORD[n] destinations    Each destination if used or $FFFF if none
  //    WORD    checksum        Word checksum of all of the above

  // Write the header
  checksum := 0;
  WriteDWord(DFA_HEADER_MAGIC1);
  WriteDWord(DFA_HEADER_MAGIC2);
  WriteDWord(DFA_HEADER_MAGIC3);
  WriteDWord(DFA_HEADER_ID);
  WriteWord(DFA_VERSION_LOW + (DFA_VERSION_HIGH shl 8));
  _cs := Dictionary;
  _dictsize := 0;
  for c in _cs do
    Inc(_dictsize);
  WriteWord(_dictsize);
  for i := 0 to 31 do
    masks[i] := 0;
  for c in char do
    if c in _cs then
      masks[Ord(c) div 8] := masks[Ord(c) div 8] or (1 shl (Ord(c) and $07));
  for i := 0 to 31 do
    WriteByte(masks[i]);
  WriteWord(Count);
  WriteWord(checksum);
  // Write each record
  for i := 0 to Count-1 do
    begin
      checksum := 0;
      WriteWord(i);
      WriteWord(Items[i].Accepting);
      WriteWord(Ord(Items[i].AcceptType));
      for c in _cs do
        WriteWord(Items[i].Destinations[c]);
      WriteWord(checksum);
    end;
end;

end.

