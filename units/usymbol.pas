unit usymbol;

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2022 Duncan Munro

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


{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

//------------------------------------------------------------------------------
//
//  usymbol.pas
//
//  Handle symbol table operations
//
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, fgl, uutility, uifstack;

const
  DEFAULT_HASH_SIZE = 1000;        // Initial size for hash table
  HASH_MARGIN = 4;                 // Hash table is > 4 times size of sym table
  HASH_MULTIPLIER = 8;             // Hash table multiplies by 8 on resize


type

  TSymType = (stInteger16,stString);

  TSymbolEntry = record
    SymName:     string;      // Symbol name
    SymType:     TSymType;    // Symbol type
    SymPass:     integer;     // Pass defined in (1 or 2)
    SymHasValue: boolean;     // True if a number value is associate with symbol
    SymUsed:     boolean;     // True if the symbol has been referenced
    SymValue:    uint16;      // Value
    SymValueS:   string;      // Value of string
    class operator = (se1, se2: TSymbolEntry): boolean;
  end;

  TSymbolTable = class (specialize TFPGList<TSymbolEntry>)
    private
      FHashTable: array of integer;
      FHashSize:  integer;
      FIfAllowed: boolean;
      procedure AddHash(_name: string; _recno: integer);
      function  CalcHash(_name: string): integer;
      procedure CheckHashSize;
      procedure ClearHash;
      procedure ReHash;
    public
      constructor Create;
      destructor Destroy; override;
      function  Define(_pass: integer; const _name: string; _value: integer = -1): string;
      function  Define(_pass: integer; const _name: string; _value: string): string;
      function  Define(_pass: integer; _isstring: boolean; const _name: string; const _variable: string; _overwrite: boolean = False): string;
      procedure Dump(_sl: TStringList);
      function  IndexOf(_key: string): integer;
      function  Exists(_name: string): boolean;
      function  ExistsInPass(_pass: integer; _name: string): boolean;
      procedure SetUsed(_index: integer);
      procedure SetUsed(const _name: string);
      procedure SortByAddr;
      procedure SortByName;
      function  Undefine(const _name: string): string;
      function  Variable(_pass: integer; _name: string; _default: string; _if: TIfStack): string;
    published
      property IfAllowed: boolean read FIfAllowed write FIfAllowed;
  end;


implementation

{ Class operators }

class operator TSymbolEntry.= (se1, se2: TSymbolEntry): boolean;
begin
  Result := (se1.SymName = se2.SymName);
end;

{ Utility routines }

function NameCompareFunc(const se1, se2: TSymbolEntry): integer;
begin
  if se1.SymName = se2.SymName then
    result := 0
  else if se1.SymName > se2.SymName then
    result := 1
  else
    result := -1;
end;

function AddrCompareFunc(const se1, se2: TSymbolEntry): integer;
var v1,v2: integer;
begin
  v1 := se1.SymValue;
  v2 := se2.SymValue;
  if not se1.SymHasValue then
    v1 := -1;
  if not se2.SymHasValue then
    v2 := -1;
  if v1 = v2 then
    Result := NameCompareFunc(se1,se2)
  else if v1 > v2 then
    Result := 1
  else
    Result := -1;
end;

{ TSymbolTable }

constructor TSymbolTable.Create;
begin
  inherited Create;
  FHashSize := NextPrime(DEFAULT_HASH_SIZE);
  SetLength(FHashTable,FHashSize);
  ClearHash;
end;

destructor TSymbolTable.Destroy;
begin
  inherited Destroy;
end;

procedure TSymbolTable.AddHash(_name: string; _recno: integer);
var index: integer;
    done:  boolean;
begin
  index := CalcHash(_name);
  done := False;
  while not done do
    if FHashTable[index] < 0 then
      begin
        FHashTable[index] := _recno;
        done := True;
      end
    else
      begin
        Inc(index);
        if index >= FHashSize then
          index := 0;
      end;
end;

function TSymbolTable.CalcHash(_name: string): integer;
var hashval: int64;
    i:       integer;
begin
  {%H-}_name := UpperCase(_name);
  hashval := 0;
  for i := 1 to Length(_name) do
    begin
      hashval := hashval * 257;
      hashval := hashval + Ord(_name[i]);
      hashval := hashval xor (hashval shr 32);
      hashval := hashval and $7fffffff;
    end;
  Result := hashval mod FHashSize;
end;

procedure TSymbolTable.CheckHashSize;
begin
  if (Count * HASH_MARGIN) > FHashSize then
    begin // Englarge the hash table
      FHashSize := NextPrime(FHashSize * HASH_MULTIPLIER);
      SetLength(FHashTable,FHashSize);
      // Now re-hash everything
      ReHash;
    end;
end;

procedure TSymbolTable.ClearHash;
var i: integer;
begin
  for i := 0 to FHashSize-1 do
    FHashTable[i] := -1;
end;

function TSymbolTable.Define(_pass: integer; const _name: string; _value: integer): string;
var entry: TSymbolEntry;
    index: integer;
begin
  Result := '';
  case _pass of
    1:  begin
          if Exists(_name) then
            Result := 'Symbol ' + _name + ' already exists'
          else
            begin
              entry.SymName     := UpperCase(_name);
              entry.SymPass     := _pass;
              entry.SymUsed     := False;
              if _value >= 0 then
                begin
                  entry.SymHasValue := True;
                  entry.SymValue    := _value;
                end
              else
                begin
                  entry.SymHasValue := False;
                  entry.SymValue    := 0;
                end;
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
        end;
    2:  begin
          if Exists(_name) then
            begin
              index := IndexOf(_name);
              entry := Items[index];
              entry.SymPass := _pass;
              Items[index] := entry;
            end
          else
            begin  // Was .UNDEFINEd in pass 1, need to re-define in pass 2
              entry.SymName     := UpperCase(_name);
              entry.SymPass     := _pass;
              entry.SymUsed     := False;
              if _value >= 0 then
                begin
                  entry.SymHasValue := True;
                  entry.SymValue    := _value;
                end
              else
                begin
                  entry.SymHasValue := False;
                  entry.SymValue    := 0;
                end;
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
        end;
  end; // Case
end;

function TSymbolTable.Define(_pass: integer; const _name: string; _value: string): string;
var entry: TSymbolEntry;
    index: integer;
begin
  Result := '';
  case _pass of
    1:  begin
          if Exists(_name) then
            Result := 'Symbol ' + _name + ' already exists'
          else
            begin
              entry.SymName     := UpperCase(_name);
              entry.SymPass     := _pass;
              entry.SymUsed     := False;
              if _value <> '' then
                begin
                  entry.SymHasValue := True;
                  entry.SymValueS   := _value;
                  entry.SymType     := stString;
                end
              else
                begin
                  entry.SymHasValue := False;
                  entry.SymValue    := 0;
                end;
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
        end;
    2:  begin
          if Exists(_name) then
            begin
              index := IndexOf(_name);
              entry := Items[index];
              entry.SymPass := _pass;
              Items[index] := entry;
            end
          else
            begin  // Was .UNDEFINEd in pass 1, need to re-define in pass 2
              entry.SymName     := UpperCase(_name);
              entry.SymPass     := _pass;
              entry.SymUsed     := False;
              if _value <> '' then
                begin
                  entry.SymHasValue := True;
                  entry.SymValueS   := _value;
                  entry.SymType     := stString;
                end
              else
                begin
                  entry.SymHasValue := False;
                  entry.SymValue    := 0;
                end;
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
        end;
  end; // Case
end;

function TSymbolTable.Define(_pass: integer; _isstring: boolean; const _name: string; const _variable: string; _overwrite: boolean = False): string;
var entry: TSymbolEntry;
    index: integer;
begin
  Result := '';
  case _pass of
    1:  begin
          if Exists(_name) then
            begin
              if not _overwrite then
                Result := 'Symbol ' + _name + ' already exists'
              else
                begin
                  index := IndexOf(_name);
                  entry := Items[index];
                  entry.SymPass     := _pass;
                  entry.SymHasValue := True;
                  if _isstring then
                    begin
                      entry.SymType := stString;
                      entry.SymValue  := 0;
                      entry.SymValueS := _variable;
                      entry.SymHasValue := True;
                    end
                  else
                    begin
                      entry.SymType := stInteger16;
                      entry.SymValue := StrToInt(_variable);
                      entry.SymValueS := '';
                    end;
                  Items[index] := entry;
                end;
            end
          else
            begin
              entry.SymName     := UpperCase(_name);
              entry.SymPass     := _pass;
              entry.SymHasValue := True;
              entry.SymUsed     := False;
              if _isstring then
                begin
                  entry.SymType := stString;
                  entry.SymValue  := 0;
                  entry.SymValueS := _variable;
                  entry.SymHasValue := True;
                end
              else
                begin
                  entry.SymType := stInteger16;
                  entry.SymValue := StrToInt(_variable);
                  entry.SymValueS := '';
                end;
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
        end;
    2:  begin
          if Exists(_name) then
            begin
              index := IndexOf(_name);
              entry := Items[index];
              entry.SymPass := _pass;
              entry.SymHasValue := True;
              if _isstring then
                begin
                  entry.SymType := stString;
                  entry.SymValue  := 0;
                  entry.SymValueS := _variable;
                  entry.SymHasValue := True;
                end
              else
                begin
                  entry.SymType := stInteger16;
                  entry.SymValue := StrToInt(_variable);
                  entry.SymValueS := '';
                end;
              Items[index] := entry;
            end
          else
            begin  // Was .UNDEFINEd in pass 1, need to re-define in pass 2
              entry.SymName     := UpperCase(_name);
              entry.SymPass     := _pass;
              entry.SymHasValue := True;
              entry.SymUsed     := False;
              if _isstring then
                begin
                  entry.SymType := stString;
                  entry.SymValue  := 0;
                  entry.SymValueS := _variable;
                  entry.SymHasValue := True;
                end
              else
                begin
                  entry.SymType := stInteger16;
                  entry.SymValue := StrToInt(_variable);
                  entry.SymValueS := '';
                end;
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
          end;
  end; // Case
end;

procedure TSymbolTable.Dump(_sl: TStringList);
var i: integer;
    marker: string;
    sym: TSymbolEntry;
    namestr: string;
begin
  _sl.Add('HEX  DEC   ? NAME');
  _sl.Add('---- ----- - ----');
  for i := 0 to Count-1 do
    begin
      sym := Items[i];
      marker := ' ';
      if not sym.SymUsed then
        marker := '*';
      namestr := sym.SymName;
      if sym.SymType = stString then
          namestr := namestr + ' "' + sym.SymValueS + '"';
      if (sym.SymHasValue) and (sym.SymType <> stString) then
        _sl.Add(Format('%4.4X %5d %s %s',[sym.SymValue,sym.SymValue,marker,namestr]))
      else
        _sl.Add(Format('           %s %s',[marker,namestr]));
    end;
end;

function TSymbolTable.Exists(_name: string): boolean;
begin
  Result := IndexOf(_name) >= 0;
end;

function TSymbolTable.ExistsInPass(_pass: integer; _name: string): boolean;
begin
  Result := (IndexOf(_name) >= 0) and (Items[IndexOf(_name)].SymPass = _pass);
end;

function TSymbolTable.IndexOf(_key: string): integer;
var index: integer;
    done:  boolean;
begin
  {%H-}_key := UpperCase(_key);
  Result := -1;
  index := CalcHash(_key);
  done := False;
  while not done do
    begin
      if FHashTable[index] < 0 then
        done := True
      else if Items[FHashTable[index]].SymName = _key then
        begin
          done := True;
          Result := FHashTable[index];
        end
      else
        begin
          Inc(index);
          if index >= FHashSize then
            index := 0;
        end;
    end;
end;

procedure TSymbolTable.ReHash;
var i: integer;
begin
  ClearHash;
  for i := 0 to Count-1 do
    AddHash(Items[i].SymName,i);
end;

procedure TSymbolTable.SetUsed(_index: integer);
var rec: TSymbolEntry;
begin
  rec := Items[_index];
  rec.SymUsed := True;
  Items[_index] := rec;
end;

procedure TSymbolTable.SetUsed(const _name: string);
begin
  if IndexOf(_name) >= 0 then
    SetUsed(IndexOf(_name));
  {
  else
    raise Exception.Create(Format('Attempt to set symbol %s as used when it does not exist',[_name]));
  }
end;

procedure TSymbolTable.SortByAddr;
begin
  Sort(@AddrCompareFunc);
end;

procedure TSymbolTable.SortByName;
begin
  Sort(@NameCompareFunc);
end;

function TSymbolTable.Undefine(const _name: string): string;
var index: integer;
begin
  Result := '';
  index := IndexOf(_name);
  if index < 0 then
    Result := 'Cannot UNDEFINE non-existent symbol ' + _name
  else
    begin
      Delete(index);
      ReHash;
    end;
end;

function TSymbolTable.Variable(_pass: integer; _name: string; _default: string; _if: TIfStack): string;
var i: integer;
begin
  Result := '';
  {%H-}_name := UpperCase(_name);
  i := IndexOf(_name);
  if (i >= 0) and (not Items[i].SymHasValue) then
    raise Exception.Create(Format('Attempt to use the value of a symbol %s with no defined value',[_name]));
  if i >= 0 then
    SetUsed(i);
  case _pass of
    1:  begin
          if i < 0 then
            result := _default
          else
            case Items[i].SymType of
              stString:    Result := Items[i].SymValueS;
              stInteger16: Result := IntToStr(Items[i].SymValue);
              otherwise
                raise Exception.Create(Format('Variable type not catered for in lookup of %s',[_Name]));
            end; // case
        end;
    2:  begin
          if i < 0 then
            begin
              if _if.Allowed then
                raise Exception.Create('Symbol ' + _name + ' not found')
              else
                result := _default;
             end
          else
            case Items[i].SymType of
              stString:    Result := Items[i].SymValueS;
              stInteger16: Result := IntToStr(Items[i].SymValue);
              otherwise
                raise Exception.Create(Format('Variable type not catered for in lookup of %s',[_Name]));
            end; // case
        end;
  end;
end;

end.

