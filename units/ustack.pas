unit ustack;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TAsmStackEntryType = (setNone,setIf,setWhile,setRepeat);

  TAsmStackEntry = record
    EntryType:       TAsmStackEntryType;
    Filename:        string;
    LineNumber:      integer;
    EvalResult:      boolean;
    ParentGen:       boolean;
    RepeatRemain:    integer;
    ElseDoneAlready: boolean;
  end;

  TAsmStack = class (specialize TList<TAsmStackEntry>)
    public
      function  CanGenerate: boolean;
      function  MostRecentIfStatus: boolean;
      function  Pop: TAsmStackEntry;
      procedure Push(const _item: TAsmStackEntry);
      function  TOS: TAsmStackEntry;
      function  TOStype: TAsmStackEntryType;
      function  TOSmatch(_item: TAsmStackEntry): boolean;
  end;

implementation

uses
  umessages, lacogen_types;

function TAsmStack.CanGenerate: boolean;
var _tos: TAsmStackEntry;
begin
  if Count = 0 then
    Result := True
  else
    begin
      _tos := TOS;
      case _tos.EntryType of
        setNone:        ErrorObj.Show(ltInternal,X3001_UNHANDLED_CASE_OPTION,['TasmStack.CanGenerate']);
        setIf:          Result := _tos.EvalResult and _tos.ParentGen;
        setWhile:       Result := _tos.EvalResult and _tos.ParentGen;
        setRepeat:      Result := (_tos.RepeatRemain > 0) and _tos.ParentGen;
      end;
    end;
end;

function TAsmStack.MostRecentIfStatus: boolean;
var i: integer;
begin
  Result := True;  // Assume true for now
  i := Count-1;
  while (i >= 0) and (Items[i].EntryType <> setIf) do
    Dec(i);
  if (i >= 0) then
    Result := Items[i].EvalResult;
end;

function TAsmStack.Pop: TAsmStackEntry;
begin
  Result := TOS;
  Delete(Count-1);
end;

procedure TAsmStack.Push(const _item: TAsmStackEntry);
begin
  Add(_item);
end;

function TAsmStack.TOS: TAsmStackEntry;
begin
  if Count = 0 then
    ErrorObj.Show(ltInternal,X3015_POP_FROM_EMPTY_STACK);
  Result := Items[Count-1];
end;

function TAsmStack.TOSmatch(_item: TAsmStackEntry): boolean;
var _tos: TAsmStackEntry;
begin
  if Count = 0 then
    Result := False
  else
    begin
      _tos := TOS;
      Result := (_tos.EntryType  = _item.EntryType) and
                (_tos.Filename   = _item.Filename) and
                (_tos.LineNumber = _item.LineNumber);
    end;
end;

function TAsmStack.TOStype: TAsmStackEntryType;
begin
  if Count = 0 then
    Result := setNone
  else
    Result := Items[Count-1].EntryType;
end;

end.

