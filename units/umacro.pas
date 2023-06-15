unit umacro;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TMacroEntry = class(TObject)
    public
      Name:       string;
      Filename:   string;
      LineNumber: integer;
      Headings:   TStringList;
      Params:     TStringList;
      Content:    TStringList;
      constructor Create;
      destructor Destroy; override;
  end;

  TMacroList = class(specialize TObjectList<TMacroEntry>)
    private
      FCaseSensitive: boolean;
      FStaticSerial:  integer;
    public
      constructor Create;
      function  AllocateSerial: integer;
      function  IndexOf(const _s: string): integer; reintroduce;
      procedure Init;
      property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
  end;

  TMacroStack = class(specialize TStack<TMacroEntry>)
  end;

implementation

uses
  umessages, lacogen_types;

constructor TMacroEntry.Create;
begin
  inherited Create;
  Headings := TStringList.Create;
  Params   := TStringList.Create;
  Content  := TStringList.Create;
end;

destructor TMacroEntry.Destroy;
begin
  FreeAndNil(Content);
  FreeAndNil(Params);
  FreeAndNil(Headings);
  inherited Destroy;
end;

constructor TMacroList.Create;
begin
  inherited Create;
  FStaticSerial := 0;
  FCaseSensitive := False;
end;

function TMacroList.AllocateSerial: integer;
begin
  Result := FStaticSerial;
  Inc(FStaticSerial);
end;

function TMacroList.IndexOf(const _s: string): integer;
var i: integer;
    ss: string;
begin
{$IFDEF DEBUG_LOGX}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['IndexOf('+_s+')']);
{$ENDIF}
  Result := -1;
  i := 0;
  while (i < Count) and (Items[i].Name <> _s) do
    Inc(i);
  {
  if i >= 0 then
    ss := Self.Items[i].Name
  else
    ss := '';
  while (i < Count) and (ss <> _s) do
    begin
      Inc(i);
      if i >= 0 then
        ss := Items[i].Name
      else
        ss := '';
    end;
  }
{
  while (i < Count) and (Items[i].Name <> _s) do
    Inc(i);
}
  if (i < Count) then
    Result := i;
{$IFDEF DEBUG_LOGX}
  ErrorObj.Show(ltDebug,I9999_DEBUG_MESSAGE,['--------------------------------------------------']);
{$ENDIF}
end;

procedure TMacroList.Init;
begin
  FStaticSerial := 0;
end;

end.

