unit ucodebuffer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uasmglobals;

type
  TCodeArray = array[0..MAX_BYTES_PER_CODE_RECORD-1] of byte;

  TCodeBuffer = class(TObject)
    private
    FBuffer:   TCodeArray;
    FContains: integer;
    FGenerate: boolean;
    public
      function  AsString: string;
      procedure Push(_b: byte);
      procedure PushMany(_count: integer; _b: byte; _generate: boolean = True);
      procedure Init;
      property Buffer: TCodeArray read FBuffer;
      property Contains: integer read FContains;
      property Generate: boolean read FGenerate write FGenerate;
  end;

implementation

uses
  strutils, umessages, deployment_parser_types_12;

function TCodeBuffer.AsString: string;
var i: integer;
    s: string;
begin
  s := '';
  if not Generate then
    s := ''
  else if FContains <= MAX_HEX_BYTES_IN_LISTING then
    begin
      for i := 0 to FContains-1 do
        begin
          if s <> '' then
            s := s + ' ';
          s := s + Format('%2.2X',[FBuffer[i]]);
        end;
    end
  else
    begin // Need to truncate the listing
      for i := 0 to MAX_HEX_BYTES_IN_LISTING-2 do
        begin
          if s <> '' then
            s := s + ' ';
          s := s + Format('%2.2X',[FBuffer[i]]);
        end;
      s := s + '...';
    end;
  s := PadRight(s,MAX_HEX_WIDTH);
  Result := s;
end;

procedure TCodeBuffer.Push(_b: byte);
begin
  if FContains >= MAX_BYTES_PER_CODE_RECORD then
    ErrorObj.Show(ltError,E2024_CODE_BUFFER_OVERFLOW,[MAX_BYTES_PER_CODE_RECORD]);
  FBuffer[FContains] := _b;
  Inc(FContains);
end;

procedure TCodeBuffer.PushMany(_count: integer; _b: byte; _generate: boolean);
var i: integer;
begin
  if (FContains + _count) >= MAX_BYTES_PER_CODE_RECORD then
    ErrorObj.Show(ltError,E2024_CODE_BUFFER_OVERFLOW,[MAX_BYTES_PER_CODE_RECORD]);
  Generate := _generate;
  for i := 0 to _count-1 do
    FBuffer[FContains+i] := _b;
  FContains := FContains + _count;
end;

procedure TCodeBuffer.Init;
begin
  FContains := 0;
  FGenerate := True;  // Assume we will always generate code, DS change change this...
end;

end.

