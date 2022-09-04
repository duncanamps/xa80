unit uenvironment;

{$mode ObjFPC}{$H+}

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

interface

uses
  Classes, SysUtils;

type
  TGrammarType = (gtError,gtXA80);

  TGrammarSet = set of TGrammarType;

  TProcessorType = (ptError,pt8080,pt8085,ptZ80,ptZ180);

  TProcessorSet = set of TProcessorType;

  TEnvironment = class(TObject)
    private
      FGrammarType: TGrammarType;
      FProcessorType: TProcessorType;
      function GetGrammarList: string;
      function GetProcessorList: string;
      procedure SetGrammarType(_gt: TGrammarType);
      procedure SetProcessorType(_pt: TProcessorType);
    public
      function  GrammarTypeToString(_gt: TGrammarType): string;
      function  ProcessorTypeToString(_pt: TProcessorType): string;
      function  StringToGrammarType(const _s: string): TGrammarType;
      function  StringToProcessorType(const _s: string): TProcessorType;
      function  ValidProcessor(const _p: string): boolean;
      property GrammarList: string           read GetGrammarList;
      property GrammarType: TGrammarType     read FGrammarType write SetGrammarType;
      property ProcessorList: string         read GetProcessorList;
      property ProcessorType: TProcessorType read FProcessorType write SetProcessorType;
  end;

var
  EnvObject: TEnvironment;  // Global environment object


implementation

uses
  typinfo, uasmglobals;

function TEnvironment.GetGrammarList: string;
var g: TGrammarType;
    s: string;
begin
  s := '';
  for g in TGrammarType do
    if g <> gtError then
      begin
        if (g = High(TGrammarType)) and (s <> '') then
          s := s + ' or '
        else if Ord(g) > 1 then
          s := s + ', ';
        s := s + Copy(GetEnumName(TypeInfo(TGrammarType),Ord(g)),3,999);
        if g = StringToGrammarType(DEFAULT_GRAMMAR_VALUE) then
          s := s + ' (default)';
      end;
  Result := s;
end;

function TEnvironment.GetProcessorList: string;
var p: TProcessorType;
    s: string;
begin
  s := '';
  for p in TProcessorType do
    if p <> ptError then
      begin
        if (p = High(TProcessorType)) and (s <> '') then
          s := s + ' or '
        else if Ord(p) > 1 then
          s := s + ', ';
        s := s + Copy(GetEnumName(TypeInfo(TProcessorType),Ord(p)),3,999);
        if p = StringToProcessorType(DEFAULT_PROCESSOR_VALUE) then
          s := s + ' (default)';
      end;
  Result := s;
end;

function TEnvironment.GrammarTypeToString(_gt: TGrammarType): string;
begin

end;

function TEnvironment.ProcessorTypeToString(_pt: TProcessorType): string;
begin

end;

procedure TEnvironment.SetGrammarType(_gt: TGrammarType);
begin

end;

procedure TEnvironment.SetProcessorType(_pt: TProcessorType);
begin

end;

function TEnvironment.StringToGrammarType(const _s: string): TGrammarType;
var g: TGrammarType;
begin
  Result := gtError;
  for g in TGrammarType do
    if UpperCase(_s) = Copy(GetEnumName(TypeInfo(TGrammarType),Ord(g)),3,999) then
      Result := g;
end;

function TEnvironment.StringToProcessorType(const _s: string): TProcessorType;
var p: TProcessorType;
begin
  Result := ptError;
  for p in TProcessorType do
    if UpperCase(_s) = Copy(GetEnumName(TypeInfo(TProcessorType),Ord(p)),3,999) then
      Result := p;
end;

function TEnvironment.ValidProcessor(const _p: string): boolean;
var pt: TProcessorType;
begin
  pt := StringToProcessorType(_p);
  Result := (pt <> ptError);
end;

initialization
  EnvObject := TEnvironment.Create;

finalization
  FreeAndNil(EnvObject);

end.

