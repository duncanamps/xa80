unit uenvironment;

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2024 Duncan Munro

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

//
// Sets up the environment in the following order:
//   Default values
//   Values from the grammar file
//   Values from the environment variable XA80
//   Values from the command line
//
// Note that the grammar entry in the environment variable and command line
// are processed before anything else as the grammar file has to be selected
// before the remainder of the environment variable and command line are dealt
// with.
//
// The items dealt with here are:
//
//   High level control:
//     Grammar type
//     Processor type (overrides the default processor in the grammar)
//
//   Folders:
//     Debug file folder
//     Hex file folder
//     Listing file folder
//     Map file folder
//     Object file folder
//
//   Folder lists:
//     Include file folder list
//
//   Misc parameters:
//     Defines
//     Tab size
//     Verbosity
//
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, ucommandline;

type

  { TVersion }

  TVersion = class (TPersistent)
  private
    FMajor:   Integer;
    FMinor:   Integer;
    FRelease: Integer;
    FBuild:   Integer;
  published
    property Major:   Integer read FMajor   write FMajor;
    property Minor:   Integer read FMinor   write FMinor;
    property Release: Integer read FRelease write FRelease;
    property Build:   Integer read FBuild   write FBuild;
  end;

  TEnvironmentSource = (esDefault,esEnvironment,esCommandline);

  TEnvironmentElement = class(TObject)
    public
      Key:    string;
      Data:   string;
      Source: TEnvironmentSource;
//      function Compare(constref left, right: TEnvironmentElement): integer;
      {
      property AsString:  string;
      property AsInteger: integer;
      property AsList:    TStringList;
      }
  end;

  TEnvComparer = specialize IComparer<TEnvironmentElement>;

  TShowAndQuit = (saqNone,saqHelp,saqDistribution,saqEnvironment,
                  saqInstructions,saqProcessors,
                  saqReserved,saqVersion,saqWarranty);

  TEnvironment = class(specialize TSortedList<TEnvironmentElement>)
    private
      EnvComparer: specialize IComparer<TEnvironmentElement>;
    public
      Build:       string;
      ShowAndQuit: TShowAndQuit;
      Version:     string;
      constructor Create(_setdefaults: boolean = true);
      destructor Destroy; override;
      procedure AddSourceName(const _filename: string);
      procedure ApplyDefaults;
      procedure Dump;
      procedure ExpandIncludes(_src: TEnvironmentSource);
      function  GetSource(_key: string): TEnvironmentSource;
      function  GetValue(_key: string): string;
      function  GetValueAsInteger(_key: string): integer;
      procedure ProcessCommandLine;
      procedure ProcessCommandList(_elements: TCommandElementList; _src: TEnvironmentSource);
      procedure ProcessEnvironmentVariable;
      procedure SetValue(_key: string; _data: boolean; _source: TEnvironmentSource);
      procedure SetValue(_key: string; _data: integer; _source: TEnvironmentSource);
      procedure SetValue(_key: string; _data: string;  _source: TEnvironmentSource);
  end;

var
  EnvObject: TEnvironment;  // Global environment object


implementation

uses
  typinfo, umessages, lacogen_types, uasmglobals, fileinfo;


function EnvironmentSourceAsString(_es: TEnvironmentSource): string;
begin
  case _es of
    esDefault:     Result := 'Default';
    esEnvironment: Result := 'EnvironmentVar';
    esCommandline: Result := 'CommandLine';
    otherwise
      ErrorObj.Show(ltInternal,X3999_UNHANDLED_EXCEPTION,[Format('Internal error - Environment source %s not catered for',[GetEnumName(TypeInfo(TEnvironmentSource),Ord(_es))])]);
  end;
end;


function Compare(constref left, right: TEnvironmentElement): integer;
begin
  if left.Key = right.Key then
    Result := 0
  else if left.Key > right.Key then
    Result := 1
  else
    Result := -1;
end;


//------------------------------------------------------------------------------
//
//  TEnvironment code
//
//------------------------------------------------------------------------------

constructor TEnvironment.Create(_setdefaults: boolean);
var aVersionInfo: TVersionInfo;
    VersionInfo:  TVersion;
begin
  EnvComparer := specialize TComparer<TEnvironmentElement>.Construct(@Compare);
  inherited Create(EnvComparer);
  Duplicates := dupError;
  Sorted := True;
  ShowAndQuit := saqNone;
  if _setdefaults then
    ApplyDefaults;
  // Get the version information
  aVersionInfo:=TVersionInfo.Create;
  VersionInfo:=TVersion.Create;
  try
    aVersionInfo.Load(HINSTANCE);
    VersionInfo.Major   := aVersionInfo.FixedInfo.FileVersion[0];
    VersionInfo.Minor   := aVersionInfo.FixedInfo.FileVersion[1];
    VersionInfo.Release := aVersionInfo.FixedInfo.FileVersion[2];
    VersionInfo.Build   := aVersionInfo.FixedInfo.FileVersion[3];
    Version := IntToStr(VersionInfo.Major) + '.' + IntToStr(VersionInfo.Minor) + '.' + IntToStr(VersionInfo.Release);
    Build   := IntToStr(VersionInfo.Build);
  finally
    FreeAndNil(VersionInfo);
    FreeAndNil(aVersionInfo);
  end;
end;

destructor TEnvironment.Destroy;
begin
  while Count > 0 do
    begin
      Items[Count-1].Free;
      Delete(Count-1);
    end;
  inherited Destroy;
end;

procedure TEnvironment.AddSourceName(const _filename: string);
var s: string;
    folder: string;
    Info: TSearchRec;
  procedure PushFile(const _fn: string);
  var ss: string;
  begin
    ss := GetValue('SourceFiles');
    if ss <> '' then
      ss := ss + ';';
    ss := ss + ExpandFilename(_fn);
    SetValue('SourceFiles',ss,esCommandline);
  end;
begin
  // Expand wildcards if required
  if (Pos('?',_filename) > 0) or (Pos('*',_filename) > 0) then
    begin
      s := ExpandFilename(_filename);
      folder := ExtractFilePath(s);
      if SysUtils.FindFirst(s,faAnyFile,Info) = 0 then
        repeat
          PushFile(folder+Info.Name);
        until SysUtils.FindNext(Info) <> 0;
    end
  else
    PushFile(_filename);
end;

procedure TEnvironment.ApplyDefaults;
begin
  // The default items which will be optionally overridden by the
  // environment variable, and command line
  // Key items
  SetValue('CaseSensitive', '0',                 esDefault);
  SetValue('DebugLevel', '0',                    esDefault);
  SetValue('Defines',    '',                     esDefault);
  SetValue('Includes',   '',                     esDefault);
  SetValue('Processor',  DEFAULT_PROCESSOR,      esDefault);
  SetValue('Tab',        4,                      esDefault);
  SetValue('Verbose',    3,                      esDefault);
  SetValue('Warnings',   DEFAULT_WARNINGS,       esDefault);
  SetValue('SourceFiles','',                     esDefault);
  // File specific
  SetValue('FilenameCom',       '',              esDefault);
  SetValue('FilenameDebug',     '',              esDefault);
  SetValue('FilenameError',     '',              esDefault);
  SetValue('FilenameHex',       '',              esDefault);
  SetValue('FilenameListing',   '',              esDefault);
  SetValue('FilenameMap',       '',              esDefault);
  SetValue('FilenameObj',       '',              esDefault);
end;

procedure TEnvironment.Dump;
var i,j: integer;
    sl: TStringList;
begin
  WriteLn('ENVIRONMENT');
  WriteLn('-----------');
  for i := 0 to Count-1 do
    if  ((Items[i].Key = 'Defines') or
         (Items[i].Key = 'Includes') or
         (Items[i].Key = 'SourceFiles')) and
        (Items[i].Data <> '') then
      begin // Output as a list
        sl := TStringList.Create;
        try
          sl.Delimiter := ';';
          sl.StrictDelimiter := True;
          sl.QuoteChar := #0;
          sl.DelimitedText := Items[i].Data;
          for j := 0 to sl.Count-1 do
            if j = 0 then
              WriteLn(Format('%3d: %-14s %-20s %s',[i,EnvironmentSourceAsString(Items[i].Source),Items[i].Key,sl[j]]))
            else
              WriteLn(Space(41)+sl[j]);
        finally
          FreeAndNil(sl);
        end;
      end
    else
      WriteLn(Format('%3d: %-14s %-20s %s',[i,EnvironmentSourceAsString(Items[i].Source),Items[i].Key,Items[i].Data]));
  WriteLn;
end;

procedure TEnvironment.ExpandIncludes(_src: TEnvironmentSource);
var sl: TStringList;
    i:  integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := GetValue('Includes');
    for i := 0 to sl.Count-1 do
      sl[i] := IncludeTrailingPathDelimiter(ExpandFilename(sl[i]));
    SetValue('Includes',sl.DelimitedText,_src);
  finally
    FreeAndNil(sl);
  end;
end;

function TEnvironment.GetSource(_key: string): TEnvironmentSource;
var _rec: TEnvironmentElement;
    _index: SizeInt;
begin
  Result := esDefault;
  // Check if key exists, if not flag as an error
  _rec := TEnvironmentElement.Create;
  try
    _rec.Key  := _key;
    _rec.Data := '';
    if (Count = 0) or (not BinarySearch(_rec,_index,EnvComparer)) then
      ErrorObj.Show(ltInternal,X3999_UNHANDLED_EXCEPTION,[Format('Environment value %s not found',[_key])])
    else
      Result := Items[_index].Source;
  finally
    FreeAndNil(_rec);
  end;
end;

function TEnvironment.GetValue(_key: string): string;
var _rec: TEnvironmentElement;
    _index: SizeInt;
begin
  Result := '';
  // Check if key exists, if not flag as an error
  _rec := TEnvironmentElement.Create;
  try
    _rec.Key  := _key;
    _rec.Data := '';
    if (Count = 0) or (not BinarySearch(_rec,_index,EnvComparer)) then
      ErrorObj.Show(ltInternal,X3999_UNHANDLED_EXCEPTION,[Format('Environment value %s not found',[_key])])
    else
      Result := Items[_index].Data;
  finally
    FreeAndNil(_rec);
  end;
end;

function TEnvironment.GetValueAsInteger(_key: string): integer;
var s: string;
begin
  GetValueAsInteger := 0;
  try
    s := GetValue(_key);
    GetValueAsInteger := StrToInt(s);
  except
    ErrorObj.Show(ltError,E2073_EXPECTED_INTEGER_ENV,[s]);
  end;
end;

procedure TEnvironment.ProcessCommandLine;
var cmd_element_list: TCommandElementList;
begin
  cmd_element_list := TCommandElementList.create;
  try
    cmd_element_list.ProcessCommandLine;
    ProcessCommandList(cmd_element_list,esCommandline);
  finally
    FreeAndNil(cmd_element_list);
  end;
end;

procedure TEnvironment.ProcessCommandList(_elements: TCommandElementList; _src: TEnvironmentSource);
var cmd_list: TCommandList;
    option:   TCommandRec;
    hasdata:  boolean;

    {
  procedure MyMonitor(_mt: TMonitorType; const _msg: string);
  var suffix: string;
  begin
    case _src of
      esDefault:     suffix := 'default';
      esEnvironment: suffix := 'environment variable';
      esCommandline: suffix := 'command line';
    end;
    Monitor(_mt,_msg + ' in ' + suffix);
  end;
  procedure MyMonitor(_mt: TMonitorType; const _fmt: string; const _args: array of const);
  begin
    MyMonitor(_mt,Format(_fmt,_args));
  end;
  }

  procedure ProcessOption(const _name: string; const _data: string; _src: TEnvironmentSource);
  begin
    if option.Terminal then
      begin
        case UpperCase(_data) of
          '':             ShowAndQuit := saqHelp;
          'DISTRIBUTION': ShowAndQuit := saqDistribution;
          'ENVIRONMENT':  ShowAndQuit := saqEnvironment;
          'INSTRUCTIONS': ShowAndQuit := saqInstructions;
          'PROCESSORS':   ShowAndQuit := saqProcessors;
          'RESERVED':     ShowAndQuit := saqReserved;
          'VERSION':      ShowAndQuit := saqVersion;
          'WARRANTY':     ShowAndQuit := saqWarranty;
          otherwise
            ErrorObj.Show(ltError,E2038_INVALID_SHOW_OPTION,[_data]);
        end;
      end
    else
      SetValue(_name,_data,_src);
  end;
begin
  cmd_list := TCommandList.Create;
  try
    while _elements.Count > 0 do
      begin
        if _elements[0].ElementType = cetData then
          begin
            AddSourceName(_elements[0].ElementData);
            _elements.Delete(0);
          end
        else if _elements[0].ElementType = cetShortswitch then
          begin
            option := cmd_list.CommandRecFromSwitch(_elements[0].ElementData);
            hasdata := (_elements.Count > 1) and (_elements[1].ElementType = cetData);
            if (option.Parameter = paMandatory) and (not hasdata) then
              ErrorObj.Show(ltError,E2039_SWITCH_MISSING_VALUE,[_elements[0].ElementData]);
            if (option.Parameter in [paOptional,paMandatory]) and hasdata then
              begin
                ProcessOption(option.EnvName,_elements[1].ElementData,_src);
                _elements.Delete(0); // Get rid of data
              end
            else
              ProcessOption(option.EnvName,'',_src);
            _elements.Delete(0); // Get rid of -switch
          end
        else if _elements[0].ElementType = cetLongswitch then
          begin
            option := cmd_list.CommandRecFromSwitch(_elements[0].ElementData);
            hasdata := (_elements.Count > 2) and (_elements[1].ElementType = cetEquals) and (_elements[2].ElementType = cetData);
            if (option.Parameter = paMandatory) and (not hasdata) then
              ErrorObj.Show(ltError,E2039_SWITCH_MISSING_VALUE,[_elements[0].ElementData]);
            if (option.Parameter in [paOptional,paMandatory]) and hasdata then
              begin
                ProcessOption(option.EnvName,_elements[2].ElementData,_src);
                _elements.Delete(0); // Get rid of data
                _elements.Delete(0);
              end
            else
              ProcessOption(option.EnvName,'',_src);
            _elements.Delete(0); // Get rid of -switch
          end
        else
          ErrorObj.Show(ltError,E2034_UNEXPECTED_TOKEN,[_elements[0].ElementData]);
      end;
  finally
    FreeAndNil(cmd_list);
  end;
  // If include folders were set up, expand them
  ExpandIncludes(_src);
end;

procedure TEnvironment.ProcessEnvironmentVariable;
var cmd_list: TCommandElementList;
begin
  cmd_list := TCommandElementList.create;
  try
    cmd_list.ProcessEnvironmentVariable;
    ProcessCommandList(cmd_list,esEnvironment);
  finally
    FreeAndNil(cmd_list);
  end;
end;

procedure TEnvironment.SetValue(_key: string; _data: boolean; _source: TEnvironmentSource);
begin
  SetValue(_key,BoolToStr(_data),_source);
end;

procedure TEnvironment.SetValue(_key: string; _data: integer; _source: TEnvironmentSource);
begin
  SetValue(_key,IntToStr(_data),_source);
end;

procedure TEnvironment.SetValue(_key: string; _data: string; _source: TEnvironmentSource);
var _rec: TEnvironmentElement;
    _index: SizeInt;
begin
  // Check if key exists, if not add it
  _rec := TEnvironmentElement.Create;
  _rec.Key    := _key;
  _rec.Data   := _data;
  _rec.Source := _source;
  if (Count = 0) or (not BinarySearch(_rec,_index,EnvComparer)) then
    _index := Add(_rec)
  else
    begin
      Items[_index].Key    := _key;
      Items[_index].Data   := _data;
      Items[_index].Source := _source;
      FreeAndNil(_rec);
    end;
end;

end.

