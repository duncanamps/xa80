unit ucommandline;

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

//
// Deal with command line and environment variable processing
//
 
{$mode ObjFPC}

interface

uses
  Classes, SysUtils, Generics.Collections, typinfo;

type
  TCommandAllowed = (caEnvironment,caCommandline);

  TCommandAllowedSet = set of TCommandAllowed;

  TParameterAllowed = (paNever,paOptional,paMandatory);

  TCommandRec = record
    ShortOption:  string;
    LongOption:   string;
    ParamName:    string;
    EnvName:      string;
    Description:  string;
    Allowed:      TCommandAllowedSet;
    Parameter:    TParameterAllowed;
    MandateFiles: boolean;
    Terminal:     boolean;
    Value:        string;
  end;

  TCommandList = class(specialize TSortedList<TCommandRec>)
    public
      constructor Create;
      function  CommandRecFromSwitch(const _switch: string): TCommandRec;
      procedure ShowHelp;
  end;

  TCommandElementType = (cetShortswitch,cetLongswitch,cetEquals,cetData);

  TCommandElement = record
    ElementType: TCommandElementType;
    ElementData: string;
  end;

  TCommandElementList = class(specialize TList<TCommandElement>)
    public
      procedure ParseString(_str: string);
      procedure ProcessCommandLine;
      procedure ProcessEnvironmentVariable;
  end;


implementation

uses
  Dos, uasmglobals, umessages, lacogen_types;

const

  CommandEntries: array[0..14] of TCommandRec =
    (
      (
        ShortOption:  '-b';
        LongOption:   '--debug';
        ParamName:    '<d>';
        EnvName:      'DebugInfo';
        Description:  'Set the debug info inclusion level to <d>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-c';
        LongOption:   '--com';
        ParamName:    '<cn>';
        EnvName:      'FilenameCom';
        Description:  'Set the .com output name to <cn>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paOptional;
        MandateFiles: True;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-d';
        LongOption:   '--define';
        ParamName:    '<id>';
        EnvName:      'Defines';
        Description:  'Define one or more symbols';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-e';
        LongOption:   '--errorlog';
        ParamName:    '<en>';
        EnvName:      'FilenameError';
        Description:  'Set error log to <en>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paOptional;
        MandateFiles: True;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-h';
        LongOption:   '--help';
        ParamName:    '';
        EnvName:      '';
        Description:  'Display help message';
        Allowed:      [caCommandLine];
        Parameter:    paNever;
        MandateFiles: False;
        Terminal:     True;
        Value:        ''
      ),
      (
        ShortOption:  '-i';
        LongOption:   '--include';
        ParamName:    '<il>';
        EnvName:      'Includes';
        Description:  'Set the include folders to <il>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-l';
        LongOption:   '--listing';
        ParamName:    '<ln>';
        EnvName:      'FilenameListing';
        Description:  'Set listing output file to <ln>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paOptional;
        MandateFiles: True;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-m';
        LongOption:   '--map';
        ParamName:    '<mn>';
        EnvName:      'FilenameMap';
        Description:  'Set map output file to <mn>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paOptional;
        MandateFiles: True;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-o';
        LongOption:   '--object';
        ParamName:    '<on>';
        EnvName:      'FilenameObj';
        Description:  'Set object output file to <mn>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paOptional;
        MandateFiles: True;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-p';
        LongOption:   '--processor';
        ParamName:    '<pt>';
        EnvName:      'Processor';
        Description:  'Set processor type to <pt>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-s';
        LongOption:   '--show';
        ParamName:    '<tp>';
        EnvName:      '';
        Description:  'Show more information on topic <tp>';
        Allowed:      [caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     True;
        Value:        ''
      ),
      (
        ShortOption:  '-t';
        LongOption:   '--tab';
        ParamName:    '<ts>';
        EnvName:      'Tab';
        Description:  'Set tab size to <ts>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-v';
        LongOption:   '--verbose';
        ParamName:    '<n>';
        EnvName:      'Verbose';
        Description:  'Set verbosity to <n>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-w';
        LongOption:   '--warnings';
        ParamName:    '<w>';
        EnvName:      'Warnings';
        Description:  'Turn warnings on or off';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paMandatory;
        MandateFiles: False;
        Terminal:     False;
        Value:        ''
      ),
      (
        ShortOption:  '-x';
        LongOption:   '--hex';
        ParamName:    '<hn>';
        EnvName:      'FilenameHex';
        Description:  'Set hex output file to <hn>';
        Allowed:      [caEnvironment,caCommandLine];
        Parameter:    paOptional;
        MandateFiles: True;
        Terminal:     False;
        Value:        ''
      )
    );


//------------------------------------------------------------------------------
//
//  TCommandElementList code
//
//------------------------------------------------------------------------------

// Break the string up into a list of elements which could be a short switch,
// long switch, equals sign or data

procedure TCommandElementList.ParseString(_str: string);
var filename: string;
    src:      string;
  function Peek: Char;
  begin
    if _str = '' then
      Result := #0
    else
      Result := _str[1];
  end;
  function Fetch: Char;
  begin
    Result := Peek;
    System.Delete(_str,1,1);
  end;
  procedure EatWhitespace;
  begin
    while Peek in [#9,' '] do
      Fetch;
  end;
  procedure GrabSwitch;
  var switch: string;
      r:      TCommandElement;
  begin
    switch := '';
    while not (Peek in [#0,#9,' ','=']) do
      switch := switch + Fetch;
    r.ElementType := cetShortswitch;
    r.ElementData := switch;
    if (Length(switch) >= 2) and (LeftStr(switch,2) = '--') then
      r.ElementType := cetLongswitch;
    Add(r);
    EatWhitespace;
  end;
  procedure GrabEquals;
    var r: TCommandElement;
  begin
    if Peek <> '=' then
      ErrorObj.Show(ltError,E2040_MISSING_EQUALS);
    r.ElementType := cetEquals;
    r.ElementData := '=';
    Fetch;
    Add(r);
  end;
  procedure GrabData;
  var dat: string;
      done: boolean;
      quoting: boolean;
      r: TCommandElement;
  begin
    dat := '';
    quoting := False;
    done := False;
    while not done do
      begin
        if quoting then
          begin // Quoting
            if Peek = #0 then
              ErrorObj.Show(ltError,E2041_PREMATURE_STRING_END,[src]);
            if Peek = #34 then
              quoting := False;
            dat := dat + Fetch;
          end
        else
          begin // Not quoting
            if Peek = #34 then
              quoting := True;
            if Peek in [#0,#9] then
              done := True
            else
              dat := dat + Fetch;
          end;
      end;
    r.ElementType := cetData;
    r.ElementData := dat;
    Add(r);
    EatWhitespace;
  end;
begin
  src := _str;
  Clear; // Ensure list if empty
  while _str <> '' do
    begin
      // Check for a - comming up, this is a switch
      // otherwise it will be a filename (could be in quotes)
      if Peek = '-' then
        GrabSwitch
      else if Peek = '=' then
        GrabEquals
      else
        GrabData;
    end;
end;

procedure TCommandElementList.ProcessCommandLine;
var cmd_line:  string;
    i:         integer;
begin
  cmd_line := '';
  for i := 1 to ParamCount do
    begin
      if i > 1 then
        cmd_line := cmd_line + #9;
      cmd_line := cmd_line + ParamStr(i);
    end;
  if cmd_line = '' then
    exit; // No need to do anything
  ParseString(cmd_line);
end;

procedure TCommandElementList.ProcessEnvironmentVariable;
var env_var:  string;
    i:        integer;
    quoting:  boolean;
begin
  env_var := GetEnv(ENVIRONMENT_VARIABLE);
  if env_var = '' then
    exit; // No need to do anything
  // Replace any unquoted spaces with tabs
  quoting := False;
  for i := 1 to Length(env_var) do
    begin
      if quoting then
        begin
          if env_var[i] = #34 then
            quoting := False;
        end
      else
        if env_var[i] = ' ' then
          env_var[i] := #9
        else if env_var[i] = #34 then
          quoting := True;
    end;
  // Remove any double quotes
  for i := Length(env_var) downto 1 do
    if env_var[i] = #34 then
      System.Delete(env_var,i,1);
  // And parse
  ParseString(env_var);
end;



//------------------------------------------------------------------------------
//
//  TCommandList code
//
//------------------------------------------------------------------------------

constructor TCommandList.Create;
var i: integer;
begin
  for i := Low(CommandEntries) to High(CommandEntries) do
    Add(CommandEntries[i]);
end;

function TCommandList.CommandRecFromSwitch(const _switch: string): TCommandRec;
var i: integer;
// @@@@@ MAKE THIS LOOKUP MORE EFFICIENT!
begin
  i := 0;
  while i <= Count-1 do
    begin
      if (Items[i].ShortOption = _switch) or (Items[i].LongOption = _switch) then
        begin
          Result := Items[i];
          Exit;
        end;
      Inc(i);
    end;
  if i = Count then
    ErrorObj.Show(ltError,E2042_INVALID_COMMAND_LINE_SWITCH,[_switch]);
end;

procedure TCommandList.ShowHelp;
var i: integer;
    short_opt, long_opt, desc: string;
begin
  WriteLn('Switches:');
  WriteLn;
  for i := 0 to Count-1 do
    begin
      short_opt := Items[i].ShortOption + ' ' + Items[i].ParamName;
      long_opt  := Items[i].LongOption;
      if Items[i].ParamName <> '' then
        long_opt := long_opt + '=' + Items[i].ParamName;
      desc := Items[i].Description;
      WriteLn(Format('%-7s %-16s %s',[short_opt,long_opt,desc]));
    end;
  WriteLn;
end;

end.

