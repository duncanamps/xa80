program xa80;

{$mode objfpc}{$H+}

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

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  uenvironment, ucommandline, typinfo, uutility, uasmglobals,
  uassembler80, uprocessors, umessages, upreparser3, usymboltable,
  lacogen_types;

const
  CRLF = #13 + #10;
//             .........|.........|.........|.........|.........|.........|.........|.........|
  HELP_INFO = '<cn>, <en>, <ln>, <mn>, <on>, <hn> can be filenames or folders. If a folder,' + CRLF +
              'the output file will become the assembler file name with the filetype changed' + CRLF +
              'to .com/.log/.lst/.map/.obj80/.hex as appropriate. If a filename then this will' + CRLF +
              'be used for the output, if no filetype specified one will be added.' + CRLF +
              CRLF +
              '<b> is the debug info inclusion level and defines what gets added to the object' + CRLF +
              'files, and can be:' + CRLF +
              '  0 No debug info inclusion (default)' + CRLF +
              '  1 Include line numbers and source file names in object file' + CRLF +
              '  2 Include line numbers and all source code in object file' + CRLF +
              CRLF +
              '<id> is the define and can be a symbol name, or symbol=value. 16 bit numbers' + CRLF +
              'and strings can be used. The ; separates multiple entries. For example: ' + CRLF +
              '--define=DEBUG;PSIZE=128;TITLE="Home page"' + CRLF +
              CRLF +
              '<il> is the include list. Like with <id> ; can be used to separate.' + CRLF +
              CRLF +
              '<tp> topics are case insensitive and can be:' + CRLF +
              '  DFAE         Show DFA for Expression analyser' + CRLF +
              '  DFAP         Show DFA for Preparser' + CRLF +
              '  Distribution Show distribution terms for this software' + CRLF +
              '  Environment  Show the environment for the assembler' + CRLF +
              '  Instructions Show the instruction set for the chosen processor' + CRLF +
              '  NFAE         Show the NFA for the Expression analyser' + CRLF +
              '  NFAP         Show the NFA for the Preparser' + CRLF +
              '  Operators    Show the logical and mathematical operators' + CRLF +
              '  Processors   Show the different processors available by default' + CRLF +
              '  Reserved     Show a list of reserved words' + CRLF +
              '  Version      Show the version information for the software' + CRLF +
              '  Warranty     Show the warranty information for the software' + CRLF +
              CRLF +
              '<ts> sets the Tab size for tab expansions, the default is 4.' + CRLF +
              CRLF +
              '<n> can be one of:' + CRLF +
              '  0: Silent, only show fatal and internal software errors' + CRLF +
              '  1: Show only warnings and errors' + CRLF +
              '  2: Normal level, the default' + CRLF +
              '  3: Verbose, show more information' + CRLF +
              '  4: War and Peace, show much more information' + CRLF +
              '  5: Debug, only relevant with debug versions of the software' + CRLF +
              CRLF +
              '<w> can be one of:' + CRLF +
              '  0: Warnings are off and are unaffected by WARNON/OFF' + CRLF +
              '  1: Warnings are on respecting WARNON/OFF in code' + CRLF +
              CRLF;

type

  { TVersion }
  TVersion = class (TPersistent)
  private
    FBuild: Integer;
    FMajor: Integer;
    FMinor: Integer;
    FVersion: Integer;
  published
    property Version: Integer read FVersion write FVersion;
    property Major: Integer read FMajor write FMajor;
    property Minor: Integer read FMinor write FMinor;
    property Build: Integer read FBuild write FBuild;
  end;

  { TXA80 }

  TXA80 = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assemble;
    procedure Initialisation;
    procedure ShowAndQuit;
    procedure ShowEnvironment;
    procedure ShowHelp;
    procedure ShowProcessors;
    procedure ShowTitle;
    procedure ShowVersion;
  end;



{ TXA80 }

procedure TXA80.DoRun;
begin

  try // Try block to catch initialisation exceptions
    // Initialisation stuff
    EnvObject   := TEnvironment.Create;
    try
      Initialisation;
      if EnvObject.ShowAndQuit <> saqNone then
        begin
          ShowTitle;
          ShowAndQuit;
        end
      else
        begin // Normal file processing
          Asm80 := TAssembler80.Create(UpperCase(EnvObject.GetValue('Processor')));
          try
            Assemble;
          finally
            FreeAndNil(Asm80);
          end;
        end;
    finally
      FreeAndNil(EnvObject);
    end;
  except
    On E:LCGErrorException do    ; // Silent handler, we've already caught this one
    On E:LCGInternalException do ; // Silent handler, we've already caught this one
    On E:Exception do
      try
        ErrorObj.Show(ltInternal,X3999_UNHANDLED_EXCEPTION,[E.Message]);  // Unhandled exception
      except
      end;
  end;

  // stop program loop
  Terminate;
end;

constructor TXA80.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
//StopOnException:=True;
  StopOnException:=False;
end;

destructor TXA80.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TXA80;

{$R *.res}

procedure TXA80.Initialisation;
begin
    // Initial processing of environment variable and command line to get grammar file
    EnvObject.ProcessEnvironmentVariable;
    EnvObject.ProcessCommandLine;
end;

procedure TXA80.Assemble;
var sl: TStringList;
    filename: string;
    listing: string;
    map:     string;
    verbose: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := EnvObject.GetValue('SourceFiles');

    // Set up the environment
    listing := EnvObject.GetValue('FilenameListing');
    if (listing = '') and (EnvObject.GetSource('FilenameListing') = esCommandLine) then
      listing := '*';
    Asm80.OptionListing := listing;
    map := EnvObject.GetValue('FilenameMap');
    if (map = '') and (EnvObject.GetSource('FilenameMap') = esCommandLine) then
      map := '*';
    Asm80.OptionMap := map;
    verbose := StrToInt(EnvObject.GetValue('Verbose'));
    case verbose of
      0: ErrorObj.InfoLimit := ltError;
      1: ErrorObj.InfoLimit := ltWarning;
      2: ErrorObj.InfoLimit := ltInfo;
      3: ErrorObj.InfoLimit := ltVerbose;
      4: ErrorObj.InfoLimit := ltWarAndPeace;
      5: ErrorObj.InfoLimit := ltDebug;
    end;
    if (EnvObject.GetValue('Warnings') = '0') then
      ErrorObj.WarningsAvailable := False
    else
      ErrorObj.WarningsAvailable := True;

    // DO the list of files
    if ErrorObj.InfoLimit >= ltInfo then
      ShowTitle;
    if sl.Count > 0 then
      begin
        ErrorObj.Show(ltInfo,I0001_ASSEMBLY_STARTED);
        ErrorObj.Show(ltVerbose,I0005_PROCESSOR_IS,[Asm80.Processor]);
        for filename in sl do
          begin
            try
              Asm80.Assemble(filename);
            except
              On E:LCGErrorException do    ; // Silent handler, we've already caught this one
              On E:LCGInternalException do ; // Silent handler, we've already caught this one
              On E:Exception do
                begin
                  try
                    ErrorObj.Show(ltInternal,X3999_UNHANDLED_EXCEPTION,[E.Message]);
                  except
                    // Do nothing
                  end;
                end;
            end;
          end;
        ErrorObj.Show(ltInfo,I0002_ASSEMBLY_ENDED);
      end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TXA80.ShowAndQuit;
begin
  case EnvObject.ShowAndQuit of
    saqHelp:          ShowHelp;
    saqDFAE:          ;
    saqDFAP:          ;
    saqDistribution:  ;
    saqEnvironment:   ShowEnvironment;
    saqInstructions:  ;
    saqNFAE:          ;
    saqNFAP:          ;
    saqOperators:     ;
    saqProcessors:    ShowProcessors;
    saqReserved:      ;
    saqVersion:       ShowVersion;
    saqWarranty:      ;
    otherwise
      raise Exception.Create(Format('No handler for ShowAndQuit option %s',[GetEnumName(TypeInfo(TShowAndQuit),Ord(EnvObject.ShowAndQuit))]));
  end;
end;

procedure TXA80.ShowEnvironment;
begin
  EnvObject.Dump;
end;

procedure TXA80.ShowHelp;
var cmd_list: TCommandList;
begin
  WriteLn('Usage: xa80 filename(s) switches');
  WriteLn;
  WriteLn('Filenames can be discrete or wildcards, e.g. ..\source\*.z80');
  WriteLn;
  cmd_list := TCommandList.Create;
  try
    cmd_list.ShowHelp;
  finally
    FreeAndNil(cmd_list);
  end;
  WriteLn(HELP_INFO);
end;

procedure TXA80.ShowProcessors;
var i: integer;
begin
  WriteLn('XA80 Processors available with -p/--processor switch');
  WriteLn;
  for i := 0 to ProcessorList.Count-1 do
    WriteLn('    ' + ProcessorList[i]);
end;

procedure TXA80.ShowTitle;
begin
  WriteLn;
  WriteLn('XA80 Cross Assembler for x80 processors V' + EnvObject.Version);
  WriteLn('Copyright (C)2020-2023 Duncan Munro');
  WriteLn;
  if ParamCount = 0 then
    begin
      WriteLn('This program is free software: you can redistribute it and/or modify');
      WriteLn('it under the terms of the GNU General Public License as published by');
      WriteLn('the Free Software Foundation, either version 3 of the License, or');
      WriteLn('(at your option) any later version.');
      WriteLn;
      WriteLn('This program is distributed in the hope that it will be useful,');
      WriteLn('but WITHOUT ANY WARRANTY; without even the implied warranty of');
      WriteLn('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
      WriteLn('GNU General Public License for more details.');
      WriteLn;
      WriteLn('You should have received a copy of the GNU General Public License');
      WriteLn('along with this program.  If not, see <https://www.gnu.org/licenses/>.');
      WriteLn;
      WriteLn('Use xa80 --help for more details');
      WriteLn;
    end;
end;

procedure TXA80.ShowVersion;
begin
  WriteLn('Version:    V', EnvObject.Version);
  WriteLn('Build:      ',  EnvObject.Build);
  WriteLn('Target CPU: ' + {$I %FPCTARGETCPU%});
  WriteLn('Target OS:  ' + {$I %FPCTARGETOS%});
end;

begin
  Application:=TXA80.Create(nil);
  Application.Title:='XA80';
  Application.Run;
  Application.Free;
end.

