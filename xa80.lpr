program xa80;

{$mode objfpc}{$H+}

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

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  uenvironment, ucommandline, umonitor, typinfo, uutility, uasmglobals,
  uassembler80;

type

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
    procedure ShowTitle;
  end;

{ TXA80 }

procedure TXA80.DoRun;
begin
  ShowTitle;

  try // Try block to catch initialisation exceptions
    // Initialisation stuff
    EnvObject   := TEnvironment.Create;
    try
      Initialisation;
      if EnvObject.ShowAndQuit <> saqNone then
        ShowAndQuit
      else
        begin // Normal file processing
          Asm80 := TAssembler80.Create;
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
    On E:AsmException do ; // Silent handler, we've already caught this one
    On E:Exception do
      Monitor(mtInternal,E.Message);  // Unhandled exception
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
    // Load the grammar file and other initialisation
    Monitor(mtVerbose,'Loading grammar file');
end;

procedure TXA80.Assemble;
var sl: TStringList;
    filename: string;
begin
  // Set up Grammar, DFA, NFA etc. here

  // Then loop through the list of files
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := EnvObject.GetValue('SourceFiles');
    for filename in sl do
      begin
        try
          Asm80.Assemble(filename);
        except
          On E:AsmException do ; // Silent handler, we've already caught this one
          On E:Exception do
            Asm80.Monitor(mtInternal,E.Message);  // Unhandled exception
        end;
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
    saqGrammar:       ;
    saqInstructions:  ;
    saqNFAE:          ;
    saqNFAP:          ;
    saqOperators:     ;
    saqReserved:      ;
    saqVersion:       ;
    saqWarranty:      ;
    otherwise
      Monitor(mtInternal,'No handler for ShowAndQuit option %s',[GetEnumName(TypeInfo(TShowAndQuit),Ord(EnvObject.ShowAndQuit))]);
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
  WriteLn('<bn>, <cn>, <en>, <ln>, <mn>, <on>, <hn> can be filenames or folders. If a');
  WriteLn('folder, the output file will become the assembler file name with the filetype');
  WriteLn('changed to .dbg80/.com/.log/.lst/.map/.obj80/.hex as appropriate. If a filename');
  WriteLn('then this will be used for the output, if no filetype specified one will be');
  WriteLn('added.');
  WriteLn;
  WriteLn('<id> is the define and can be a symbol name, or symbol=value. 16 bit numbers');
  WriteLn('and strings can be used. The ; separates multiple entries. For example:');
  WriteLn('--define=DEBUG;PSIZE=128;TITLE="Home page"');
  WriteLn;
  WriteLn('<gt> is the grammar type, <pt> is the processor type. Please refer to');
  WriteLn('documentation for more details. The default is XA80 and Z80 respectively.');
  WriteLn;
  WriteLn('<il> is the include list. Like with <id> ; can be used to separate.');
  WriteLn;
  WriteLn('<tp> topics are case insensitive and can be:');
  WriteLn('  DFAE         Show DFA for Expression analyser');
  WriteLn('  DFAP         Show DFA for Preparser');
  WriteLn('  Distribution Show distribution terms for this software');
  WriteLn('  Environment  Show the environment for the assembler');
  WriteLn('  Grammar      Show the grammar rules in use');
  WriteLn('  Instructions Show the instruction set for the chosen processor');
  WriteLn('  NFAE         Show the NFA for the Expression analyser');
  WriteLn('  NFAP         Show the NFA for the Preparser');
  WriteLn('  Operators    Show the logical and mathematical operations for this grammar');
  WriteLn('  Reserved     Show a list of reserved words for this grammar/processor');
  WriteLn('  Version      Show the version information for the software');
  WriteLn('  Warranty     Show the warranty information for the software');
  WriteLn;
  WriteLn('<ts> sets the Tab size for tab expansions, the default is 4.');
  WriteLn;
  WriteLn('<n> can be one of:');
  WriteLn('  0: Silent, only show fatal and internal software errors');
  WriteLn('  1: Normal level, the default');
  WriteLn('  2: Verbose, show more information');
  WriteLn('  3: War and Peace, show much more information');
  WriteLn('  4: Debug, only relevant with debug versions of the software');
  WriteLn;
end;

procedure TXA80.ShowTitle;
begin
  WriteLn;
  WriteLn('XA80 Cross Assembler for x80 processors V' + VERSION_STRING);
  WriteLn('Copyright (C)2020-2022 Duncan Munro');
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

begin
  Application:=TXA80.Create(nil);
  Application.Title:='XA80';
  Application.Run;
  Application.Free;
end.

