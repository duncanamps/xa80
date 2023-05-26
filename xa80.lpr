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
  uenvironment, ucommandline, umonitor, typinfo, uutility, uasmglobals,
  uassembler80, ugrammar;

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
              '<gt> is the grammar type, <pt> is the processor type. Please refer to' + CRLF +
              'documentation for more details. The default is XA80 and Z80 respectively.' + CRLF +
              CRLF +
              '<il> is the include list. Like with <id> ; can be used to separate.' + CRLF +
              CRLF +
              '<tp> topics are case insensitive and can be:' + CRLF +
              '  DFAE         Show DFA for Expression analyser' + CRLF +
              '  DFAP         Show DFA for Preparser' + CRLF +
              '  Distribution Show distribution terms for this software' + CRLF +
              '  Environment  Show the environment for the assembler' + CRLF +
              '  Grammar      Show the grammar rules in use' + CRLF +
              '  Grammars     Show the different grammars available by default' + CRLF +
              '  Instructions Show the instruction set for the chosen processor' + CRLF +
              '  NFAE         Show the NFA for the Expression analyser' + CRLF +
              '  NFAP         Show the NFA for the Preparser' + CRLF +
              '  Operators    Show the logical and mathematical operations for this grammar' + CRLF +
              '  Processors   Show the different processors available by default' + CRLF +
              '  Reserved     Show a list of reserved words for this grammar/processor' + CRLF +
              '  Version      Show the version information for the software' + CRLF +
              '  Warranty     Show the warranty information for the software' + CRLF +
              CRLF +
              '<ts> sets the Tab size for tab expansions, the default is 4.' + CRLF +
              CRLF +
              '<n> can be one of:' + CRLF +
              '  0: Silent, only show fatal and internal software errors' + CRLF +
              '  1: Normal level, the default' + CRLF +
              '  2: Verbose, show more information' + CRLF +
              '  3: War and Peace, show much more information' + CRLF +
              '  4: Debug, only relevant with debug versions of the software' + CRLF +
              CRLF;

type

  { TXA80 }

  TXA80 = class(TCustomApplication)
  protected
    FGrammar: TGrammar;
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
  WriteLn(HELP_INFO);
end;

procedure TXA80.ShowTitle;
begin
  WriteLn;
  WriteLn('XA80 Cross Assembler for x80 processors V' + VERSION_STRING);
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

begin
  Application:=TXA80.Create(nil);
  Application.Title:='XA80';
  Application.Run;
  Application.Free;
end.

