program xa80;

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, fileinfo, usymbol, uutility, uassembler,
  ufilestack, uexpression, uoutput, uifstack, umacro,
  deployment_parser_types_12, deployment_parser_module_12, udebuglist,
  uinstruction, uasmglobals;

const
  SHORT_OPTIONS = 'b::d:e::hI:l::m::o:p:rt:v:Vwx::';
  LONG_OPTIONS: array [1..15] of string =
    (
      'debug::',
      'define:',
      'errorlog::',
      'help',
      'include:',
      'listing::',
      'map::',
      'object:',
      'processor:',
      'redistribution',
      'tab:',
      'verbose:',
      'version',
      'warranty',
      'hex::'
    );

type

  { TXA80 }

  TXA80 = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ProcessFilename(basename: string; var filename: string; shortopt: char; longopt: string; defaultext: string);
    procedure WriteHelp; virtual;
    procedure WriteTitle;
    procedure WriteVersion;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Monitor({%H-}Parser: TLCGParser; {%H-}LogType: TLCGLogType; const Message: string);
  end;

{ TXA80 }

procedure TXA80.DoRun;
var
  ErrorMsg:   String;
  xa80:       TAssembler;
  procoption: string;
  procvalue:  string;
  taboption:  string;
  tabvalue:   integer;
  verboption: string;
  verblevel:  integer;
  nonoptions: TStringList;
  filename:   string;
  basename:   string;  // Filename without .ext on the end
begin
  WriteTitle;

  // quick check parameters
  ErrorMsg:=CheckOptions(SHORT_OPTIONS, LONG_OPTIONS);
  if ErrorMsg<>'' then begin
    WriteLn(ErrorMsg);
    Terminate;
    Exit;
  end;

  // Help if needed...
  if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;


  // parse parameters
  if HasOption('p', 'processor') then
    begin
      procoption := UpperCase(GetOptionValue('p', 'processor'));
      if (procoption <> 'Z80') and
         (procoption <> 'Z180') then
        begin
          WriteLn('Illegal or missing option value for -p / --processor, must be Z80 or Z180');
          Terminate;
          Exit;
        end;
      try
        procvalue := procoption;
      except
        WriteLn('Illegal or missing option value for -p / --processor, must be Z80 or Z180');
        Terminate;
        Exit;
      end;
    end
  else
    procvalue := DEFAULT_PROCESSOR_VALUE;

  // Redistribution information
  if HasOption('r', 'redistribution') then
    begin
      WriteLn('This program is free software: you can redistribute it and/or modify' + #13 + #10 +
              'it under the terms of the GNU General Public License as published by' + #13 + #10 +
              'the Free Software Foundation, either version 3 of the License, or' + #13 + #10 +
              '(at your option) any later version.' + #13 + #10 + #13 + #10 +
              'This program is distributed in the hope that it will be useful,'  + #13 + #10 +
              'but WITHOUT ANY WARRANTY; without even the implied warranty of'  + #13 + #10 +
              'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + #13 + #10 +
              'GNU General Public License for more details.' + #13 + #10 + #13 + #10 +
              'You should have received a copy of the GNU General Public License' + #13 + #10 +
              'along with this program.  If not, see <https://www.gnu.org/licenses/>.' + #13 + #10);
      Terminate;
      Exit;
    end;

  if HasOption('t', 'tab') then
    begin
      taboption := GetOptionValue('t', 'tab');
      if (Length(taboption) < 1) or
         (Length(taboption) > 2) then
        begin
          WriteLn('Illegal or missing option value for -t / --tab, must be 1 to 99');
          Terminate;
          Exit;
        end;
      try
        tabvalue := StrToInt(taboption);
      except
        WriteLn('Illegal or missing option value for -t / --tab, must be 1 to 99');
        Terminate;
        Exit;
      end;
    end
  else
    tabvalue := DEFAULT_TAB_SIZE;

  if HasOption('V', 'version') then
    begin
      WriteVersion;
      Terminate;
      Exit;
    end;

  if HasOption('v', 'verbose') then
    begin
      verboption := GetOptionValue('v', 'verbose');
      if (Length(verboption) <> 1) or
         (verboption < '0') or
         (verboption > '3') then
        begin
          WriteLn('Illegal option value for -v / --verbose, must be 0, 1, 2 or 3');
          Terminate;
          Exit;
        end;
      verblevel := StrToInt(verboption);
    end
  else
    verblevel := 0;

  // Warranty information
  if HasOption('w', 'warranty') then
    begin
      WriteLn('Disclaimer of Warranty');
      WriteLn;
      WriteLn('THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY' + #13 + #10 +
              'APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT' + #13 + #10 +
              'HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY' + #13 + #10 +
              'OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,' + #13 + #10 +
              'THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR' + #13 + #10 +
              'PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM' + #13 + #10 +
              'IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF' + #13 + #10 +
              'ALL NECESSARY SERVICING, REPAIR OR CORRECTION.' + #13 + #10);
      Terminate;
      Exit;
    end;

  // Check filename is specified

  nonoptions := TStringList.Create;
  try
    GetNonOptions(SHORT_OPTIONS,LONG_OPTIONS,nonoptions);
    if nonoptions.Count < 1 then
      begin
        WriteLn('Filename not specified');
        Terminate;
        Exit;
      end;
    if nonoptions.Count > 1 then
      begin
        WriteLn('More than one filename specified');
        Terminate;
        Exit;
      end;
    filename := ExpandFilename(nonoptions[0]);
  finally
    nonoptions.Free;
  end;

  { add your program here }


  // Create the assembler and run it

  xa80 := TAssembler.Create('XA80',procvalue);
  try
    // Set up the initial parameters
    xa80.FilenameSrc := filename; // Has to go first!
    xa80.OnMonitor   := @Monitor;
    xa80.TabSize     := tabvalue;
    case verblevel of
      0: xa80.LogLevel := ltInfo;
      1: xa80.LogLevel := ltVerbose;
      2: xa80.LogLevel := ltWarAndPeace;
      3: xa80.LogLevel := ltDebug;
    end;
    basename := StringReplace(xa80.FilenameSrc,ExtractFileExt(xa80.FilenameSrc),'',[rfReplaceAll]);
    ProcessFilename(basename,xa80.FilenameDbg,'b','debug',   '.d80');
    ProcessFilename(basename,xa80.FilenameHex,'x','hex',     '.hex');
    ProcessFilename(basename,xa80.FilenameLst,'l','listing', '.lst');
    ProcessFilename(basename,xa80.FilenameLog,'e','errorlog','.log');
    ProcessFilename(basename,xa80.FilenameMap,'m','map',     '.map');
    ProcessFilename(basename,xa80.FilenameObj,'o','object',  '.o80');
    CmdOptionToList(Self,'d','define', xa80.CmdDefines);
    CmdOptionToList(Self,'I','include',xa80.CmdIncludes,true);
    AugmentIncludes(GetCurrentDir,xa80.CmdIncludes);
    AugmentIncludes(ExtractFilePath(filename),xa80.CmdIncludes);
    try
      xa80.Assemble;
    except
      on E: LCGErrorException do ;    // Nothing, it's dealt with already
      on E: LCGInternalException do ; // Ditto
      on E: Exception do Monitor(xa80,ltInternal,'UNHANDLED EXCEPTION: ' + E.Message);
    end;
  finally
    xa80.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TXA80.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TXA80.Destroy;
begin
  inherited Destroy;
end;

procedure TXA80.Monitor(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
begin
  WriteLn(Message);
end;

procedure TXA80.ProcessFilename(basename: string; var filename: string; shortopt: char; longopt: string; defaultext: string);
var newname: string;
begin
  if HasOption(shortopt,longopt) then
    begin
      newname := GetOptionValue(shortopt,longopt);
      if newname = '' then
          newname := basename;
      if ExtractFileName(newname) = newname then // No path specified
        newname := ExtractFilePath(basename) + newname;
      if ExtractFileExt(newname) = '' then
        newname := newname + defaultext;
      filename := ExpandFilename(newname);
    end;
end;

procedure TXA80.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: xa80 filename <options>');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('    -b <bn> --debug=<bn>     Set the debug name to <bn>');
  WriteLn('    -d <id> --define=<id>    Define one or more symbols');
  WriteLn('    -e <en> --errorlog=<en>  Set error log to <en>');
  WriteLn('    -h      --help           Display this message');
  WriteLn('    -I <id> --include=<id>   Set the include directory to <id>');
  WriteLn('    -l <ln> --listing=<ln>   Set the listing name to <ln>');
  WriteLn('    -m <mn> --map=<mn>       Set the map filename to <mn>');
  WriteLn('    -o <on> --object=<on>    Set the object name to <on>');
  WriteLn('    -p <pt> --processor=<pt> Use the nominated processor, default Z80');
  WriteLn('    -r      --redistribution Info on redistributing this software');
  WriteLn('    -t <n>  --tab=<n>        Tab size for input file (default 4)');
  WriteLn('    -v <n>  --verbose=<n>    Verbose output while assembling');
  WriteLn('    -V      --version        Display version and other status info');
  WriteLn('    -w      --warranty       Display warranty information');
  WriteLn('    -x <hn> --hex=<hn>       Set the hex filename to <hn>');
  WriteLn('');
  WriteLn('<bn>/<en>/<hn>/<ln>/<mn>/<on> default to the filename with ext changed to');
  WriteLn('.d80/.log/.hex/.lst/.map/.o80 respectively. Not specifying <bn>, <en>,');
  WriteLn('<hn>, <ln>, <mn> or <on> will stop that output.');
  WriteLn('');
  WriteLn('verbose <n> options:');
  WriteLn('    0 Normal output levels (the default)');
  WriteLn('    1 Verbose output');
  WriteLn('    2 "War and Peace", lots more output');
  WriteLn('    3 Debug level output');
  WriteLn('');
  WriteLn('Processor type <pt> can be Z80 or Z180');
  WriteLn('');
  WriteLn('The include file directory and define list <id> can contain names or');
  WriteLn('symbols delimited by ; for example:');
  WriteLn('    --define=DEBUG;TAB_SIZE=4;CODE_NAME="Project ASM"');
  WriteLn('    --include=source/tables;source/help;/users/me/includes');
  WriteLn('');
end;




// Write the title when the program starts up

procedure TXA80.WriteTitle;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    WriteLn('');
    WriteLn('XA80 Cross Assembler V' + FileVerInfo.VersionStrings.Values['ProductVersion']);
    WriteLn('Copyright (C)2020-' + FormatDateTime('YYYY',Now) + ' Duncan Munro');
    if ParamCount = 0 then
      begin
        WriteLn('This program comes with ABSOLUTELY NO WARRANTY; for details type xa80 -w');
        WriteLn('This is free software, and you are welcome to redistribute it');
        WriteLn('under certain conditions; type xa80 -r for details.');
        WriteLn;
        WriteLn('Type ''xa80 -h'' for a full list of command line parameters');
      end;
    WriteLn('');
  finally
    FileVerInfo.Free;
  end;
end;


// Write the version information

procedure TXA80.WriteVersion;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    WriteLn('VERSION INFORMATION');
    WriteLn('');
    WriteLn('Software version: ' + FileVerInfo.VersionStrings.Values['FileVersion']);
    WriteLn('Compiler version: ' + {$I %FPCVERSION%});
    WriteLn('Compiled:         ' + {$I %DATE%} + ' ' + {$I %TIME%});
    WriteLn('Target OS:        ' + {$I %FPCTARGETOS%});
    WriteLn('Target CPU:       ' + {$I %FPCTARGETCPU%});
    WriteLn;
  finally
    FileVerInfo.Free;
  end;
end;



var
  Application: TXA80;

{$R *.res}

begin
  Application:=TXA80.Create(nil);
  Application.Title:='X80 Cross Assembler';
  Application.Run;
  Application.Free;
end.

