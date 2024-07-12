program xlib80_v10;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  lacogen_types, umessages, uxlibenvironment, uxlib80, typinfo,
  uxlibcommandline, uasmglobals;

const
  CRLF = #13 + #10;
//             .........|.........|.........|.........|.........|.........|.........|.........|
  HELP_INFO = '<on> is the object name and can be filenames or folders. If no filetype is' + CRLF +
              'given, the default object file type (' + FILETYPE_OBJECT + ') will be added' + CRLF + CRLF +
              '<mn> is the module name to be removed' + CRLF + CRLF +
              '<tp> topics are case insensitive and can be:' + CRLF +
              '  Distribution Show distribution terms for this software' + CRLF +
              '  Environment  Show the environment for the assembler tool set' + CRLF +
              '  Version      Show the version information for the software' + CRLF +
              '  Warranty     Show the warranty information for the software' + CRLF +
              CRLF +
              '<x> is the eXtra detail for --list, and can be one of:' + CRLF +
              '  0: Just show the module names <default>' + CRLF +
              '  1: Show modules and segments' + CRLF +
              '  2: Show modules, segments and exports' + CRLF +
              '  3: Show modules, segments and exports as CSV' + CRLF +
              CRLF +
              '<n> can be one of:' + CRLF +
              '  0: Silent, only show fatal and internal software errors' + CRLF +
              '  1: Show only warnings and errors' + CRLF +
              '  2: Normal level, the default' + CRLF +
              '  3: Verbose, show more information' + CRLF +
              '  4: War and Peace, show much more information' + CRLF +
              '  5: Debug, only relevant with debug versions of the software' + CRLF;

type

  { TXLIB80 }

  TXLIB80 = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ShowAndQuit;
    procedure ShowDistribution;
    procedure ShowEnvironment;
    procedure ShowHelp;
    procedure ShowTitle;
    procedure ShowVersion;
    procedure ShowWarranty;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialisation;
  end;

var
  Application: TXLIB80;

{$R *.res}


{ TXLIB80 }

constructor TXLIB80.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TXLIB80.Destroy;
begin
  inherited Destroy;
end;

procedure TXLIB80.DoRun;
var
  ErrorMsg: String;
begin
  try
    EnvObject := TXlibEnvironment.Create;
    try
      Initialisation;
      Lib80 := TLib80.Create;
      try
        if EnvObject.ShowAndQuit <> saqNone then
          begin
            ShowTitle;
            ShowAndQuit;
          end
        else
          begin // Normal file processing
            if ErrorObj.InfoLimit >= ltInfo then
              ShowTitle;
            //  :  :  :
            // Lib80.dostuff
          end;
       finally
         FreeAndNil(Lib80);
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

  { add your program here }

  // stop program loop
  Terminate;
end;

procedure TXLIB80.Initialisation;
begin
  EnvObject.ProcessCommandLine;
end;

procedure TXLIB80.ShowAndQuit;
begin
  case EnvObject.ShowAndQuit of
    saqHelp:          ShowHelp;
    saqDistribution:  ShowDistribution;
    saqEnvironment:   ShowEnvironment;
    saqVersion:       ShowVersion;
    saqWarranty:      ShowWarranty;
    otherwise
      raise Exception.Create(Format('No handler for ShowAndQuit option %s',[GetEnumName(TypeInfo(TShowAndQuit),Ord(EnvObject.ShowAndQuit))]));
  end;
end;

procedure TXLIB80.ShowDistribution;
begin
  WriteLn('This program is free software: you can redistribute it and/or modify');
  WriteLn('it under the terms of the GNU General Public License as published by');
  WriteLn('the Free Software Foundation, either version 3 of the License, or');
  WriteLn('any later version.');
  WriteLn;
end;

procedure TXLIB80.ShowEnvironment;
begin
  EnvObject.Dump;
end;

procedure TXLIB80.ShowHelp;
var cmd_list: TCommandList;
begin
  WriteLn('Usage: xlib80 libraryname[' + FILETYPE_LIBRARY + '] switches filename(s)');
  WriteLn;
  WriteLn('Filenames can be discrete or wildcards, e.g. ..\source\*.obj80');
  WriteLn;
  cmd_list := TCommandList.Create;
  try
    cmd_list.ShowHelp;
  finally
    FreeAndNil(cmd_list);
  end;
  WriteLn(HELP_INFO);
end;

procedure TXLIB80.ShowTitle;
begin
  WriteLn;
  WriteLn('XLIB80 Cross Librarian for x80 processors V' + EnvObject.Version);
  WriteLn('Copyright (C)2020-' + COPYRIGHT_YEAR + ' Duncan Munro');
  WriteLn;
  if ParamCount = 0 then
    begin
      WriteLn('This program comes with ABSOLUTELY NO WARRANTY; for details type ''xlib80 --show=Warranty''');
      WriteLn('This is free software, and you are welcome to redistribute it');
      WriteLn('under certain conditions; type ''xlib80 --show=Distribution'' for details.');
      WriteLn('Use ''xlib80 --help'' for further command line options');
      WriteLn;
    end;
end;

procedure TXLIB80.ShowVersion;
begin
  WriteLn('Version:    V', EnvObject.Version);
  WriteLn('Build:      ',  EnvObject.Build);
  WriteLn('Target CPU: ' + {$I %FPCTARGETCPU%});
  WriteLn('Target OS:  ' + {$I %FPCTARGETOS%});
end;

procedure TXLIB80.ShowWarranty;
begin
  WriteLn('This program is distributed in the hope that it will be useful,');
  WriteLn('but WITHOUT ANY WARRANTY; without even the implied warranty of');
  WriteLn('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
  WriteLn('GNU General Public License for more details.');
  WriteLn;
end;


begin
  Application:=TXLIB80.Create(nil);
  Application.Title:='XLIB80';
  Application.Run;
  Application.Free;
end.

