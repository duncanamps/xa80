program xlib80_v10;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  lacogen_types, umessages, typinfo,
  uasmglobals, uxlibcommandline, uenvironment, uxlib80, uobject;

type

  { TXLIB80 }

  TXLIB80 = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ProcessAdd;
    procedure ProcessList;
    procedure ProcessRemove;
    procedure ShowHelp;
    procedure ShowTitle;
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
begin
  try
    Initialisation;
    EnvObject := TEnvironment.Create;
    CmdObject := TLibCommandLine.Create;
    try
      ShowTitle;
      if ParamCount > 0 then
        begin
          //  :  :  :
          // Lib80.dostuff
          CmdObject.ProcessCommandLine;
          case CmdObject.CommandType of
            lctAdd:    ProcessAdd;
            lctList:   ProcessList;
            lctRemove: ProcessRemove;
            otherwise
              raise Exception.Create('No command flag specified (-a -l -r)');
          end;
        end;
    finally
      FreeAndNil(CmdObject);
      FreeAndNil(EnvObject);
    end;
  except
    On E:Exception do
      try
        WriteLn('EXCEPTION: ' + E.Message);
      except
      end;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

procedure TXLIB80.Initialisation;
begin
  // Initialisation stuff here
end;

procedure TXLIB80.ShowHelp;
begin
  WriteLn('Usage can be add, list or remove:');
  WriteLn;
  WriteLn('xlib80 -a libraryname[' + FILETYPE_LIBRARY + '] filename[' + FILETYPE_OBJECT + ']');
  WriteLn('xlib80 -l libraryname[' + FILETYPE_LIBRARY + '] modulename');
  WriteLn('xlib80 -r libraryname[' + FILETYPE_LIBRARY + '] modulename');
  WriteLn;
  WriteLn('Filenames can be discrete or wildcards, e.g. ..\source\*.obj80');
  WriteLn('Module names can include ? or * for wildcards, e.g. MOD?FLOAT*');
  WriteLn('Multiple file or module names can be specified');
  WriteLn('Adding a module that already exists will overwrite');
  WriteLn;
end;

procedure TXLIB80.ProcessAdd;
var i:   integer;
    lib: TLib80;
begin
  CmdObject.TidyLibraryName(False);
  CmdObject.TidyTargetFilenames;
  lib := TLib80.Create(CmdObject.LibName);
  WriteLn('Adding to library ' + CmdObject.LibName);
  try
    if CmdObject.Targets.Count = 0 then
      WriteLn('No targets to add to library')
    else
      begin
        for i := 0 to CmdObject.Targets.Count-1 do
          begin
            WriteLn('Adding ' + CmdObject.Targets[i]);
            lib.AddFile(CmdObject.Targets[i]);
          end;
        lib.Save;
      end;
  finally
    FreeAndNil(lib);
  end;
end;

procedure TXLIB80.ProcessList;
begin
  CmdObject.TidyLibraryName(True);
end;

procedure TXLIB80.ProcessRemove;
begin
  CmdObject.TidyLibraryName(True);
end;

procedure TXLIB80.ShowTitle;
begin
  WriteLn;
  WriteLn('XLIB80 Cross Librarian for x80 processors V' + EnvObject.Version);
  WriteLn('Copyright (C)2020-' + COPYRIGHT_YEAR + ' Duncan Munro');
  WriteLn;
  if ParamCount = 0 then
    begin
      WriteLn('This program comes with ABSOLUTELY NO WARRANTY');
      WriteLn('This is free software, and you are welcome to redistribute it');
      WriteLn('under certain conditions');
      WriteLn;
      ShowHelp;
    end;
end;


begin
  Application:=TXLIB80.Create(nil);
  Application.Title:='XLIB80';
  Application.Run;
  Application.Free;
end.

