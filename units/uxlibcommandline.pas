unit uxlibcommandline;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLibCommandType = (lctUnknown,lctAdd,lctList,lctRemove);

  TLibCommandLine = class(TObject)
    protected
      FCommandType: TLibCommandType;
      FLibName:     string;
      FTargets:     TStringList;
      procedure SetCommandType(_cmdtyp: TLibCommandType);
    public
      property CommandType: TLibCommandType read FCommandType write SetCommandType;
      property LibName: string read FLibName;
      property Targets: TStringList read FTargets;
      constructor Create;
      destructor Destroy; override;
      procedure ProcessCommandLine;
      procedure TidyLibraryName(_mustexist: boolean);
      procedure TidyTargetFilenames;
  end;

var
  CmdObject: TLibCommandLine;


implementation

uses
  uasmglobals;

constructor TLibCommandLine.Create;
begin
  inherited Create;
  FCommandType := lctUnknown;
  FTargets := TStringList.Create;
end;

destructor TLibCommandLine.Destroy;
begin
  FreeAndNil(FTargets);
  inherited Destroy;
end;

procedure TLibCommandLine.ProcessCommandLine;
var i: integer;
begin
  // Command line will be a combo of
  //   switches
  //   library name
  //   module or filenames (targets)
  // Switch can appear anywhere but library name must appear before targets
  for i := 1 to ParamCount do
    begin
      case ParamStr(i) of
        '-a': CommandType := lctAdd;
        '-l': CommandType := lctList;
        '-r': CommandType := lctRemove;
        otherwise
          begin
            if FLibName = '' then
              FLibName := ParamStr(i)
            else
              FTargets.Add(ParamStr(i));
          end;
      end;
    end;
  // If the library name is missing, we are screwed
  if FLibName = '' then
    raise Exception.Create('Library name not specified');
  if (Pos('*',FLibName) > 0) or (Pos('?',FLibName) > 0) then
    raise Exception.Create('Library name cannot contain wildcards');
end;


procedure TLibCommandLine.SetCommandType(_cmdtyp: TLibCommandType);
begin
  if FCommandType <> lctUnknown then
    raise Exception.Create('More than one flag specified on command line');
  FCommandType := _cmdtyp;
end;

procedure TLibCommandLine.TidyLibraryName(_mustexist: boolean);
begin
  FLibName := ExpandFilename(FLibName);
  if Pos('.',FLibName) = 0 then
    FLibName := FLibName + FILETYPE_LIBRARY;
  if _mustexist and (not FileExists(FLibName)) then
    raise Exception.Create('Library ' + FLibName + ' cannot be found');
end;

procedure TLibCommandLine.TidyTargetFilenames;
var ptr, nptr: integer;
    srchname: UnicodeString;
    srchrec:  TUnicodeSearchRec;
begin
  // Sort out wildcards first
  // Work from the end backwards
  ptr := FTargets.Count-1;
  while ptr >= 0 do
    begin
      FTargets[ptr] := ExpandFilename(FTargets[ptr]);
      if (Pos('?',FTargets[ptr]) > 0) or (Pos('*',FTargets[ptr]) > 0) then
        begin  // Process wildcard
          srchname := UnicodeString(FTargets[ptr]);
          FTargets.Delete(ptr);
          nptr := ptr;
          if FindFirst(srchname,faReadonly or faArchive,srchrec) = 0 then
            begin
              repeat
                FTargets.Insert(nptr,ExtractFilePath(srchname)+string(srchrec.Name));
                nptr := nptr + 1;
              until FindNext(srchrec) <> 0;
              FindClose(srchrec);
            end;
        end;
      ptr := ptr-1; // Move back one
    end;
  // Add .obj80 if required
  for ptr := 0 to FTargets.Count-1 do
    begin
      if Pos('.',FTargets[ptr]) = 0 then
        FTargets[ptr] := FTargets[ptr] + FILETYPE_OBJECT;
    end;
end;

end.

