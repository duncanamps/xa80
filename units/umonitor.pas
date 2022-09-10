unit umonitor;

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

//
// Monitor unit, handles and logs error messages and informational messages
//

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  AsmException = class(Exception);

  TMonitorType = (mtInternal,mtError,mtWarning,mtInfo,mtVerbose,mtWarAndPeace,mtDebug);

var
  MonitorFilename: string;
  MonitorLevel:    TMonitorType = mtInfo;

procedure Monitor(_type: TMonitorType; const _msg: string);
procedure Monitor(_type: TMonitorType; const _fmt: string; _args: array of const);
procedure Monitor(_pass: integer; const _filename: string; _line, _col: integer;  _type: TMonitorType; const _msg: string);
procedure Monitor(_pass: integer; const _filename: string; _line, _col: integer;  _type: TMonitorType; const _fmt: string; _args: array of const);
function  mtToString(_mt: TMonitorType): string;

implementation

uses
  uutility;

var
  ProgramStartTime: TDateTime;

// Taken from RTL file and converted to use string length instead of canvas textwidth

function MinimizeName(FileName: String; MaxWidth: Integer): String;
{
 This function will return a shortened version of FileName, so that it fits
 with a given MaxWidth.
 eg. C:\Documents and Settings\User\Application Data\Microsoft\Word\custom.dic
     would become something like: C:\...\Word\custom.dic
}
  procedure RemoveFirstDir(var Dir: String);
  {
   This procedure will remove the first directory from Dir
   and will set ADelim to the Delimiter that separated the first Dir
   eg. In: Dir: 'Dir1\Dir2\Dir3'
  }
  var p: Integer;
  begin
    p:= Pos(PathDelim,Dir);
    if (p > 0) then
    begin
      Dir := Copy(Dir,p+1,Length(Dir)-p);
    end;
  end;

var Drive, Dir, Fn: String;
    ComposedName: String;
    TWidth: Integer;
begin
  Result := FileName;
  //if FileName does not contain any (sub)dir then return FileName
  if Pos(PathDelim, FileName) = 0 then Exit;
  //if FileName fits, no need to do anyhing
  if Length(FileName) <= MaxWidth then Exit;
  Drive := ExtractFileDrive(FileName);
  Fn := ExtractFileName(FileName);
  Dir := ExtractFilePath(FileName);
  //Remove Drive from Dir
  if (Length(Drive) > 0) then System.Delete(Dir, 1, Length(Drive));
  //Transfer all PathDelimiters at the start of Dir to Drive
  While (Length(Dir) > 0) and (Dir[1] in ['/','\']) do
  begin
    Drive := Drive + Dir[1];
    System.Delete(Dir,1,1);
  end;
  //if Dir is empty then we cannot shorten it,
  //and we know at this point that Drive+FileName is too long, so we return only filename
  if (Length(Dir) = 0) then
  begin
    Result := Fn;
    Exit;
  end;
  repeat
    //at this point we know that Dir ends with PathDelim (otherwise we exited before this point,
    //so RemoveFirstDir will return a truncated Dir or an empty string
    RemoveFirstDir(Dir);
    ComposedName := Drive+'...'+PathDelim+Dir+Fn;
    TWidth := Length(ComposedName);
  until (Length(Dir) = 0) or (TWidth <= MaxWidth);
  if (TWidth <= MaxWidth) then Result := ComposedName else Result := Fn;
end;

function mtToString(_mt: TMonitorType): string;
begin
  case _mt of
    mtInternal:    Result := 'INTERNAL   ';
    mtError:       Result := 'ERROR      ';
    mtWarning:     Result := 'WARNING    ';
    mtInfo:        Result := 'Info       ';
    mtVerbose:     Result := 'Verbose    ';
    mtWarAndPeace: Result := 'WarAndPeace';
    mtDebug:       Result := 'Debug      ';
  end;
end;

procedure Monitor(_type: TMonitorType; const _msg: string);
var msg: string;
    elapsed: double;
    strm:    TFileStream;
begin
  if _type > MonitorLevel then
    Exit;
  elapsed := (Now - ProgramStartTime) * 86400.0;
  msg := Format('%6.3f %s %s',[elapsed,mtToString(_type),_msg]);
  WriteLn(msg);
  if MonitorFilename <> '' then
    begin
      strm := TFileStream.Create(MonitorFilename,fmOpenWrite);
      strm.Position := strm.Size; // Seek to end
      try
        strm.WriteAnsiString(msg + LineTerminator);
      finally
        FreeAndNil(strm);
      end;
    end;
  // Raise as an exception if we have a bad error
  if _type <= mtError then
    raise AsmException.Create(msg);
end;

procedure Monitor(_type: TMonitorType; const _fmt: string; _args: array of const);
begin
  Monitor(_type,Format(_fmt,_args));
end;

procedure Monitor(_pass: integer; const _filename: string; _line, _col: integer;  _type: TMonitorType; const _msg: string);
const MONITOR_FILENAME_WIDTH = 40;
var fn: string;
begin
  fn := MinimizeName(_filename,MONITOR_FILENAME_WIDTH);
  Monitor(_type,'[%1d:%5d/%3d] %s in %s',[_pass,_line,_col,_msg,fn]);
end;

procedure Monitor(_pass: integer; const _filename: string; _line, _col: integer;  _type: TMonitorType; const _fmt: string; _args: array of const);
begin
  Monitor(_pass,_filename,_line,_col,_type,Format(_fmt,_args));
end;

initialization
  ProgramStartTime := Now;


end.

