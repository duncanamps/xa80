unit umessages;

{$mode ObjFPC}{$H+}

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

interface

//
//  Messages are store in four levels:
//
//    I - Information
//    W - Warning (assembly can continue)
//    E - Error (assembly fails)
//    X - Internal exception - should not happen
//
//  Each of these is followed by a four digit number:
//
//    0000-0999 - Information
//    1000-1999 - Warning
//    2000-2999 - Error
//    3000-3999 - eXception
//
//  Examples are:
//
//    I0001 - Assembly started
//    I0002 - Assembly completed %7.3f seconds
//    W1078 -
//    E2015 - Illegal escape character %s
//    E2016 - Unterminated string
//    X3002 - Unhandled option in %s
//

uses
  Classes, SysUtils;

type
  TLogType = (ltDebug,ltWarAndPeace,ltVerbose,ltInfo,ltWarning,ltError,ltInternal);

  EErrorException = class(Exception);      // Exception for trapped errors
  EInternalException = class(Exception);   // Exception for internal errors
                                           // which shouldn't happen...


  TMessageNumbers = (I0001_ASSEMBLY_STARTED,
                     I0002_ASSEMBLY_ENDED,
                     I9999_DEBUG_MESSAGE,
                     W1001_CODE_WRAPPED_ROUND,
                     W1002_UNRECOGNISED_COMMAND_LINE_OPTION,
                     E2001_ILLEGAL_ESCAPE_CHARACTER,
                     E2002_UNTERMINATED_STRING,
                     E2003_UNRECOGNISED_CONTENT,
                     X3001_UNHANDLED_CASE_OPTION,
                     X3999_UNHANDLED_EXCEPTION
                    );

// TMonitorProc = procedure (LogType: TLogType; const Message: string) of object;

  TErrorObject = class(TObject)
    private
      FLogStream: TFileStream;
      FStartTime: TDateTime;
      function  MonitorString(_logtype: TLogType; _msgno: TMessageNumbers; _args: array of const): string;
    public
      ColNumber:  integer;
      Filename:   string;
      InfoLimit:  TLogType;
      LineNumber: integer;
      Silent:     boolean;
      constructor Create;
      destructor  Destroy; override;
      procedure SetFilename(_fn: string);
      procedure Show(_logtype: TLogType; _msgno: TMessageNumbers);
      procedure Show(_logtype: TLogType; _msgno: TMessageNumbers; _args: array of const);
  end;

var
  ErrorObj: TErrorObject;
  IsTerminal: set of TLogType = [ltInternal,ltError];


implementation

uses
  uasmglobals, typinfo;

var
  ErrorMessages: array[I0001_ASSEMBLY_STARTED..X3999_UNHANDLED_EXCEPTION] of string =
  (
    'Assembly started',
    'Assembly completed',
    'DEBUG: %s',
    'Code wrapped around back to zero',
    'Unrecognised command line option',
    'Illegal escape character %s, valid are %s',
    'Unterminated string %s',
    'Unrecognised content %s',
    'Unhandled case option at %s',
    'Unhandled exception %s'
  );

constructor TErrorObject.Create;
begin
  inherited Create;
  FLogStream  := nil;
  ColNumber   := 0;
  Filename    := '';
  InfoLimit   := ltInfo;
  LineNumber  := 0;
  Silent      := False;
  FStartTime  := Now;
end;

destructor TErrorObject.Destroy;
begin
  if Assigned(FLogStream) then
    FreeAndNil(FLogStream);
  inherited Destroy;
end;

function TErrorObject.MonitorString(_logtype: TLogType; _msgno: TMessageNumbers; _args: array of const): string;
var s: string;
    enum: string;
begin
  s := Format('[%7.3f] ',[(Now-FStartTime)*86400.0]);
  if _logtype >= ltWarning then
    begin
      // Prepend the line number, column number and ANNNN message number
      enum := GetEnumName(TypeInfo(TMessageNumbers),Ord(_msgno));
      enum := LeftStr(enum,5);
      s := s + enum + ' ';
    end;
  s := s + Format(ErrorMessages[_msgno],_args);
  if _logtype >= ltWarning then
    begin
      // Append the line number and filename
      if (LineNumber > 0) and (ColNumber > 0) and (Filename <> '') then
        begin
          s := s + LINE_TERMINATOR;
          s := s + Format('          at %d,%d in %s',[LineNumber,ColNumber,Filename]);
        end;
    end;
  MonitorString := s;
end;

procedure TErrorObject.SetFilename(_fn: string);
begin
  if Assigned(FLogStream) then
    FreeAndNil(fLogStream);
  FLogStream := TFileStream.Create(_fn,fmCreate);
end;

procedure TErrorObject.Show(_logtype: TLogType; _msgno: TMessageNumbers);
begin
  Show(_logtype,_msgno,[]);
end;

procedure TErrorObject.Show(_logtype: TLogType; _msgno: TMessageNumbers; _args: array of const);
var msg: string;
begin
  msg := MonitorString(_logtype,_msgno,_args);
  WriteLn(msg);
  if Assigned(FLogStream) then
    begin
      msg := msg + LINE_TERMINATOR;
      FLogStream.Write(msg[1],Length(msg));
    end;
  case _logtype of
    ltError:    raise EErrorException.Create(msg);
    ltInternal: raise EInternalException.Create(msg);
  end;
end;

initialization
  ErrorObj := TErrorObject.Create;

finalization
  FreeAndNil(ErrorObj);

end.

