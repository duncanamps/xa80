unit umessages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogType = (ltInternal,ltError,ltWarning,ltInfo,ltVerbose,ltWarAndPeace,ltDebug);

  TErrorException = class(Exception);      // Exception for trapped errors
  TInternalException = class(Exception);   // Exception for internal errors
                                             // which shouldn't happen...

  TMonitorProc = procedure (LogType: TLogType; const Message: string) of object;

implementation

end.

