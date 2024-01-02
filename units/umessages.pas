unit umessages;

{$mode ObjFPC}{$H+}

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2024 Duncan Munro

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
  Classes, SysUtils, lacogen_types;

type
//  TLogType = (ltDebug,ltWarAndPeace,ltVerbose,ltInfo,ltWarning,ltError,ltInternal);

//  EErrorException = class(Exception);      // Exception for trapped errors
//  EInternalException = class(Exception);   // Exception for internal errors
                                           // which shouldn't happen...


  TMessageNumbers = (I0000_USER_INFO,
                     I0001_ASSEMBLY_STARTED,
                     I0002_ASSEMBLY_ENDED,
                     I0003_ASSEMBLING_FILE,
                     I0004_FILENAME_ASSIGNMENT,
                     I0005_PROCESSOR_IS,
                     I0006_SEARCHING_FOR_INCLUDE,
                     I0007_PROCESSING_INCLUDE,
                     I9999_DEBUG_MESSAGE,

                     W1000_USER_WARNING,
                     W1001_CODE_WRAPPED_ROUND,
                     W1002_DIRECTIVE_IGNORED,
                     W1003_LABEL_REDEFINED,
                     W1004_END_OPERANDS_IGNORED,
                     W1005_SYMBOL_UNDEFINED,
                     W1006_COMMAND_AS_SYMBOL,
                     W1007_MACRO_CALL_MISMATCH,
                     W1008_NO_DEFAULT_SEGMENT,
                     W1009_SEGMENT_MODIFIERS_IGNORED,
                     W1010_SEGMENT_MODIFIER_CLASH,

                     E2000_USER_ERROR,
                     E2001_ILLEGAL_ESCAPE_CHARACTER,
                     E2002_UNTERMINATED_STRING,
                     E2003_UNRECOGNISED_CONTENT,
                     E2004_EXPECTED_NUMBER,
                     E2005_INTEGER_OVERFLOW,
                     E2006_REMOVED,
                     E2007_REMOVED,
                     E2008_REMOVED,
                     E2009_DIVIDE_BY_ZERO,
                     E2010_EXPECTED_STRING,
                     E2011_EXPECTED_POS_NUMBER,
                     E2012_CONVERSION_ERROR,
                     E2013_PARSER_ERROR,
                     E2014_UNABLE_TO_PARSE,
                     E2015_CODE_SYMBOL_DEFINED,
                     E2016_COLON_NOT_PRESENT,
                     E2017_UNEXPECTED_OPERANDS,
                     E2018_OPERAND_NO_DATA_TYPE,
                     E2019_EXPECTED_INTEGER,
                     E2020_UNEXPECTED_LABEL,
                     E2021_INSTRUCTION_UNAVAILABLE,
                     E2022_OPERANDS_EXPECTED,
                     E2023_BYTE_RANGE_ERROR,
                     E2024_CODE_BUFFER_OVERFLOW,
                     E2025_EMPTY_STRING_NOT_ALLOWED,
                     E2026_INTEGER_RANGE_ERROR,
                     E2027_RELATIVE_DISTANCE,
                     E2028_BIT_NUMBER,
                     E2029_IM_NUMBER,
                     E2030_USING_RESERVED_AS_LABEL,
                     E2031_FILE_NOT_FOUND,
                     E2032_UNEXPECTED_CHARACTER,
                     E2033_UNDEFINED_PARSER_TABLE_ACTION,
                     E2034_UNEXPECTED_TOKEN,
                     E2035_PARSER_STACK_EXCEEDED,
                     E2036_INVALID_LABEL_CHARACTER,
                     E2037_CODE_AFTER_END,
                     E2038_INVALID_SHOW_OPTION,
                     E2039_SWITCH_MISSING_VALUE,
                     E2040_MISSING_EQUALS,
                     E2041_PREMATURE_STRING_END,
                     E2042_INVALID_COMMAND_LINE_SWITCH,
                     E2043_INVALID_COMMAND_OPCODE,
                     E2044_INCLUDE_FILE_NOT_FOUND,
                     E2045_MAXIMUM_INCLUDES_EXCEEDED,
                     E2046_EXPECTED_LABEL,
                     E2047_UNEXPECTED_END,
                     E2048_UNEXPECTED_ENDIF,
                     E2049_UNEXPECTED_ELSE,
                     E2050_ELSE_ALREADY_USED,
                     E2051_UNEXPECTED_ENDW,
                     E2052_ENDW_IN_DIFFERENT_FILE,
                     E2053_UNEXPECTED_ENDR,
                     E2054_ENDR_IN_DIFFERENT_FILE,
                     E2055_UNEXPECTED_ENDM,
                     E2056_MACRO_IN_MACRO_DEFINE,
                     E2057_MACRO_NOT_FOUND,
                     E2058_NO_LABEL_ON_ENDM,
                     E2059_COMMAND_LINE_DEFINE,
                     E2060_UNEXPECTED_DIRECTIVE,
                     E2061_PROCESSOR_NOT_LOADED,
                     E2062_COMMAND_ALREADY_USED_AS_LABEL,
                     E2063_COMMAND_ALREADY_USED,
                     E2064_FAILED_MACRO_EXPANSION,
                     E2065_ILLEGAL_SEGMENT_MODIFIER,

                     X3001_UNHANDLED_CASE_OPTION,
                     X3002_PREPARSER_PEEK_ERROR,
                     X3003_PROCEDURE_NOT_IN_GRAMMAR,
                     X3004_REDUCTION_NOT_DEFINED,
                     X3005_BINARY_CONVERSION_FAILURE,
                     X3006_HEX_CONVERSION_FAILURE,
                     X3007_INVALID_ELEMENT_TYPE,
                     X3008_RESOURCE_NOT_FOUND,
                     X3009_PARSER_TABLES_NOT_LOADED,
                     X3010_UNEXPECTED_GOTO,
                     X3011_STACK_EMPTY,
                     X3012_PARSER_GOTO_ERROR,
                     X3013_LEXER_SET_ERROR,
                     X3015_POP_FROM_EMPTY_STACK,

                     X3999_UNHANDLED_EXCEPTION
                    );

// TMonitorProc = procedure (LogType: TLogType; const Message: string) of object;

  TErrorObject = class(TObject)
    private
      FLogFilename:       string;
      FLogStream:         TFileStream;
      FStartTime:         TDateTime;
      FWarnings:          boolean;
      FWarningsAvailable: boolean;
      function  MonitorString(_logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const): string;
    public
      ColNumber:  integer;
      Filename:   string;
      InfoLimit:  TLCGLogType;
      LineNumber: integer;
      Silent:     boolean;
      SourceLine: string;
      constructor Create;
      destructor  Destroy; override;
      procedure SetLogFilename(_fn: string);
      procedure Show(_logtype: TLCGLogType; _msgno: TMessageNumbers);
      procedure Show(_logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
      property LogFilename: string read FLogFilename write SetLogFilename;
      property Warnings: boolean read FWarnings write FWarnings;
      property WarningsAvailable: boolean read FWarningsAvailable write FWarningsAvailable;
  end;

var
  ErrorObj: TErrorObject;
  IsTerminal: set of TLCGLogType = [ltInternal,ltError];


implementation

uses
  uasmglobals, typinfo;

var
  ErrorMessages: array[I0000_USER_INFO..X3999_UNHANDLED_EXCEPTION] of string =
  (
    '%s',
    'Assembly started',
    'Assembly completed',
    'Assembling file %s',
    'Filename for %s is %s',
    'Processor is %s',
    'Searching for include file %s at %s',
    'Processing include file %s',
    'DEBUG: %s',

    '%s',
    'Code wrapped around back to zero',
    'Directive %s ignored',
    'Symbol %s has been redefined',
    'Operands after END directive ignored',
    'Symbol %s is undefined',
    'Symbol %s replaces command of the same name',
    'Macro parameter count mismatch - expected %d parameters, received %d',
    'Outputting code with no segment definition, default CSEG created',
    'Segment modifiers ignored, segment %s has already been defined',
    'Segment modifier %s clashes with a preceding modifier',

    '%s',
    'Illegal escape character %s, valid are %s',
    'Unterminated string %s',
    'Unrecognised content %s',
    'Expected number %s',
    'Integer overflow',
    'Binary literal %s is too short',
    'Octal literal %s is too short',
    'Hex literal %s is too short',
    'Divide by zero',
    'Expected string %s',
    'Expected positive number %s',
    'String %s failed to convert',
    'Parser error %s',
    'Unable to parse input %s',
    'Code symbol %s has already been defined',
    'Mandatory colon not present in code label %s',
    'Unexpected operands',
    'Operand no. %d is of indeterminate data type',
    'Expected integer',
    'Unexpected label %s, ignored',
    'Instruction not available for %s',
    'Operands expected',
    'Byte must be in range -127..255',
    'Code buffer limit of %d bytes exceeded',
    'Empty string is not allowed',
    'Integer must be in the range %d to %d',
    'Illegal distance of %d for relative branch, should be -128..+127',
    'Bit number for SET/RES must be in the range 0..7',
    'Operand for IM instruction must be in the range 0..2',
    'Using reserved word for label %s',
    'File not found %s',
    'Unexpected character %s in input',
    'Undefined parser table action for state %d and token %s',
    'Unexpected token %s in input',
    'PARSER_STACK_SIZE_MAX (%d) exceeded',
    'Invalid character in label',
    'Attempt to perform activities after END directive',
    'Invalid option %s for -s/--show command line parameter',
    'Mandatory value missing after switch %s',
    'Equals expected but not found in command or environment',
    'Premature end of string in command or environment %s',
    'Invalid command line switch %s',
    '%s is not a valid command directive or processor instruction',
    'Include file %s not found',
    'Maximum number of includes (%d) exceeded',
    'Expected label, found %s',
    'Unexpected end: %s',
    'Unexpected ENDIF',
    'Unexpected ELSE',
    'More than one ELSE statement between IF and ENDIF',
    'Unexpected ENDW',
    'ENDW in different file to WHILE statement (%s)',
    'Unexpected ENDR',
    'ENDR in different file to REPEAT statement (%s)',
    'Unexpected ENDM',
    'Cannot define a macro within another macro',
    'Macro %s not found',
    'Cannot place a label on an ENDM directive',
    'Command line define error, could not process %s',
    'Unexpected directive %s, have already processed directive for %s',
    'Could not load processor details from %s',
    'Command %s has already been used as a label',
    'Command %s cannot now be used as a label',
    'Failed macro expansion',
    'Illegal segment modifier %s',

    'Unhandled case option at %s',
    'Preparser peeek error',
    'Could not find procedure %s in grammar',
    'Reduction code not defined for rule no. %d (%s)',
    'Binary constant %s failed to convert',
    'Hex constant %s failed to convert',
    'Invalid element type',
    'Resource not found %s',
    'Parser tables not loaded',
    'Unexpected goto from table',
    'Stack empty',
    'Goto expected for rule index #%d but not found in table',
    'Attempt to set lexer buffer size while in the middle of an activity',
    'Attempt to pop from empty execution stack',

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
  SourceLine  := '';
  Silent      := False;
  FStartTime  := Now;
  FWarnings   := True;
end;

destructor TErrorObject.Destroy;
begin
  if Assigned(FLogStream) then
    FreeAndNil(FLogStream);
  inherited Destroy;
end;

function TErrorObject.MonitorString(_logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const): string;
var s: string;
    enum: string;
begin
  s := Format('[%7.3f] ',[(Now-FStartTime)*86400.0]);
  if _logtype <= ltWarning then
    begin
      // Prepend the type of message
      case _logtype of
        ltInternal:     s := s + 'INTERNAL ';
        ltError:        s := s + 'ERROR ';
        ltWarning:      s := s + 'Warning ';
        ltInfo,
        ltVerbose,
        ltWarAndPeace:  s := s + 'Info ';
        ltDebug:        s := s + 'Debug ';
      end;
      // Prepend the line number, column number and ANNNN message number
      enum := GetEnumName(TypeInfo(TMessageNumbers),Ord(_msgno));
      enum := LeftStr(enum,5);
      s := s + enum + ' ';
    end;
  s := s + Format(ErrorMessages[_msgno],_args);
  if _logtype <= ltWarning then
    begin
      // Append the line number and filename
      if (LineNumber > 0) and (Filename <> '') then
        begin
          s := s + LINE_TERMINATOR;
          if ColNumber <= 0 then
            s := s + Format('          at line %d in %s',[LineNumber,Filename])
          else
            s := s + Format('          at line %d col %d in %s',[LineNumber,ColNumber,Filename]);
          s := s + LINE_TERMINATOR;
          if ColNumber > 0 then
            begin
              s := s + Space(10) + SourceLine;
              s := s + LINE_TERMINATOR;
              s := s + Space(10+ColNumber-2) + '_^_';
            end;
        end;
    end;
  MonitorString := s;
end;

procedure TErrorObject.SetLogFilename(_fn: string);
begin
  if Assigned(FLogStream) then
    FreeAndNil(FLogStream);
  if _fn <> '' then
    FLogStream := TFileStream.Create(_fn,fmCreate);
  FLogFilename   := _fn;
end;

procedure TErrorObject.Show(_logtype: TLCGLogType; _msgno: TMessageNumbers);
begin
  Show(_logtype,_msgno,[]);
end;

procedure TErrorObject.Show(_logtype: TLCGLogType; _msgno: TMessageNumbers; _args: array of const);
var msg: string;
begin
  if (not (FWarnings and FWarningsAvailable)) and (_logtype = ltWarning) then
    Exit;
  if _logtype > InfoLimit then
    Exit;
  msg := MonitorString(_logtype,_msgno,_args);
{$IFDEF CONSOLE_APP}
  WriteLn(msg);
{$ENDIF}
  if Assigned(FLogStream) then
    begin
      msg := msg + LINE_TERMINATOR;
      FLogStream.Write(msg[1],Length(msg));
    end;
  case _logtype of
    ltError:    raise LCGErrorException.Create(msg);
    ltInternal: raise LCGInternalException.Create(msg);
  end;
end;

initialization
  ErrorObj := TErrorObject.Create;

finalization
  FreeAndNil(ErrorObj);

end.

