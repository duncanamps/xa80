unit deployment_parser_types_12;

{
    LaCoGen - LAzarus COmpiler GENerator
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

interface

uses
  Classes, SysUtils;


const

  // Some defaults

  LEXBUF_BLOCK_SIZE_DEFAULT = 4096;      // Size of each block in bytes to read into the lexer
  LEXBUF_MIN                = 64;        // Minimum number of characters to keep in lexer buffer for comment lookahead
  PARSER_STACK_SIZE_DEFAULT = 100;       // Default, the parser stack size will grow if needed
  PARSER_STACK_SIZE_MAX     = 800;       // Maximum
  PREDEFINED_EMPTY_STATE    = $7FFFFFFF;
  PREDEFINED_EMPTY_TOKEN    = $7FFFFFFF;
  PREDEFINED_TOKEN_ERROR    = 0;
  PREDEFINED_TOKEN_EOF      = 1;
  PREDEFINED_TOKEN_COMMENT  = 2;
  TOKEN_BUF_SIZE_DEFAULT    = 1024;      // The maximum size of a token in characters
  UNICODE_ERROR_CHARACTER   = $00FFFD;
  UNICODE_MAXIMUM_CHARACTER = $10FFFF;
  UNICODE_MAXIMUM_CODEBYTE  = $F4;

{$IFDEF UTF8}
  STRING_START = 0;
  STRING_END   = -1;
{$ELSE}
  STRING_START = 1;
  STRING_END   = 0;
{$ENDIF}


type

  // Error handling stuff


  TLCGLogType = (ltInternal,ltError,ltWarning,ltInfo,ltVerbose,ltWarAndPeace,ltDebug);

  LCGErrorException = class(Exception);      // Exception for trapped errors
  LCGInternalException = class(Exception);   // Exception for internal errors
                                             // which shouldn't happen...


  // Some data types

  TChar   = char;
  TCharN  = UINT8;
  TString = AnsiString;
  TSetOfChar = set of char;

  TLCGStateIdentifier = UINT32;
  TLCGTokenIdentifier = UINT32;
  TStringArray = array of TString;

  TLCGLexerMode = (lmStart,lmOperating,lmFiledone,lmEOF);

  TLCGParserOutputType = (potUndefined,potError,potShift,potGoto,potReduce,potAccept);


  // Lexer info record, use this to pass stuff back

  TToken = record
    Row:      integer;
    Col:      integer;
    Buf:      TString;
    ID:       TLCGTokenIdentifier;
  end;

  { @@@@@ DOES TLCGLexerInfo ACTUALLY GET USED? @@@@@ }

  TLCGLexerInfo = record
    Overflow: boolean;
    Next:     char;     // Next character in the queue
    Token:    TToken;
  end;

  // Stack items

  TLCGParserStackType = (pstINT32,pstString);

  TLCGParserStackEntry = record
      State:     TLCGStateIdentifier;
      Token:	 TToken;
      BufType:   TLCGParserStackType;
      Buf:       TString;
      BufInt:    int32;
    end;

  TLCGParserStack = array of TLCGParserStackEntry;

const
  EmptyStackEntry: TLCGParserStackEntry = (State:   0;
                                           Token:   (Row: 0; Col: 0; Buf: ''; ID: 0);
                                           BufType: pstString;
                                           Buf:     '';
                                           BufInt:  0);

implementation

end.

