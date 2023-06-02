unit uasmglobals;

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

{$mode objfpc}{$H+}


//
// Global definitions for the assembler
//

interface

uses
  Classes, SysUtils;


const
  DEFAULT_GRAMMAR_VALUE             = 'XA80';
  DEFAULT_LISTING_MARGIN_LEFT       = 0;
  DEFAULT_LISTING_MARGIN_TOP        = 2;
  DEFAULT_LISTING_PAGE_LENGTH       = 60;
  DEFAULT_LISTING_PAGE_WIDTH        = 120;
  DEFAULT_ORG                       = 0;
  DEFAULT_PROCESSOR                 = 'Z80';
  DEFAULT_TAB_SIZE                  = 4;
  DEFAULT_WARNINGS                  = 1;
  ENVIRONMENT_VARIABLE              = 'XA80';
  FILETYPE_COM                      = '.com';
  FILETYPE_DEBUG                    = '.dbg80';
  FILETYPE_HEX                      = '.hex';
  FILETYPE_LIST                     = '.lst';
  FILETYPE_LOG                      = '.log';
  FILETYPE_MAP                      = '.map';
  FILETYPE_OBJECT                   = '.obj80';
  INCLUDE_FILE_DELIMITER            = ',';
  HEX_FILE_BYTES_PER_LINE           = 16;
  MAX_BYTES_PER_CODE_RECORD         = 8192;
  MAX_DIGITS_LINENUMBER             = 5;
  MAX_DIGITS_PAGENUMBER             = 4;
  MAX_HEX_BYTES_IN_LISTING          = 6;
  MAX_INDENT_DISPLAY                = 5; // Number of >>> to show for include and macro
  MAX_LINE_LENGTH                   = 4096;
  MAX_NESTED_INCLUDES               = 16;
  MAX_OPERANDS                      = 2;
  MAX_SOURCE_BYTES_PER_LISTING_LINE = 4;
  TEMP_FILE_DIVIDER                 = '?END?';
  VERSION_STRING                    = '0.2';
  // Derived constants
  MAX_HEX_WIDTH                     = (MAX_HEX_BYTES_IN_LISTING*2+MAX_HEX_BYTES_IN_LISTING-1);
//  HEXBUF_LENGTH                     = ((3*MAX_HEX_BYTES_IN_LISTING) + 7);


const DIGITS:  set of char = ['0'..'9'];
      ALPHA:   set of char = ['A'..'Z','a'..'z'];
      DEFAULT_ESCAPE = '\'; // Escape character
      DEFAULT_ESCAPED: set of char = [#34,#39,'a','n','r','t','\'];
      BAD:     set of char = [#0..#31,#127];
      LABELX:  set of char = ['.','?','@','_','$'];
      TAB = #9;     // Tab
      LF = #10;     // Line feed
      FF = #12;     // Form feed
      CR = #13;     // Carriage return
      SQ = #39;     // Single quote '
      DQ = #34;     // Double quote "
{$IFDEF WINDOWS}
      LINE_TERMINATOR = CR + LF;
{$ELSE}
      LINE_TERMINATOR = LF;
{$ENDIF}


type
  TAsmInt = int32;
//  TSetOfChar = set of char;


type
  TInstructionType = (itOpcode,itDirective,itMacro);

  // TInstruction could be Opcode, Directive, Macro
  // This is an abstract class so other classes need to be derived from it
  TInstruction = class(TObject)
    private
      function GetInstructionType: TInstructionType; virtual; abstract;
    published
      property InstructionType: TInstructionType read GetInstructionType;
  end;

  // Directive instructions
  TDirectiveInstruction = class(TInstruction)

  end;

  // Macro instructions
  TMacroInstruction = class(TInstruction)

  end;

  // Opcode instructions
  TOpcodeInstruction = class(TInstruction)

  end;


  TCodeRecord = record
    start_address: word;
    data_length:   integer;
    code_segment:  array [0..MAX_BYTES_PER_CODE_RECORD-1] of byte;
  end;

  TSourceLine = record
    Filename:   string;
    LineNumber: integer;
    OutputCode: boolean;
    OutputList: boolean;
    SourceLine: string;
    CodeRecord: TCodeRecord;
  end;



implementation

end.

