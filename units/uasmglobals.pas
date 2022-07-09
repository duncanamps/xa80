unit uasmglobals;

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
// Global definitions for the assembler
//

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


const
  DEFAULT_LISTING_MARGIN_LEFT       = 0;
  DEFAULT_LISTING_MARGIN_TOP        = 2;
  DEFAULT_LISTING_PAGE_LENGTH       = 40;
  DEFAULT_LISTING_PAGE_WIDTH        = 120;
  DEFAULT_PROCESSOR_VALUE           = 'Z80';
  DEFAULT_TAB_SIZE                  = 4;
  INCLUDE_FILE_DELIMITER            = ',';
  HEX_FILE_BYTES_PER_LINE           = 16;
  MAX_BYTES_PER_CODE_RECORD         = 256;
  MAX_DIGITS_LINENUMBER             = 5;
  MAX_DIGITS_PAGENUMBER             = 4;
  MAX_HEX_BYTES_IN_LISTING          = 8;
  MAX_LINE_LENGTH                   = 4096;
  MAX_NESTED_FILES                  = 16;
  MAX_OPERANDS                      = 2;
  MAX_SOURCE_BYTES_PER_LISTING_LINE = 4;
  TEMP_FILE_DIVIDER                 = '?END?';
  // Derived constants
  HEXBUF_LENGTH                     = ((3*MAX_HEX_BYTES_IN_LISTING) + 7);


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


function BinToDecStr(_s: string): string;
function BooleanToYN(_b: boolean): string;
// function ExpandTabs(_src: string; _tabsize: integer): string;
function NCSPos(_a, _b: string): integer;
function OctToDecStr(_s: string): string;
procedure UnderlinedText(_sl: TStringList; _text: string; _blank_after: boolean = True; _underline_char: char = '-');

implementation

function BinToDecStr(_s: string): string;
var i: integer;
    v: integer;
begin
  v := 0;
  for i := 3 to Length(_s) do // Start at 3 to skip the '0b' at the start
    begin
      v := v * 2;
      v := v + Ord(_s[i]) - Ord('0');
    end;
  result := IntToStr(v);
end;

function BooleanToYN(_b: boolean): string;
begin
  if _b then
    BooleanToYN := 'Y'
  else
    BooleanToYN := 'N';
end;

{
function ExpandTabs(_src: string; _tabsize: integer): string;
var i: integer;
begin
  result := '';
  for i := 1 to Length(_src) do
    if _src[i] = #9 then
      repeat  // Expand to boundary
        result := result + ' ';
      until (Length(result) mod _tabsize) = 0
    else
      result := result + _src[i];
  ExpandTabs := result;
end;
}

function NCSPos(_a, _b: string): integer;
begin
  NCSPos := Pos(UpperCase(_a),UpperCase(_b));
end;

function OctToDecStr(_s: string): string;
var i: integer;
    v: integer;
begin
  v := 0;
  for i := 2 to Length(_s) do // Start at 2 to skip the '0' at the start
    begin
      v := v * 8;
      v := v + Ord(_s[i]) - Ord('0');
    end;
  result := IntToStr(v);
end;

procedure UnderlinedText(_sl: TStringList; _text: string; _blank_after: boolean = True; _underline_char: char = '-');
begin
  _sl.Add(_text);
  _sl.Add(StringOfChar(_underline_char,Length(_text)));
  if _blank_after then
    _sl.Add('');
end;


end.

