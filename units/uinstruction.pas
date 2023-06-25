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

unit uinstruction;


{$mode ObjFPC}{$H+}

//
// Deals with the instruction table which is loaded from a resource as
// required
//

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TOperandOption = (OPER_NULL,
  	            OPER_A,
  	            OPER_AF,
  	            OPER_AF_,
  	            OPER_B,
  	            OPER_BC,
  	            OPER_BC_IND,
  	            OPER_C,
  	            OPER_C_IND,
  	            OPER_D,
  	            OPER_DE,
  	            OPER_DE_IND,
  	            OPER_E,
  	            OPER_F,
  	            OPER_H,
  	            OPER_HL,
  	            OPER_HL_IND,
  	            OPER_I,
  	            OPER_IX,
                    OPER_IXH,
                    OPER_IXL,
  	            OPER_IX_IND,
  	            OPER_IXPD_IND,
  	            OPER_IY,
                    OPER_IYH,
                    OPER_IYL,
  	            OPER_IY_IND,
  	            OPER_IYPD_IND,
  	            OPER_L,
                    OPER_M,
  	            OPER_NC,
  	            OPER_NZ,
  	            OPER_P,
  	            OPER_PE,
  	            OPER_PO,
                    OPER_PSW,
  	            OPER_R,
  	            OPER_SP,
  	            OPER_SP_IND,
  	            OPER_U8,
  	            OPER_U8_IND,
  	            OPER_U16,
  	            OPER_U16_IND,
  	            OPER_Z);

const
  CODE_ELEMENT_COUNT_MINIMUM = 1;
  CODE_ELEMENT_COUNT_MAXIMUM = 4;
  INSTRUCTION_COUNT_MINIMUM = 128;
  INSTRUCTION_COUNT_MAXIMUM = 2048;
  OPCODE_COUNT_MINIMUM = 50;
  OPCODE_COUNT_MAXIMUM = 512;
  OPCODE_LENGTH_MAXIMUM = 8;
  OPCODE_MAP_MAGIC = $4D43504F;

  OperandStrings: array[TOperandOption] of string =  ('',
  	                                        'A',
  	                                        'AF',
  	                                        'AF_',
  	                                        'B',
  	                                        'BC',
  	                                        'BC_IND',
  	                                        'C',
  	                                        'C_IND',
  	                                        'D',
  	                                        'DE',
  	                                        'DE_IND',
  	                                        'E',
  	                                        'F',
  	                                        'H',
  	                                        'HL',
  	                                        'HL_IND',
  	                                        'I',
  	                                        'IX',
                                                'IXH',
                                                'IXL',
  	                                        'IX_IND',
  	                                        'IXPD_IND',
  	                                        'IY',
                                                'IYH',
                                                'IYL',
  	                                        'IY_IND',
  	                                        'IYPD_IND',
  	                                        'L',
                                                'M',
  	                                        'NC',
  	                                        'NZ',
  	                                        'P',
  	                                        'PE',
  	                                        'PO',
                                                'PSW',
  	                                        'R',
  	                                        'SP',
  	                                        'SP_IND',
  	                                        'U8',
  	                                        'U8_IND',
  	                                        'U16',
  	                                        'U16_IND',
  	                                        'Z');

  OperandActual: array[TOperandOption] of string =  ('',
                                                'A',
  	                                        'AF',
  	                                        'AF''',
  	                                        'B',
  	                                        'BC',
  	                                        '(BC)',
  	                                        'C',
  	                                        '(C)',
  	                                        'D',
  	                                        'DE',
  	                                        '(DE)',
  	                                        'E',
  	                                        'F',
  	                                        'H',
  	                                        'HL',
  	                                        '(HL)',
  	                                        'I',
  	                                        'IX',
                                                'IXH',
                                                'IXL',
  	                                        '(IX)',
  	                                        '', // IXPD_IND
  	                                        'IY',
                                                'IYH',
                                                'IYL',
  	                                        '(IY)',
  	                                        '', // IYPD_IND
  	                                        'L',
                                                'M',
  	                                        'NC',
  	                                        'NZ',
  	                                        'P',
  	                                        'PE',
  	                                        'PO',
                                                'PSW',
  	                                        'R',
  	                                        'SP',
  	                                        '(SP)',
  	                                        '', // U8
  	                                        '', // U8_IND
  	                                        '', // U16
  	                                        '', // U16_IND
  	                                        'Z');

  OperandSanitised: array[TOperandOption] of string =  ('',
                                                'A',
  	                                        'AF',
  	                                        'AF''',
  	                                        'B',
  	                                        'BC',
  	                                        '(BC)',
  	                                        'C',
  	                                        '(C)',
  	                                        'D',
  	                                        'DE',
  	                                        '(DE)',
  	                                        'E',
  	                                        'F',
  	                                        'H',
  	                                        'HL',
  	                                        '(HL)',
  	                                        'I',
  	                                        'IX',
                                                'IXH',
                                                'IXL',
  	                                        '(IX)',
  	                                        '(IX+NN)', // IXPD_IND
  	                                        'IY',
                                                'IYH',
                                                'IYL',
  	                                        '(IY)',
  	                                        '(IY+NN)', // IYPD_IND
  	                                        'L',
                                                'M',
  	                                        'NC',
  	                                        'NZ',
  	                                        'P',
  	                                        'PE',
  	                                        'PO',
                                                'PSW',
  	                                        'R',
  	                                        'SP',
  	                                        '(SP)',
  	                                        'NN', // U8
  	                                        '(NN)', // U8_IND
  	                                        'NNNN', // U16
  	                                        '(NNNN)', // U16_IND
  	                                        'Z');

  IsIndirectArray: array[TOperandOption] of boolean = (
      False,	// OPER_NULL
      False,	// OPER_A
      False,	// OPER_AF
      False,	// OPER_AF_
      False,	// OPER_B
      False,	// OPER_BC
      True,	// OPER_BC_IND
      False,	// OPER_C
      True,	// OPER_C_IND
      False,	// OPER_D
      False,	// OPER_DE
      True,	// OPER_DE_IND
      False,	// OPER_E
      False,	// OPER_F
      False,	// OPER_H
      False,	// OPER_HL
      True,	// OPER_HL_IND
      False,	// OPER_I
      False,	// OPER_IX
      False,    // OPER_IXH
      False,    // OPER_IXL
      True,	// OPER_IX_IND
      True,	// OPER_IXPD_IND
      False,	// OPER_IY
      False,    // OPER_IYH
      False,    // OPER_IYL
      True,	// OPER_IY_IND
      True,	// OPER_IYPD_IND
      False,	// OPER_L
      False,	// OPER_M
      False,	// OPER_NC
      False,	// OPER_NZ
      False,	// OPER_P
      False,	// OPER_PE
      False,	// OPER_PO
      False,	// OPER_PSW
      False,	// OPER_R
      False,	// OPER_SP
      True,	// OPER_SP_IND
      False,	// OPER_U8
      True,	// OPER_U8_IND
      False,	// OPER_U16
      True,	// OPER_U16_IND
      False 	// OPER_Z
    );

type
  TCodeElementType = (cetNull,cetB3,cetHex,cetIM,cetR8,cetS8,cetRST,cetU8,cetU16);

  TCodeElement = record
    ElementType: TCodeElementType;
    OperandNo:   byte;              // Operand number either 1 or 2
    Value:       byte;              // Value if appropriate for code output
    Offset:      byte;              // Bit offset within binary for RST and B3
  end;

  TInstructionRec = record
    OpcodeIndex:       word;
    Operand1Index:     TOperandOption;
    Operand2Index:     TOperandOption;
    CodeElementCount:  byte;
    CodeElements:      array[0..CODE_ELEMENT_COUNT_MAXIMUM-1] of TCodeElement;
    CodeElementSize:   integer;
  end;

  TInstructionListBase = specialize TList<TInstructionRec> ;

  TInstructionList = class(TInstructionListBase)
    protected
      FHashOperandMult:  integer;
      FHashOpcodeMult:   integer;
      FHashSize:         integer;
      FHashTable:        array of integer;
      FIndirectAllowed:  boolean;
      FOpcodes:          TStringList;
      FOperandAvailable: array[TOperandOption] of boolean;
   public
      constructor Create;
      constructor Create(const _processor: string);
      destructor Destroy; override;
      function  Add(constref AValue: TInstructionRec): SizeInt; override;
      procedure AddOpcode(const _opcode: string);
      function  CalcCodeElementSize(_r: TInstructionRec): integer;
      function  CalculateHash(_opcode: word; _operand1: TOperandOption; _operand2: TOperandOption): integer;
      function  CodeElementToString(_element: TCodeElement): string;
      procedure ConstructHashTable;
      procedure Dump(_sanitised: boolean = False);
      function  FindInstruction(_opcode: word; _operand1: TOperandOption; _operand2: TOperandOption; var _r: TInstructionRec): boolean;
      function  FindNextPrime(_start: integer): integer;
      function  FindOpcode(const _opcode: string; var _index: integer): boolean;
      procedure GetSimpleOperands(_sl: TStringList);
      procedure GetInstructions(_sl: TStringList);
      procedure LoadFromFile(const _filename: string);
      procedure LoadFromStream(const _stream: TStream);
      procedure LoadFromResource(const _resource: string);
      function  OpcodeAtIndex(_index: integer): string;
      function  OpcodeCount: integer;
      procedure SaveToFile(const _filename: string);
      procedure SaveToStream(const _stream: TStream);
      procedure Sort;
      function  SimpleOpToOperandOption(const _operand: string): TOperandOption;
      property IndirectAllowed: boolean read FIndirectAllowed;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows, // For definition of RT_RCDATA
{$ENDIF}
  StrUtils, lacogen_types, umessages, uasmglobals;

{ TInstructionList }

constructor TInstructionList.Create;
begin
  inherited Create;
  FOpcodes := TStringList.Create;
  FOpcodes.Sorted := True;
  FIndirectAllowed := False;
end;

constructor TInstructionList.Create(const _processor: string);
var _proc: string;
    filename: string;
begin
  Create;
  _proc := UpperCase(_processor);
  // Try and load from one of the baked in tables first
  try
    LoadFromResource(_proc + '.OPCODE');
  except
    // Try and load from a .opcode.bin file stored near the
    // executable
    try
      filename := ExpandFilename(_proc+'.opcode.bin');
      LoadFromFile(filename);
    except
      // Couldn't do it, re-raise exception
      ErrorObj.Show(ltError,E2061_PROCESSOR_NOT_LOADED,[filename]);
    end;
  end;
end;

destructor TInstructionList.Destroy;
begin
  FreeAndNil(FOpcodes);
  inherited Destroy;
end;

function TInstructionList.Add(constref AValue: TInstructionRec): SizeInt;
begin
  Result := inherited Add(AValue);
end;

procedure TInstructionList.AddOpcode(const _opcode: string);
begin
  FOpcodes.Add(_opcode);
end;

function TInstructionList.CalcCodeElementSize(_r: TInstructionRec): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to _r.CodeElementCount-1 do
    begin
      case _r.CodeElements[i].ElementType of
        cetB3:  Result := Result + 1;
        cetHex: Result := Result + 1;
        cetIM:  Result := Result + 1;
        cetR8:  Result := Result + 1;
        cetRST: Result := Result + 1;
        cetS8:  Result := Result + 1;
        cetU8:  Result := Result + 1;
        cetU16: Result := Result + 2;
        otherwise
          raise Exception.Create('Code element not catered for');
      end;
    end;
end;

function TInstructionList.CalculateHash(_opcode: word; _operand1: TOperandOption; _operand2: TOperandOption): integer;
begin
  Result := Ord(_operand2) +
            Ord(_operand1) * FHashOperandMult +
            _opcode * FHashOperandMult * FHashOpcodeMult;
  Result := Result mod FHashSize;
end;

function TInstructionList.CodeElementToString(_element: TCodeElement): string;
begin
  case _element.ElementType of
    cetB3:  Result := Format('$%2.2X/%d:B3<<%d',[_element.Value,_element.OperandNo,_element.Offset]);
    cetHex: Result := '$' + IntToHex(_element.Value);
    cetIM:  Result := Format('%d:IM',[_element.OperandNo]);
    cetR8:  Result := Format('%d:Relative8',[_element.OperandNo]);
    cetRST: Result := Format('$%2.2X/%d:RST<<%d',[_element.Value,_element.OperandNo,_element.Offset]);
    cetS8:  Result := Format('%d:Signed8',[_element.OperandNo]);
    cetU8:  Result := Format('%d:Unsigned8',[_element.OperandNo]);
    cetU16: Result := Format('%d:Unsigned16',[_element.OperandNo]);
    otherwise
      raise Exception.Create('Code element not catered for');
  end; // Case
end;

procedure TInstructionList.ConstructHashTable;
var hash_size: integer;
    i:         integer;
    hash:      integer;
    r:         TInstructionRec;
    crashes:   integer;
    max_crash: integer;
    cur_crash: integer;
begin
  // First work out the hash table size by coming up with a value that's
  // approximately 7 times the size of the instruction table (memory is cheap)
  // so will be about 3500 entries
  hash_size := 7 * Count;
  FHashSize := FindNextPrime(hash_size);
  FHashOpcodeMult := FindNextPrime(OpcodeCount);
  FHashOperandMult := FindNextPrime(Ord(High(TOperandOption)) + 1);
  SetLength(FHashTable,FHashSize);
  for i := 0 to FHashSize-1 do
    FHashTable[i] := -1; // Indicates a null entry
  crashes := 0;
  max_crash := 0;
  for i := 0 to Count-1 do
    begin
      r := Items[i];
      hash := CalculateHash(r.OpcodeIndex,
                            r.Operand1Index,
                            r.Operand2Index);
      cur_crash := 0;
      while FHashTable[hash] <> -1 do
        begin
          // Need to bump
          Inc(hash);
          if hash >= FHashSize then
            hash := hash - FHashSize;
          Inc(crashes);
          Inc(cur_crash);
        end;
      if cur_crash > max_crash then
        max_crash := cur_crash;
      FHashTable[hash] := i;
    end;
end;

procedure TInstructionList.Dump(_sanitised: boolean);
var i: integer;
    j: integer;
    s: string;
    r: TInstructionRec;
    cs: string;
    longest: integer;
    sl: TStringList;
begin
  // Dump contents of opcode list
  WriteLn(Format('OPCODE LIST (%d items in total)',[FOpcodes.Count]));
  longest := 0;
  for i := 0 to FOpcodes.Count-1 do
    begin
      WriteLn(Format('%d:%s',[i,FOpcodes[i]]));
      if Length(FOpcodes[i]) > longest then
        longest := Length(FOpcodes[i]);
    end;
  WriteLn;
  // Dump contents of operand list
  WriteLn(Format('OPERAND LIST (%d items in total - not all of them may be listed below)',[Ord(High(TOperandOption))+1]));
  for i := 0 to Ord(High(TOperandOption))-1 do
    if FOperandAvailable[TOperandOption(i)] then
      WriteLn(Format('%d: %s',[i,OperandSanitised[TOperandOption(i)]]));
  WriteLn;
  // Dump contents of instruction list
  sl := TStringList.Create;
  try
    WriteLn(Format('INSTRUCTION LIST (%d items in total)',[Count]));
    for i := 0 to Count-1 do
      begin
        r := Items[i];
        // Do the instruction first
        s := OpcodeAtIndex(r.OpcodeIndex);
        s := PadRight(s,longest+1);
        if _sanitised then
          begin
            if r.Operand1Index <> OPER_NULL then
              s := s + OperandSanitised[r.Operand1Index];
            if r.Operand2Index <> OPER_NULL then
              s := s + ',' + OperandSanitised[r.Operand2Index];
          end
        else
          begin
            if r.Operand1Index <> OPER_NULL then
              s := s + ' ' + OperandStrings[r.Operand1Index];
            if r.Operand2Index <> OPER_NULL then
              s := s + ',' + OperandStrings[r.Operand2Index];
          end;
        // Then the code string
        if not _sanitised then
          begin
            cs := '';
            for j := 0 to r.CodeElementCount-1 do
              begin
                if j > 0 then cs := cs + ', ';
                cs := cs + CodeElementToString(r.CodeElements[j]);
              end;
            s := s + ' [' + cs + ']';
          end;
        sl.Add(s);
      end;
    if _sanitised then
      sl.Sort;
    for i := 0 to sl.Count-1 do
      WriteLn(Format('%4d: %s',[i,sl[i]]));
    WriteLn;
  finally
    FreeAndNil(sl);
  end;
end;

function TInstructionList.FindInstruction(_opcode: word; _operand1: TOperandOption; _operand2: TOperandOption; var _r: TInstructionRec): boolean;
var hash: integer;
    index: integer;
begin
  Result := False; // Assume not found for now
  hash := CalculateHash(_opcode,_operand1,_operand2);
  while (FHashTable[hash] >= 0) and (not Result) do
    begin
      index := FHashTable[hash];
      if (Items[index].OpcodeIndex = _opcode) and
         (Items[index].Operand1Index = _operand1) and
         (Items[index].Operand2Index = _operand2) then
        begin
          _r := Items[index];
          Result := True;
        end
      else
        begin
          Inc(hash);
          if hash >= FHashSize then
            hash := hash - FHashSize;
        end;
    end;
  // If not found then try and swap round U16 to U8 and U16_IND to U8_IND
  if not Result then
    begin // Below calls are recursive!!!!
      if _operand1 = OPER_U16 then
        Result := FindInstruction(_opcode,OPER_U8,_operand2,_r);
      if _operand1 = OPER_U16_IND then
        Result := FindInstruction(_opcode,OPER_U8_IND,_operand2,_r);
      if _operand2 = OPER_U16 then
        Result := FindInstruction(_opcode,_operand1,OPER_U8,_r);
      if _operand2 = OPER_U16_IND then
        Result := FindInstruction(_opcode,_operand1,OPER_U8_IND,_r);
    end;
end;

function TInstructionList.FindNextPrime(_start: integer): integer;
  function IsPrime(_v: integer): boolean; // Assumes _v is odd number
  var t: integer;
      i: integer;
  begin
    Result := True; // Assume prime for now
    i := 3;
    t := Trunc(Sqrt(_v)+1);
    while (i <= t) and (Result = True) do
      if _v mod i = 0 then
        Result := False
      else
        i := i + 2;
  end;
begin
  // If number is even, make it odd
  if _start mod 2 = 0 then
    Inc(_start);
  // Keep going until we get a prime number
  while not IsPrime(_start) do
    _start := _start + 2;
  Result := _start;
end;

function TInstructionList.FindOpcode(const _opcode: string; var _index: integer): boolean;
begin
  Result := FOpcodes.Find(_opcode,_index);
end;

procedure TInstructionList.GetSimpleOperands(_sl: TStringList);
var i:  integer;
    r:  TInstructionRec;

  procedure TryAdd(_oo: TOperandOption);
  begin
    if (_oo <> OPER_NULL) and
       (FOperandAvailable[_oo]) and
       (OperandActual[_oo] <> '') and
       (_sl.IndexOf(OperandActual[_oo]) < 0) then
      _sl.Add(OperandActual[_oo]);
  end;

begin
  _sl.Clear;
  _sl.Sorted := True;
  for i := 0 to Count-1 do
    begin
      r := Items[i];
      TryAdd(r.Operand1Index);
      TryAdd(r.Operand2Index);
    end;
end;

procedure TInstructionList.GetInstructions(_sl: TStringList);
var i: integer;
    oo: TOperandOption;
begin
  // Get contents of opcode list
  for i := 0 to FOpcodes.Count-1 do
    _sl.Add(FOpcodes[i]);
  // Get contents of operand list
  for oo in TOperandOption do
    if FOperandAvailable[oo] and (OperandActual[oo] <> '') then
      _sl.Add(OperandActual[oo]);
end;

procedure TInstructionList.LoadFromFile(const _filename: string);
var stream: TFileStream;
begin
  stream := TFileStream.Create(_filename,fmOpenRead);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TInstructionList.LoadFromStream(const _stream: TStream);
var i, j: integer;
    cbuf: array[0..OPCODE_LENGTH_MAXIMUM] of char;
    s: string;
    r: TInstructionRec;
    magic: dword;
    temp_byte: word;
    oo: TOperandOption;
    opc_version: Word;
    mnem_count: Word;
    inst_count: Word;
    checksum: Word;
    disk_checksum: Word;

  function ReadByte: BYTE;
  begin
    _stream.Read(Result,sizeof(Result));
  end;

  function ReadWord: WORD;
  begin
    _stream.Read(Result,sizeof(Result));
  end;

  function ReadDword: DWORD;
  begin
    _stream.Read(Result,sizeof(Result));
  end;

  procedure SetOperandAvailable(_op: TOperandOption);
  begin
    if _op <> OPER_NULL then
      FOperandAvailable[_op] := True;
    if IsIndirectArray[_op] then
      FIndirectAllowed := True; // Must be > 8080/8085 processor
  end;

begin
  // Some initialisation stuff
  FIndirectAllowed := False; // Always default to False now
  for oo in TOperandOption do
    FOperandAvailable[oo] := False;
  // See TInstructionList.SaveToStream for file format
  magic := ReadDword;
  if magic <> OPCODE_MAP_MAGIC then
    raise Exception.Create('File is not an Opcode Map file');
  // Read the version number
  opc_version := ReadWord;
  if opc_version <> OPCODE_VERSION then
    raise Exception.Create('This .opcode.bin file is not compatible with this version of the assembler');
  // Read the record counts
  mnem_count := ReadWord;
  inst_count := ReadWord;
  // Read the checksum and test it
  checksum := (OPCODE_MAP_MAGIC shr 16) xor
              (OPCODE_MAP_MAGIC and $0000FFFF) xor
              opc_version xor
              mnem_count xor
              inst_count;
  disk_checksum := ReadWord;
  if checksum <> disk_checksum then
    raise Exception.Create('Checksum failure when loading opcode file');
  // Read the mnemonics
  FOpcodes.Clear;
  if (mnem_count < OPCODE_COUNT_MINIMUM) or (mnem_count > OPCODE_COUNT_MAXIMUM) then
    raise Exception.Create(Format('Number of opcodes (%d) was not in the expected range of %d to %d',[mnem_count,OPCODE_COUNT_MINIMUM,OPCODE_COUNT_MAXIMUM]));
  for i := 0 to mnem_count-1 do
    begin
      _stream.Read(cbuf,OPCODE_LENGTH_MAXIMUM+1);
      FOpcodes.Add(cbuf);
    end;
  // Read the instructions
  Clear;
  if (inst_count < INSTRUCTION_COUNT_MINIMUM) or (inst_count > INSTRUCTION_COUNT_MAXIMUM) then
    raise Exception.Create(Format('Number of instructions (%d) was not in the expected range of %d to %d',[inst_count,INSTRUCTION_COUNT_MINIMUM,INSTRUCTION_COUNT_MAXIMUM]));
  for i := 0 to inst_count-1 do
    begin
      for j := 0 to CODE_ELEMENT_COUNT_MAXIMUM-1 do
        begin
          r.CodeElements[j].ElementType := cetNull;
          r.CodeElements[j].OperandNo   := 0;
          r.CodeElements[j].Value       := 0;
          r.CodeElements[j].Offset      := 0;
        end;
      r.OpcodeIndex := ReadWord;
      r.Operand1Index := TOperandOption(ReadByte);
      r.Operand2Index := TOperandOption(ReadByte);
      SetOperandAvailable(r.Operand1Index);
      SetOperandAvailable(r.Operand2Index);
      temp_byte := ReadByte;
      if (temp_byte < CODE_ELEMENT_COUNT_MINIMUM) or (temp_byte > CODE_ELEMENT_COUNT_MAXIMUM) then
        raise Exception.Create(Format('Number of code elements was not in the expected range of %d to %d',[CODE_ELEMENT_COUNT_MINIMUM,CODE_ELEMENT_COUNT_MAXIMUM]));
      r.CodeElementCount := temp_byte;
      for j := 0 to temp_byte-1 do
        begin
          r.CodeElements[j].ElementType := TCodeElementType(ReadByte);
          r.CodeElements[j].OperandNo   := ReadByte;
          r.CodeElements[j].Value       := ReadByte;
          r.CodeElements[j].Offset      := ReadByte;
        end;
      r.CodeElementSize := CalcCodeElementSize(r);
      Add(r);
    end;
  ConstructHashTable;
end;

procedure TInstructionList.LoadFromResource(const _resource: string);
var stream: TResourceStream;
begin
  stream := TResourceStream.Create(HInstance,_resource,RT_RCDATA);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

function TInstructionList.OpcodeAtIndex(_index: integer): string;
begin
  Result := FOpcodes[_index];
end;

function TInstructionList.OpcodeCount: integer;
begin
  Result := FOpcodes.Count;
end;

procedure TInstructionList.SaveToFile(const _filename: string);
var stream: TFileStream;
begin
  stream := TFileStream.Create(_filename,fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TInstructionList.SaveToStream(const _stream: TStream);
var i, j: integer;
    cbuf: array[0..OPCODE_LENGTH_MAXIMUM] of char;
    s: string;
    r: TInstructionRec;
    checksum: word;

  procedure WriteByte(_v: BYTE);
  begin
    _stream.Write(_v,sizeof(_v));
  end;

  procedure WriteWord(_v: WORD);
  begin
    _stream.Write(_v,sizeof(_v));
  end;

  procedure WriteDword(_v: DWORD);
  begin
    _stream.Write(_v,sizeof(_v));
  end;

begin
  // Saved format is:
  //
  //  Magic number $4D43504F (reversed = OPCM = OPCode Map)
  //  File version (U16) currently 2
  //  Number of opcode records (U16)
  //  Number of instruction records (U16)
  //  Checksum - XOR of all of the above
  //
  //  Opcode record 0: Six bytes containing 2-5 characters followed by NUL
  //  Opcode record 1:   "   "
  //     :     :
  //  Opcode record n-1: "   "
  //
  //  Instr record 0: Opcode index U16
  //                  Operand1 index U8
  //                  Operand2 index U8
  //                  Code element count U8
  //                    Code element 0: Element type U8
  //                                    Operand U8 (either 1 or 2)
  //                                    Value U8
  //                                    Offset U8
  //                    Code element 1:   "     "
  //                      :      :
  //                    Code element n-1: "     "
  //  Instr record 1:   "      "
  //    :     :
  //  Instr record n-1: "      "
  WriteDword(OPCODE_MAP_MAGIC);
  // Write the file format number
  WriteWord(OPCODE_VERSION);
  // Write the counts
  WriteWord(OpcodeCount);
  WriteWord(Count);
  checksum := (OPCODE_MAP_MAGIC shr 16) xor
              (OPCODE_MAP_MAGIC and $0000FFFF) xor
              OPCODE_VERSION xor
              Word(OpcodeCount) xor
              Word(Count);
  WriteWord(checksum);
  for i := 0 to OpcodeCount-1 do
    begin
      s := OpcodeAtIndex(i);
      if Length(s) > OPCODE_LENGTH_MAXIMUM then
        raise Exception.Create('OPCODE_LENGTH_MAXIMUM exceeded');
      for j := 0 to OPCODE_LENGTH_MAXIMUM do
        cbuf[j] := #0;
      for j := 1 to Length(s) do
        cbuf[j-1] := s[j];
      _stream.Write(cbuf,OPCODE_LENGTH_MAXIMUM+1);
    end;
  // Write the instructions
  for i := 0 to Count-1 do
    begin
      r := Items[i];
      WriteWord(r.OpcodeIndex);
      WriteByte(Ord(r.Operand1Index));
      WriteByte(Ord(r.Operand2Index));
      WriteByte(r.CodeElementCount);
      for j := 0 to r.CodeElementCount-1 do
        begin
          WriteByte(Ord(r.CodeElements[j].ElementType));
          WriteByte(r.CodeElements[j].OperandNo);
          WriteByte(r.CodeElements[j].Value);
          WriteByte(r.CodeElements[j].Offset);
        end;
    end;
end;

function TInstructionList.SimpleOpToOperandOption(const _operand: string): TOperandOption;
var i: TOperandOption;
    s: string;
begin
  Result := OPER_NULL;
  s := UpperCase(_operand);
  for i in TOperandOption do
    if s = OperandActual[i] then
      begin
        Result := i;
      end;
end;

procedure TInstructionList.Sort;
var i: integer;
    sorted: boolean;
    tmp: TInstructionRec;
    loops: integer;

  function compare(a,b: integer): integer;
  var ra, rb: TInstructionRec;
  begin
    ra := Items[a];
    rb := Items[b];
    if ra.OpcodeIndex > rb.OpcodeIndex then
      Result := 1
    else if ra.OpcodeIndex < rb.OpcodeIndex then
      Result := -1
    else if ra.Operand1Index > rb.Operand1Index then
      Result := 1
    else if ra.Operand1Index < rb.Operand1Index then
      Result := -1
    else if ra.Operand2Index > rb.Operand2Index then
      Result := 1
    else if ra.Operand2Index < rb.Operand2Index then
      Result := -1
    else
      raise Exception.Create('Multiple entries for the same opcode/operand combination');
  end;

begin
  sorted := False;
  loops := 0;
  while not sorted do
    begin
      sorted := True;
      for i := 0 to Count-2 do
        begin
          if compare(i,i+1) > 0 then
            begin
              sorted := False;
              tmp := Items[i];
              Items[i] := Items[i+1];
              Items[i+1] := tmp;
            end;
        end;
      Inc(loops);
    end;
end;

end.

