{
    XA80 - Cross compiler for x80 processors
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

unit upreparser2;

{$mode ObjFPC}{$H+}

//
// Preparse routines
//
// Splits the input into [label] [directive [operands]] [comments]
// Comments are not processed
//
// A label can be an item that is not a keyword. Keywords are DB, INCLUDE,
// MACRO + opcodes like LD LXI CPIR etc.
// Labels cannot be any of the reseved key words. They do not have to start
// with a . or end with a : but if there is colon on the end, it is removed
//
// Operands are further procesed so that if they are fully enclosed in () they
// are changed to []. This is non-trivial as an operand could be (HL) or (0x200)
// which are changed to [HL] or [0x200] respectively, however (2+3)*(4+5) while
// being enclosed in () is *not* an indirection and should be left alone. The
// situation is further complicated by the fact that operands can contain
// strings and escape sequences...
//

interface

uses
  Classes, SysUtils, Generics.Collections,
  uasmglobals, ukeywords, uinstruction;

type

  // The initial parse will yield four parser object types: ppoComma,
  // ppoKeyword, ppoSomething, ppoString as these are the items the grammar
  // can detect. The Optimisexxx routines shape the input:
  //
  // OptimiseComments will mark * at start of line or ; // at any part of
  // line as a comment. All fields following will be a comment. All the
  // comments are blended together into 1 item.
  //
  // OptimiseLabel will mark the first field as a ppoLabel type if it's a
  // ppoSomething. At this stage, the line should start with ppoLabel,
  // ppoKeyword or ppoComment, anything else is a fail. Also removes trailing :
  // character
  //
  // OptimiseOperands: Will bunch up operands by removing the commas. Should
  // never have two or more commas together. Anything after a keyword which
  // is a ppoSomething or ppoString will be changed to a ppoOperand. This
  // routine will take care of commas within operands, e.g. (5+Left("VX",1))
  //
  // OptimiseIndirection will change the indirection brackets () to []. This
  // has to take care, e.g. LDA (2) is not the same as LDA (1)+(1). The first
  // example has an indirection, the second does not.

  TPreparserType = (ppoComma,
                    ppoComment,
                    ppoKeyword,
                    ppoLabel,
                    ppoOperand,
                    ppoSomething,
                    ppoString);

  TPreparserRec = record
    RecType:  TPreparserType;
    Payload:  string;
    ColumnNo: integer;
    Brackets: integer;
  end;

  TPreparserList = class(specialize TList<TPreparserRec>);

  TPreparser = class(TObject)
    protected
      FInstructions: TInstructionList;
      FKeywords:     TKeywordList;
      FPPList: TPreparserList;
      function  IsKeyword(_kw: string): boolean;
//    function  MyReduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
      procedure OptimiseComments;
      procedure OptimiseIndirection;
      procedure OptimiseKeywords;
      procedure OptimiseLabel;
      procedure OptimiseOperands;
      procedure ProcessComma(const _s: string; _col: integer);
      procedure ProcessSomething(const _s: string; _col: integer);
      procedure ProcessString(const _s: string; _col: integer);
    public
      property Keywords: TKeywordList read FKeywords;
      property PPList: TPreparserList read FPPList;
      constructor Create(const _processor: string; _default_handler: TKeywordProc);
      destructor Destroy; override;
      procedure InitRun; // reintroduce;
      procedure Optimise;
      procedure Split(var _label: string; var _keyword: string; _operands: TStringList);
  end;

implementation

// Check if an operand has indirection
// So (HL) = yes
//    (IX+3) = yes
//    (0x200+(1&3)) = yes
//    (1+2)*(3+4) = NO
// Count open brackets in a string

function HasIndirection(const _s: string): boolean;
type TIstate = (isNone,isNormal,isStringS,isStringD,isEscD,isEscS);
var i: integer;
    ch: char;
    bcount: integer;
    state:  TIstate;
begin
  if Length(_s) < 2 then
    Result := False
  else if (_s[1] <> '(') or (_s[Length(_s)] <> ')') then
    Result := False
  else
    begin
      Result := True; // Assume true for now
      bcount := 0;
      state  := isNone;
      for i := 1 to Length(_s) do
        begin
          ch := _s[i];
          case state of
            isNone:     case ch of
                          '(': Inc(bcount);
                          #34: state := isStringD;
                          #39: state := isStringS;
                          otherwise
                            state := isNormal;
                        end;
            isNormal:   case ch of
                          '(': Inc(bcount);
                          ')': Dec(bcount);
                          #34: state := isStringD;
                          #39: state := isStringS;
                        end;
            isStringD:  case ch of
                          #34: state := isNormal;
                          '\': state := isEscD;
                        end;
            isStringS:  case ch of
                          #39: state := isNormal;
                          '\': state := isEscS;
                        end;
            isEscD:     state := isEscD;
            isEscS:     state := isEscS;
          end; // case
          if (bcount = 0) and (i < Length(_s)) then
            Result := False;
        end;
    end;
end;

function OpenBrackets(const _s: string): integer;
var i: integer;
begin
  Result := 0;
  for i := 1 to Length(_s) do
    if _s[i] = '(' then
      Inc(Result);
end;

// Count close brackets in a string

function CloseBrackets(const _s: string): integer;
var i: integer;
begin
  Result := 0;
  for i := 1 to Length(_s) do
    if _s[i] = ')' then
      Inc(Result);
end;



//=============================================================================
//
//  TPreparser code
//
//=============================================================================

constructor TPreparser.Create(const _processor: string; _default_handler: TKeywordProc);
begin
  inherited Create;
  {
  LoadFromResource('XA80PRE');
  FPPList := TPreparserList.Create;
  FInstructions := TInstructionList.Create(_processor);
  FKeywords := TKeywordList.Create;
  FKeywords.AddInstructions(FInstructions,_default_handler);
  OnReduce  := @MyReduce;
  }
end;

destructor TPreparser.Destroy;
begin
  {
  OnReduce := nil;
  FreeAndNil(FKeywords);
  FreeAndNil(FInstructions);
  FreeAndNil(FPPList);
  }
  inherited Destroy;
end;

procedure TPreparser.InitRun;
begin
//  inherited InitRun;
  FPPList.Clear;
end;

function TPreparser.IsKeyword(_kw: string): boolean;
var r: TKeywordRec;
begin
  if not Assigned(FKeywords) then
    raise Exception.Create('Keyword list not assigned in preparser');
  _kw := UpperCase(_kw);
  r.Text := _kw;
  r.Proc := nil;
  Result := (FKeywords.IndexOf(r) >= 0);
end;

function  TPreparser.MyReduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
var col: integer;
    s:   string;
begin
  s := Parser.ParserStack[Parser.ParserSP-1].Buf;
  col := Parser.ParserStack[Parser.ParserSP-1].Token.Col;
  case RuleIndex of
    0: ; // Do nothing <assembler_line> : <assembler_line> <asm_item>
    1: ; // Do nothing <assembler_line> : <asm_item>
    2: ProcessString(s,col);
    3: ProcessSomething(s,col);
    4: ProcessComma(s,col);
  end;
  Result.Buf := '';
end;

procedure TPreparser.Optimise;
begin
  OptimiseComments;
  OptimiseKeywords;
  OptimiseLabel;
  OptimiseOperands;
  OptimiseIndirection;
end;

procedure TPreparser.OptimiseComments;
var i: integer;
    commenting: boolean;
    r:          TPreparserRec;
    comment:    string;
    orec:       integer;
begin
  commenting := False;
  i := 0;
  orec := -1;
  comment := '';
  while i < FPPList.Count do
    begin
      r := FPPList[i];
      if (r.RecType = ppoSomething) and
         ((LeftStr(r.payload,1) = ';') or
          (LeftStr(r.payload,2) = '//') or
          ((LeftStr(r.payload,1) = '*') and (i = 0))) then
        begin
          commenting := True;
          orec := i;
        end;
      if commenting then
        begin
          r.RecType := ppoComment;
          if comment <> '' then
            comment := comment + ' ';
          comment := comment + r.Payload;
          FPPList[i] := r;
        end;
      Inc(i);
    end;
  // Final pass if we had some comments
  if orec >= 0 then
    begin
      r := FPPList[orec];
      r.Payload := comment;
      FPPList[orec] := r;
      // Now delete the stuff after the composite comment
      while FPPList.Count > (orec+1) do
        FPPlist.Delete(orec+1);
    end;
end;

procedure TPreparser.OptimiseIndirection;
var r: TPreparserRec;
    i: integer;
begin
  for i := 0 to FPPList.Count-1 do
    if (FPPList[i].RecType = ppoOperand) and (HasIndirection(FPPList[i].Payload)) then
      begin
        r := FPPList[i];
        if Length(r.Payload) >= 2 then
          begin
            r.Payload[1] := '[';
            r.Payload[Length(r.Payload)] := ']';
          end;
        FPPList[i] := r;
      end;
end;

procedure TPreparser.OptimiseKeywords;
var r: TPreparserRec;
    i: integer;
begin
  for i := 0 to FPPList.Count-1 do
    if (FPPList[i].RecType = ppoSomething) and IsKeyword(FPPList[i].Payload) then
      begin
        r := FPPList[i];
        r.RecType := ppoKeyword;
        FPPList[i] := r;
      end;
end;

procedure TPreparser.OptimiseLabel;
var r: TPreparserRec;
begin
  if (FPPList.Count > 0) and (FPPList[0].RecType = ppoSomething) then
    begin
      r := FPPList[0];
      r.RecType := ppoLabel;
      if (Length(r.Payload) > 0) and (RightStr(r.Payload,1) = ':') then
        r.Payload := LeftStr(r.Payload,Length(r.Payload)-1);
      FPPList[0] := r;
    end;
  // Check to ensure first entry is a label or a keyword
  if (FPPList.Count > 0) and
     not (FPPList[0].RecType in [ppoKeyword,ppoLabel,ppoComment]) and
     (FPPList[0].RecType <> ppoLabel) then
    raise Exception.Create(Format('Line did not start with a label or a keyword (%s)',[FPPList[0].Payload]));
end;

procedure TPreparser.OptimiseOperands;
var i: integer;
    haskeyword: boolean;
    r:          TPreparserRec;
begin
  // Check for two or more commas together
  for i := 0 to FPPList.Count-2 do
    if (FPPList[i].RecType = ppoComma) and (FPPList[i+1].RecType = ppoComma) then
      raise Exception.Create('Cannot have two commas with nothing in between');
  // Turn Something entries into operands if we have a keyword before
  // Count the brackets along the way
  for i := 0 to FPPList.Count-1 do
    begin
      r := FPPList[i];
      if r.RecType = ppoSomething then  // Ignore strings!
        r.Brackets := OpenBrackets(r.Payload) - CloseBrackets(r.Payload)
      else
        r.Brackets := 0;
      FPPList[i] := r;
    end;
  haskeyword := False;
  for i := 0 to FPPList.Count-1 do
    begin
      r := FPPList[i];
      if r.RecType = ppoKeyword then
        haskeyword := True
      else if r.RecType in [ppoSomething,ppoString] then
        begin
          if not haskeyword then
            raise Exception.Create(Format('Attempt to define operand (%s) before directive/opcode',[FPPList[i].Payload]));
          r.RecType := ppoOperand;
        end;
      FPPList[i] := r;
    end;
  // Now go through and group the operands together
  i := 0;
  while i <= FPPList.Count-2 do
    begin
      r := FPPList[i];
      while (i <= FPPList.Count-2) and (r.RecType = ppoOperand) and (FPPList[i+1].RecType = ppoOperand) do
        begin
          r.Payload  := r.Payload + FPPList[i+1].Payload;
          r.Brackets := r.Brackets + FPPList[i+1].Brackets;
          FPPList.Delete(i+1);
        end;
      FPPList[i] := r;
      Inc(i);
    end;
  // Group commas into operands if the commas are between brackets
  i := 0;
  while i <= FPPList.Count-2 do
    begin
      r := FPPList[i];
      while (i <= FPPlist.Count-2) and (r.RecType = ppoOperand) and (FPPList[i+1].RecType in [ppoComma,ppoOperand]) and (r.Brackets > 0) do
        begin
          r.Payload  := r.Payload + FPPList[i+1].Payload;
          r.Brackets := r.Brackets + FPPList[i+1].Brackets;
          FPPList.Delete(i+1);
        end;
      FPPList[i] := r;
      Inc(i);
    end;
  // Remove the commas from between the operands
  i := 0;
  while i <= FPPLIst.Count-3 do
    begin
      r := FPPList[i];
      while (i <= FPPlist.Count-3) and (r.RecType = ppoOperand) and (FPPList[i+1].RecType = ppoComma) and (FPPList[i+2].RecType = ppoOperand) do
        FPPList.Delete(i+1);
      FPPList[i] := r;
      Inc(i);
    end;
end;

procedure TPreparser.ProcessComma(const _s: string; _col: integer);
var r: TPreparserRec;
begin
  r.ColumnNo := _col;
  r.Payload  := _s;
  r.RecType  := ppoComma;
  FPPList.Add(r);
end;

procedure TPreparser.ProcessSomething(const _s: string; _col: integer);
var r: TPreparserRec;
begin
  r.ColumnNo := _col;
  r.Payload  := _s;
  r.RecType  := ppoSomething;
  FPPList.Add(r);
end;

procedure TPreparser.ProcessString(const _s: string; _col: integer);
var r: TPreparserRec;
begin
  r.ColumnNo := _col;
  r.Payload  := _s;
  r.RecType  := ppoString;
  FPPList.Add(r);
end;

procedure TPreparser.Split(var _label: string; var _keyword: string; _operands: TStringList);
var r: TPreparserRec;
begin
  // Initialise
  _label := '';
  _keyword := '';
  _operands.Clear;
  _operands.Sorted := False;
  // Now loop through and fill out
  for r in FPPLIst do
    begin
      case r.RecType of
        ppoLabel:
          if _label <> '' then
            Monitor(ltInternal,'Attempt to define label more than once on line')
          else if (_keyword <> '') or (_operands.Count > 0) then
            Monitor(ltError,'Attempt to define label %s after keyword or operands defined',[r.Payload])
          else
            _label := r.Payload;
        ppoKeyword:
          if _keyword <> '' then
            Monitor(ltError,'Attempt to define a keyword %s when keyword %s already defined',[r.Payload,_keyword])
          else
            _keyword := r.Payload;
        ppoOperand:
          if _keyword = '' then
            Monitor(ltError,'Attempt to define operand with no preceding keyword')
          else
            _operands.Add(r.Payload);
        ppoComment:
          ;  // Ignore
        ppoComma,
        ppoSomething,
        ppoString:
          Monitor(ltInternal,'Attempt to process Comma/Something/String which has not already been resolved');
        otherwise
          Monitor(ltInternal,'Record type not catered for in TPreparser.Split');
      end; // Case
    end;
end;

end.

