unit udfa_preparser;

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

uses
  Classes, SysUtils, udmnfa, udmdfa, uinstruction, ucommand;

type
  TPreparserDFA = class(TDFA)
    private
      FNFA:           TNFA;
      FOffsetOpcode:  integer;
      FOffsetCommand: integer;
      FOffsetOperand: integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddOpcodesCommands(_inst: TInstructionList; _cmdlist: TCommandList);
      procedure DumpNFAList(_strm: TStream);
      procedure DumpNFATable(_strm: TStream);
      procedure DumpDFATable(_strm: TStream);
      function  Tokenise(const _s: string): TNFAtoken;
      property OffsetOpcode:  integer read FOffsetOpcode;
      property OffsetCommand: integer read FOffsetCommand;
      property OffsetOperand: integer read FOffsetOperand;
  end;

implementation

uses
  umessages, deployment_parser_types_12;

constructor TPreparserDFA.Create;
begin
  inherited Create;
  FNFA := TNFA.Create;
end;

destructor TPreparserDFA.Destroy;
begin
  FreeAndNil(FNFA);
  inherited Destroy;
end;

procedure TPreparserDFA.AddOpcodesCommands(_inst: TInstructionList; _cmdlist: TCommandList);
var index: integer;
    i:     integer;
    nodezero: TNFArecord;
    opcode:   string;
    frag1:    TNFAfragment;
    opt:      TOperandOption;
begin
  index := TOKEN_WHITESPACE + 1;
  FOffsetOpcode := index;
  // Add the first node
  nodezero := FNFA.AllocateNode;
  // Go through and add all the instructions
  for i := 0 to _inst.OpcodeCount-1 do
    begin
      opcode := _inst.OpcodeAtIndex(i);
      frag1 := FNFA.MakeFragText(opcode);
      FNFA[frag1.LastNode].Accepting  := index;
      FNFA[frag1.LastNode].AcceptType := atKeyword;
      nodezero.Epsilons.Add(frag1.FirstNode);
      Inc(index);
    end;
  // Now add the commands
  FOffsetCommand := index;
  for i := 0 to _cmdlist.Count-1 do
    begin
      frag1 := FNFA.MakeFragText(_cmdlist.Items[i].CommandName);
      FNFA[frag1.LastNode].Accepting  := index;
      FNFA[frag1.LastNode].AcceptType := atKeyword;
      nodezero.Epsilons.Add(frag1.FirstNode);
      Inc(index);
    end;
  // Finally add the operands
  FOffsetOperand := index;
  for opt in TOperandOption do
    begin
      if OperandActual[opt] <> '' then
        begin
          frag1 := FNFA.MakeFragText(OperandActual[opt]);
          FNFA[frag1.LastNode].Accepting  := index;
          FNFA[frag1.LastNode].AcceptType := atKeyword;
          nodezero.Epsilons.Add(frag1.FirstNode);
        end;
      Inc(index);
    end;
  // Construct the DFA from the NFA
  CreateFromNFA(FNFA);
end;

procedure TPreparserDFA.DumpNFAList(_strm: TStream);
begin
  FNFA.DumpList(_strm);
end;

procedure TPreparserDFA.DumpNFATable(_strm: TStream);
begin
  FNFA.DumpTable(_strm);
end;

procedure TPreparserDFA.DumpDFATable(_strm: TStream);
begin
  DumpTable(_strm);
end;

function TPreparserDFA.Tokenise(const _s: string): TNFAtoken;
var state:     TSetIndex;
    newstate:  TSetIndex;
    accept:    TNFAtoken;
    stringlen: integer;
    stringidx: integer;
    ch:        char;

  function CharacterAvailable: boolean;
  begin
    CharacterAvailable := (stringidx <= stringlen);
  end;

  function Peek: char;
  begin
    Peek := #0;
    if not CharacterAvailable then
      ErrorObj.Show(ltInternal,X3002_PREPARSER_PEEK_ERROR);
    Peek := _s[stringidx];
  end;

begin
  Tokenise  := TOKEN_EOF;
  state     := 0;
  accept    := NULL_NFA_TOKEN;
  stringlen := Length(_s);
  stringidx := 1;
  repeat
    if CharacterAvailable then
      begin
        ch := Peek;
        newstate := Items[state].Destinations[ch];
        if newstate = NULL_INDEX then
          // Cannot move forward to new state so accept or error
          accept := TOKEN_ERROR
        else
          begin // Moving forward, go to next state
            state := newstate;
            accept := Items[state].Accepting;
            Inc(stringidx);
          end;
      end;
  until (not CharacterAvailable) or (accept = TOKEN_ERROR);
  if accept = NULL_NFA_TOKEN then
    Tokenise := TOKEN_ERROR
  else
    Tokenise := accept;
end;

end.

