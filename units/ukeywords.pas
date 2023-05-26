unit ukeywords;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, uinstruction;

type
  TKeywordProc = procedure of object;

  TKeywordType = (ktDirective,ktOpcode);

  TKeywordRec = record
    Text:  string;
    KType: TKeywordType;
    Proc:  TKeywordProc;
  end;

  TKeywordList = class(specialize TSortedList<TKeywordRec>)
    public
      constructor Create;
      procedure AddInstructions(_instructions: TInstructionList; _default_handler: TKeywordProc);
  end;

type
  TKeywordComparer = specialize TComparer<TKeywordRec>;

implementation

function CompareKeywordRec(constref Left, Right: TKeywordRec): Integer;
begin
  Result := AnsiCompareStr(Left.Text,Right.Text);
end;

constructor TKeywordList.Create;
begin
  inherited Create;
  FComparer := TKeywordComparer.Construct(@CompareKeywordRec);
end;

procedure TKeywordList.AddInstructions(_instructions: TInstructionList; _default_handler: TKeywordProc);
var i: integer;
    s: string;
    r: TKeywordRec;
begin
  // Delete any existing opcodes
  for i := Count-1 downto 0 do
    if Items[i].KType = ktOpcode then
      Delete(i);
  // Now add the opcodes in
  for i := 0 to _instructions.OpcodeCount-1 do
    begin
      s := _instructions.OpcodeAtIndex(i);
      r.KType := ktOpcode;
      r.Proc  := _default_handler;
      r.Text  := s;
      Add(r);
    end;
end;

end.

