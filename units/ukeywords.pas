unit ukeywords;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults;

type
  TKeywordProc = procedure of object;

  TKeywordType = (ktDirective,ktOpcode);

  TKeywordRec = record
    Text: string;
    Proc: TKeywordProc;
  end;

  TKeywordList = class(specialize TSortedList<TKeywordRec>)
    public
      constructor Create;
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

end.

