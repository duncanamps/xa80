unit fgrammareditstringlist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditStringList }

  TfrmGrammarEditStringList = class(TGrammarEditor)
    btnCancel: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    Memo1: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmGrammarEditStringList: TfrmGrammarEditStringList;

implementation

{$R *.lfm}

{ TfrmGrammarEditStringList }

procedure TfrmGrammarEditStringList.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Delimiter := #13;
  Memo1.Lines.DelimitedText := FObj.strlistVar.DelimitedText;
end;

procedure TfrmGrammarEditStringList.btnOKClick(Sender: TObject);
var _v: string;
    i:  integer;
begin
  // Convert back to string list
  _v := Memo1.Lines.DelimitedText;
  FObj.strListVar.Delimiter := #13;
  try
    FObj.strListVar.DelimitedText := _v;
  finally
    FObj.strListVar.Delimiter := #13;
  end;
  for i := 0 to FObj.strListVar.Count-1 do
    FObj.strListVar[i] := Trim(FObj.DecodeStr(FObj.strListVar[i]));
end;

end.

