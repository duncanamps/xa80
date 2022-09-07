unit fgrammareditmlr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditMLR }

  TfrmGrammarEditMLR = class(TGrammarEditor)
    btnCancel: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmGrammarEditMLR: TfrmGrammarEditMLR;

implementation

{$R *.lfm}

{ TfrmGrammarEditMLR }

procedure TfrmGrammarEditMLR.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := Ord(FObj.mlrVar);
end;

procedure TfrmGrammarEditMLR.btnOKClick(Sender: TObject);
begin
  FObj.mlrVar := TMacroLabelRule(ComboBox1.ItemIndex);
end;

end.

