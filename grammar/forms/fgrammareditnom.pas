unit fgrammareditnom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditNOM }

  TfrmGrammarEditNOM = class(TGrammarEditor)
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
  frmGrammarEditNOM: TfrmGrammarEditNOM;

implementation

{$R *.lfm}

{ TfrmGrammarEditNOM }

procedure TfrmGrammarEditNOM.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := Ord(FObj.nomVar);
end;

procedure TfrmGrammarEditNOM.btnOKClick(Sender: TObject);
begin
  FObj.nomVar := TNOM(ComboBox1.ItemIndex);
end;

end.

