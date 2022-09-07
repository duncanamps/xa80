unit fgrammareditboolean;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditBoolean }

  TfrmGrammarEditBoolean = class(TGrammarEditor)
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
  frmGrammarEditBoolean: TfrmGrammarEditBoolean;

implementation

{$R *.lfm}

{ TfrmGrammarEditBoolean }

procedure TfrmGrammarEditBoolean.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := Ord(FObj.boolVar);
end;

procedure TfrmGrammarEditBoolean.btnOKClick(Sender: TObject);
begin
  FObj.boolVar := boolean(ComboBox1.ItemIndex);
end;

end.

