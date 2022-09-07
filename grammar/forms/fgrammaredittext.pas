unit fgrammaredittext;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditText }

  TfrmGrammarEditText = class(TGrammarEditor)
    btnCancel: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    edtString: TEdit;
    Label1: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmGrammarEditText: TfrmGrammarEditText;

implementation

{$R *.lfm}

{ TfrmGrammarEditText }

procedure TfrmGrammarEditText.FormCreate(Sender: TObject);
begin
  edtString.Text := FObj.strVar;
end;

procedure TfrmGrammarEditText.btnOKClick(Sender: TObject);
begin
  FObj.strVar := edtString.Text;
end;

end.

