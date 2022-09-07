unit fgrammareditstring;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditString }

  TfrmGrammarEditString = class(TGrammarEditor)
      btnOK: TButton;
      btnCancel: TButton;
      btnHelp: TButton;
      edtString: TEdit;
      Label1: TLabel;
      procedure btnOKClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
    private
    public
  end;

//var
//  frmGrammarEditString: TfrmGrammarEditString;

implementation

{$R *.lfm}


{ TfrmGrammarEditString }

procedure TfrmGrammarEditString.FormCreate(Sender: TObject);
begin
  edtString.Text := FObj.strVar;
end;

procedure TfrmGrammarEditString.btnOKClick(Sender: TObject);
begin
  FObj.strVar := edtString.Text;
end;

end.

