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

end.

