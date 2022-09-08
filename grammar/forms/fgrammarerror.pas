unit fgrammarerror;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmGrammarError }

  TfrmGrammarError = class(TForm)
    btnClose: TButton;
    btnHelp: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    FMsg: string;
  public
    constructor Create(const _msg: string); reintroduce;
  end;

var
  frmGrammarError: TfrmGrammarError;

implementation

{$R *.lfm}

procedure TfrmGrammarError.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Delimiter := #13;
  Memo1.Lines.StrictDelimiter := True;
  Memo1.Lines.DelimitedText := FMsg;
end;

constructor TfrmGrammarError.Create(const _msg: string);
begin
  inherited Create(nil);
  FMsg := _msg;
end;

end.

