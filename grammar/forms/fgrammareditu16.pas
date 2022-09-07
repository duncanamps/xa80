unit fgrammareditu16;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ugrammar;

type

  { TfrmGrammarEditU16 }

  TfrmGrammarEditU16 = class(TGrammarEditor)
    btnCancel: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    edtDecimal: TEdit;
    edtBinary: TEdit;
    edtOctal: TEdit;
    edtHex: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure edtBinaryChange(Sender: TObject);
    procedure edtDecimalChange(Sender: TObject);
    procedure edtHexChange(Sender: TObject);
    procedure edtOctalChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChanging: boolean;
    FValue: dword;
    procedure SetValues(_avoid: TEdit);
  public

  end;

var
  frmGrammarEditU16: TfrmGrammarEditU16;

implementation

{$R *.lfm}

uses
  uutility;

{ TfrmGrammarEditU16 }

procedure TfrmGrammarEditU16.FormCreate(Sender: TObject);
begin
  FValue := FObj.wordVar;
  FChanging := False;
  SetValues(nil);
end;

procedure TfrmGrammarEditU16.btnOKClick(Sender: TObject);
begin
  FObj.wordVar := FValue and $FFFF;
end;

procedure TfrmGrammarEditU16.edtBinaryChange(Sender: TObject);
begin
  try
    FValue := BinaryStrToInt(edtBinary.Text);
  except // Silent exception
  end;
  SetValues(edtBinary);
end;

procedure TfrmGrammarEditU16.edtDecimalChange(Sender: TObject);
begin
  try
    FValue := StrToInt(edtDecimal.Text);
  except // Silent exception
  end;
  SetValues(edtDecimal);
end;

procedure TfrmGrammarEditU16.edtHexChange(Sender: TObject);
begin
  try
    FValue := StrToInt('$' + edtHex.Text);
  except // Silent exception
  end;
  SetValues(edtHex);
end;

procedure TfrmGrammarEditU16.edtOctalChange(Sender: TObject);
begin
  try
    FValue := OctalStrToInt(edtOctal.Text);
  except // Silent exception
  end;
  SetValues(edtOctal);
end;

procedure TfrmGrammarEditU16.SetValues(_avoid: TEdit);
begin
  if FChanging then exit;
  FChanging := True;
  try
    if _avoid <> edtDecimal then
      edtDecimal.Text := IntToStr(FValue);
    if _avoid <> edtBinary then
      edtBinary.Text := IntToBinaryStr(FValue,16);
    if _avoid <> edtOctal then
      edtOctal.Text := IntToOctalStr(FValue,6);
    if _avoid <> edtHex then
      edtHex.Text := Format('%4.4X',[FValue]);
  finally
    FChanging := False;
  end;
end;

end.

