unit fgrammareditcharset;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ugrammar;

type

  { TfrmGrammarEditCharSet }

  TfrmGrammarEditCharSet = class(TGrammarEditor)
    btnCancel: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    edtString: TEdit;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    procedure btnOKClick(Sender: TObject);
    procedure edtStringChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
  private
    FCharacterSet: TGrammarCharSet;
    FFaulty: boolean;
    procedure MyPaint(Sender: TObject);
  public

  end;

var
  frmGrammarEditCharSet: TfrmGrammarEditCharSet;

implementation

{$R *.lfm}

uses
  uutility;

const
  CHARACTER_COUNT = 256;
  CHARACTER_PER_ROW = 16;
  CHARACTER_ROWS = (CHARACTER_COUNT div CHARACTER_PER_ROW);
  LINE_WIDTH = 1;

{ TfrmGrammarEditCharSet }

procedure TfrmGrammarEditCharSet.FormCreate(Sender: TObject);
begin
  FFaulty := False;
  FCharacterSet := FObj.csetVar;
  edtString.Text :=FObj.CharsetToStr(FCharacterSet);
  PaintBox1.OnPaint := @MyPaint;
  PaintBox1.Invalidate;
end;

procedure TfrmGrammarEditCharSet.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var row,col: integer;
    c:       char;
begin
  col := (X-LINE_WIDTH) * CHARACTER_PER_ROW div (PaintBox1.Width - 2 * LINE_WIDTH);
  row := (Y-LINE_WIDTH) * CHARACTER_ROWS    div (PaintBox1.Height - 2 * LINE_WIDTH);
  if (col < 0) or (col >= CHARACTER_PER_ROW) or (row < 0) or (row >= CHARACTER_ROWS) then
    exit;
  c := Chr(col + row * CHARACTER_PER_ROW);
  if c in FCharacterSet then
    FCharacterSet := FCharacterSet - [c]
  else
    FCharacterSet := FCharacterSet + [c];
  edtString.Text :=FObj.CharsetToStr(FCharacterSet);
  PaintBox1.Invalidate;
end;

procedure TfrmGrammarEditCharSet.edtStringChange(Sender: TObject);
var myset: TGrammarCharSet;
begin
  try
    myset := FObj.StrToCharSet(edtString.Text);
    FFaulty := False;
    FCharacterSet := myset;
    PaintBox1.Invalidate;
  except
    FFaulty := True;
    PaintBox1.Invalidate;
  end;
end;

procedure TfrmGrammarEditCharSet.btnOKClick(Sender: TObject);
begin
  FObj.csetVar := FObj.StrToCharSet(edtString.Text);
end;

procedure TfrmGrammarEditCharSet.MyPaint(Sender: TObject);
var x,y: integer;
    xp,yp: integer;
    c:     char;
    boxwidth, boxheight: integer;
    s: string;
    tw,th: integer;
begin
  // Calculate some stuff first
  boxwidth  := (PaintBox1.Width - 2 * LINE_WIDTH) div CHARACTER_PER_ROW;
  boxheight := (PaintBox1.Height - 2 * LINE_WIDTH) div CHARACTER_ROWS;
  // Do each character
  for c in char do
    begin
      x := Ord(c) mod CHARACTER_PER_ROW;
      y := Ord(c) div CHARACTER_PER_ROW;
      xp := (PaintBox1.Width - 2 * LINE_WIDTH) * x div CHARACTER_PER_ROW;
      yp := (PaintBox1.Height - 2 * LINE_WIDTH) * y div CHARACTER_ROWS;
      // Background box
      if FFaulty then
        PaintBox1.Canvas.Brush.Color := $4050FF
      else if c in FCharacterSet then
        PaintBox1.Canvas.Brush.Color := clWindow
      else
        PaintBox1.Canvas.Brush.Color := clMedGray;
      PaintBox1.Canvas.FillRect(xp,yp,xp+boxwidth+1,yp+boxheight+1);
      // Fill out the text
      s := CharAsReadable(c);
      tw := PaintBox1.Canvas.TextWidth(s);
      th := PaintBox1.Canvas.TextHeight(s);
      PaintBox1.Canvas.TextOut(xp + (boxwidth - tw) div 2, yp + (boxheight - th) div 2, s);
    end;
  // Do vertical lines
  PaintBox1.Canvas.Pen.Color := clWindowFrame;
  PaintBox1.Canvas.Pen.Width := LINE_WIDTH;
  for x := 0 to CHARACTER_PER_ROW do
    begin
      xp := (PaintBox1.Width - 2 * LINE_WIDTH) * x div CHARACTER_PER_ROW;
      PaintBox1.Canvas.Line(xp,0,xp,PaintBox1.Height-1);
    end;
  // Do horizontal lines
  for y := 0 to CHARACTER_ROWS do
    begin
      yp := (PaintBox1.Height - 2 * LINE_WIDTH) * y div CHARACTER_ROWS;
      PaintBox1.Canvas.Line(0,yp,PaintBox1.Width-1,yp);
    end;
end;

end.

