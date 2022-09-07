unit fgrammar;

{$mode objfpc}{$H+}

{
    XA80 - Cross Assembler for x80 processors
    Copyright (C)2020-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  Grids, ComCtrls, ugrammar;

const
  WINDOW_CAPTION = 'XA80 Grammar Editor';
  WINDOW_FILENAME_PIXELS_MARGIN = 150;

type

  { TfrmGrammar }

  TfrmGrammar = class(TForm)
    actFileOpen: TAction;
    actFileNew: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileSep1: TMenuItem;
    miFileSep2: TMenuItem;
    miFileExit: TMenuItem;
    miFile: TMenuItem;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
  private
    FDirty:    boolean;
    FFilename: string;
    FGrammar:  TGrammar;
    function  CheckDirtyClose: boolean;
    procedure SetCaption;
    procedure SetDirty(_v: boolean);
    procedure SetFilename(const _fn: string);
    procedure SyncGrammar;
    procedure WriteToXML;
  public

  published
    property Dirty:    boolean read FDirty    write SetDirty;
    property Filename: string  read FFilename write SetFilename;
  end;

var
  frmGrammar: TfrmGrammar;

implementation

{$R *.lfm}

uses
  FileCtrl, typinfo, fgrammareditstring, fgrammaredittext, uutility, DOM,
  XMLWrite, fgrammareditstringlist, fgrammareditnom, fgrammareditu16,
  fgrammareditboolean, fgrammareditmlr, fgrammareditcharset;

procedure TfrmGrammar.FormActivate(Sender: TObject);
begin
  SetCaption;
  actFileNewExecute(Self);
end;

procedure TfrmGrammar.FormCreate(Sender: TObject);
begin
  FGrammar := TGrammar.Create;
  SaveDialog1.InitialDir := ProgramData;
end;

procedure TfrmGrammar.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGrammar);
end;

procedure TfrmGrammar.actFileExitExecute(Sender: TObject);
begin
  if CheckDirtyClose then
    Close;
end;

procedure TfrmGrammar.actFileNewExecute(Sender: TObject);
begin
  FGrammar.New;
  SyncGrammar;
end;

procedure TfrmGrammar.actFileOpenExecute(Sender: TObject);
begin

end;

procedure TfrmGrammar.actFileSaveAsExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      Filename := SaveDialog1.FileName;
      WriteToXML;
      Dirty := False;
    end;
end;

procedure TfrmGrammar.actFileSaveExecute(Sender: TObject);
begin
  if Filename = '' then
    actFileSaveAsExecute(Self)
  else
    begin
      WriteToXML;
      Dirty := False;
    end;
end;

procedure TfrmGrammar.miFileClick(Sender: TObject);
begin

end;

procedure TfrmGrammar.StringGrid1DblClick(Sender: TObject);
var row: integer;
    obj: TGrammarObj;
    inputform: TGrammarEditor;
begin
  row := StringGrid1.Row - 1;
  obj := FGrammar.GrammarList[row];
  // Construct an editor for the variable
  inputform := nil;
  case obj.DataType of
    gdtBoolean:        inputform := TfrmGrammarEditBoolean.Create(Self,obj);
    gdtCharSet:        inputform := TfrmGrammarEditCharSet.Create(Self,obj);
    gdtFuncDef:        inputform := TfrmGrammarEditStringList.Create(Self,obj);
    gdtMacroLabelRule: inputform := TfrmGrammarEditMLR.Create(Self,obj);
    gdtNOM:            inputform := TfrmGrammarEditNOM.Create(Self,obj);
    gdtOperatorDef:    inputform := TfrmGrammarEditStringList.Create(Self,obj);
    gdtString:         inputform := TfrmGrammarEditString.Create(Self,obj);
    gdtStringList:     inputform := TfrmGrammarEditStringList.Create(Self,obj);
    gdtText:           inputform := TfrmGrammarEditText.Create(Self,obj);
    gdtU16:            inputform := TfrmGrammarEditU16.Create(Self,obj);
    otherwise
      raise Exception.Create(Format('Object DataType %s not catered for',[GetEnumName(TypeInfo(TGrammarDataType),Ord(obj.DataType))]));
  ;
  end;
  if Assigned(inputform) then
    begin
      try
        if inputform.ShowModal = mrOK then
          begin
            Dirty := True;
            SyncGrammar;
          end;
      finally
        FreeAndNil(inputform);
      end;
    end;
end;

function TfrmGrammar.CheckDirtyClose: boolean;
begin
  Result := True; // Assume OK to close for now
  if Dirty then
    begin
      case MessageDlg('File not saved','Warning, your file has not been saved. Do you wish to save the file? Pick Yes if you wish to save, No if you wish to lose the changes, or Cancel if you want to go back to what you were doing',mtWarning,[mbYes,mbNo,mbCancel],0) of
        mrYes:    if Filename = '' then
                    actFileSaveAsExecute(Self)
                  else
                    actFileSaveExecute(Self);
        mrNo:     ; // Do nothing
        mrCancel: Result := False; // Don't kill the file
      end;
    end;
end;

procedure TfrmGrammar.SetCaption;
var s: string;
    fn: string;
begin
  // Set window caption
  s := WINDOW_CAPTION + ' ';
  if Filename = '' then
    s := s + '<no file>'
  else
    begin
      fn := MinimizeName(Filename,Canvas,Width-WINDOW_FILENAME_PIXELS_MARGIN);
      s := s + fn;
    end;
  if Dirty then
    s := s + ' *';
  Caption := s;
  // Set some other stuff
  actFileSave.Enabled := (Filename <> '') and Dirty;
end;

procedure TfrmGrammar.SetDirty(_v: boolean);
begin
  FDirty := _v;
  SetCaption;
end;

procedure TfrmGrammar.SetFilename(const _fn: string);
begin
  FFilename := _fn;
  SetCaption;
end;

procedure TfrmGrammar.SyncGrammar;
var i: integer;
    w0,w1: integer;

  procedure SetCWidth(var _w: integer; const _s: string);
  begin
    if StringGrid1.Canvas.TextWidth(_s) > _w then
      _w := StringGrid1.Canvas.TextWidth(_s);
  end;

begin
  // Synchronise the screen table with the grammar objects
  // Some setup first
  StringGrid1.ColCount := 2;
  StringGrid1.RowCount := 1 + FGrammar.GrammarList.Count; // Add 1 for titles
  // Draw the titles
  StringGrid1.Cells[0,0] := 'Element';
  StringGrid1.Cells[1,0] := 'Setting';
  w0 := 0; // First column width
  w1 := 0; // Second column width
  // Now do the sync
  for i := 0 to FGrammar.GrammarList.Count-1 do
    begin
      StringGrid1.Cells[0,1+i] := FGrammar.GrammarList[i].Title;
      StringGrid1.Cells[1,1+i] := FGrammar.GrammarList[i].AsText;
      SetCWidth(w0,StringGrid1.Cells[0,1+i]);
      SetCWidth(w1,StringGrid1.Cells[1,1+i]);
    end;
  // Finally set widths
  StringGrid1.ColWidths[0] := w0 + 8;
  StringGrid1.ColWidths[1] := w1 + 8;
end;

procedure TfrmGrammar.WriteToXML;
var doc: TXMLDocument;
    root, section, item: TDOMNode;
    i:    integer;
begin
  doc := TXMLDocument.Create;
  try
    // Create the root
    root := doc.CreateElement('grammar');
    doc.AppendChild(root);
    // Write out the sections
    for i := 0 to FGrammar.GrammarList.Count-1 do
      begin
        section := doc.CreateElement(FGrammar.GrammarList[i].Title);
        root.AppendChild(section);
        item    := doc.CreateTextNode(FGrammar.GrammarList[i].AsText);
        section.AppendChild(item);
      end;
    WriteXMLFile(doc,Filename);
  finally
    FreeAndNil(doc);
  end;
end;

end.

