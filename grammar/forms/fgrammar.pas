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
  MAXIMUM_MRU_PIXELS = 400;
  MI_MRU_PREFIX = 'miFileMRU';
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
    actToolsTestGrammar: TAction;
    actToolsAutoTest: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    miToolsAutoCheck: TMenuItem;
    miToolsTestGrammar: TMenuItem;
    miTools: TMenuItem;
    miFileSepMRU: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileSep1: TMenuItem;
    miFileSep2: TMenuItem;
    miFileExit: TMenuItem;
    miFile: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actToolsAutoTestExecute(Sender: TObject);
    procedure actToolsTestGrammarExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
  private
    FAutoTest:        boolean;
    FDirty:           boolean;
    FFilename:        string;
    FGrammar:         TGrammar;
    FMRUCount:        integer;
    FMRUArray:        array of string;
    MRUMenuItems:     array of TMenuItem;
    function  CheckDirtyClose: boolean;
    procedure DoOpen(const _fn: string);
    procedure MRUAdd(const _filename: string);
    procedure MRUGo(Sender: TObject);
    procedure MRURemove(const _filename: string);
    procedure MRUSync;
    procedure ReadFromXML;
    procedure SetAutoTest(_v: boolean);
    procedure SetCaption;
    procedure SetDirty(_v: boolean);
    procedure SetFilename(const _fn: string);
    procedure SyncGrammar;
    procedure TestEndRules(var msg: string);
    procedure TestEscapeRules(var msg: string);
    procedure TestFilenameQuoting(var msg: string);
    procedure TestFunctions(var msg: string);
    procedure TestLabelCharactersMid(var msg: string);
    procedure TestLabelCharactersStart(var msg: string);
    procedure TestLabelColonRule(var msg: string);
    procedure TestLabelMaximum(var msg: string);
    procedure WriteToXML;
  public

  published
    property AutoTest:  boolean read FAutoTest  write SetAutoTest;
    property Dirty:     boolean read FDirty     write SetDirty;
    property Filename:  string  read FFilename  write SetFilename;
  end;

var
  frmGrammar: TfrmGrammar;

implementation

{$R *.lfm}

uses
  FileCtrl, typinfo, fgrammareditstring, fgrammaredittext, uutility, DOM,
  XMLWrite, fgrammareditstringlist, fgrammareditnom, fgrammareditu16,
  fgrammareditboolean, fgrammareditmlr, fgrammareditcharset, XMLRead,
  ugrammaroptions, fgrammarerror, umonitor;

procedure TfrmGrammar.FormActivate(Sender: TObject);
begin
  FGrammar.New;
  Dirty := False;
  SyncGrammar;
  SetCaption;
end;

procedure TfrmGrammar.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
    s_state: integer;
begin
  // Restore form if shrunk down and save parameters
  s_state := Ord(WindowState);
  // Now save
  if TWindowState(s_state) <> wsNormal then
    WindowState := wsNormal;  // Make sure not minimized or maximized
  GlobalOptions[PROP_SCREEN_HEIGHT].SetValue(Height);
  GlobalOptions[PROP_SCREEN_LEFT].SetValue(Left);
  GlobalOptions[PROP_SCREEN_TOP].SetValue(Top);
  GlobalOptions[PROP_SCREEN_WIDTH].SetValue(Width);
  GlobalOptions[PROP_SCREEN_STATE].SetValue(s_state);
  // Sort out the MRU list
  GlobalOptions[PROP_MRU_COUNT].SetValue(FMRUCount);
  for i := 0 to FMRUCount-1 do
    GlobalOptions[PROP_MRU_ENTRY+IntToStr(i)].SetValue(FMRUArray[i]);
  // And any other params
  GlobalOptions.Save;
end;

procedure TfrmGrammar.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckDirtyClose;
end;

procedure TfrmGrammar.FormCreate(Sender: TObject);
var i: integer;
    mi: TMenuItem;
begin
  GlobalOptions := TGlobalOptionList.Create;
  GlobalOptions.Load;
  FMRUCount := GlobalOptions[PROP_MRU_COUNT].GetValueI;
  SetLength(FMRUArray,MAXIMUM_MRU_ITEMS);
  if (FMRUCount >= 0) and (FMRUCount <= MAXIMUM_MRU_ITEMS) then
    begin
      for i := 0 to FMRUCount-1 do
        FMRUArray[i] := GlobalOptions[PROP_MRU_ENTRY + IntToStr(i)].GetValueS;
    end;

  FGrammar := TGrammar.Create;
  OpenDialog1.InitialDir := ProgramData;
  SaveDialog1.InitialDir := ProgramData;

  // Set up MRU items
  SetLength(MRUMenuItems,MAXIMUM_MRU_ITEMS);
  for i := 0 to MAXIMUM_MRU_ITEMS-1 do
    begin
      mi := TMenuItem.Create(miFile);
      mi.Name := MI_MRU_PREFIX + IntToStr(i);
      mi.Caption := 'MRU ' + IntToStr(i+1);
      mi.OnClick := @MRUGo;
      MRUMenuItems[i] := mi;
      miFile.Add(mi);
    end;
  MRUSync;
end;

procedure TfrmGrammar.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGrammar);
  FreeAndNil(GlobalOptions);
end;

procedure TfrmGrammar.FormResize(Sender: TObject);
begin
  MRUSync;
end;

procedure TfrmGrammar.FormShow(Sender: TObject);
begin
  if (GlobalOptions[PROP_SCREEN_WIDTH].GetValueI > 0) and
     (GlobalOptions[PROP_SCREEN_HEIGHT].GetValueI > 0) and
     (GlobalOptions[PROP_SCREEN_STATE].GetValueI >= 0) then
    begin
      Left   := GlobalOptions[PROP_SCREEN_LEFT].GetValueI;
      Top    := GlobalOptions[PROP_SCREEN_TOP].GetValueI;
      Width  := GlobalOptions[PROP_SCREEN_WIDTH].GetValueI;
      Height := GlobalOptions[PROP_SCREEN_HEIGHT].GetValueI;
      if TWindowState(GlobalOptions[PROP_SCREEN_STATE].GetValueI) = wsMaximized then
        WindowState := wsMaximized;
    end;
  AutoTest := GlobalOptions[PROP_AUTO_TEST].GetValueB;
end;

procedure TfrmGrammar.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmGrammar.actFileNewExecute(Sender: TObject);
begin
  FGrammar.New(True);
  Dirty := False;
  SyncGrammar;
end;

procedure TfrmGrammar.actFileOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    DoOpen(OpenDialog1.FileName);
end;

procedure TfrmGrammar.actFileSaveAsExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      Filename := SaveDialog1.FileName;
      WriteToXML;
      MRUAdd(Filename);
      GlobalOptions[PROP_FILE_SAVEAS_FOLDER].SetValue(ExtractFilePath(Filename));
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

procedure TfrmGrammar.actToolsAutoTestExecute(Sender: TObject);
begin
  AutoTest := not AutoTest;
end;

procedure TfrmGrammar.actToolsTestGrammarExecute(Sender: TObject);
var msg: string;
begin
  // Do some idiot checks first, then if all is good try and build the NFA/DFA
  // for both the pre-parser and the expression analyser.

  msg := '';

  // Do the simplest data checks first as they are quick to do
  TestEndRules(msg);
  TestEscapeRules(msg);
  TestFilenameQuoting(msg);
  TestFunctions(msg);
  TestLabelCharactersMid(msg);
  TestLabelCharactersStart(msg);
  TestLabelColonRule(msg);
  TestLabelMaximum(msg);
  // Start checking the literal formats
  if (msg <> '') then
    begin
      frmGrammarError := TfrmGrammarError.Create(msg);
      try
        frmGrammarError.ShowModal;
      finally
        FreeAndNil(frmGrammarError);
      end;
    end
  else if Sender <> nil then
    MessageDlg('Grammar OK','Grammar passed the checks',Dialogs.mtInformation,[mbOK],0);
end;

procedure TfrmGrammar.StringGrid1DblClick(Sender: TObject);
var key: string;
    obj: TGrammarObj;
    inputform: TGrammarEditor;
begin
  key := StringGrid1.Cells[0,StringGrid1.Row];
  obj := FGrammar.GrammarList[key];
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
      GrammarMonitor(mtInternal,'Object DataType %s not catered for',[GetEnumName(TypeInfo(TGrammarDataType),Ord(obj.DataType))]);
  ;
  end;
  if Assigned(inputform) then
    begin
      try
        if inputform.ShowModal = mrOK then
          begin
            Dirty := True;
            SyncGrammar;
            if AutoTest then
              actToolsTestGrammarExecute(nil);
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
      case MessageDlg('File not saved',
                      'Warning, your file has not been saved. Do you wish to save the file? Pick Yes if you wish to save, No if you wish to lose the changes, or Cancel if you want to go back to what you were doing',
                      Dialogs.mtWarning,
                      [mbYes,mbNo,mbCancel],
                      0) of
        mrYes:    if Filename = '' then
                    actFileSaveAsExecute(Self)
                  else
                    actFileSaveExecute(Self);
        mrNo:     ; // Do nothing
        mrCancel: Result := False; // Don't kill the file
      end;
    end;
end;


procedure TfrmGrammar.DoOpen(const _fn: string);
begin
  Filename := _fn;
  if Filename <> '' then
    MRUAdd(Filename);
  ReadFromXML;
  Dirty := False;
  SyncGrammar;
end;

procedure TfrmGrammar.MRUAdd(const _filename: string);
var i: integer;
begin
  MRURemove(_filename); // Take it out if already in the list
  // Expand if possible
  if FMRUCount < MAXIMUM_MRU_ITEMS then
    begin
      SetLength(FMRUArray,FMRUCount+1);
      Inc(FMRUCount);
    end;
  // Increase by one and move up
  for i := FMRUCount-1 downto 1 do
    FMRUArray[i] := FMRUArray[i-1];
  FMRUArray[0] := _filename;
  // Finally
  MRUSync;
end;

procedure TfrmGrammar.MRUGo(Sender: TObject);
var index: integer;
    s: string;
begin
  if Sender is TMenuItem then
    with Sender as TMenuItem do
      if Pos(MI_MRU_PREFIX,Name) > 0 then
        begin
          s := Name;
          System.Delete(s,1,Length(MI_MRU_PREFIX)); // Should just leave number
          index := StrToInt(s);
          if (index >= 0) and (index < FMRUCount) then
            begin
              if CheckDirtyClose then
                begin
                  if FileExists(FMRUArray[index]) then
                    DoOpen(FMRUArray[index])
                  else
                    begin
                      MessageDlg('Error',
                                 Format('The file "%s" no longer exists. Removing from recently used files list',[FMRUArray[index]]),
                                 Dialogs.mtError,
                                 [mbOK],
                                 '');
                      MRURemove(FMRUArray[index]);
                    end;
                end;
            end;
        end;
end;

procedure TfrmGrammar.MRURemove(const _filename: string);
var index: integer;
    i:     integer;
    mruc:  integer;
begin
  mruc := FMRUCount;
  index := -1;
  for i := 0 to mruc-1 do
    if FMRUArray[i] = _filename then
      index := i;
  if index >= 0 then
    begin
      for i := index+1 to mruc-1 do
        FMRUArray[i-1] := FMRUArray[i];
      SetLength(FMRUArray,mruc-1);
      Dec(FMRUCount);
      // Finally
      MRUSync;
    end;
end;

procedure TfrmGrammar.MRUSync;
var i: integer;
    mruc: integer;
    fn: string;
begin
  mruc := FMRUCount;
  // Work out visible or not
  for i := 0 to MAXIMUM_MRU_ITEMS-1 do
      MRUMenuItems[i].Visible := i < mruc;
  miFileSepMRU.Visible := mruc > 0;
  // Add in entries
  for i := 0 to mruc-1 do
    begin
      fn := FMRUArray[i];
      fn := MinimizeName(fn,Canvas,MAXIMUM_MRU_PIXELS);
      if i < 9 then
        MRUMenuItems[i].Caption := '&' + Chr(i+Ord('1')) + ' ' + fn
      else
        MRUMenuItems[i].Caption := fn;
    end;
end;

procedure TfrmGrammar.ReadFromXML;
var doc: TXMLDocument;
    root, section: TDOMNode;
    msg:  string;
    value: string;
    pair: TGrammarList.TDictionaryPair;

begin
  ReadXMLFile(doc,Filename);
  try
    // Create the root
    root := doc.FindNode('grammar');
    if not Assigned(root) then
      GrammarMonitor(mtError,'Could not find <grammar> entry in file');
    // Read each section
    for pair in FGrammar.GrammarList do
      begin
        section := root.FindNode(UnicodeString(FGrammar.GrammarList[pair.key].Title));
        if section = nil then
          value := FGrammar.GrammarList[pair.key].DefaultStr
        else
          value := AnsiString(section.TextContent);
          if not FGrammar.GrammarList[pair.key].SetVal(value,msg{%H-}) then
            GrammarMonitor(mtError,msg)
      end;
  finally
    FreeAndNil(doc);
  end;
end;

procedure TfrmGrammar.SetAutoTest(_v: boolean);
begin
  FAutoTest := _v;
  actToolsAutoTest.Checked := _v;
  GlobalOptions[PROP_AUTO_TEST].SetValue(_v);
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
    sl: TStringList;
    obj: TGrammarObj;

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
  // Now do the sync - get the string list so we can sort the dictionary
  // output instead of having a randomly ordered list
  sl := FGrammar.GrammarList.SortedList;
  try
    for i := 0 to sl.Count-1 do
      begin
        obj := FGrammar.GrammarList[sl[i]];
        StringGrid1.Cells[0,1+i] := obj.Title;
        StringGrid1.Cells[1,1+i] := obj.AsText;
        SetCWidth(w0,StringGrid1.Cells[0,1+i]);
        SetCWidth(w1,StringGrid1.Cells[1,1+i]);
      end;
  finally
    FreeAndNil(sl);
  end;
  // Finally set widths
  StringGrid1.ColWidths[0] := w0 + 8;
  StringGrid1.ColWidths[1] := w1 + 8;
end;

procedure TfrmGrammar.TestEndRules(var msg: string);
begin
  if (FGrammar.GrammarList['EndBaggage'].nomVar in [tnOptional,tnMandatory]) and
     (FGrammar.GrammarList['EndRule'].nomVar = tnNever) then
    msg := msg + 'Inconsistency: EndRule is specified as Never, however EndBaggage is Optional or Mandatory' + #13;
end;

procedure TfrmGrammar.TestEscapeRules(var msg: string);
begin
  if (FGrammar.GrammarList['EscapeSet'].csetVar <> []) and
     (FGrammar.GrammarList['EscapeCharacter'].strVar = '') then
    msg := msg + 'Inconsistency: EscapeSet is defined, however EscapeCharacter is blank' + #13;
end;

procedure TfrmGrammar.TestFilenameQuoting(var msg: string);
begin
  // @@@@@ add code
end;

procedure TfrmGrammar.TestFunctions(var msg: string);
begin
  // @@@@@ add code
end;

procedure TfrmGrammar.TestLabelCharactersMid(var msg: string);
begin
  // @@@@@ add code
end;

procedure TfrmGrammar.TestLabelCharactersStart(var msg: string);
begin
  // @@@@@ add code
end;

procedure TfrmGrammar.TestLabelColonRule(var msg: string);
begin
  // @@@@@ add code
end;

procedure TfrmGrammar.TestLabelMaximum(var msg: string);
begin
  if (FGrammar.GrammarList['LabelMaximumLimit'].wordVar > GRAMMAR_MAXIMUM_LABEL_LENGTH) then
    msg := msg + 'Error: LabelMaximumLimit exceeds the assembler maximum of ' + IntToStr(GRAMMAR_MAXIMUM_LABEL_LENGTH) + #13;
  if (FGrammar.GrammarList['LabelMaximumUsed'].wordVar > FGrammar.GrammarList['LabelMaximumLimit'].wordVar) then
    msg := msg + 'Inconsistency: LabelMaximumUsed exceeds LabelMaximumLimit' + #13;
end;

procedure TfrmGrammar.WriteToXML;
var doc: TXMLDocument;
    root, section, item: TDOMNode;
    sl:   TStringList;
    i:    integer;
    obj:  TGrammarObj;
begin
  doc := TXMLDocument.Create;
  try
    // Create the root
    root := doc.CreateElement('grammar');
    doc.AppendChild(root);
    // Write out the sections
    sl := FGrammar.GrammarList.SortedList;
    try
      for i := 0 to sl.Count-1 do
        begin
          obj := FGrammar.GrammarList[sl[i]];
          section := doc.CreateElement(UnicodeString(obj.Title));
          root.AppendChild(section);
          item    := doc.CreateTextNode(UnicodeString(obj.AsText));
          section.AppendChild(item);
        end;
    finally
      FreeAndNil(sl);
    end;
    WriteXMLFile(doc,Filename);
  finally
    FreeAndNil(doc);
  end;
end;

end.

