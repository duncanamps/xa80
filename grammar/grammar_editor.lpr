program grammar_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fgrammar, ugrammar, uutility, umonitor, fgrammareditstring,
  fgrammaredittext, fgrammareditstringlist, fgrammareditnom, fgrammareditu16,
  fgrammareditboolean, fgrammareditmlr, fgrammareditcharset, ugrammaroptions,
  fgrammarerror;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmGrammar, frmGrammar);
  Application.Run;
end.

