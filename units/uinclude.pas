unit uinclude;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIncludeEntry = class(TObject)
    private
      FFilename: string;
      procedure SetFilename(const _filename: string);
    public
      property FileName: string read FFilename write SetFilename;
  end;

implementation

//-----------------------------------------------------------------------------
//
//  TIncludeEntry code
//
//-----------------------------------------------------------------------------

procedure TIncludeEntry.SetFilename(const _filename: string);
var fn: string;
begin
  fn := ExpandFilename(_filename);
  if FileExists(fn) then
    FFileName := fn

end;



//-----------------------------------------------------------------------------
//
//  TIncludeList code
//
//-----------------------------------------------------------------------------

end.

