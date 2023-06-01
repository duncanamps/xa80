unit ulisting;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uasmglobals;

type
  TListing = class(TObject)
    private
      FFilename:   string;
      FLineNo:     integer;
      FListing:    boolean;
      FPageNo:     integer;
      FPageWidth:  integer;
      FPageLength: integer;
      FStream:     TFileStream;
      FTitle:      string;
      procedure Header;
      procedure OutputLine(_s: string);
      procedure SetFilename(const _filename: string);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Output(const _s: string);
      property Filename: string  read FFilename write SetFilename;
      property Listing:  boolean read FListing  write FListing;
      property Title:    string  read FTitle    write FTitle;
  end;

implementation

constructor TListing.Create;
begin
  FStream := nil;
  FPageWidth := DEFAULT_LISTING_PAGE_WIDTH;
  FPageLength := DEFAULT_LISTING_PAGE_LENGTH;
  FPageNo := 0;
  FLineNo := 0;
end;

destructor TListing.Destroy;
begin
  if Assigned(FStream) then
    begin
      if FPageNo > 0 then
        OutputLine(FF);
      FreeAndNil(FStream);
    end;
end;

procedure TListing.Header;
var caption: string;
    pagestr: string;
    gaps:    integer;
begin
  if FPageNo > 0 then
    OutputLine(FF);
  Inc(FPageNo);
  caption := 'Assembly Listing';
  pagestr := Format('Page: %d',[FPageNo]);
  gaps := FPageWidth - Length(caption) - Length(FTitle) - Length(pagestr);
  OutputLine('');
  OutputLine(caption + Space(gaps div 2) + FTitle + Space(gaps - gaps div 2) + pagestr);
  OutputLine(StringOfChar('-',FPageWidth));
  OutputLine('');
  FLineNo := 4;
end;

procedure TListing.Output(const _s: string);
begin
  if FListing and Assigned(FStream) then
    begin
      if (FLineNo >= FPageLength) or (FPageNo = 0) then
        Header;
      OutputLine(_s);
    end;
end;

procedure TListing.OutputLine(_s: string);
begin
  _s := _s + LINE_TERMINATOR;
  FStream.Write(_s[1],Length(_s));
  Inc(FLineNo);
end;

procedure TListing.SetFilename(const _filename: string);
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  FFilename := _filename;
  if _filename <> '' then
    FStream := TFileStream.Create(FFilename,fmCreate);
end;

end.

