unit ucoder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCoder = class(TObject)
    private
      FSL: TStringList; // Contains the text data
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadFromFile(const filename: string);
      procedure SaveToFile(const filename: string);
  end;

implementation

constructor TCoder.Create;
begin
  inherited Create;
  FSL := TStringList.Create;
end;

destructor TCoder.Destroy;
begin
  FreeAndNil(FSL);
  inherited Destroy;
end;

procedure TCoder.LoadFromFile(const filename: string);
begin
  FSL.LoadFromFile(filename);
end;

procedure TCoder.SaveToFile(const filename: string);
begin

end;

end.

