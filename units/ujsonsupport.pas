unit ujsonsupport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function FindOrMakeJSON(_parent: TJSONdata; const _name: string): TJSONdata;
function FindOrMakeJSONarray(_parent: TJSONdata; const _name: string): TJSONarray;

implementation

function FindOrMakeJSON(_parent: TJSONdata; const _name: string): TJSONdata;
var obj: TJSONdata;
begin
  obj := _parent.FindPath(_name);
  if not Assigned(obj) then
    begin
      TJSONobject(_parent).Add(_name,GetJSON('{}'));
      obj := _parent.FindPath(_name);
    end;
  FindOrMakeJSON := obj;
end;

function FindOrMakeJSONarray(_parent: TJSONdata; const _name: string): TJSONarray;
var obj: TJSONarray;
begin
  obj := _parent.FindPath(_name) as TJSONarray;
  if not Assigned(obj) then
    begin
      TJSONobject(_parent).Add(_name,GetJSON('[]'));
      obj := _parent.FindPath(_name) as TJSONarray;
    end;
  FindOrMakeJSONarray := obj;
end;

end.

