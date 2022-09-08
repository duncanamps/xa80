unit ugrammaroptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, XMLconf, Generics.Collections;

const
  GRAMMAR_CONFIG_NAME = 'grammar_editor.config';
  MAXIMUM_MRU_ITEMS = 10;

  PROP_AUTO_TEST                                = 'AutoTest';
  PROP_FILE_OPEN_FOLDER                         = 'FileOpenFolder';
  PROP_FILE_SAVEAS_FOLDER                       = 'FileSaveAsFolder';
  PROP_MRU_COUNT                                = 'MRUCount';
  PROP_MRU_ENTRY                                = 'MRUEntry';
  PROP_SCREEN_HEIGHT                            = 'ScreenHeight';
  PROP_SCREEN_LEFT                              = 'ScreenLeft';
  PROP_SCREEN_STATE                             = 'ScreenState';
  PROP_SCREEN_TOP                               = 'ScreenTop';
  PROP_SCREEN_WIDTH                             = 'ScreenWidth';

type
  TGlobalOptionList = class; // Pre-declaration

  TGlobalOption = class(TObject)
    protected
      FDeflt: string;
      FValue: string;
    public
      function GetValueB: boolean;
      function GetValueD: double;
      function GetValueI: integer;
      function GetValueS: string;
      procedure SetValue(_v: boolean);
      procedure SetValue(_v: double);
      procedure SetValue(_v: integer);
      procedure SetValue(const _v: string);
  end;

  TGlobalOptionListBase = specialize TObjectDictionary<string,TGlobalOption>;

  TGlobalOptionList = class(TGlobalOptionListBase)
    protected
      procedure RegisterOption(const _optname: string;       _deflt: boolean);
      procedure RegisterOption(const _optname: string;       _deflt: double);
      procedure RegisterOption(const _optname: string;       _deflt: integer);
      procedure RegisterOption(const _optname: string; const _deflt: string);
    public
      constructor Create; reintroduce;
      procedure Assign(_src: TGlobalOptionList);
      function  GetOption(const _optname: string): string;
      procedure Load;
      function  Matches(_src: TGlobalOptionList): boolean;
      procedure SetOption(const _optname: string; const _value: string);
      procedure Save;
  end;


var
  GlobalOptions: TGlobalOptionList;


implementation

uses
  uutility;

function TGlobalOption.GetValueB: boolean;
begin
  Result := StrToBool(FValue);
end;

function TGlobalOption.GetValueD: double;
begin
  Result := StrToFloat(FValue);
end;

function TGlobalOption.GetValueI: integer;
begin
  Result := StrToInt(FValue);
end;

function TGlobalOption.GetValueS: string;
begin
  Result := FValue;
end;

procedure TGlobalOption.SetValue(_v: boolean);
begin
  SetValue(BoolToStr(_v));
end;

procedure TGlobalOption.SetValue(_v: double);
begin
  SetValue(FloatToStr(_v));
end;

procedure TGlobalOption.SetValue(_v: integer);
begin
  SetValue(IntToStr(_v));
end;

procedure TGlobalOption.SetValue(const _v: string);
begin
  FValue := _v;
end;

constructor TGlobalOptionList.Create;
var i: integer;
begin
  inherited Create([doOwnsValues]);
  RegisterOption(PROP_AUTO_TEST,                                True);
  RegisterOption(PROP_FILE_OPEN_FOLDER,                         '');
  RegisterOption(PROP_FILE_SAVEAS_FOLDER,                       '');
  RegisterOption(PROP_MRU_COUNT,                                0);
  for i := 0 to MAXIMUM_MRU_ITEMS-1 do
    RegisterOption(                                       PROP_MRU_ENTRY + IntToStr(i),          '');
  RegisterOption(PROP_SCREEN_HEIGHT,                      -1);
  RegisterOption(PROP_SCREEN_LEFT,                        -1);
  RegisterOption(PROP_SCREEN_STATE,                       -1);
  RegisterOption(PROP_SCREEN_TOP,                         -1);
  RegisterOption(PROP_SCREEN_WIDTH,                       -1);
end;

procedure TGlobalOptionList.Assign(_src: TGlobalOptionList);
var pair: TGlobalOptionList.TDictionaryPair;
    keyval: string;
begin
  for pair in _src do
    begin
      keyval := pair.Key;
      Self[keyval].SetValue(pair.Value.FValue);
    end;
end;

function TGlobalOptionList.Matches(_src: TGlobalOptionList): boolean;
var pair: TGlobalOptionList.TDictionaryPair;
    keyval: string;
begin
  Result := True; // Assume match for now
  for pair in _src do
    begin
      keyval := pair.Key;
      if Self[keyval].FValue <> pair.Value.FValue then
        Result := False;
    end;
end;

procedure TGlobalOptionList.RegisterOption(const _optname: string; _deflt: boolean);
begin
  RegisterOption(_optname,BoolToStr(_deflt));
end;

procedure TGlobalOptionList.RegisterOption(const _optname: string; _deflt: double);
begin
  RegisterOption(_optname,FloatToStr(_deflt));
end;

procedure TGlobalOptionList.RegisterOption(const _optname: string; _deflt: integer);
begin
  RegisterOption(_optname,IntToStr(_deflt));
end;

procedure TGlobalOptionList.RegisterOption(const _optname: string; const _deflt: string);
var obj: TGlobalOption;
begin
  obj := TGlobalOption.Create;
  obj.FValue := _optname;
  obj.FDeflt := _deflt;
  Add(_optname,obj);
end;

function TGlobalOptionList.GetOption(const _optname: string): string;
var obj: TGlobalOption;
begin
  if TryGetValue(_optname,obj) then
    Result := obj.FValue
  else
    raise Exception.Create('Could not find global option ' + _optname + ' in dictionary');
end;

procedure TGlobalOptionList.Load;
var _XMLConfig: TXMLConfig;
    pair: TGlobalOptionList.TDictionaryPair;
begin
  ForceDirectories(ProgramData);
  _XMLConfig := TXMLConfig.create(nil);
  try
    {$IFDEF WINDOWS}
        _XMLConfig.FileName := ProgramData + GRAMMAR_CONFIG_NAME;
    {$ENDIF}
    {$IFDEF LINUX}
        _XMLConfig.FileName := GetAppConfigFile(False,True);
    {$ENDIF}
    for pair in Self do
      pair.Value.FValue := AnsiString(_XMLConfig.GetValue(UnicodeString(pair.Key),UnicodeString(pair.Value.FDeflt)));
  finally
    _XMLConfig.Free;
  end;
end;

procedure TGlobalOptionList.Save;
var _XMLConfig: TXMLConfig;
    pair: TGlobalOptionList.TDictionaryPair;
begin
  ForceDirectories(ProgramData);
  _XMLConfig := TXMLConfig.create(nil);
  try
{$IFDEF WINDOWS}
    _XMLConfig.FileName := ProgramData + GRAMMAR_CONFIG_NAME;
{$ENDIF}
{$IFDEF LINUX}
    _XMLConfig.FileName := GetAppConfigFile(False,True);
{$ENDIF}
    for pair in Self do
      _XMLConfig.SetValue(UnicodeString(pair.key),UnicodeString(pair.Value.FValue));
  finally
    _XMLConfig.Free;
  end;
end;

procedure TGlobalOptionList.SetOption(const _optname: string; const _value: string);
var obj: TGlobalOption;
begin
  if TryGetValue(_optname,obj) then
    obj.FValue := _value
  else
    raise Exception.Create('Could not find global option ' + _optname + ' in dictionary');
end;

end.

