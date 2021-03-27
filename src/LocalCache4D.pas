unit LocalCache4D;

interface

uses
  LocalCache4D.Interfaces,
  System.Generics.Collections;

type
  TLocalCache4D = class(TInterfacedObject, iLocalCache4D)
    private
      FCacheList : TDictionary<string, string>;
    public
      constructor Create;
      destructor Destroy; override;
      class function New : iLocalCache4D;
      function LoadDatabase ( aDabaseName : String = '' ) : iLocalCache4D;
      function SaveToStorage (aDabaseName : String = '' ) : iLocalCache4D;
      function ListItens : TDictionary<string, string>;
      function TryGetItem (aItem : String; out aResult : String) : Boolean;
      function GetItem ( aItem : String ) : string;
      function SetItem( aKey : String; aValue : String ) : iLocalCache4D;
      function RemoveItem (aKey : String) : iLocalCache4D;
  end;

var
  LocalCache : iLocalCache4D;

implementation

uses
  System.IniFiles,
  System.Classes,
  System.SysUtils;

const
  C_SECTION = 'LOCALCACHEDATABASE';

{ TLocalCache4D }

constructor TLocalCache4D.Create;
begin
  FCacheList := TDictionary<string, string>.Create;
end;

destructor TLocalCache4D.Destroy;
begin
  FCacheList.Free;
  inherited;
end;

function TLocalCache4D.GetItem(aItem: String): string;
begin
  Result := FCacheList.Items[aItem];
end;

function TLocalCache4D.ListItens: TDictionary<string, string>;
begin
  Result := FCacheList;
end;

function TLocalCache4D.LoadDatabase( aDabaseName : String = '' ) : iLocalCache4D;
var
  LFile: TMemIniFile;
  LFileName: string;
  LKeys: TStringList;
  LKey: string;
  LValue : String;
  Stream : TStringStream;
  lResult: String;
begin
  Result := Self;
  if aDabaseName = '' then
    LFileName := ChangeFileExt(ParamStr(0), '.ini')
  else
    LFileName := ChangeFileExt(ExtractFilePath(ParamStr(0)) + aDabaseName, '.ini');
  LKeys := TStringList.Create;
  try
    LFile := TMemIniFile.Create(LFileName, TEncoding.Unicode);
    try
      LFile.ReadSection(C_SECTION, LKeys);
      for LKey in LKeys do
      begin
        if not FCacheList.TryGetValue(LKey, LValue) then
          FCacheList.Add(LKey, LFile.ReadString(C_SECTION, LKey, ''));
      end;
    finally
      LFile.DisposeOf;
    end;
  finally
    LKeys.DisposeOf;
  end;

end;

class function TLocalCache4D.New: iLocalCache4D;
begin
  Result := Self.Create;
end;

function TLocalCache4D.RemoveItem(aKey: String): iLocalCache4D;
begin
  Result := Self;
  FCacheList.Remove(aKey);
end;

function TLocalCache4D.SaveToStorage(aDabaseName : String = '' ) : iLocalCache4D;
var
  LFile: TMemIniFile;
  LFileName: string;
  LValuePair: TPair<string, string>;
  LFileTmp: TextFile;
  aValue : String;
  Stream : TStringStream;
begin
  Result := Self;
  if aDabaseName = '' then
    LFileName := ChangeFileExt(ParamStr(0), '.ini')
  else
    LFileName := ChangeFileExt(ExtractFilePath(ParamStr(0)) + aDabaseName, '.ini');
  if not FileExists(LFileName) then
  begin
    AssignFile(LFileTmp, LFileName);
    Rewrite(LFileTmp);
    CloseFile(LFileTmp);
  end
  else
    DeleteFile(LFileName);

  LFile := TMemIniFile.Create(LFileName, TEncoding.Unicode);
  try
    for LValuePair in FCacheList do
      LFile.WriteString(C_SECTION, LValuePair.Key, LValuePair.Value);
    LFile.UpdateFile;
  finally
    LFile.DisposeOf;
  end;
end;

function TLocalCache4D.SetItem(aKey, aValue: String): iLocalCache4D;
begin
  Result := Self;
  if not FCacheList.TryAdd(aKey, aValue) then
    FCacheList.Items[aKey] := aValue;
end;

function TLocalCache4D.TryGetItem(aItem: String; out aResult: String): Boolean;
begin
  Result := FCacheList.TryGetValue(aItem, aResult);
end;

initialization
  LocalCache := TLocalCache4D.New;

end.
