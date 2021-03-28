unit LocalCache4D;
interface
uses
  LocalCache4D.Interfaces,
  System.Generics.Collections;
type
  TLocalCache4D = class(TInterfacedObject, iLocalCache4D)
    private
      FCacheList : TDictionary<string, string>;
      FInstanceList : TDictionary<string, TDictionary<string, string>>;
      FInstance : String;
    public
      constructor Create;
      destructor Destroy; override;
      class function New : iLocalCache4D;
      function LoadDatabase ( aDabaseName : String = '' ) : iLocalCache4D;
      function SaveToStorage (aDabaseName : String = '' ) : iLocalCache4D;
      function Instance ( aValue : String ) : iLocalCache4D;
      function RemoveInstance ( aValue : String ) : iLocalCache4D;
      function ListItens : TDictionary<string, string>;
      function ListInstances : TDictionary<String, TDictionary<string, string>>;
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
  FInstanceList := TDictionary<string, TDictionary<string, string>>.Create;
end;
destructor TLocalCache4D.Destroy;
begin
  FCacheList.Free;
  for var ListCache in FInstanceList do
    ListCache.Value.Free;
  FInstanceList.Free;
  inherited;
end;
function TLocalCache4D.GetItem(aItem: String): string;
begin
  Result := FInstanceList.Items[FInstance].Items[aItem];
  //Result := FCacheList.Items[aItem];
end;
function TLocalCache4D.Instance(aValue: String): iLocalCache4D;
begin
  Result := Self;
  FInstance := aValue;
end;

function TLocalCache4D.ListInstances: TDictionary<String, TDictionary<string, string>>;
begin
  Result := FInstanceList;
end;

function TLocalCache4D.ListItens: TDictionary<string, string>;
begin
  Result := FInstanceList.Items[FInstance];// FCacheList;
end;
function TLocalCache4D.LoadDatabase( aDabaseName : String = '' ) : iLocalCache4D;
var
  LFile: TMemIniFile;
  LFileName: string;
  LKeys: TStringList;
  LSections : TStringList;
  LKey: string;
  LValue : String;
begin
  Result := Self;
  if aDabaseName = '' then
    LFileName := ChangeFileExt(ParamStr(0), '.lc4')
  else
    LFileName := ChangeFileExt(ExtractFilePath(ParamStr(0)) + aDabaseName, '.lc4');
  LKeys := TStringList.Create;
  try
    LFile := TMemIniFile.Create(LFileName, TEncoding.Unicode);
    LSections := TStringList.Create;
    try
      LFile.ReadSections(LSections);
      for var LSection in LSections do
      begin
        FInstanceList.Add(LSection, TDictionary<string, string>.Create);
        LFile.ReadSection(LSection, LKeys);
        for LKey in LKeys do
          if not FInstanceList.Items[LSection].TryGetValue(LKey, LValue) then
            FInstanceList.Items[LSection].Add(LKey, LFile.ReadString(LSection, LKey, ''));
      end;
      //LFile.ReadSection(C_SECTION, LKeys);
      //FCacheList.Clear;
      //for LKey in LKeys do
      //begin
        //if not FCacheList.TryGetValue(LKey, LValue) then
          //FCacheList.Add(LKey, LFile.ReadString(C_SECTION, LKey, ''));
      //end;
    finally
      LFile.DisposeOf;
      LSections.DisposeOf;
    end;
  finally
    LKeys.DisposeOf;
  end;
end;
class function TLocalCache4D.New: iLocalCache4D;
begin
  if not Assigned(LocalCache) then
    LocalCache := Self.Create;

  Result := LocalCache;
end;
function TLocalCache4D.RemoveInstance(aValue: String): iLocalCache4D;
begin
  Result := Self;
  FInstanceList.Items[aValue].Free;
  FInstanceList.Remove(aValue);
end;

function TLocalCache4D.RemoveItem(aKey: String): iLocalCache4D;
begin
  Result := Self;
  FInstanceList.Items[FInstance].Remove(aKey);
  //FCacheList.Remove(aKey);
end;
function TLocalCache4D.SaveToStorage(aDabaseName : String = '' ) : iLocalCache4D;
var
  LFile: TMemIniFile;
  LFileName: string;
  //LValuePair: TPair<string, string>;
  LFileTmp: TextFile;
begin
  Result := Self;
  if aDabaseName = '' then
    LFileName := ChangeFileExt(ParamStr(0), '.lc4')
  else
    LFileName := ChangeFileExt(ExtractFilePath(ParamStr(0)) + aDabaseName, '.lc4');
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
    for var Instances in FInstanceList do
      for var CacheList in Instances.Value do
        LFile.WriteString(Instances.Key, CacheList.Key, CacheList.Value);

    //for LValuePair in FCacheList do
      //LFile.WriteString(C_SECTION, LValuePair.Key, LValuePair.Value);
    LFile.UpdateFile;
  finally
    LFile.DisposeOf;
  end;
end;
function TLocalCache4D.SetItem(aKey, aValue: String): iLocalCache4D;
begin
  Result := Self;
  if not FInstanceList.TryGetValue(FInstance, FCacheList) then
    FInstanceList.Add(FInstance, TDictionary<string, string>.Create);

  if not FInstanceList.Items[FInstance].TryAdd(aKey, aValue) then
    FInstanceList.Items[FInstance].Items[aKey] := aValue;
  //if not FCacheList.TryAdd(aKey, aValue) then
    //FCacheList.Items[aKey] := aValue;
end;
function TLocalCache4D.TryGetItem(aItem: String; out aResult: String): Boolean;
begin
  if not FInstanceList.TryGetValue(FInstance, FCacheList) then
  begin
    Result := False;
    aResult := '';
    exit;
  end;
  Result := FInstanceList.Items[FInstance].TryGetValue(aItem, aResult);
end;

initialization
  LocalCache := TLocalCache4D.New;

end.
