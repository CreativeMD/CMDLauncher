unit ResourcePackUtils;

interface

uses Task, System.Generics.Collections, VanillaUtils, ProgressBar, superobject, System.Classes, DownloadUtils, System.SysUtils,
SettingUtils;

type
  TLoadResourcePacks = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;
  TResourcePackVersion = class;
  TResourcePack = class
    private
      FID : Integer;
      FName : String;
    public
      Versions : TList<TResourcePackVersion>;
      constructor Create(Json : ISuperObject);
      property ID : Integer read FID;
      property Name : String read FName;
  end;
  TResourcePackVersion = class
    private
      FID : Integer;
      FName, FFileName, FIFileName, FURL : String;
      FMC : TStringList;
    public
      constructor Create(Json : ISuperObject);
      function getMinecraftVersion : TList<TMinecraftVersion>;
      property MC : TStringList read FMC;
      property FileName : String read FFileName;
      property IFileName : String read FIFileName;
      property URL : String read FURL;
      property ID : Integer read FID;
      property Name : String read FName;
  end;
  TResourcepackSelect = class(TSetting<TStringList>)

  end;

var
ResourcePacks : TList<TResourcePack>;

implementation

uses CoreLoader, DatabaseConnection;

constructor TLoadResourcePacks.Create;
begin
  inherited Create('Loading ResourcePacks', True, False);
end;

procedure TLoadResourcePacks.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
ResourcePack : TResourcePack;
JsonFile : ISuperObject;
JsonArray : TSuperArray;
I: Integer;
FileString : TStringList;
FileJsonData : WideString;
begin
  FileName := DownloadFolder + 'resourcepack.json';
  ResourcePacks := TList<TResourcePack>.Create;
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/resourcepackservice.php',
  FileName, True);

  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
  end;

  if FileExists(FileName) and (VanillaUtils.MinecraftVersions <> nil) then
  begin
    FileString := TStringList.Create;
    FileString.LoadFromFile(FileName);
    FileJsonData := '';
    for i := 0 to FileString.Count-1 do
      FileJsonData := FileJsonData + Trim(FileString[i]).Replace(#$FEFF, '');
    JsonFile := TSuperObject.ParseString(PWideChar(FileJsonData), False);
    JsonArray := JsonFile.AsArray;
    for I := 0 to JsonArray.Length-1 do
    begin
      ResourcePack := TResourcePack.Create(JsonArray.O[i]);
      if ResourcePack <> nil then
        ResourcePacks.Add(ResourcePack);
    end;
  end;
end;

constructor TResourcePack.Create(Json : ISuperObject);
var
VersionArray : TSuperArray;
i: Integer;
Version : TResourcePackVersion;
begin
  VersionArray := Json.A['versions'];
  Versions := TList<TResourcePackVersion>.Create;
  for i := 0 to VersionArray.Length-1 do
  begin
    Version := TResourcePackVersion.Create(VersionArray.O[i]);
    if Version <> nil then
      Versions.Add(Version);
  end;
  FID := Json.I['id'];
  FName := Json.S['name'];
end;

constructor TResourcePackVersion.Create(Json : ISuperObject);
var
MCArray : TSuperArray;
  i: Integer;
begin
  MCArray := Json.A['mc'];
  FMC := TStringList.Create;
  if MCArray <> nil then
    for i := 0 to MCArray.Length-1 do
      FMC.Add(MCArray.S[i]);
  FFileName := Json.S['filename'];
  FIFileName := Json.S['ifilename'];
  FURL := Json.S['url'];
  FID := Json.I['id'];
  FName := Json.S['name'];
end;

function TResourcePackVersion.getMinecraftVersion : TList<TMinecraftVersion>;
var
MCVersion : TMinecraftVersion;
  i: Integer;
begin
  Result := TList<TMinecraftVersion>.Create;
  for i := 0 to MC.Count-1 do
  begin
    MCVersion := VanillaUtils.getMinecraftVersion(MC[i]);
    if MCVersion <> nil then
      Result.Add(MCVersion);
  end;

end;


end.
