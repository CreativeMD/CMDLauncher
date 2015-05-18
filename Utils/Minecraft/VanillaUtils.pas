unit VanillaUtils;

interface

uses System.Generics.Collections, Task, ProgressBar, superobject, DownloadUtils,
System.SysUtils, InstanceUtils, SaveFileUtils, MinecraftLaunchCommand,
SettingUtils, JavaUtils, System.Classes, AccountUtils;

type
  TMinecraftTyp = (mvSnapshot, mvRelease, mvOld);
  TLaunchTyp = (ltOld, ltNew);
  TMinecraftVersion = class
    ReleaseTyp : TMinecraftTyp;
    LaunchTyp : TLaunchTyp;
    UUID : string;
    constructor Create(UUID : String; ReleaseTyp : TMinecraftTyp; LaunchTyp : TLaunchTyp);
  end;
  TLoadMV = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      constructor Create;
  end;
  TVanillaInstance = class(TInstance)
    protected
      procedure Load(SaveFile : TSaveFile); override;
      procedure Save(SaveFile : TSaveFile); override;
    public
      MinecraftVersion : TMinecraftVersion;
      function getUUID : String; override;
      function getSettings : TList<TSetting>; override;
      function getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>; override;
      function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
      function getLaunchSettings : TList<TSetting>; override;
      function getLaunchSaveFile : TSaveFile; override;
  end;
  TVanillaLaunch = class(TMinecraftLaunch)
    constructor Create(Java : TJava; mcversion : String; Instance : TInstance; LoginData : TLoginData);
  end;

function getMinecraftVersion(UUID : String) : TMinecraftVersion;

var
MinecraftVersions : TList<TMinecraftVersion>;

implementation

uses CoreLoader, AssetUtils, MinecraftVersionFileUtils, MinecraftLibaryUtills,
DatabaseConnection, ServerUtils, StringUtils;

function getMinecraftVersion(UUID : String) : TMinecraftVersion;
var
  i: Integer;
begin
  if MinecraftVersions = nil then
    Exit(nil);
  for i := 0 to MinecraftVersions.Count-1 do
    if MinecraftVersions[i].UUID = UUID then
      Exit(MinecraftVersions[i]);
  Exit(nil);
end;

constructor TVanillaLaunch.Create(Java : TJava; mcversion : String; Instance : TInstance; LoginData : TLoginData);
begin
  inherited Create(Java, mcversion);
  if Instance.InstanceTyp = IClient then
  begin
    Replacements.Add('${auth_player_name}', LoginData.getName);
    if not LoginData.isOffline then
    begin
      Replacements.Add('${auth_session}', LoginData.Session);
      Replacements.Add('${auth_uuid}', LoginData.Session_ID);
      Replacements.Add('${auth_access_token}', LoginData.Session);
    end;

    Replacements.Add('${user_properties}', '{}');
    Replacements.Add('${user_type}', 'legacy');

    Replacements.Add('${version_name}', MCVersion);
    Replacements.Add('${game_directory}', '"' + RemoveLastFolderSpliter(Instance.getInstanceFolder) + '"');
    Replacements.Add('${assets_root}', '"' + DownloadFolder + 'assets"');
    Replacements.Add('${game_assets}', '"' + DownloadFolder + 'assets\virtual\legacy"');

    SpecialArguments.Add('-XX:HeapDumpPath=MojangTricksIntelDriversForPerformance_javaw.exe_minecraft.exe.heapdump');
    SpecialArguments.Add('-XX:+UseConcMarkSweepGC');
    SpecialArguments.Add('-XX:+CMSIncrementalMode');
    SpecialArguments.Add('-XX:-UseAdaptiveSizePolicy');
  end;
  SpecialArguments.Add('-Xmn128M');
  SpecialArguments.Add('-Xmx' + IntToStr(Instance.RAM) + 'm');
  SpecialArguments.Add(Instance.CustomCommand);
end;

constructor TMinecraftVersion.Create(UUID : String; ReleaseTyp : TMinecraftTyp; LaunchTyp : TLaunchTyp);
begin
  Self.UUID := UUID;
  Self.ReleaseTyp := ReleaseTyp;
  Self.LaunchTyp := LaunchTyp;
end;

constructor TLoadMV.Create;
begin
  inherited Create('Loading Minecraft Versions', False);
end;

procedure TLoadMV.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
MCV : ISuperObject;
MCVA : TSuperArray;
i : Integer;
LaunchTyp : TLaunchTyp;
MinecraftTyp : TMinecraftTyp;
begin
  MinecraftVersions := TList<TMinecraftVersion>.Create;
  DownloadTask := TDownloadTask.Create('http://s3.amazonaws.com/Minecraft.Download/versions/versions.json', DownloadFolder + 'versions.json');

  if DatabaseConnection.online then
    DownloadTask.downloadFile(nil);

  if FileExists(DownloadFolder + 'versions.json') then
  begin
    MCV := TSuperObject.ParseFile(DownloadFolder + 'versions.json', true);
    MCVA := MCV.A['versions'];

    LaunchTyp := TLaunchTyp.ltNew;

    Bar.StartStep(MCVA.Length);

    for i := 0 to MCVA.Length-1 do
    begin
      if MCVA[i].S['id'] = '1.7.2' then
        LaunchTyp := TLaunchTyp.ltOld;

      if MCVA[i].S['type'] = 'release' then
        MinecraftTyp := mvRelease
      else if MCVA[i].S['type'] = 'snapshot' then
        MinecraftTyp := mvSnapshot
      else
        MinecraftTyp := mvOld;
      MinecraftVersions.Add(TMinecraftVersion.Create(MCVA[i].S['id'], MinecraftTyp, LaunchTyp));
      Bar.StepPos := i;
    end;
    Self.Log.log('Loaded ' + InttoStr(MinecraftVersions.Count) + ' Minecraftversions');
  end;
  Bar.FinishStep;
end;

procedure TVanillaInstance.Load(SaveFile : TSaveFile);
begin
  MinecraftVersion := VanillaUtils.getMinecraftVersion(SaveFile.getString('mcversion'));
end;

procedure TVanillaInstance.Save(SaveFile : TSaveFile);
begin
  SaveFile.setString('mcversion', MinecraftVersion.UUID);
end;

function TVanillaInstance.getUUID : String;
begin
  Result := 'Vanilla';
end;

function TVanillaInstance.getSettings : TList<TSetting>;
var
Selected : String;
Items : TStringList;
  i: Integer;
isOnline : Boolean;
begin
  Result := TList<TSetting>.Create;
  Items := TStringList.Create;
  isOnline := True;
  if VanillaUtils.MinecraftVersions <> nil then
  begin
    for i := 0 to VanillaUtils.MinecraftVersions.Count-1 do
      if VanillaUtils.MinecraftVersions[i].ReleaseTyp = mvRelease then
        Items.Add(VanillaUtils.MinecraftVersions[i].UUID);
    Selected := Items[0];
    isOnline := False;
  end
  else
    Selected := '';

  Result.Add(TSelectSetting.Create('mcversion', 'Version', Items, Selected, isOnline));
end;

function TVanillaInstance.getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>;
begin
  Result := TList<TTask>.Create;
  if InstanceTyp = IClient then
  begin
    if DatabaseConnection.online then
    begin
      Result.Add(TDownloadVersion.Create(MinecrafComand));
    end;
    Result.Add(TLoadAssets.Create(MinecrafComand, MinecraftVersion.LaunchTyp));
    Result.Add(TDownLoadLibary.Create(MinecrafComand));
  end
  else
    Result.Add(TDownloadServerVersion.Create(getInstanceFolder, MinecrafComand));
end;

function TVanillaInstance.getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch;
begin
  if MinecraftVersion = nil then
    Result := TVanillaLaunch.Create(Java, getSaveFile.getString('mcversion'), Self, LoginData)
  else
    Result := TVanillaLaunch.Create(Java, MinecraftVersion.UUID, Self, LoginData)
end;

function TVanillaInstance.getLaunchSettings : TList<TSetting>;
begin
  Result := TList<TSetting>.Create;
  if InstanceTyp = IServer then
  begin
    Result.AddRange(ServerUtils.getStandardServerSettings(Self));
  end;
end;

function TVanillaInstance.getLaunchSaveFile : TSaveFile;
begin
  Result := TSaveFile.Create(getInstanceFolder + 'server.properties');
end;

end.
