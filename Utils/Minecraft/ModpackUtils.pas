unit ModpackUtils;

interface

uses ForgeUtils, System.Generics.Collections, Task, ModUtils, ProgressBar, DownloadUtils,
superobject, System.SysUtils, System.Classes, SaveFileUtils, SettingUtils,
AccountUtils, MinecraftLaunchCommand, JavaUtils, StringUtils, SideUtils, LaunchTaskUtils,
Vcl.Dialogs, System.UITypes, Vcl.Forms, Winapi.Windows;

type
  TCustomFile = class
    private
      FFileName, FURL, FIFileName : String;
      FSideType : TSideType;
    public
      constructor Create(Json : ISuperObject);
      property FileName : String read FFileName;
      property IFileName : String read FIFileName;
      property URL : String read FFileName;
      property SideType : TSideType read FSideType;
  end;
  TModpackVersion = class;
  TModpack = class
    private
      FName : String;
      FID : Integer;
      FLoaded : Boolean;
    public
      Versions : TList<TModPackVersion>;
      constructor Create(Json : ISuperObject);
      procedure loadModPack(Json : ISuperObject);
      function getVersionByID(ID : Integer) : TModpackVersion;
      function getVersionByName(Name : String) : TModpackVersion;
      function getNewestVersion : TModpackVersion;
      property Loaded : Boolean read FLoaded;
      property Name : String read FName;
      property ID : Integer read FID;
  end;
  TModpackVersion = class
    private
      FID : Integer;
      FName, FForge : String;
      FMods : TDictionary<TMod, TModVersion>;
      FCustomFiles : TList<TCustomFile>;
      FArchiveURL : String;
    public
      constructor Create(Json : ISuperObject);
      function hasArchive : Boolean;
      property ID : Integer read FID;
      property Forge : String read FForge;
      property Name : String read FName;
      property Mods : TDictionary<TMod, TModVersion> read FMods;
      property ArchiveURL : String read FArchiveURL;
      property CustomFiles : TList<TCustomFile> read FCustomFiles;
  end;
  TLoadModpack = class(TTask)
    procedure runTask(Bar : TCMDProgressBar); override;
    constructor Create;
  end;
  TFullLoadModpack = class(TLoadModpack)
    procedure runTask(Bar : TCMDProgressBar); override;
  end;
  TModPackInstance = class(TForgeInstance)
    protected
      Modpack : TPair<TModpack, TModPackVersion>;
      procedure Load(SaveFile : TSaveFile); override;
      procedure Save(SaveFile : TSaveFile); override;
    public
      procedure LoadPost(SaveFile : TSaveFile); override;
      function getUUID : String; override;
      function getSettings : TList<TSetting>; override;
      function getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>; override;
      //function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
      function getCustomPopUpMenu : TDictionary<string, String>; override;
      procedure onCustomMenuClicked(Item : string); override;
  end;
   TDownloadModpackArchive = class(TLaunchTask)
    Instance : TModpackInstance;
    constructor Create(Instance : TModpackInstance; Command : TMinecraftLaunch);
    procedure runTask(Bar : TCMDProgressBar); override;
  end;
  TCustomFileDownload = class(TLaunchTask)
    Instance : TModpackInstance;
    CustomFiles : TList<TCustomFile>;
    constructor Create(Instance : TModpackInstance; CustomFiles : TList<TCustomFile>; Command : TMinecraftLaunch);
    procedure runTask(Bar : TCMDProgressBar); override;
  end;

function createModpack(Json : ISuperObject) : TModpack;
function getModPackByID(ID : Integer) : TModpack;

var
ModPacks : TList<TModpack>;
ModPacksLoaded : Boolean;

implementation

uses CoreLoader, DatabaseConnection, Logger, InstanceUtils, FileDownload,
ModSettings, ModpackSettings, ZipUtils, Overview;

function getModPackByID(ID : Integer) : TModpack;
var
  i: Integer;
begin
  for i := 0 to ModPacks.Count-1 do
    if ModPacks[i].ID = ID then
      Exit(ModPacks[i]);
  Exit(nil);
end;

function createModpack(Json : ISuperObject) : TModpack;
begin
  try
    Exit(TModPack.Create(Json));
  except
    on E : Exception do
      Exit(nil);
  end;
end;

constructor TCustomFileDownload.Create(Instance : TModpackInstance; CustomFiles : TList<TCustomFile>; Command : TMinecraftLaunch);
var
i : Integer;
begin
  inherited Create('Download Custom Files', Command, True);
  Self.Instance := Instance;
  Self.CustomFiles := TList<TCustomFile>.Create;
  for i := 0 to Self.CustomFiles.Count-1 do
    if Self.CustomFiles[i].SideType.isCompatible(Instance.Side) then
      Self.CustomFiles.Add(CustomFiles[i]);
end;

procedure TCustomFileDownload.runTask(Bar : TCMDProgressBar);
var
Downloader : TDownloaderF;
i: Integer;
ModsFolder : String;
DResult : TDownloadR;
begin
  ModsFolder := Instance.getInstanceModsFolder;
  Downloader := TDownloaderF.Create(nil);
  Downloader.Show;
  Downloader.DownloadBar.StartProcess(CustomFiles.Count);
  Bar.StartStep(CustomFiles.Count);

  for i := 0 to CustomFiles.Count-1 do
  begin
    Self.Log.log('Searching ' + CustomFiles[i].IFileName);
    if FileExists(ModsFolder + CustomFiles[i].IFileName) then
    begin
      Self.Log.logLastLine('Downloading ' + CustomFiles[i].IFileName);
      Downloader.lblProgress.Caption := IntToStr(Bar.StepPos+1) + '/' + IntToStr(CustomFiles.Count) + ' Files';
      DResult := Downloader.downloadItem(TDownloadItem.Create(CustomFiles[i].URL, CustomFiles[i].FileName));
      if DResult = drCancel then
      begin
        if Downloader.Progress <> nil then
          Downloader.Progress.Destroy;
        Downloader.Destroy;
        Self.Log.log('Canceled custom files download ');
        Exit;
      end;
      if DResult = drFail then
      begin
        if Downloader.Progress <> nil then
          Downloader.Progress.Destroy;
        Self.Log.logLastLine('Failed to download custom file! ' + CustomFiles[i].IFileName);
      end;

      if DResult = drSuccess then
      begin
        Downloader.Progress.lblTask.Caption := 'Installing Custom File';
        Application.ProcessMessages;
        RenameFile(TempFolder + CustomFiles[i].FileName, ModsFolder + CustomFiles[i].IFileName);
        Downloader.Progress.Destroy;
        Self.Log.logLastLine('Downloaded ' + CustomFiles[i].IFileName);
      end;
    end;
    Downloader.DownloadBar.FinishStep;
    Bar.StepPos := Bar.StepPos + 1;
  end;
end;

procedure TModPackInstance.Load(SaveFile : TSaveFile);
begin
  inherited Load(SaveFile);
end;

procedure TModPackInstance.Save(SaveFile : TSaveFile);
var
Item : TPair<TMod, TModVersion>;
Value : TStringList;
begin
  SaveFile.setInteger('modpack', Modpack.Key.ID);
  if not SaveFile.hasKey('modpackv') or (SaveFile.getInteger('modpackv') <> -1) then
    SaveFile.setInteger('modpackv', Modpack.Value.ID);
  Value := TStringList.Create;
  for Item in Mods do
    Value.Add(InttoStr(Item.Key.ID) + ':' + InttoStr(Item.Value.ID));
  SaveFile.setStringList('mods', Value);
end;

procedure TModPackInstance.LoadPost(SaveFile : TSaveFile);
var
i: Integer;
Item : TPair<TMod, TModVersion>;
Splits, Value : TStringList;
begin
  Modpack.Key := getModPackByID(SaveFile.getInteger('modpack'));
  if Modpack.Key <> nil then
    if SaveFile.hasKey('modpackv') then
      Modpack.Value := Modpack.Key.getVersionByID(SaveFile.getInteger('modpackv'));
  if Modpack.Value = nil then
    Modpack.Value := Modpack.Key.getNewestVersion;

  if Modpack.Value <> nil then
    Forge := ForgeUtils.getForgeByUUID(Modpack.Value.Forge);

  Mods := TDictionary<TMod, TModVersion>.Create;
  Value := SaveFile.getStringList('mods');
  for i := 0 to Value.Count-1 do
  begin
    Splits := Explode(Value[i], ':');
    if Splits.Count = 2 then
    begin
      try
        Item.Key := ModUtils.getModByID(StrtoInt(Splits[0]));
      except
        on E: Exception do
        begin
          Logger.MainLog.log('Failed to load a mod from data! data=' + Value[i]);
        end;
      end;
      if (Item.Key <> nil) and (Item.Key.hasLoaded) then
      begin
        try
          Item.Value := Item.Key.getVersionByID(StrToInt(Splits[1]));
        except
          on E: Exception do
            Logger.MainLog.log('Failed to load mod version: ' + Splits[1]);
        end;
        if (Item.Value <> nil) and (Item.Key.Optional) and (not Modpack.Value.Mods.ContainsKey(Item.Key)) then
          Mods.Add(Item.Key, Item.Value);
      end;
    end;
  end;
end;

function TModPackInstance.getUUID : String;
begin
  Result := 'Modpack';
end;

function TModPackInstance.getSettings : TList<TSetting>;
var
ModSelect : TModSelect;
ModpackSelect : TModpackSelect;
begin
  Result := TList<TSetting>.Create;
  ModpackSelect := TModpackSelect.Create('modpack', 'Modpack');
  Result.Add(ModpackSelect);

  ModSelect := TModSelect.Create('mods', 'Mods', True, Side = TServer, nil);
  ModSelect.CustomNotification := ModpackSelect.getMCVersion;
  Result.Add(ModSelect);
end;

function TModPackInstance.getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>;
var
  i,j: Integer;
  Item : TPair<TMod, TModVersion>;
  Added : Boolean;
begin
  Added := False;
  Result := inherited getStartupTasks(MinecraftComand);
  i := 0;
  while i < Result.Count do
  begin
    if Result[i] is TDownloadMods then
    begin
      if not Added then
      begin
        Result.Insert(i, TDownloadModpackArchive.Create(Self, MinecraftComand));
        Result.Insert(i, TCustomFileDownload.Create(Self, Modpack.Value.CustomFiles, MinecraftComand));
      end;

      for Item in Modpack.Value.Mods do
      begin
        if TDownloadMods(Result[i]).Mods.ContainsKey(Item.Key) then
          TDownloadMods(Result[i]).Mods.Remove(Item.Key);
        TDownloadMods(Result[i]).addMod(Item);
      end;
    end;
    if Result[i] is TModCleaning then
    begin
      Added := True;
      for Item in Modpack.Value.Mods do
      begin
        if TModCleaning(Result[i]).Mods.ContainsKey(Item.Key) then
          TModCleaning(Result[i]).Mods.Remove(Item.Key);
        TModCleaning(Result[i]).addMod(Item);
      end;

      for j := 0 to Modpack.Value.CustomFiles.Count-1 do
        TModCleaning(Result[i]).Exclude.Add(Modpack.Value.CustomFiles[j].IFileName);

      Result.Insert(i, TDownloadModpackArchive.Create(Self, MinecraftComand));
      i := i + 1;
    end;
    i := i + 1;
  end;
end;

function TModPackInstance.getCustomPopUpMenu : TDictionary<string, String>;
begin
  Result := TDictionary<string, String>.Create;
  Result.Add('convert', 'Convert to Mod Instance');
end;

procedure TModPackInstance.onCustomMenuClicked(Item : string);
var
ModInstance : TForgeInstance;
Key : TMod;
Tasks : TList<TTask>;
begin
  if Item = 'convert' then
  begin
    if MessageDlg('Do you really want to convert this instance? ' + Self.Title,mtCustom, [mbYes,mbNo], 0) = mrYes then
    begin
      getSaveFile.setString('forge', Modpack.Value.Forge);
      ModInstance := TForgeInstance.Create(Self.Title, True);
      ModInstance.LoadPost(ModInstance.getSaveFile);
      for Key in Modpack.Value.Mods.Keys do
        if not ModInstance.getMods.ContainsKey(Key) then
          ModInstance.getMods.Add(Key, Modpack.Value.Mods.Items[Key]);
      if Modpack.Value.CustomFiles.Count > 0 then
        ModInstance.Custom := True;
      ModInstance.SaveInstance;
      //Self.Destroy;
      Tasks := TList<TTask>.Create;
      Tasks.Add(TLoadInstance.Create);
      Tasks.Add(TFinishLoadInstance.Create);
      OverviewF.runForegroundTasks(Tasks);
    end;
  end;
end;

{function TModPackInstance.getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch;
begin

end;}

constructor TModpackVersion.Create(Json : ISuperObject);
var
InstallArray : TSuperArray;
i : Integer;
Item : TPair<TMod, TModVersion>;
begin
  FID := Json.I['id'];
  FArchiveURL := Json.S['archive'];
  FName := Json.S['name'];
  FForge := Json.S['forge'];

  FCustomFiles := TList<TCustomFile>.Create;
  InstallArray := Json.A['custom_files'];
  if InstallArray <> nil then
  begin
    for i := 0 to InstallArray.Length-1 do
    begin
      CustomFiles.Add(TCustomFile.Create(InstallArray.O[i]));
    end;
  end;

  InstallArray := Json.A['mods'];
  FMods := TDictionary<TMod, TModVersion>.Create;
  if InstallArray <> nil then
  begin
    for i := 0 to InstallArray.Length-1 do
    begin
      Item.Key := ModUtils.getModByID(StrToInt(InstallArray.O[i].S['id']));
      if Item.Key <> nil then
      begin
        Item.Value := Item.Key.getVersionByID(StrtoInt(InstallArray.O[i].S['vid']));
      end
      else
        Logger.MainLog.log('Failed to load a mod in modpack!');
      if (Item.Key <> nil) and (Item.Value <> nil) then
        FMods.Add(Item.Key, Item.Value);
    end;

  end;
end;

function TModpackVersion.hasArchive : Boolean;
begin
  Result := ArchiveURL <> '';
end;

constructor TCustomFile.Create(Json : ISuperObject);
begin
  Self.FURL := Json.S['url'].Replace(quoteString, '''');
  Self.FFileName := Json.S['filename'].Replace(quoteString, '''');
  Self.FSideType := parseSideType(Json.S['type']);
  Self.FIFileName := Json.S['ifilename'].Replace(quoteString, '''');
  if Self.IFileName = '' then
    Self.FIFileName := Self.FFileName;
end;

constructor TModpack.Create(Json : ISuperObject);
begin
  FName := Json.S['title'];
  FID := Json.I['id'];
  FLoaded := False;
end;

function TModpack.getVersionByName(Name : String) : TModpackVersion;
var
  i: Integer;
begin
  for i := 0 to Versions.Count-1 do
    if Versions[i].Name = Name then
      Exit(Versions[i]);
  Exit(nil);
end;

function TModpack.getVersionByID(ID : Integer) : TModpackVersion;
var
  i: Integer;
begin
  for i := 0 to Versions.Count-1 do
    if Versions[i].ID = ID then
      Exit(Versions[i]);
  Exit(nil);
end;

function TModpack.getNewestVersion : TModpackVersion;
begin
  Result := nil;
  if Versions.Count > 0 then
    Result := Versions[0];
end;

procedure TModpack.loadModPack(Json : ISuperObject);
var
VArray : TSuperArray;
i: Integer;
ModPackVersion : TModpackVersion;
begin
  Versions := TList<TModpackVersion>.Create;
  VArray := Json.A['versions'];
  for i := 0 to VArray.Length-1 do
  begin
    ModPackVersion := TModpackVersion.Create(VArray[i]);
    if ModPackVersion <> nil then
      Versions.Add(ModPackVersion);
  end;
end;

procedure TFullLoadModpack.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
JsonFile : ISuperObject;
ModPackArray : TSuperArray;
i: Integer;
TempModpack : TModpack;
FileString : TStringList;
FileJsonData : WideString;
begin
  FileName := DownloadFolder + 'modpack.json';
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/modpackservice.php?type=full',FileName, True);
  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(Bar);
  end;
  if FileExists(FileName) then
  begin
    FileString := TStringList.Create;
    FileString.LoadFromFile(FileName);
    FileJsonData := '';
    for i := 0 to FileString.Count-1 do
      FileJsonData := FileJsonData + Trim(FileString[i]).Replace(#$FEFF, '');
    JsonFile := TSuperObject.ParseString(PWideChar(FileJsonData), False);
    ModPackArray := JsonFile.AsArray;
    for i := 0 to ModPackArray.Length-1 do
    begin
      TempModpack := getModPackByID(ModPackArray.O[i].I['id']);
      if TempModpack <> nil then
        TempModpack.loadModpack(ModPackArray.O[i])
      else
        Self.Log.log('Failed to load "' + ModPackArray.O[i].S['title'] + '"');
    end;
  end;
  ModPacksLoaded := True;
  Self.Log.log('Loaded ' + InttoStr(ModPacks.Count) + ' modpacks');
end;

constructor TDownloadModpackArchive.Create(Instance : TModpackInstance; Command : TMinecraftLaunch);
begin
  inherited Create('Download Modpack Archive', Command, True);
  Self.sync := True;
  Self.Instance := Instance;
end;

procedure TDownloadModpackArchive.runTask(Bar : TCMDProgressBar);
var
//DownloadTask : TDownloadTask;
Extractor : TExtractZip;
FileName, SaveFileName : String;
InstalledArchive : TSaveFile;
ShouldInstall : Boolean;
Downloader : TDownloaderF;
downloadItem : TDownloadItem;
begin
  if Instance.Modpack.Value.hasArchive then
  begin
    ShouldInstall := True;
    SaveFileName := Instance.getInstanceFolder + 'lastinstalled.cfg';
    InstalledArchive := TSaveFile.Create(SaveFileName);

    if InstalledArchive.getInteger('last-installed') = Instance.Modpack.Value.ID then
      ShouldInstall := False;


    if ShouldInstall then
    begin
      FileName := TempFolder + 'modpack-archive.zip';
      {DownloadTask := TDownloadTask.Create(Instance.Modpack.Value.ArchiveURL, FileName);
      DownloadTask.setLog(Self.Log);
      if DownloadTask.downloadFile(bar) then
      begin
        Extractor := TExtractZip.Create(FileName, Instance.getInstanceFolder);
        Extractor.setLog(log);
        Extractor.runTask(nil);
        InstalledArchive.setInteger('last-installed', Instance.Modpack.Value.ID);
      end;}
      Downloader := TDownloaderF.Create(nil);
      Downloader.Show;
      downloadItem := TDownloadItem.Create('http://launcher.creativemd.de/service/downloadservice.php?id=' + IntToStr(Instance.Modpack.Key.ID) + '&versionID=' + IntToStr(Instance.Modpack.Value.ID) + '&cat=modpack&url=' + Instance.Modpack.Value.ArchiveURL, 'modpack-archive.zip', True);

      //downloadItem := TDownloadItem.Create(ResourcePackVersion.URL, ResourcePackVersion.FileName);
      if Downloader.downloadItem(downloadItem) = drSuccess then
      begin
        Extractor := TExtractZip.Create(FileName, Instance.getInstanceFolder);
        Extractor.setLog(Self.log);
        Extractor.runTask(nil);
        InstalledArchive.setInteger('last-installed', Instance.Modpack.Value.ID);

      end;
      try
        Downloader.Progress.Destroy;
      except
        on E : Exception do
      end;
      DeleteFile(PWideChar(FileName));
      //Downloader.DestroyComponents;
      //Downloader.Close;
      Downloader.Destroy;

    end;
  end;
  Bar.FinishStep;
end;

constructor TLoadModpack.Create;
begin
  inherited Create('Loading Modpacks', False, False);
end;

procedure TLoadModpack.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
JsonFile : ISuperObject;
ModArray : TSuperArray;
i: Integer;
TempMod : TModpack;
FileString : TStringList;
FileJsonData : WideString;
begin
  ModPacksLoaded := False;
  Modpacks := TList<TModpack>.Create;
  FileName := DownloadFolder + 'modpacklite.json';
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/modpackservice.php',FileName, True);
  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
  end;
  if FileExists(FileName) then
  begin
    FileString := TStringList.Create;
    FileString.LoadFromFile(FileName);
    FileJsonData := '';
    for i := 0 to FileString.Count-1 do
      FileJsonData := FileJsonData + Trim(FileString[i]).Replace(#$FEFF, '');
    JsonFile := TSuperObject.ParseString(PWideChar(FileJsonData), False);
    //JsonFile := TSuperObject.ParseFile(FileName, true);
    ModArray := JsonFile.AsArray;
    for i := 0 to ModArray.Length-1 do
    begin
      TempMod := createModpack(ModArray.O[i]);

      if TempMod <> nil then
        ModPacks.Add(TempMod)
      else
        Self.Log.log('Failed to load modpack!');
    end;
  end;
  Self.Log.log('Pre-Loaded ' + InttoStr(ModPacks.Count) + ' modpacks');
end;

end.
