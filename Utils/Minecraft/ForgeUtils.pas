unit ForgeUtils;

interface

uses VanillaUtils, Task, ProgressBar, InstanceUtils, SaveFileUtils, System.Generics.Collections,
MinecraftLaunchCommand, SettingUtils, JavaUtils, System.Classes, AccountUtils,
System.SysUtils, superobject, Vcl.Controls, Vcl.StdCtrls, AssetUtils,
MinecraftLibaryUtills, ModUtils, System.UITypes, SideUtils, System.IOUtils, Vcl.Forms, System.Types, Winapi.Windows;

type
  TForge = class
    private
      FUUID, FMV, FBranch : string;
    public
      constructor Create(UUID, MV : String; Branch : String = '');
      function getMinecraftVersion : TMinecraftVersion;
      property UUID : string read FUUID;
      property MV : string read FMV;
      property Branch : string read FBranch;
      function getVersionID : Integer;
      function getJarFileName : String;
      function getJsonFileName : String;
  end;
  TLoadForge = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;

  TForgeSelect = class(TSelectSetting)
    protected
      NotifySettings : TList;
      procedure onChanged(Sender: TObject); override;
      procedure onChangedVersion(Sender: TObject);
      procedure update;
    public
      constructor Create(Name, Title : String);
      procedure addNotifySetting(Setting : TObject);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getMCVersion : String; virtual;
  end;
  IForgeUpdateSetting = interface
  ['{A1F42682-5ED0-4B17-BE93-C3604473A9C3}']
    procedure onForgeChanges(ForgeSelect : TForgeSelect);
  end;
  TForgeInstance = class(TInstance)
    protected
      FCustom : Boolean;
      Mods : TDictionary<TMod, TModVersion>;
      procedure Load(SaveFile : TSaveFile); override;
      procedure Save(SaveFile : TSaveFile); override;
    public
      Forge : TForge;
      function getMods : TDictionary<TMod, TModVersion>;
      procedure LoadPost(SaveFile : TSaveFile); override;
      function getUUID : String; override;
      function getSettings : TList<TSetting>; override;
      function getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>; override;
      function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
      function getLaunchSettings : TList<TSetting>; override;
      function getLaunchSaveFile : TSaveFile; override;
      function canInstanceLaunch : Boolean; override;
      function getMinecraftVersion : string; override;
      property Custom : Boolean read FCustom write FCustom;
  end;
  TDownloadMods = class(TTask)
    protected
      ModsFolder : String;
      Side : TSide;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      Mods : TDictionary<TMod, TModVersion>;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; Side : TSide);
      procedure addMod(PMod : TPair<TMod, TModVersion>);
  end;
  TModCleaning = class(TTask)
    protected
      ModsFolders : TStringList;
      Side : TSide;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      Mods : TDictionary<TMod, TModVersion>;
      Exclude : TStringList;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolders : TStringList; Side : TSide); overload;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; Side : TSide); overload;
      procedure addMod(PMod : TPair<TMod, TModVersion>);
  end;

function getForgeByUUID(UUID : string) : TForge;
function getForgeByBuildID(BuildID : string) : TForge;

var
ForgeList : TList<TForge>;
SupportedMV : TStringList;

implementation

uses ServerUtils, DownloadUtils, CoreLoader, DatabaseConnection, StringUtils,
ForgeInstallation, SortingUtils, ModSettings, Logger, FileDownload;

function getForgeByBuildID(BuildID : string) : TForge;
var
  i: Integer;
  Data : TStringList;
begin
  if ForgeList = nil then
    Exit(nil);

  for i := 0 to ForgeList.Count-1 do
  begin
    Data := Explode(ForgeList[i].UUID, '.');
    if (Data.Count > 0) and (Data[Data.Count-1] = BuildID) then
      Exit(ForgeList[i]);
  end;

  Exit(nil);
end;

function getForgeByUUID(UUID : string) : TForge;
var
  i: Integer;
begin
  if ForgeList = nil then
    Exit(nil);

  for i := 0 to ForgeList.Count-1 do
    if ForgeList[i].FUUID = UUID then
      Exit(ForgeList[i]);

  Exit(nil);
end;

constructor TForgeSelect.Create(Name, Title : String);
begin
  inherited Create(Name, Title, SupportedMV, '', SupportedMV.Count = 0);
  NotifySettings := TList.Create;
end;

procedure TForgeSelect.addNotifySetting(Setting : TObject);
begin
  NotifySettings.Add(Setting);
end;

procedure TForgeSelect.createControl(x, y : Integer; Parent : TWinControl);
var
ComboBox : TComboBox;
Forge : TForge;
begin
  inherited createControl(x, y, Parent);
  ComboBox := TComboBox.Create(Parent);
  ComboBox.Parent := Parent;
  ComboBox.Top := y;
  ComboBox.Left := Controls[0].Left + Controls[0].Width + 5;
  ComboBox.Style := csOwnerDrawFixed;
  ComboBox.OnChange := onChangedVersion;
  Controls.Add(ComboBox);

  Forge := getForgeByUUID(Value);
  if Forge <> nil then
  begin
    TComboBox(Controls[0]).ItemIndex := Items.IndexOf(Forge.MV);
    onChanged(Controls[0]);
    ComboBox.ItemIndex := ComboBox.Items.IndexOf(Value);
  end
  else
    onChanged(Controls[0]);
end;

function TForgeSelect.getMCVersion : String;
var
TempForge : TForge;
begin
  TempForge := ForgeUtils.getForgeByUUID(Value);
  Result := '';
  if TempForge <> nil then
    Result := TempForge.MV;
end;

procedure TForgeSelect.destroyControl;
begin
  if Controls[1] is TComboBox then
    Value := TComboBox(Controls[1]).Text
end;

procedure TForgeSelect.onChanged(Sender : TObject);
var
mcversion : string;
i : Integer;
Strings : TStringList;

begin
  inherited onChanged(Sender);
  if Controls.Count = 2 then
  begin
    mcversion := TComboBox(Controls[0]).Text;
    TComboBox(Controls[1]).Clear;
    Strings := TStringList.Create;
    if mcversion <> '' then
      for i := 0 to ForgeList.Count-1 do
        if ForgeList[i].MV = mcversion then
          Strings.Add(ForgeList[i].UUID);
    Strings.Sort;
    for i := 0 to Strings.Count-1 do
      TComboBox(Controls[1]).Items.Add(Strings[Strings.Count-1-i]);

    update;
  end;
end;

procedure TForgeSelect.update;
var
i : Integer;
ForgeUpdate : IForgeUpdateSetting;
begin
  for i := 0 to NotifySettings.Count-1 do
      if System.SysUtils.Supports(NotifySettings[i],IForgeUpdateSetting,ForgeUpdate) then
        ForgeUpdate.onForgeChanges(Self);
end;

procedure TForgeSelect.onChangedVersion(Sender: TObject);
begin
  Value := TComboBox(Sender).Text;
  update;
end;

constructor TForge.Create(UUID, MV : String; Branch : String = '');
begin
  Self.FUUID := UUID;
  Self.FMV := MV;
  Self.FBranch := Branch;
end;

function TForge.getVersionID : Integer;
begin
  Result := StrToInt(getLastPiece(UUID, '.'));
end;

function TForge.getMinecraftVersion : TMinecraftVersion;
begin
  Result := VanillaUtils.getMinecraftVersion(mv);
end;

function TForge.getJarFileName : String;
begin
  Result := DownloadFolder + 'versions\' + MV + '-Forge' + UUID + '\' + MV + '-Forge' + UUID + '.jar';
end;

function TForge.getJsonFileName : String;
begin
  Result := DownloadFolder + 'versions\' + MV + '-Forge' + UUID + '\' + MV + '-Forge' + UUID + '.json';
end;

constructor TLoadForge.Create;
begin
  inherited Create('Loading Forge', False, False);
end;

procedure TLoadForge.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
FileName, TempFileName, MC , Branch: string;
JsonFile : ISuperObject;
Versions, Version : TSuperArray;
i : Integer;
begin
  FileName := DownloadFolder + 'forgeversions.json';
  TempFileName := DownloadFolder + 'forgeversionsTemp.json';
  ForgeList := TList<TForge>.Create;
  DownloadTask := TDownloadTask.Create('https://launcher.creativemd.de/service/forgeservice.php?only=new',
  TempFileName, True);
  SupportedMV := TStringList.Create;

  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
    if FileExists(TempFileName) then
    begin
      if FileExists(FileName) then
        System.SysUtils.DeleteFile(FileName);
      RenameFile(TempFileName, FileName);
    end;
  end;

  if FileExists(FileName) and (VanillaUtils.MinecraftVersions <> nil) then
  begin
    JsonFile := TSuperObject.ParseFile(FileName, true);
    Versions := JsonFile.A['versions'];
    Bar.StartStep(Versions.Length);
    for i := 0 to Versions.Length-1 do
    begin
      Version := Versions.O[i].AsArray;
      MC := Version.S[1];
      if VanillaUtils.getMinecraftVersion(MC) <> nil then
      begin
        Branch := '';
        if Version.Length > 2 then
          Branch := Version.S[2];
        ForgeList.Add(TForge.Create(Version.S[0], MC, Branch));
        if SupportedMV.IndexOf(MC) = -1 then
          SupportedMV.Add(MC);
      end;
      Bar.StepPos := i;
    end;

  end
  else
    ForgeList := nil;
  SupportedMV.CustomSort(SortVersions);
  Bar.FinishStep;
end;

procedure TForgeInstance.LoadPost(SaveFile : TSaveFile);
var
i: Integer;
Item : TPair<TMod, TModVersion>;
Splits, Value : TStringList;
begin
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
        if Item.Value <> nil then
          Mods.Add(Item.Key, Item.Value);
      end;
    end;
  end;
end;

function TForgeInstance.getMods : TDictionary<TMod, TModVersion>;
begin
  Result := Mods;
end;

procedure TForgeInstance.Load(SaveFile : TSaveFile);
begin
  Forge := nil;
  if ForgeList <> nil then
    Forge := getForgeByUUID(SaveFile.getString('forge'));
  FCustom := SaveFile.getBoolean('custom');
end;

procedure TForgeInstance.Save(SaveFile : TSaveFile);
var
Item : TPair<TMod, TModVersion>;
Value : TStringList;
begin
  SaveFile.setString('forge', Forge.UUID);
  SaveFile.setBoolean('custom', FCustom);
  Value := TStringList.Create;
  for Item in Mods do
    Value.Add(InttoStr(Item.Key.ID) + ':' + InttoStr(Item.Value.ID));
  SaveFile.setStringList('mods', Value);
end;

function TForgeInstance.getUUID : String;
begin
  Result := 'Forge';
end;

function TForgeInstance.getSettings : TList<TSetting>;
var
ForgeSelect : TForgeSelect;
EnhancedModSelect : TEnhancedModSelect;
begin
  Result := TList<TSetting>.Create;
  ForgeSelect := TForgeSelect.Create('forge', 'Forge');
  Result.Add(ForgeSelect);
  Result.Add(TCheckOption.Create('custom', 'Allow Custom Mods', False));
  EnhancedModSelect := TEnhancedModSelect.Create('mods', 'Mods', false, Side = TServer, ForgeSelect);
  Result.Add(EnhancedModSelect);
  ForgeSelect.addNotifySetting(EnhancedModSelect);
end;

function TForgeInstance.getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>;
var
DownloadLibary : TDownloadFLibary;
DownloadAssets : TLoadAssets;
begin
  Result := TList<TTask>.Create;
  if Forge = nil then
    Exit(Result);
  if Side = TClient then
  begin
    if DatabaseConnection.online then
    begin
      Result.Add(TInstallForge.Create(TForgeLaunch(MinecraftComand)));
    end;
    DownloadAssets := TLoadAssets.Create(MinecraftComand, Forge.getMinecraftVersion.LaunchTyp);
    DownloadAssets.CustomJsonPath := Forge.getJsonFileName;
    Result.Add(DownloadAssets);
    DownloadLibary := TDownloadFLibary.Create(MinecraftComand);
    DownloadLibary.CustomJsonPath := Forge.getJsonFileName;
    Result.Add(DownloadLibary);
  end
  else
  begin
    Result.Add(TInstallForge.Create(TForgeLaunch(MinecraftComand)));
    Result.Add(TInstallServerForge.Create(getInstanceFolder, TForgeLaunch(MinecraftComand)));
    DownloadLibary := TDownloadFLibary.Create(MinecraftComand);
    DownloadLibary.CustomJsonPath := Forge.getJsonFileName;
    DownloadLibary.CustomLibaryFolder := getInstanceFolder + 'libraries\';
    DownloadLibary.IsServer := True;
    Result.Add(DownloadLibary);
  end;
  if not FCustom then
    Result.Add(TModCleaning.Create(Mods, getInstanceFolder + 'mods\', Side));

  Result.Add(TDownloadMods.Create(Mods, getInstanceFolder + 'mods\', Side));
end;

function TForgeInstance.getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch;
begin
  Result := nil;
  if Forge <> nil then
    Result := TForgeLaunch.Create(Java, Forge, Self, LoginData);
end;

function TForgeInstance.getLaunchSettings : TList<TSetting>;
begin
  Result := TList<TSetting>.Create;
  if Side = TServer then
  begin
    Result.AddRange(ServerUtils.getStandardServerSettings(Self));
  end;
end;

function TForgeInstance.canInstanceLaunch : Boolean;
begin
  Result := LoadedInstances;
end;

function TForgeInstance.getMinecraftVersion;
begin
  Result := Forge.MV;
end;

function TForgeInstance.getLaunchSaveFile : TSaveFile;
begin
  Result := TSaveFile.Create(getInstanceFolder + 'server.properties');
end;

constructor TModCleaning.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolders : TStringList;  Side : TSide);
var
Item : TPair<TMod, TModVersion>;
begin
  inherited Create('Clean Mods', True);
  Self.Exclude := TStringList.Create;
  Self.ModsFolders := ModsFolders;
  Self.Side := Side;
  Self.Mods := TDictionary<TMod, TModVersion>.Create;
  for Item in Mods do
  begin
    addMod(Item);
  end;
end;

procedure TModCleaning.addMod(PMod : TPair<TMod, TModVersion>);
begin
  if PMod.Key.ModType.isCompatible(Side) and not Self.Mods.ContainsKey(PMod.Key) then
      Self.Mods.Add(PMod.Key, PMod.Value);
end;

constructor TModCleaning.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String;  Side : TSide);
begin
  Self.ModsFolders := TStringList.Create;
  Self.ModsFolders.Add(ModsFolder);
  Create(Mods, Self.ModsFolders, Side);
end;

procedure TModCleaning.runTask(Bar : TCMDProgressBar);
var
Files : TStringList;
i, j: Integer;
Item : TPair<TMod, TModVersion>;
isFileOfMod : Boolean;
begin
  for i := 0 to ModsFolders.Count-1 do
  begin
    if DirectoryExists(ModsFolders[i]) then
    begin
      Files := ArrayToList(TDirectory.GetFiles(ModsFolders[i]));

      Bar.StartStep(Files.Count);
      for j := 0 to Files.Count-1 do
      begin
        isFileOfMod := False;

        for Item in Mods do
          if Item.Value.isModFile(Files[j].Replace(ModsFolders[i], '').Replace('\', '/'), Side) then
          begin
            isFileOfMod := True;
            Break;
          end;

        if not isFileOfMod and not Exclude.Contains(Files[j].Replace(ModsFolders[i], '').Replace('\', '/')) then
        begin
          System.SysUtils.DeleteFile(Files[j]);
          Self.Log.log('Deleted ' + Files[j].Replace(ModsFolders[i], '').Replace('\', '/'));
        end;

        Bar.StepPos := j;
      end;
    end;
  end;
  Bar.FinishStep;
end;

procedure TDownloadMods.addMod(PMod : TPair<TMod, TModVersion>);
begin
  if PMod.Key.ModType.isCompatible(Side) and not Self.Mods.ContainsKey(PMod.Key) then
      Self.Mods.Add(PMod.Key, PMod.Value);
end;

constructor TDownloadMods.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; Side : TSide);
var
Item : TPair<TMod, TModVersion>;
begin
  inherited Create('Downloading Mods', True, False);
  Self.ModsFolder := ModsFolder;
  Self.sync := True;
  Self.Side := Side;
  Self.Mods := TDictionary<TMod, TModVersion>.Create;
  for Item in Mods do
  begin
    addMod(Item);
  end;

end;

procedure TDownloadMods.runTask(Bar : TCMDProgressBar);
var
Downloader : TDownloaderF;
Item : TPair<TMod, TModVersion>;
DResult : TDownloadR;
i : Integer;
NeedInstallation, IsModValid, IsModInstalled : Boolean;
  j: Integer;
begin
  NeedInstallation := False;
  for Item in Mods do
  begin
    if not Item.Value.isInstalled(ModsFolder, Side) then
    begin
      NeedInstallation := True;
    end;
  end;

  if NeedInstallation then
  begin
    Downloader := TDownloaderF.Create(nil);
    //Downloader.chrmDownloader.
    Downloader.Show;
    Downloader.DownloadBar.StartProcess(Mods.Count);
    Bar.StartStep(Mods.Count);
    for Item in Mods do
    begin
      IsModValid := True;

      if IsModValid and not Item.Value.isInstalled(ModsFolder, Side) then
      begin
        for i := 0 to Item.Value.Files.Count-1 do
        begin
          if not Item.Value.Files[i].isInstalled(ModsFolder, Side) and Item.Value.Files[i].SideType.isCompatible(Side) then
          begin
            IsModInstalled := False;

            //SEARCH FOR ALREADY DOWNLOADED MODS
            Self.Log.log('Searching ' + Item.Key.Title);
            for j := 0 to Instances.Count-1 do
              if (Instances[j].getInstanceModsFolder <> Self.ModsFolder) and Item.Value.Files[i].isInstalled(Instances[j].getInstanceModsFolder, Side) then
              begin
                Item.Value.Files[i].copyObj(Instances[j].getInstanceModsFolder, ModsFolder, Side);
                Self.Log.logLastLine('Found ' + Item.Key.Title);
                IsModInstalled := True;
                break;
              end;

            if not IsModInstalled then
            begin
              Self.Log.logLastLine('Downloading ' + Item.Key.Title);
              Downloader.lblProgress.Caption := IntToStr(Bar.StepPos+1) + '/' + IntToStr(Mods.Count) + ' Mods';
              DResult := Downloader.downloadItem(TDownloadItem.Create('http://launcher.creativemd.de/service/downloadservice.php?id=' + IntToStr(Item.Key.ID) + '&versionID=' + IntToStr(Item.Value.ID) + '&cat=mod&url=' + Item.Value.Files[i].DownloadLink, Item.Value.Files[i].DFileName));
              if DResult = drCancel then
              begin
                if Downloader.Progress <> nil then
                  Downloader.Progress.Destroy;
                Downloader.Destroy;
                Self.Log.log('Canceled mod download ');
                Exit;
              end;
              if DResult = drFail then
              begin
                if Downloader.Progress <> nil then
                  Downloader.Progress.Destroy;
                Self.Log.logLastLine('Failed to download mod! ' + Item.Key.Title);
              end;

              if DResult = drSuccess then
              begin
                Downloader.Progress.lblTask.Caption := 'Installing Mod';
                Application.ProcessMessages;
                Item.Value.Files[i].installObj(TempFolder, ModsFolder, Side);
                Downloader.Progress.Destroy;
                Self.Log.logLastLine('Downloaded ' + Item.Key.Title);
              end;
            end;
          end;
        end;

      end;
      Downloader.DownloadBar.FinishStep;
      Bar.StepPos := Bar.StepPos + 1;
    end;
    Downloader.Destroy;
  end;
  Bar.FinishStep;

end;

end.
