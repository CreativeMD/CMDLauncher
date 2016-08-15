unit SpongeForge;

interface

uses ForgeUtils, Task, ProgressBar, System.Generics.Collections, superobject, superxmlparser, VanillaUtils, System.Classes, DownloadUtils,
System.SysUtils, StringUtils, SettingUtils, MinecraftLaunchCommand, ForgeInstallation, LaunchTaskUtils, ZipUtils, SaveFileUtils,
Vcl.StdCtrls, Vcl.Controls, JavaUtils, AccountUtils, FileDownload, SideUtils, InstanceUtils, BuildUtils, ModpackUtils;

type
TSpongeForge = class
  private
    FVersion, FMC, FURL, FFile : String;
    FForge : TForge;
    BuildType : TBuildType;
  public
    constructor Create(Input : String);
    property MC : String read FMC;
    property URL : String read FURL;
    property ServerFile : String read FFile;
    property Forge : TForge read FForge;
    property Version : String read FVersion;
    function getMinecraftVersion : TMinecraftVersion;
end;
TSpongeForgeInstance = class(TForgeInstance)
  protected
    procedure Load(SaveFile : TSaveFile); override;
    procedure Save(SaveFile : TSaveFile); override;
  public
    SpongeForge : TSpongeForge;
    function getUUID : String; override;
    function getSettings : TList<TSetting>; override;
    function getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>; override;
    function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
end;
TSpongeModpackInstance = class(TModPackInstance)
  protected

  public
    SpongeForge : TSpongeForge;
    function getUUID : String; override;
    function getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>; override;
    function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
    procedure LoadPost(SaveFile : TSaveFile); override;
end;
TLoadSpongeForge = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;
TSpongeForgeSelect = class(TForgeSelect)
  protected
      procedure onChanged(Sender: TObject); override;
  public
    constructor Create(Name, Title : String);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    function getMCVersion : String; override;
end;
TInstallServerSpongeForge = class(TLaunchTask)
  SpongeForge : TSpongeForge;
  Instance : TInstance;
  procedure runTask(Bar : TCMDProgressBar); override;
  constructor Create(Command : TMinecraftLaunch; SpongeForge : TSpongeForge; Instance : TInstance);
end;

function getSpongeForgeByVersion(Version : string) : TSpongeForge;

var
SpongeForgeList : TList<TSpongeForge>;
SpongeForgeMCList : TStringList;
implementation

uses CoreLoader, DatabaseConnection, ModSettings, ConsoleServer, Console, MinecraftLibaryUtills, Cauldron;

function getSpongeForgeByVersion(Version : string) : TSpongeForge;
var
i : Integer;
begin
  for I := 0 to SpongeForgeList.Count-1 do
    if SpongeForgeList[i].Version = Version then
      Exit(SpongeForgeList[i]);
  Exit(nil);
end;

constructor TSpongeForgeSelect.Create(Name, Title : String);
begin
  inherited Create(Name, Title);
  Self.Items := SpongeForgeMCList;
  Self.Online := SpongeForgeMCList.Count = 0;
end;

procedure TSpongeForgeSelect.createControl(x, y : Integer; Parent : TWinControl);
var
SpongeForge : TSpongeForge;
  i: Integer;
begin
  inherited createControl(x, y, Parent);

  for i := 0 to Controls.Count-1 do
    if Controls[i] is TComboBox then
      TComboBox(Controls[i]).OnDrawItem := drawItem;

  SpongeForge := getSpongeForgeByVersion(Value);
  if SpongeForge <> nil then
  begin
    TComboBox(Controls[0]).ItemIndex := Items.IndexOf(SpongeForge.MC);
    onChanged(Controls[0]);
    TComboBox(Controls[1]).ItemIndex := TComboBox(Controls[1]).Items.IndexOf(Value);
  end
  else
    onChanged(Controls[0]);
end;

function TSpongeForgeSelect.getMCVersion : String;
var
TempSpongeForge : TSpongeForge;
begin
  TempSpongeForge := getSpongeForgeByVersion(Value);
  Result := '';
  if TempSpongeForge <> nil then
    Result := TempSpongeForge.MC;
end;

procedure TSpongeForgeSelect.onChanged(Sender : TObject);
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
      for i := 0 to SpongeForgeList.Count-1 do
        if SpongeForgeList[i].MC = mcversion then
        begin
          Strings.AddObject(SpongeForgeList[i].Version, SpongeForgeList[i].BuildType.getObject);
        end;
    Strings.Sort;
    TComboBox(Controls[1]).Items := Strings;
   // for i := 0 to Strings.Count-1 do
      //TComboBox(Controls[1]).Items.Add(Strings[Strings.Count-1-i]);
  end;
end;

procedure TInstallServerSpongeForge.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
begin
  if not FileExists(Instance.getInstanceFolder + 'mods\' + SpongeForge.ServerFile) then
  begin
    DownloadTask := TDownloadTask.Create(SpongeForge.URL, Instance.getInstanceFolder + 'mods\' + SpongeForge.ServerFile, False);
    DownloadTask.setLog(Log);
    DownloadTask.downloadFile(Bar);
  end;
  Bar.FinishStep;
end;

constructor TInstallServerSpongeForge.Create(Command : TMinecraftLaunch; SpongeForge : TSpongeForge; Instance : TInstance);
begin
  inherited Create('Installing SpongeForge Server', Command, True);
  Self.SpongeForge := SpongeForge;
  Self.Instance := Instance;
end;

constructor TSpongeForge.Create(Input : String);
var
Data : TStringList;
begin
  Data := Explode(Input, '-');
  //1.8-1521-2.1-DEV-729
  //mc-v(4)-v(2)-buildtype-buildnumber
  if Data.Count = 5 then
  begin
    FMC := Data[0];
    FVersion := Data[2] + '.' + Data[4];
    FForge := getForgeByBuildID(Data[1]);
    BuildType := parseBuildTypeString(Data[3]);
    FFile := 'spongeforge-' + Input + '.jar';
    FURL := 'http://files.minecraftforge.net/maven/org/spongepowered/spongeforge/' + Input + '/' + FFile;
    //http://files.minecraftforge.net/maven/org/spongepowered/spongeforge/1.8-1521-2.1-DEV-744/spongeforge-1.8-1521-2.1-DEV-744.jar
  end;
end;

function TSpongeForge.getMinecraftVersion : TMinecraftVersion;
begin
  Result := VanillaUtils.getMinecraftVersion(MC);
end;

procedure TSpongeForgeInstance.Load(SaveFile : TSaveFile);
begin
  inherited Load(SaveFile);
  SpongeForge := getSpongeForgeByVersion(SaveFile.getString('SpongeForge'));
  Side := TServer;
end;

procedure TSpongeForgeInstance.Save(SaveFile : TSaveFile);
begin
  inherited Save(SaveFile);
  SaveFile.setString('SpongeForge', SpongeForge.Version);
end;

function TSpongeForgeInstance.getUUID : String;
begin
  Result := 'SpongeForge';
end;

function TSpongeForgeInstance.getSettings : TList<TSetting>;
var
i: Integer;
SpongeForgeSelect : TSpongeForgeSelect;
begin
  Result := inherited getSettings;
  SpongeForgeSelect := TSpongeForgeSelect.Create('SpongeForge', 'SpongeForge');
  for i := 0 to Result.Count-1 do
    if Result[i] is TForgeSelect then
    begin
      Result[i].Destroy;
      Result[i] := SpongeForgeSelect;
    end
    else if Result[i] is TEnhancedModSelect then
      TEnhancedModSelect(Result[i]).ForgeSelect := SpongeForgeSelect;
end;

function TSpongeForgeInstance.getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>;
var
ModCleaning : TModCleaning;
DownloadLibary : TDownloadFLibary;
begin
  Result := TList<TTask>.Create;
  Result.Add(TInstallForge.Create(TForgeLaunch(MinecraftComand)));
  Result.Add(TInstallServerForge.Create(getInstanceFolder, TForgeLaunch(MinecraftComand)));
  DownloadLibary := TDownloadFLibary.Create(MinecraftComand);
  DownloadLibary.CustomJsonPath := SpongeForge.Forge.getJsonFileName;
  DownloadLibary.CustomLibaryFolder := getInstanceFolder + 'libraries\';
  DownloadLibary.IsServer := True;
  Result.Add(DownloadLibary);
  //Result.Add(TInstallServerForge.Create(getInstanceFolder, TForgeLaunch(MinecraftComand)));
  Result.Add(TInstallServerSpongeForge.Create(MinecraftComand, SpongeForge,Self));
  if not Custom then
  begin
    ModCleaning := TModCleaning.Create(Mods, getInstanceFolder + 'mods\', Side);
    ModCleaning.Exclude.Add(SpongeForge.ServerFile);
    Result.Add(ModCleaning);
  end;

  Result.Add(TDownloadMods.Create(Mods, getInstanceFolder + 'mods\', Side));
end;

function TSpongeForgeInstance.getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch;
begin
  Result := nil;
  if SpongeForge <> nil then
    Result := TCauldronLaunch.Create(Java, SpongeForge.Forge, Self, LoginData);
end;

procedure TSpongeModpackInstance.LoadPost(SaveFile : TSaveFile);
var
ClosestBelow : TSpongeForge;
i, ForgeVersion, SpongeForgeVersion, lastBelow, lastAbove : Integer;
begin
  inherited LoadPost(SaveFile);
  if Modpack.Key <> nil then
  begin
    ForgeVersion := StrToInt(getLastPiece(Modpack.Value.Forge, '.'));
    lastBelow := -1;
    lastAbove := -1;
    ClosestBelow := nil;
    for i := 0 to SpongeForgeList.Count-1 do
    begin
      if SpongeForgeList[i].Forge.MV <> Forge.MV then
        continue;
      SpongeForgeVersion := SpongeForgeList[i].Forge.getVersionID;
      {if SpongeForgeVersion = ForgeVersion then
      begin
        SpongeForge := SpongeForgeList[i];
        break;
      end;}

      if SpongeForgeVersion < ForgeVersion then
      begin
        if (lastBelow = -1) or (lastBelow <= SpongeForgeVersion) then
        begin
          lastBelow := SpongeForgeVersion;
          ClosestBelow := SpongeForgeList[i];
        end;
      end
      else if SpongeForgeVersion >= ForgeVersion then
      begin
        if (lastAbove = -1) or (lastAbove >= SpongeForgeVersion) then
        begin
          SpongeForge := SpongeForgeList[i];
          lastAbove := SpongeForgeVersion;
        end;
      end;
    end;

    if SpongeForge = nil then
      SpongeForge := ClosestBelow;

    Forge := SpongeForge.Forge;
  end;

  Side := TServer;
end;

function TSpongeModpackInstance.getUUID : String;
begin
  Result := 'SpongeForgeModpack';
end;

function TSpongeModpackInstance.getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>;
var
  i: Integer;
begin
  Result := inherited getStartupTasks(MinecraftComand);
  i := 0;
  while i < Result.Count do
  begin
    if Result[i] is TDownloadFLibary then
    begin
      Result.Insert(i+1, TInstallServerSpongeForge.Create(MinecraftComand, SpongeForge, Self));
    end;
    if Result[i] is TModCleaning then
    begin
      TModCleaning(Result[i]).Exclude.Add(SpongeForge.ServerFile);
    end;
    i := i + 1;
  end;
end;

function TSpongeModpackInstance.getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch;
begin
  Result := nil;
  if SpongeForge <> nil then
    Result := TCauldronLaunch.Create(Java, SpongeForge.Forge, Self, LoginData);
end;

constructor TLoadSpongeForge.Create;
begin
  inherited Create('Loading SpongeForge', False, False);
end;

procedure TLoadSpongeForge.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
SpongeForge : TSpongeForge;
JsonFile : ISuperObject;
JsonArray : TSuperArray;
i : Integer;
begin
  FileName := DownloadFolder + 'SpongeForge.xml';
  SpongeForgeList := TList<TSpongeForge>.Create;
  DownloadTask := TDownloadTask.Create('http://files.minecraftforge.net/maven/org/spongepowered/spongeforge/maven-metadata.xml',
  FileName, True);
  SpongeForgeMCList := TStringList.Create;

  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
  end;

  if FileExists(FileName) and (VanillaUtils.MinecraftVersions <> nil) then
  begin
    JsonFile := XMLParseFile(FileName, True);
    JsonArray := JsonFile.O['versioning'].O['versions'].A['version'];

    for I := 0 to JsonArray.Length-1 do
    begin
      SpongeForge := TSpongeForge.Create(JsonArray.S[i]);
      if (SpongeForge <> nil) and (SpongeForge.Forge <> nil) then
      begin
        if not SpongeForgeMCList.Contains(SpongeForge.MC) then
          SpongeForgeMCList.Add(SpongeForge.MC);
        SpongeForgeList.Add(SpongeForge);
      end;
    end;
  end;
end;

end.
