unit ForgeUtils;

interface

uses VanillaUtils, Task, ProgressBar, InstanceUtils, SaveFileUtils, System.Generics.Collections,
MinecraftLaunchCommand, SettingUtils, JavaUtils, System.Classes, AccountUtils,
System.SysUtils, superobject, Vcl.Controls, Vcl.StdCtrls, AssetUtils,
MinecraftLibaryUtills, ModUtils;

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
      procedure onChanged(Sender: TObject); override;
    public
      constructor Create(Name, Title : String);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
  end;
  TForgeInstance = class(TInstance)
    protected
      Custom : Boolean;
      Mods : TDictionary<TMod, TModVersion>;
      procedure Load(SaveFile : TSaveFile); override;
      procedure Save(SaveFile : TSaveFile); override;
    public
      Forge : TForge;
      function getUUID : String; override;
      function getSettings : TList<TSetting>; override;
      function getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>; override;
      function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
      function getLaunchSettings : TList<TSetting>; override;
      function getLaunchSaveFile : TSaveFile; override;
  end;

function getForgeByUUID(UUID : string) : TForge;

var
ForgeList : TList<TForge>;
SupportedMV : TStringList;

implementation

uses ServerUtils, DownloadUtils, CoreLoader, DatabaseConnection, StringUtils,
ForgeInstallation, SortingUtils;

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
  end;
end;

constructor TForge.Create(UUID, MV : String; Branch : String = '');
begin
  Self.FUUID := UUID;
  Self.FMV := MV;
  Self.FBranch := Branch;
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
  inherited Create('Loading Forge', False);
end;

procedure TLoadForge.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
FileName, UUID , Branch: string;
JsonFile, Numbers : ISuperObject;
BranchTable : TSuperTableString;
BranchArray, VersionArray, ValueArray : TSuperArray;
  i, j: Integer;
begin
  FileName := DownloadFolder + 'forgeversions.json';
  ForgeList := TList<TForge>.Create;
  DownloadTask := TDownloadTask.Create('http://files.minecraftforge.net/maven/net/minecraftforge/forge/json',
  FileName, True);

  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
  end;
  SupportedMV := TStringList.Create;
  if FileExists(FileName) and (VanillaUtils.MinecraftVersions <> nil) then
  begin
    JsonFile := TSuperObject.ParseFile(FileName, true);
    BranchTable := JsonFile.N['mcversion'].AsObject;
    Numbers := JsonFile.O['number'];
    BranchArray := BranchTable.GetNames.AsArray;
    ValueArray := BranchTable.GetValues.AsArray;
    Bar.StartStep(BranchArray.Length);
    for i := 0 to BranchArray.Length-1 do
    begin
      UUID := BranchArray[i].AsString;
      if isHigher('1.6', UUID) and (VanillaUtils.getMinecraftVersion(UUID) <> nil) then
      begin
        VersionArray := ValueArray.N[i].AsArray;
        for j := 0 to VersionArray.Length-1 do
          if Numbers.O[VersionArray[j].AsString] <> nil then
          begin
            Branch := '';
            if Numbers.O[VersionArray[j].AsString].S['branch'] <> 'null' then
              Branch := Numbers.O[VersionArray[j].AsString].S['branch'];
            ForgeList.Add(TForge.Create(Numbers.O[VersionArray[j].AsString].S['version'], UUID, Branch));
          end;
        SupportedMV.Add(UUID);
      end;
      Bar.StepPos := i;
    end;

  end;
  SupportedMV.CustomSort(SortVersions);
  Bar.FinishStep;
end;

procedure TForgeInstance.Load(SaveFile : TSaveFile);
var
ModArray : TSuperArray;
begin
  Forge := nil;
  if ForgeList <> nil then
    Forge := getForgeByUUID(SaveFile.getString('forge'));
  Custom := SaveFile.getBoolean('custom');
  if not Custom then
  begin
    { TODO 2 : Load Mods }

  end;
end;

procedure TForgeInstance.Save(SaveFile : TSaveFile);
begin
  SaveFile.setString('forge', Forge.UUID);
  SaveFile.setBoolean('custom', Custom);
  { TODO 2 : Save Mods }
end;

function TForgeInstance.getUUID : String;
begin
  Result := 'Forge';
end;

function TForgeInstance.getSettings : TList<TSetting>;
begin
  Result := TList<TSetting>.Create;
  Result.Add(TForgeSelect.Create('forge', 'Forge'));
  Result.Add(TCheckOption.Create('custom', 'Custom Mods', True));
  { TODO 2 : Add Mod Select Option }
end;

function TForgeInstance.getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>;
var
DownloadLibary : TDownloadFLibary;
DownloadAssets : TLoadAssets;
begin
  Result := TList<TTask>.Create;
  if Forge = nil then
    Exit(Result);
  if InstanceTyp = IClient then
  begin
    if DatabaseConnection.online then
    begin
      Result.Add(TInstallForge.Create(TForgeLaunch(MinecrafComand)));
    end;
    DownloadAssets := TLoadAssets.Create(MinecrafComand, Forge.getMinecraftVersion.LaunchTyp);
    DownloadAssets.CustomJsonPath := Forge.getJsonFileName;
    Result.Add(DownloadAssets);
    DownloadLibary := TDownloadFLibary.Create(MinecrafComand);
    DownloadLibary.CustomJsonPath := Forge.getJsonFileName;
    Result.Add(DownloadLibary);
  end
  else
  begin
    Result.Add(TInstallForge.Create(TForgeLaunch(MinecrafComand)));
    Result.Add(TInstallServerForge.Create(getInstanceFolder, TForgeLaunch(MinecrafComand)));
    DownloadLibary := TDownloadFLibary.Create(MinecrafComand);
    DownloadLibary.CustomJsonPath := Forge.getJsonFileName;
    DownloadLibary.CustomLibaryFolder := getInstanceFolder + 'libraries\';
    DownloadLibary.IsServer := True;
    Result.Add(DownloadLibary);
  end;
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
  if InstanceTyp = IServer then
  begin
    Result.AddRange(ServerUtils.getStandardServerSettings(Self));
  end;
end;

function TForgeInstance.getLaunchSaveFile : TSaveFile;
begin
  Result := TSaveFile.Create(getInstanceFolder + 'server.properties');
end;

end.
