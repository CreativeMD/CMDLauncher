unit InstanceUtils;

interface

uses SaveFileUtils, System.Generics.Collections, Task, ProgressBar, System.Classes, StringUtils, System.IOUtils, System.Types,
System.SysUtils, SettingUtils, MinecraftLaunchCommand, JavaUtils, AccountUtils, SideUtils;

type
  TInstance = class abstract
    private
      FTitle : String;
      SaveFile : TSaveFile;
      procedure LoadCore(SaveFile : TSaveFile);
      procedure SaveCore(SaveFile : TSaveFile);
    protected
      procedure Load(SaveFile : TSaveFile); virtual; abstract;
      procedure Save(SaveFile : TSaveFile); virtual; abstract;
    public
      RAM : Integer;
      Group, IconName, CustomCommand, CustomJava, ExternalFolder : String;
      Side : TSide;
      constructor Create(Title : String; read : Boolean = True; ExternalFolder : String = '');
      function isExternal : Boolean;
      function getInstanceFolder : string;
      function getUUID : String; virtual; abstract;
      function getSettings : TList<TSetting>; virtual; abstract;
      function getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>; virtual; abstract;
      function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; virtual; abstract;
      function getLaunchSettings : TList<TSetting>; virtual; abstract; //No groups avaiable
      function getLaunchSaveFile : TSaveFile; virtual; abstract;
      function canInstanceLaunch : Boolean; virtual;
      procedure LoadPost(SaveFile : TSaveFile); virtual;
      procedure SaveInstance;
      property Title : string read FTitle;
      property getSaveFile : TSaveFile read SaveFile;
  end;
  TLoadInstance = class(TTask)
    constructor Create;
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
  end;
  TFinishLoadInstance = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;

const
SaveFileName : String = 'Instance.cfg';

var
Instances : TList<TInstance>;
InstanceTypes : TList<TPair<TInstance, TSideType>>;
LoadedInstances : Boolean;
ExternalInstances, HiddenGroups : TStringList;

function CreateInstance(Title, UUID : String; ExternalFolder : String = '') : TInstance;
function RenameInstance(Instance : TInstance; NewName : string) : Boolean;
function canRenameInstance(Instance : TInstance; NewName : string) : Boolean;
function getInstanceByUUID(UUID : String; Side : TSide) : TInstance;
function getInstanceByUUIDIgnoreSide(UUID : String) : TInstance;
function getTypes(Side : TSide) : TStringList;
procedure registerInstanceTyp(Instance : TInstance; SideType : TSideType);

implementation

uses CoreLoader, FileUtils, Overview, Logger, VanillaUtils, ForgeUtils, ModpackUtils, Cauldron;

function getTypes(Side : TSide) : TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to InstanceTypes.Count-1 do
    if InstanceTypes[i].Value.isCompatible(Side) then
      Result.Add(InstanceTypes[i].Key.getUUID);
end;

function getInstanceByUUIDIgnoreSide(UUID : String) : TInstance;
var
  i: Integer;
begin
  for i := 0 to InstanceTypes.Count-1 do
    if InstanceTypes[i].Key.getUUID = UUID then
      Exit(InstanceTypes[i].Key);
  Exit(nil);
end;

function getInstanceByUUID(UUID : String; Side : TSide) : TInstance;
var
  i: Integer;
begin
  for i := 0 to InstanceTypes.Count-1 do
    if (InstanceTypes[i].Key.getUUID = UUID) and (InstanceTypes[i].Value.isCompatible(Side)) then
      Exit(InstanceTypes[i].Key);
  Exit(nil);
end;

function canRenameInstance(Instance : TInstance; NewName : string) : Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Instances.Count-1 do
  begin
    if (Instances[i].Title = NewName) and (Instances[i] <> Instance) then
      Result := False;
  end;
end;

function RenameInstance(Instance : TInstance; NewName : string) : Boolean;
var
Tasks : TList<TTask>;
begin
  Result := canRenameInstance(Instance, NewName);
  if Result then
  begin
    Tasks := TList<TTask>.Create;
    Tasks.Add(TRenameFolder.Create('Changing Name', Instance.getInstanceFolder, CoreLoader.InstanceFolder + NewName));
    OverviewF.runForegroundTasks(Tasks);
    Instance.FTitle := NewName;
  end;
end;

procedure registerInstanceTyp(Instance : TInstance; SideType : TSideType);
var
Item : TPair<TInstance, TSideType>;
begin
  Item.Key := Instance;
  Item.Value := SideType;
  InstanceTypes.Add(Item);
end;

constructor TFinishLoadInstance.Create;
begin
  inherited Create('Finish loading Forge Instances', False);
end;

procedure TFinishLoadInstance.runTask(Bar : TCMDProgressBar);
var
i: Integer;
begin
  for i := 0 to InstanceUtils.Instances.Count-1 do
  begin
    Instances[i].LoadPost(Instances[i].getSaveFile);
  end;
  LoadedInstances := True;
end;

constructor TLoadInstance.Create;
begin
  inherited Create('Loading Instances');
  LoadedInstances := False;
end;

procedure TLoadInstance.runTask(Bar : TCMDProgressBar);
var
Folders : TStringList;
i: Integer;
Instance : TInstance;
begin
  if not DirectoryExists(InstanceFolder) then
    ForceDirectories(InstanceFolder);
  Self.Log.log('Loading Instances ...');
  InstanceTypes := TList<TPair<TInstance, TSideType>>.Create;
  registerInstanceTyp(TVanillaInstance.Create('VanillaTyp',False), Universal);
  registerInstanceTyp(TForgeInstance.Create('ForgeTyp', False), Universal);
  registerInstanceTyp(TModpackInstance.Create('ModpackTyp', False), Universal);
  registerInstanceTyp(TCauldronInstance.Create('CauldronTyp', False), OnlyServer);

  if Assigned(Instances) then
  begin
    for i := 0 to Instances.Count-1 do
      Instances[i].Destroy;
    Instances.Destroy;
  end;

  ExternalInstances := ProgramSettings.getStringList('external');
  HiddenGroups := ProgramSettings.getStringList('hidden');
  Instances := TList<TInstance>.Create;

  Folders := ArrayToList(TDirectory.GetDirectories(InstanceFolder));
  Bar.StartStep(Folders.Count+ExternalInstances.Count);

  for i := 0 to Folders.Count-1 do
  begin
    if FileExists(Folders[i] + '\' + SaveFileName) then
    begin
      Instance := CreateInstance(Folders[i].Replace(InstanceFolder, '').Replace('\', ''), TSaveFile.Create(Folders[i] + '\' + SaveFileName).getString('uuid'));
      if Instance <> nil then
        Instances.Add(Instance);
    end;
    Bar.StepPos := i;
  end;

  i := 0;
  while i < ExternalInstances.Count do
  begin
    if FileExists(ExternalInstances[i] + '\' + SaveFileName) then
    begin
      Instance := CreateInstance(ExtractLastFolder(ExternalInstances[i]), TSaveFile.Create(ExternalInstances[i] + '\' + SaveFileName).getString('uuid'), ExternalInstances[i]);
      if Instance <> nil then
        Instances.Add(Instance);
        i := i + 1;
    end
    else
    begin
      Self.Log.log('Could not load/find external folder=' + ExternalInstances[i]);
      ExternalInstances.Delete(i);
    end;
    Bar.StepPos := i+Folders.Count;
  end;

  OverviewF.loadInstances;
  Self.Log.log('Loaded ' + InttoStr(Instances.Count) + ' Instances');
  Bar.FinishStep;
end;

function CreateInstance(Title, UUID : String; ExternalFolder : String = '') : TInstance;
var
InstanceTyp : TInstance;
begin
  Result := nil;
  InstanceTyp := getInstanceByUUIDIgnoreSide(UUID);

  if InstanceTyp <> nil then
  begin
    try
      Result := TInstance(InstanceTyp.ClassType.NewInstance).Create(Title, True, ExternalFolder);
      if (ExternalFolder <> '') and not ExternalInstances.Contains(ExternalFolder) then
        ExternalInstances.Add(ExternalFolder);
    except
      Logger.Log.log('Failed to create class uuid=' + UUID + ' (title=' + Title + ')');
    end;
  end
  else
    Logger.Log.log('Found invalid uuid=' + UUID + ' (title=' + Title + ')');
end;

constructor TInstance.Create(Title : String; read : Boolean = True; ExternalFolder : String = '');
begin
  Self.FTitle := Title;
  Self.SaveFile := nil;
  Self.ExternalFolder := ExternalFolder;
  RAM := 1024;
  Group := '';
  IconName := '';
  if read then
  begin
    SaveFile := TSaveFile.Create(getInstanceFolder + SaveFileName);
    LoadCore(SaveFile);
    Load(SaveFile);

    if isExternal then
    begin
      Title := SaveFile.getString('title');
    end;
  end;
end;

procedure TInstance.LoadCore(SaveFile : TSaveFile);
begin
  RAM := SaveFile.getInteger('ram');
  if RAM = 0 then
    RAM := 1024;
  Group := SaveFile.getString('group');
  IconName := SaveFile.getString('icon');
  CustomCommand := SaveFile.getString('customcommand');
  if SaveFile.getBoolean('cjava') then
    CustomCommand := '';
  CustomJava := SaveFile.getString('java');
  if SaveFile.getString('itype') = 'Server' then
    Side := TServer
  else
    Side := TClient;
end;

procedure TInstance.SaveCore(SaveFile : TSaveFile);
begin
  SaveFile.setInteger('ram', RAM);
  SaveFile.setString('group', Group);
  SaveFile.setString('icon', IconName);
  SaveFile.setString('uuid', getUUID);
  SaveFile.setString('itype', Side.toString);
  SaveFile.setString('customcommand', CustomCommand);
  SaveFile.setString('java', CustomJava);
  SaveFile.setString('external', ExternalFolder);
end;

function TInstance.isExternal : Boolean;
begin
  Result := ExternalFolder <> '';
end;

function TInstance.getInstanceFolder : string;
begin
  if isExternal then
    Result := ExternalFolder
  else
    Result := CoreLoader.InstanceFolder + Self.FTitle + '\';
end;

procedure TInstance.SaveInstance;
begin
  SaveCore(SaveFile);
  Save(SaveFile);
end;

function TInstance.canInstanceLaunch : Boolean;
begin
  Result := True;
end;

procedure TInstance.LoadPost(SaveFile : TSaveFile);
begin

end;

end.
