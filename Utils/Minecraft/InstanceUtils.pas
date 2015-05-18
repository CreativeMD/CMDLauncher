unit InstanceUtils;

interface

uses SaveFileUtils, System.Generics.Collections, Task, ProgressBar, System.Classes, StringUtils, System.IOUtils, System.Types,
System.SysUtils, SettingUtils, MinecraftLaunchCommand, JavaUtils, AccountUtils;

type
  TInstanceTyp = (IClient, IServer);
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
      Group, IconName, CustomCommand, CustomJava : String;
      InstanceTyp : TInstanceTyp;
      constructor Create(Title : String; read : Boolean = True);
      function getInstanceFolder : string;
      function getUUID : String; virtual; abstract;
      function getSettings : TList<TSetting>; virtual; abstract;
      function getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>; virtual; abstract;
      function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; virtual; abstract;
      function getLaunchSettings : TList<TSetting>; virtual; abstract; //No groups avaiable
      function getLaunchSaveFile : TSaveFile; virtual; abstract;
      procedure SaveInstance;
      property Title : string read FTitle;
      property getSaveFile : TSaveFile read SaveFile;
  end;
  TLoadInstance = class(TTask)
    constructor Create;
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
  end;

const
SaveFileName : String = 'Instance.cfg';

var
Instances : TList<TInstance>;
InstanceTypes : TList<TInstance>;

function CreateInstance(Title, UUID : String) : TInstance;
function RenameInstance(Instance : TInstance; NewName : string) : Boolean;
function canRenameInstance(Instance : TInstance; NewName : string) : Boolean;
function getInstanceByUUID(UUID : String) : TInstance;
function ITypeToString(InstanceTyp : TInstanceTyp) : String;

implementation

uses CoreLoader, FileUtils, Overview, Logger, VanillaUtils, ForgeUtils;

function ITypeToString(InstanceTyp : TInstanceTyp) : String;
begin
  if InstanceTyp = IClient then
    Result := 'Client'
  else
    Result := 'Server';
end;

function getInstanceByUUID(UUID : String) : TInstance;
var
  i: Integer;
begin
  for i := 0 to InstanceTypes.Count-1 do
    if InstanceTypes[i].getUUID = UUID then
      Exit(InstanceTypes[i]);
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

constructor TLoadInstance.Create;
begin
  inherited Create('Loading Instances');
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
  InstanceTypes := TList<TInstance>.Create;
  InstanceTypes.Add(TVanillaInstance.Create('VanillaTyp',False));
  InstanceTypes.Add(TForgeInstance.Create('ForgeTyp', False));

  if Assigned(Instances) then
  begin
    for i := 0 to Instances.Count-1 do
      Instances[i].Destroy;
    Instances.Destroy;
  end;

  Instances := TList<TInstance>.Create;

  Folders := ArrayToList(TDirectory.GetDirectories(InstanceFolder));
  Bar.StartStep(Folders.Count);
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

  OverviewF.loadInstances;
  Self.Log.log('Loaded ' + InttoStr(Instances.Count) + ' Instances');
  Bar.FinishStep;
end;

function CreateInstance(Title, UUID : String) : TInstance;
var
i : Integer;
InstanceTyp : TInstance;
begin
  Result := nil;
  InstanceTyp := nil;
  for i := 0 to InstanceTypes.Count-1 do
    if InstanceTypes[i].getUUID = UUID then
       InstanceTyp := InstanceTypes[i];

  if InstanceTyp <> nil then
  begin
    try
      Result := TInstance(InstanceTyp.ClassType.NewInstance).Create(Title);
    except
      Logger.Log.log('Failed to create class uuid=' + UUID + ' (title=' + Title + ')');
    end;
  end
  else
    Logger.Log.log('Found invalid uuid=' + UUID + ' (title=' + Title + ')');
end;

constructor TInstance.Create(Title : String; read : Boolean = True);
begin
  Self.FTitle := Title;
  Self.SaveFile := nil;
  RAM := 1024;
  Group := '';
  IconName := '';
  if read then
  begin
    SaveFile := TSaveFile.Create(getInstanceFolder + SaveFileName);
    LoadCore(SaveFile);
    Load(SaveFile);
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
    InstanceTyp := IServer
  else
    InstanceTyp := IClient;
end;

procedure TInstance.SaveCore(SaveFile : TSaveFile);
begin
  SaveFile.setInteger('ram', RAM);
  SaveFile.setString('group', Group);
  SaveFile.setString('icon', IconName);
  SaveFile.setString('uuid', getUUID);
  SaveFile.setString('itype', ITypeToString(InstanceTyp));
  SaveFile.setString('customcommand', CustomCommand);
  SaveFile.setString('java', CustomJava);
end;

function TInstance.getInstanceFolder : string;
begin
  Result := CoreLoader.InstanceFolder + Self.FTitle + '\';
end;

procedure TInstance.SaveInstance;
begin
  SaveCore(SaveFile);
  Save(SaveFile);
end;

end.
