unit InstanceSettings;

interface

uses SettingUtils, System.Generics.Collections, InstanceUtils, IconUtils, SaveFileUtils,
System.Classes, Vcl.Controls, Vcl.StdCtrls, Task, System.SysUtils;

type
  TNumberSelect = class(TTextSelectSetting)
    public
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      function getUUID : string; override;
      procedure onKeyPress(Sender: TObject; var Key: Char);
  end;
  TInstanceSelect = class(TSelectSetting, IExpandableSetting)
    protected
      SubSettings : TDictionary<String, TList<TSetting>>;
      GroupList : TSettingGroupList;
      procedure onChanged(Sender: TObject); override;
      function createCustomBox(Parent : TWinControl) : TGroupBox;
    public
      destructor Destroy; override;
      function getExpandedSettings : TList<TSetting>;
      function getGroupTitle : string;
      procedure updateSettings;
      procedure setGroupList(GroupList : TSettingGroupList);
  end;
  TInstanceSetting = class(TSettingGroupList)
    Instance : TInstance;
    copyOf : String;
    procedure onBeforeSaving(GroupList : TSettingGroupList); override;
    procedure onSaved(GroupList : TSettingGroupList); override;
    function getErrors : TStringList; override;
  end;

function loadInstanceSettings(Instance : TInstance) : TInstanceSetting;

implementation

uses Overview, CoreLoader, StringUtils, FileUtils, JavaUtils;

procedure TNumberSelect.onKeyPress(Sender: TObject; var Key: Char);
begin
  if not (CharinSet(key,['0'..'9',#8])) then
    Key := #0;
end;

procedure TNumberSelect.createControl(x, y : Integer; Parent : TWinControl);
begin
  inherited createControl(x, y, Parent);
  TComboBox(Controls[0]).OnKeyPress := onKeyPress;
end;

function TNumberSelect.getUUID : string;
begin
  Result := 'numsel';
end;

procedure TInstanceSelect.setGroupList(GroupList : TSettingGroupList);
begin
  Self.GroupList := GroupList;
end;

function TInstanceSelect.getExpandedSettings : TList<TSetting>;
begin
  if SubSettings = nil then
    SubSettings := TDictionary<String, TList<TSetting>>.Create;
  if not SubSettings.ContainsKey(Value) then
    updateSettings;

  Result := SubSettings.Items[Value];
  if Result = nil then
    Result := TList<TSetting>.Create;
end;

destructor TInstanceSelect.Destroy;
var
Keys : TArray<string>;
  i: Integer;
begin
  Keys := SubSettings.Keys.ToArray;
  for i := 0 to Length(Keys)-1 do
    if SubSettings.Items[Keys[i]] <> nil then
      SubSettings.Items[Keys[i]].Destroy;
end;

procedure TInstanceSelect.updateSettings;
var
Instance : TInstance;
i: Integer;
begin
  if not SubSettings.ContainsKey(Value) then
  begin
    Instance := InstanceUtils.getInstanceByUUID(Value);
    if Instance <> nil then
    begin
      SubSettings.Add(Value, Instance.getSettings);
      if GroupList.SaveFile <> nil then
      begin
        for i := 0 to SubSettings[Value].Count-1 do
          SubSettings[Value][i].LoadFromFile(GroupList.SaveFile);
      end;
    end
    else
      SubSettings.Add(Value, TList<TSetting>.Create);
  end;
end;

function TInstanceSelect.getGroupTitle : string;
begin
  Result := Value;
end;

procedure TInstanceSelect.onChanged(Sender: TObject);
begin
  if GroupList <> nil then
    GroupList.refreshPage;
end;

function TInstanceSelect.createCustomBox(Parent : TWinControl) : TGroupBox;
begin
  Result := nil;
end;

procedure TInstanceSetting.onBeforeSaving(GroupList : TSettingGroupList);
begin
  GroupList.SaveFile := TSaveFile.Create(CoreLoader.InstanceFolder + TSetting<string>(GroupList.getSetting('title')).Value + '\' + InstanceUtils.SaveFileName);
end;

procedure TInstanceSetting.onSaved(GroupList : TSettingGroupList);
var
NewInstance, OldInstance : TInstance;
Tasks : TList<TTask>;

begin
  NewInstance := CreateInstance(TSetting<string>(getSetting('title')).Value, TSetting<string>(getSetting('uuid')).Value);

  if Instance = nil then
    Instances.Add(NewInstance)
  else
  begin
    if Instance.Title <> NewInstance.Title then
      RenameInstance(Instance, NewInstance.Title);

    if Instances.IndexOf(Instance) = -1 then
    begin
      Tasks  := TList<TTask>.Create;
      Tasks.Add(TLoadInstance.Create);
      OverviewF.runForegroundTasks(Tasks);
    end
    else
    begin
      Instances.Items[Instances.IndexOf(Instance)] := NewInstance;
      Instance.Destroy;
    end;
  end;
  if copyOf <> '' then
  begin
    OldInstance := OverviewF.getInstanceByName(copyOf);
    if OldInstance <> nil then
    begin
      Tasks  := TList<TTask>.Create;
      ForceDirectories(NewInstance.getInstanceFolder + '\config\');
      Tasks.Add(TCopyFolder.Create('Copying folder', OldInstance.getInstanceFolder + '\config', NewInstance.getInstanceFolder + '\config\'));
      OverviewF.runForegroundTasks(Tasks);
    end;
  end;
  OverviewF.loadInstances;
end;

function TInstanceSetting.getErrors : TStringList;
begin
  Result := TStringList.Create;
  if not canRenameInstance(Instance, TSetting<string>(getSetting('title')).Value) then
    Result.Add('This name is already taken!');
end;

function getRam : TStringList;
begin
  Result := TStringList.Create;
  Result.Add('1024');
  Result.Add('2048');
  Result.Add('4096');
  Result.Add('8192');
  Result.Add('16384');
end;

function loadInstanceSettings(Instance : TInstance) : TInstanceSetting;
var
Title: String;
SaveFile : TSaveFile;
Groups : TList<TSettingGroup>;
Group : TSettingGroup;
Page : TSettingPage;
UUIDs : TStringList;
i: Integer;
CheckExpand : TCheckExpand;
begin

  if Instance <> nil then
  begin
    Title := 'Edit ' + Instance.Title;
    SaveFile := Instance.getSaveFile;
  end
  else
  begin
    Title := 'Create Instance';
    SaveFile := nil;
  end;

  Groups := TList<TSettingGroup>.Create;
  Group := TSettingGroup.Create('Game');
  Page := TSettingPage.Create('General', 'Minecraft.png');
  Page.AddSetting(TStringSetting.Create('title', 'Name', ''));
  Page.AddSetting(TStringListSetting.Create('description', 'Description').setNotNeedFill);
  Page.AddSetting(TTextSelectSetting.Create('group', 'Group', OverviewF.Groups).setNotNeedFill);
  Page.AddSetting(TIconPicker.Create('icon', 'Icon', OverviewF.InstanceIcons, OverviewF.Icons).setNotNeedFill);
  Group.AddPage(Page);
  Page := TSettingPage.Create('Custom', 'Edit.png');

  UUIDs := TStringList.Create;
  for i := 0 to InstanceTypes.Count-1 do
    UUIDs.Add(InstanceTypes[i].getUUID);

  Page.AddSetting(TInstanceSelect.Create('uuid', 'Typ', UUIDs, 'Vanilla', False));

  Group.AddPage(Page);
  Page := TSettingPage.Create('Client & Server', 'ClientServer.png');
  Page.AddSetting(TSelectSetting.Create('itype', 'Instance Type', ArrayToList(['Client', 'Server']), 'Client'));
  Group.AddPage(Page);
  Page := TSettingPage.Create('Java', 'Java.png');
  Page.AddSetting(TNumberSelect.Create('ram', 'RAM', getRam, '1024'));
  { TODO 2 : Add PermGenSpace and Custom Settings }
  Page.AddSetting(TStringSetting.Create('customcommand', 'Command', '', False, '').setWidth(300).setNotNeedFill);
  CheckExpand := TCheckExpand.Create('cjava', 'Custom Java', False);

  CheckExpand.Settings.Add(TSelectSetting.Create('java', 'Java Version', getJavaVersions));
  Page.AddSetting(CheckExpand);
  Group.AddPage(Page);
  Groups.Add(Group);
  Group := TSettingGroup.Create('Other');
  Page := TSettingPage.Create('Screenshots', 'Screenshot.png');
  Group.AddPage(Page);
  Page := TSettingPage.Create('Resourcepacks', 'Resourcepack.png');
  Group.AddPage(Page);
  Page := TSettingPage.Create('Crash-Reports', 'Mail.png');
  Group.AddPage(Page);
  Page := TSettingPage.Create('Shaderpacks', 'Shader.png');
  Group.AddPage(Page);
  Groups.Add(Group);

  Result := TInstanceSetting.Create(Title, LauncherIcons, SaveFile, '', Groups);
  Result.Instance := Instance;
end;

end.
