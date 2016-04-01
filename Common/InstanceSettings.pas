unit InstanceSettings;

interface

uses SettingUtils, System.Generics.Collections, InstanceUtils, IconUtils, SaveFileUtils,
System.Classes, Vcl.Controls, Vcl.StdCtrls, Task, System.SysUtils, SideUtils, JvImagesViewer,
Vcl.Graphics, cefvcl, System.IOUtils, System.Types;

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
      function getSide : TSide;
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      destructor Destroy; override;
      function getExpandedSettings : TList<TSetting>;
      function getGroupTitle : string;
      procedure updateSettings;
      procedure setGroupList(GroupList : TSettingGroupList);
  end;
  TScreenshotView = class(TSetting)
    Directory : string;
    constructor Create(Directory : string);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;
  TInstanceSetting = class(TSettingGroupList)
    Instance : TInstance;
    copyOf : String;
    TempInstance : Boolean;
    procedure onBeforeSaving(GroupList : TSettingGroupList); override;
    procedure onSaved(GroupList : TSettingGroupList); override;
    function getErrors : TStringList; override;
    procedure onCancel; override;
  end;

function loadInstanceSettings(Instance : TInstance; tempInstance : Boolean = False) : TInstanceSetting;

implementation

uses Overview, CoreLoader, StringUtils, FileUtils, JavaUtils, CustomSettings, ResourcePackUtils;

constructor TScreenshotView.Create(Directory : string);
begin
  inherited Create('screenshotview', 'view');
  Self.Directory := Directory;
end;

procedure TScreenshotView.createControl(x, y : Integer; Parent : TWinControl);
{var
JvImagesViewer: TJvImagesViewer;
begin
  JvImagesViewer := TJvImagesViewer.Create(Parent);
  JvImagesViewer.Parent := Parent;
  JvImagesViewer.Directory := Directory;
  JvImagesViewer.Align := alClient;
  JvImagesViewer.DoubleBuffered := True;
  JvImagesViewer.Options.FrameColor := clWhite;
  JvImagesViewer.Options.Height := 180;
  JvImagesViewer.Options.HorzSpacing := 4;
  JvImagesViewer.Options.ImagePadding := 4;
  JvImagesViewer.Options.LazyRead := True;
  JvImagesViewer.Options.RightClickSelect := True;
  JvImagesViewer.Options.ShowCaptions := False;
  JvImagesViewer.Options.Width := 300;
  JvImagesViewer.Options.ShowCaptions := True;
  Controls.Add(JvImagesViewer);      }
var
Chromium : TChromium;
side, Files : TStringList;
  i: Integer;
begin
  side := TStringList.Create;
  Files := ArrayToList(TDirectory.GetFiles(Directory, '*.png'));
  side.Add('<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>'
  + '<script type="text/javascript" src="http://creativemd.bplaced.net/jquery.zoomooz.min.js"></script>');
  for i := 0 to Files.Count-1 do
    side.Add('<div style="max-width:480px; border-radius: 2px 10px; margin: 4pt; box-shadow: 3px 3px 3px #7C7C7C; float:left;" align="center">'
    + '<img src="' + Files[i] + '" class="zoomTarget" data-preservescroll="false" data-targetsize="0.7" data-closeclick="true" style="max-width:480px; max-height: 270px; border-radius: 2px 10px;">'
    + ExtractFileName(Files[i]) + '</div>');
  //side.Add('test');
  ForceDirectories(TempFolder);
  side.SaveToFile(TempFolder + 'screenshotoverview.html');
  Chromium := TChromium.Create(Parent);
  Chromium.Parent := Parent;
  Chromium.Left := SettingUtils.xOffset;
  Chromium.Top := y;
  //Chromium.OnAddressChange := chrmModsAddressChange;
  Chromium.Align := alClient;
  Chromium.Load('file:///' + TempFolder + 'screenshotoverview.html');
  //Chromium.Visible := False;
  Controls.Add(Chromium);
end;

procedure TScreenshotView.destroyControl;
begin

end;

function TScreenshotView.getUUID : string;
begin
  Result := 'screen';
end;

procedure TScreenshotView.SaveToFile(SaveFile : TSaveFile);
begin

end;

procedure TScreenshotView.LoadFromFile(SaveFile : TSaveFile);
begin

end;

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

function TInstanceSelect.getSide : TSide;
begin
  Result := parseSide(TSetting<String>(GroupList.getSetting('itype')).Value);
end;

procedure TInstanceSelect.createControl(x, y : Integer; Parent : TWinControl);
begin
  Self.Items := InstanceUtils.getTypes(getSide);
  inherited createControl(x, y, parent);
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
    Instance := InstanceUtils.getInstanceByUUIDIgnoreSide(Value);
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
var
ExternalFolder : String;
begin
  ExternalFolder := TSetting<String>(GroupList.getSetting('external')).Value;
  if ExternalFolder <> '' then
    GroupList.SaveFile := TSaveFile.Create(ExternalFolder + InstanceUtils.SaveFileName)
  else
    GroupList.SaveFile := TSaveFile.Create(InstanceFolder + TSetting<string>(GroupList.getSetting('title')).Value + '\' + InstanceUtils.SaveFileName);
end;

procedure TInstanceSetting.onSaved(GroupList : TSettingGroupList);
var
NewInstance, OldInstance : TInstance;
Tasks : TList<TTask>;

begin
  NewInstance := CreateInstance(TSetting<string>(getSetting('title')).Value, TSetting<string>(getSetting('uuid')).Value, TSetting<String>(GroupList.getSetting('external')).Value);
  NewInstance.LoadPost(NewInstance.getSaveFile);

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
      ForceDirectories(NewInstance.getInstanceFolder + '\mods\');
      Tasks.Add(TCopyFolder.Create('Copying folder', OldInstance.getInstanceFolder + '\config', NewInstance.getInstanceFolder + '\config\'));
      Tasks.Add(TCopyFolder.Create('Copying folder', OldInstance.getInstanceFolder + '\mods', NewInstance.getInstanceFolder + '\mods\'));
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

procedure TInstanceSetting.onCancel;
var
Tasks : TList<TTask>;
begin
  if TempInstance then
  begin
    Tasks  := TList<TTask>.Create;
    Tasks.Add(TDeleteFolder.Create('Copying folder', InstanceFolder + copyOf + '\'));
    OverviewF.runForegroundTasks(Tasks);
    //DeleteFolder(copyOf, nil);
  end;
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

function getPerm : TStringList;
begin
  Result := TStringList.Create;
  Result.Add('128');
  Result.Add('256');
  Result.Add('512');
  Result.Add('1024');
end;

function loadInstanceSettings(Instance : TInstance; tempInstance : Boolean = False) : TInstanceSetting;
var
Title: String;
SaveFile : TSaveFile;
Groups : TList<TSettingGroup>;
Group : TSettingGroup;
Page : TSettingPage;
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
  Page.AddSetting(TSelectDirSetting.Create('external', 'External Folder').setNotNeedFill);
  Group.AddPage(Page);
  Page := TSettingPage.Create('Custom', 'Edit.png');

  Page.AddSetting(TInstanceSelect.Create('uuid', 'Typ', TStringList.Create, 'Vanilla', False));

  Group.AddPage(Page);
  Page := TSettingPage.Create('Client & Server', 'ClientServer.png');
  Page.AddSetting(TSelectSetting.Create('itype', 'Instance Type', ArrayToList(['Client', 'Server']), 'Client'));
  Group.AddPage(Page);
  Page := TSettingPage.Create('Java', 'Java.png');
  Page.AddSetting(TNumberSelect.Create('ram', 'RAM', getRam, '1024'));

  Page.AddSetting(TStringSetting.Create('customcommand', 'Command', '', False, '').setWidth(300).setNotNeedFill);
  Page.AddSetting(TCheckOption.Create('classunloading', 'Class Unloading', True));
  Page.AddSetting(TNumberSelect.Create('permspace', 'PermGen Space', getPerm, '256'));

  CheckExpand := TCheckExpand.Create('cjava', 'Custom Java', False);

  CheckExpand.Settings.Add(TSelectSetting.Create('java', 'Java Version', getJavaVersions));
  Page.AddSetting(CheckExpand);
  Group.AddPage(Page);
  Groups.Add(Group);
  if Instance <> nil then
  begin
    Group := TSettingGroup.Create('Other');
    Page := TSettingPage.Create('Screenshots', 'Screenshot.png');
    Page.AddSetting(TScreenshotView.Create(Instance.getInstanceFolder + 'screenshots'));
    Group.AddPage(Page);
    Page := TSettingPage.Create('Resourcepacks', 'Resourcepack.png');
    Page.AddSetting(TResourcepackSelect.Create('resourcepack', 'Resourcepack', Instance.getInstanceFolder));
    Group.AddPage(Page);
    Page := TSettingPage.Create('Crash-Reports', 'Mail.png');
    Group.AddPage(Page);
    Page := TSettingPage.Create('Shaderpacks', 'Shader.png');
    Group.AddPage(Page);
    Groups.Add(Group);
  end;

  Result := TInstanceSetting.Create(Title, LauncherIcons, SaveFile, '', Groups);
  if not tempInstance then
    Result.Instance := Instance
  else
    Result.copyOf := Instance.Title;
  Result.TempInstance := tempInstance;
end;

end.
