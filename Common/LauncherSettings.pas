unit LauncherSettings;

interface

uses SettingUtils, System.Generics.Collections, Vcl.Controls, SaveFileUtils,
Vcl.StdCtrls, System.Classes, Task, ProgressBar, Vcl.Themes, StringUtils, JvProgressBar,
System.SysUtils;

type
  TLogger = class(TSetting)
    Lines : TStringList;
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
    procedure onKeyPress(Sender : TObject; var Key : Char);
  end;
  TLauncherSetting = class(TSettingGroupList)
    destructor Destroy; override;
    procedure onSaved(GroupList : TSettingGroupList); override;
  end;
  TStatus = (sWorking,sLoading,sErrored);
  TStatusSetting = class(TSetting<TStatus>)
    protected
      function isFilled : Boolean; override;
      function isFilledValid : Boolean; override;
    public
      Text : String;
      constructor Create(Name, Title : String; Status : TStatus; TextWorking : String);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;

procedure openSettings(Page : String);
function BoolToState(Value : Boolean) : TStatus; overload;
function BoolToState(Value, Value2 : Boolean) : TStatus; overload;

var
ChangeLog : TStringList;

implementation

uses CoreLoader, Logger, IconUtils, AccountUtils, JavaUtils, CommandUtils, DatabaseConnection, VanillaUtils,
ModUtils, ModpackUtils, ForgeUtils, Cauldron, SpongeForge, ResourcePackUtils, ShaderPackUtils;

function BoolToState(Value : Boolean) : TStatus;
begin
  if Value then
    Exit(sWorking);
  Exit(sErrored);
end;

function BoolToState(Value, Value2 : Boolean) : TStatus;
begin
  if Value then
    if Value2 then
      Exit(sWorking)
    else
      Exit(sLoading);
  Exit(sErrored);
end;

procedure openSettings(Page : String);
var
Groups : TList<TSettingGroup>;
Group : TSettingGroup;
SPage : TSettingPage;
Setting : TLogger;
Styles : TStringList;

begin
  Groups := TList<TSettingGroup>.Create;
  Group := TSettingGroup.Create('Minecraft');
  SPage := TSettingPage.Create('Java', 'Java.png');

  SPage.AddSetting(TSelectSetting.Create('javaversion', 'Java-Version', getJavaVersions).setNotNeedFill);
  Group.AddPage(SPage);
  SPage := TSettingPage.Create('Accounts', 'Account.png');
  SPage.AddSetting(TAccountSetting.Create('acc', ''));
  Group.AddPage(SPage);
  Groups.Add(Group);

  Group := TSettingGroup.Create('Launcher');
  SPage := TSettingPage.Create('Settings', 'Settings.png');
  SPage.AddSetting(TCheckOption.Create('protocol-enabled', 'Enable Protocol', True));
  Styles := TStringList.Create;
  Styles.AddStrings(TStyleManager.StyleNames);
  SPage.AddSetting(TSelectSetting.Create('style', 'Design', Styles));
  Group.AddPage(SPage);
  SPage := TSettingPage.Create('Console', 'Console.png');
  SPage.AddSetting(TLogger.Create('Log', 'Log'));
  Group.AddPage(SPage);
  SPage := TSettingPage.Create('Status', 'Diagram.png');
  SPage.AddSetting(TStatusSetting.Create('database', 'Database', BoolToState(DatabaseConnection.online), 'Database connection established'));
  SPage.AddSetting(TStatusSetting.Create('minecraft', 'Minecraft', BoolToState(VanillaUtils.MinecraftVersions <> nil), 'Loaded ' + InttoStr(VanillaUtils.MinecraftVersions.Count-1) + ' Minecraft-Versions'));
  SPage.AddSetting(TStatusSetting.Create('forge', 'Forge', BoolToState(ForgeUtils.ForgeList <> nil), 'Loaded ' + InttoStr(ForgeUtils.ForgeList.Count-1) + ' Forge-Versions'));
  SPage.AddSetting(TStatusSetting.Create('cauldron', 'Cauldron', BoolToState(Cauldron.CauldronList <> nil), 'Loaded ' + InttoStr(Cauldron.CauldronList.Count-1) + ' Cauldron-Versions'));
  SPage.AddSetting(TStatusSetting.Create('spongeforge', 'SpongeForge', BoolToState(SpongeForge.SpongeForgeList <> nil), 'Loaded ' + InttoStr(SpongeForge.SpongeForgeList.Count-1) + ' SpongeForge-Versions'));
  SPage.AddSetting(TStatusSetting.Create('mods', 'Mods', BoolToState(ModUtils.ModList <> nil, ModUtils.ModsLoaded), 'Loaded ' + InttoStr(ModUtils.ModList.Count-1) + ' Mods'));
  SPage.AddSetting(TStatusSetting.Create('modpacks', 'Modpacks', BoolToState(ModpackUtils.ModPacks <> nil, ModpackUtils.ModPacksLoaded), 'Loaded ' + InttoStr(ModpackUtils.ModPacks.Count-1) + ' Modpacks'));
  SPage.AddSetting(TStatusSetting.Create('resourcepacks', 'Resourcepacks', BoolToState(ResourcePackUtils.ResourcePacks <> nil), 'Loaded ' + InttoStr(ResourcePackUtils.ResourcePacks.Count-1) + ' Resourcepacks'));
  SPage.AddSetting(TStatusSetting.Create('shaderpacks', 'Shaderpacks', BoolToState(ShaderPackUtils.Shaderpacks <> nil), 'Loaded ' + InttoStr(ShaderPackUtils.Shaderpacks.Count-1) + ' Shaderpacks'));
  Group.AddPage(SPage);
  SPage := TSettingPage.Create('Changelog', 'Changelog.png');
  Setting := TLogger.Create('Changelog', 'Changelog', True);
  if ChangeLog = nil then
    ChangeLog := TStringList.Create;
  Setting.Lines := ChangeLog;
  SPage.AddSetting(Setting);
  Group.AddPage(SPage);
  Groups.Add(Group);
  TLauncherSetting.Create('CMDLauncher - Settings', LauncherIcons, ProgramSettings, Page, Groups);
end;

destructor TLauncherSetting.Destroy;
begin
  TStyleManager.TrySetStyle(ProgramSettings.getString('style'), False);
end;

procedure TLauncherSetting.onSaved(GroupList : TSettingGroupList);
begin
  JavaUtils.SelectedJava := ProgramSettings.getString('javaversion');
end;

procedure TLogger.createControl(x, y : Integer; Parent : TWinControl);
var
Memo : TMemo;
Edit : TEdit;
begin
  ;
  Memo := TMemo.Create(Parent);
  Memo.Parent := Parent;
  Memo.Anchors := [akLeft, akRight, akTop, akBottom];
  Memo.Top := 0;
  Memo.Left := 0;
  Memo.Width := Parent.Width;
  Memo.Height := Parent.Height;


  //Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.ScrollBars := ssVertical;
  Controls.Add(Memo);
  if Lines <> nil then
    Memo.Lines.AddStrings(Lines)
  else
  begin
    Memo.Lines := Logger.MainLog.Content;
    Logger.MainLog.Listener.Add(Memo.Lines);

    Edit := TEdit.Create(Parent);
    Edit.Parent := Parent;

    Memo.Height := Parent.Height-Edit.Height;

    Edit.Anchors := [akLeft, akRight, akBottom];
    Edit.Top := Memo.Height + Memo.Top;
    Edit.Text := '';
    Edit.Width := Parent.Width;
    Edit.OnKeyPress := onKeyPress;


    Controls.Add(Edit);
  end;
end;

procedure TLogger.onKeyPress(Sender : TObject; var Key : Char);
var
Edit : TEdit;
begin
  Edit := TEdit(Sender);
  If Key = #13 then
  begin
    if Edit.Text <> '' then
    begin
      Logger.MainLog.log(CommandUtils.processCommand(Edit.Text));
      Edit.Text := '';
    end;
    Key := #0;
  end;
end;

procedure TLogger.destroyControl;
begin
  Logger.MainLog.Listener.Remove(TMemo(Controls[0]).Lines);
end;

function TLogger.getUUID : string;
begin
  Result := 'mmLog';
end;

procedure TLogger.SaveToFile(SaveFile : TSaveFile);
begin

end;

procedure TLogger.LoadFromFile(SaveFile : TSaveFile);
begin

end;

function TStatusSetting.isFilled : Boolean;
begin
  Result := True;
end;

function TStatusSetting.isFilledValid : Boolean;
begin
  Result := True;
end;

constructor TStatusSetting.Create(Name, Title : String; Status : TStatus; TextWorking : String);
begin
  inherited Create(Name, Title, Status, False);
  case Status of
    sWorking: Self.Text := TextWorking;
    sLoading: Self.Text := 'Feature is still loading!';
    sErrored: Self.Text := 'Failed to load feature!';
  end;
end;

procedure TStatusSetting.createControl(x, y : Integer; Parent : TWinControl);
var
ProgressBar : TJvProgressBar;
lbl : TLabel;
begin
  ProgressBar := TJvProgressBar.Create(Parent);
  ProgressBar.Parent := Parent;
  ProgressBar.Left := x;
  ProgressBar.Top := y;
  ProgressBar.Width := 30;
  ProgressBar.Smooth := False;
  ProgressBar.Position := ProgressBar.Max;
  case Value of
    sWorking: ProgressBar.State := pbsNormal;
    sLoading: ProgressBar.State := pbsPaused;
    sErrored: ProgressBar.State := pbsError;
  end;
  Controls.Add(ProgressBar);
  lbl := TLabel.Create(Parent);
  lbl.Parent := Parent;
  lbl.Top := y;
  lbl.Left := ProgressBar.Width+x+5;
  lbl.Caption := Text;
  Controls.Add(lbl);
end;

procedure TStatusSetting.destroyControl;
begin

end;

function TStatusSetting.getUUID : string;
begin
  Result := 'status';
end;

procedure TStatusSetting.SaveToFile(SaveFile : TSaveFile);
begin

end;

procedure TStatusSetting.LoadFromFile(SaveFile : TSaveFile);
begin

end;


end.
