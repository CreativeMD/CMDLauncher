unit LauncherSettings;

interface

uses SettingUtils, System.Generics.Collections, Vcl.Controls, SaveFileUtils,
Vcl.StdCtrls, System.Classes, Task, ProgressBar, Vcl.Themes, StringUtils;

type
  TLogger = class(TSetting)
    Lines : TStringList;
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;
  TLauncherSetting = class(TSettingGroupList)
    destructor Destroy; override;
    procedure onSaved(GroupList : TSettingGroupList); override;
  end;

procedure openSettings(Page : String);

var
ChangeLog : TStringList;

implementation

uses CoreLoader, Logger, IconUtils, AccountUtils, JavaUtils;

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
  SPage := TSettingPage.Create('Console', 'Console.png');
  SPage.AddSetting(TLogger.Create('Log', 'Log'));
  Group.AddPage(SPage);
  SPage := TSettingPage.Create('Changelog', 'Changelog.png');
  Setting := TLogger.Create('Changelog', 'Changelog', True);
  if ChangeLog = nil then
    ChangeLog := TStringList.Create;
  Setting.Lines := ChangeLog;
  SPage.AddSetting(Setting);
  Group.AddPage(SPage);
  SPage := TSettingPage.Create('Design', 'Design.png');
  Styles := TStringList.Create;
  Styles.AddStrings(TStyleManager.StyleNames);
  SPage.AddSetting(TSelectSetting.Create('style', 'Design', Styles));
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
begin
  Memo := TMemo.Create(Parent);
  Memo.Parent := Parent;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.ScrollBars := ssVertical;
  if Lines <> nil then
    Memo.Lines.AddStrings(Lines)
  else
  begin
    Memo.Lines := Logger.MainLog.Content;
    Logger.MainLog.Listener.Add(Memo.Lines);
  end;
  Controls.Add(Memo);
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

end.
