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
    procedure onKeyPress(Sender : TObject; var Key : Char);
  end;
  TLauncherSetting = class(TSettingGroupList)
    destructor Destroy; override;
    procedure onSaved(GroupList : TSettingGroupList); override;
  end;

procedure openSettings(Page : String);

var
ChangeLog : TStringList;

implementation

uses CoreLoader, Logger, IconUtils, AccountUtils, JavaUtils, CommandUtils;

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
  Group.AddPage(SPage);
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

end.
