unit MinecraftStartup;

interface

uses Task, InstanceUtils, Logger, System.Generics.Collections, MinecraftLaunchCommand,
Console, System.SysUtils, SettingUtils, SideUtils, System.Classes, SaveFileUtils, Vcl.Controls,
Vcl.StdCtrls;

type
  TLaunchTaskManager = class(TTaskManager)
    Command : TMinecraftLaunch;
    Instance : TInstance;
    function isEndless : Boolean; override;
    procedure FinishedEvent; override;
  end;
  TServerConfigEdit = class(TStringListSetting)
    Group : TSettingGroupList;
    ServerFile : string;
    constructor Create(Name, Title, ServerFile : String);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;
  TInstanceStartupSettings = class(TSettingGroupList)
    Instance : TInstance;
    Online : Boolean;
    procedure onSaved(GroupList : TSettingGroupList); override;
  end;

procedure launchInstance(Instance : TInstance; Online : Boolean = True; ShowSettings : Boolean = True);

implementation

uses DatabaseConnection, AssetUtils, JavaUtils, LauncherSettings,
Vcl.Dialogs, AccountUtils, CoreLoader, StringUtils, IconUtils;

procedure TInstanceStartupSettings.onSaved(GroupList : TSettingGroupList);
begin
  launchInstance(Instance, Online, False);
end;

procedure launchInstance(Instance : TInstance; Online : Boolean = True; ShowSettings : Boolean = True);
var
//Console : TConsoleF;
LaunchManager : TLaunchTaskManager;
Tasks : TList<TTask>;
Command : TMinecraftLaunch;
LoginData : TLoginData;
TempAccount : TAccount;
Settings : TList<TSetting>;
StartupSettings : TInstanceStartupSettings;
Groups : TList<TSettingGroup>;
Group : TSettingGroup;
Page : TSettingPage;
i: Integer;
Java : TJava;
Listener : TStrings;
FullServerEdit : TServerConfigEdit;
begin
  Settings := Instance.getLaunchSettings;
  if ShowSettings and (Settings.Count > 0) and Instance.canInstanceLaunch then
  begin
    Group := TSettingGroup.Create('Default');
    Page := TSettingPage.Create('Easy Config', 'Settings.png');
    for i := 0 to Settings.Count-1 do
      Page.AddSetting(Settings[i]);
    Group.AddPage(Page);
    Page := TSettingPage.Create('Full Config', 'Changelog.png');
    FullServerEdit := TServerConfigEdit(TServerConfigEdit.Create('SConfig', 'SConfig', Instance.getLaunchSaveFile.getFileName).setNotNeedFill);
    Page.AddSetting(FullServerEdit);
    Group.AddPage(Page);
    Groups := TList<TSettingGroup>.Create;
    Groups.Add(Group);
    StartupSettings := TInstanceStartupSettings.Create('Server-Config', LauncherIcons, Instance.getLaunchSaveFile, '', Groups);
    FullServerEdit.Group := StartupSettings;
    StartupSettings.SettingForm.Width := 600;
    StartupSettings.SettingForm.Height := 450;
    StartupSettings.Instance := Instance;
    StartupSettings.Online := Online;
  end
  else
  begin
    Java := nil;

    if Instance.CustomJava <> '' then
      Java := JavaUtils.getJavaByTitle(Instance.CustomJava);

    if Java = nil then
      Java := JavaUtils.getSelectedJava;

    if Java = nil then
    begin
      LauncherSettings.openSettings('Java');
      ShowMessage('Please select a java version!');
      Exit;
    end;

    TempAccount := getSelectedAccount;
    if TempAccount = nil then
    begin
      TempAccount := TAccount.Create('', '', '');
      TempAccount.SavePassword := True;
    end;

    LoginData := nil;

    if Instance.Side = TClient then
    begin
      LoginData := login(TempAccount, nil, Online);
      if LoginData.hasFailed then
        Exit;

      if not LoginData.isOffline then
      begin
        if TempAccount <> getSelectedAccount then
        begin
          ProgramSettings.setString('selacc', TempAccount.LoginName);
          if not AccountUtils.MinecraftAccounts.Contains(TempAccount) then
          begin
            AccountUtils.MinecraftAccounts.Add(TempAccount);
            AccountUtils.saveAccounts;
          end;
        end;
      end;
    end;

    Command := Instance.getCommand(Java, LoginData);

    if Command = nil then
    begin
      ShowMessage('This instance could not launch! It needs internet connection!');
      Exit;
    end;

    {Console := TConsoleF.Create(nil);

    Console.Caption := Instance.Title;

    Tasks := TList<TTask>.Create;
    Tasks.AddRange(Instance.getStartupTasks(Command));
    Console.Show;
    LaunchManager := TLaunchTaskManager.Create(Tasks, Console.ProgressBar);
    LaunchManager.Log := TLog.Create;
    LaunchManager.Log.Listener.Add(Console.mmoLog.Lines);
    LaunchManager.Console := Console;
    LaunchManager.Command := Command;
    LaunchManager.Instance := Instance; }

    Tasks := TList<TTask>.Create;
    Tasks.AddRange(Instance.getStartupTasks(Command));
    LaunchManager := TLaunchTaskManager.Create(Tasks, Command.createWindow(Instance, Listener));
    LaunchManager.Log := TLog.Create;
    LaunchManager.Log.Listener.Add(Listener);
    //LaunchManager.Console := Console;
    LaunchManager.Command := Command;
    LaunchManager.Instance := Instance;
  end;
end;

constructor TServerConfigEdit.Create(Name, Title, ServerFile : String);
begin
  inherited Create(Name, Title);
  Self.ServerFile := ServerFile;
  Self.setHideTitle;
end;

procedure TServerConfigEdit.destroyControl;
var
i, j, h : Integer;
begin
  inherited destroyControl;
  Value.SaveToFile(ServerFile);
  for i := 0 to Group.Groups.Count-1 do
    for j := 0 to Group.Groups[i].Pages.Count-1 do
      for h := 0 to Group.Groups[i].Pages[j].Settings.Count-1 do
        Group.Groups[i].Pages[j].Settings[h].LoadFromFile(Group.SaveFile);
end;

function TServerConfigEdit.getUUID : string;
begin
  Result := 'SConfigEdit';
end;

procedure TServerConfigEdit.createControl(x, y : Integer; Parent : TWinControl);
begin
  inherited createControl(x, y, Parent);
  TMemo(Controls[0]).Align := alClient;
  Value.LoadFromFile(ServerFile);
end;

procedure TServerConfigEdit.SaveToFile(SaveFile : TSaveFile);
begin

end;

procedure TServerConfigEdit.LoadFromFile(SaveFile : TSaveFile);
begin
  Value := TStringList.Create;
  Value.LoadFromFile(ServerFile);
end;

function TLaunchTaskManager.isEndless : Boolean;
begin
  Result := False;
end;

procedure TLaunchTaskManager.FinishedEvent;
begin
  Command.buildListener(Instance);
  {Console.Instance := Instance;
  Console.Launching := TLaunching.Create('"' + Command.Java.Path + '"', Command.getLaunchCommand,
  Instance.getInstanceFolder, procedure(const Line: AnsiString)
      begin
          if string(Line) <> '' then
            Console.mmoLog.Lines.Append(Line);
      end);
  Console.mmoLog.Lines.Add('"' + Command.Java.Path + '"');
  Console.mmoLog.Lines.AddStrings(Command.getLaunchCommandList);
  Console.Launching.OnClosed := Console.onClosed;        }
end;

end.
