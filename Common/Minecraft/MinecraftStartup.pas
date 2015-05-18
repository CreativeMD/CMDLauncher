unit MinecraftStartup;

interface

uses Task, InstanceUtils, Logger, System.Generics.Collections, MinecraftLaunchCommand,
Console, System.SysUtils, SettingUtils;

type
  TLaunchTaskManager = class(TTaskManager)
    Console : TConsoleF;
    Command : TMinecraftLaunch;
    Instance : TInstance;
    function isEndless : Boolean; override;
    procedure FinishedEvent; override;
  end;
  TInstanceStartupSettings = class(TSettingGroupList)
    Instance : TInstance;
    Online : Boolean;
    procedure onSaved(GroupList : TSettingGroupList); override;
  end;

procedure launchInstance(Instance : TInstance; Online : Boolean = True; ShowSettings : Boolean = True);

implementation

uses DatabaseConnection, AssetUtils, JavaUtils, LauncherSettings,
Vcl.Dialogs, AccountUtils, CoreLoader, StringUtils;

procedure TInstanceStartupSettings.onSaved(GroupList : TSettingGroupList);
begin
  launchInstance(Instance, Online, False);
end;

procedure launchInstance(Instance : TInstance; Online : Boolean = True; ShowSettings : Boolean = True);
var
Console : TConsoleF;
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
begin
  Settings := Instance.getLaunchSettings;
  if ShowSettings and (Settings.Count > 0) then
  begin
    Group := TSettingGroup.Create('Default');
    Page := TSettingPage.Create('StartupConfig', '');
    for i := 0 to Settings.Count-1 do
      Page.AddSetting(Settings[i]);
    Group.AddPage(Page);
    Groups := TList<TSettingGroup>.Create;
    Groups.Add(Group);
    StartupSettings := TInstanceStartupSettings.Create('Startup Config', '', Instance.getLaunchSaveFile, '', Groups);
    StartupSettings.hideGroups;
    StartupSettings.SettingForm.Width := 300;
    StartupSettings.SettingForm.Height := 250;
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

    if Instance.InstanceTyp = IClient then
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

    Console := TConsoleF.Create(nil);

    Console.Caption := Instance.Title;

    Tasks := TList<TTask>.Create;
    Tasks.AddRange(Instance.getStartupTasks(Command));
    Console.Show;
    LaunchManager := TLaunchTaskManager.Create(Tasks, Console.ProgressBar);
    LaunchManager.Log := TLog.Create;
    LaunchManager.Log.Listener.Add(Console.mmoLog.Lines);
    LaunchManager.Console := Console;
    LaunchManager.Command := Command;
    LaunchManager.Instance := Instance;
  end;
end;

function TLaunchTaskManager.isEndless : Boolean;
begin
  Result := False;
end;

procedure TLaunchTaskManager.FinishedEvent;
begin
  Console.Instance := Instance;
  Console.Launching := TLaunching.Create('"' + Command.Java.Path + '"', Command.getLaunchCommand,
  Instance.getInstanceFolder, procedure(const Line: PAnsiChar)
      begin
          if string(Line) <> '' then
            Console.mmoLog.Lines.Append(String(Line));
      end);
  Console.mmoLog.Lines.Add('"' + Command.Java.Path + '"');
  Console.mmoLog.Lines.AddStrings(Command.getLaunchCommandList);
  Console.Launching.Consol := Console;
end;

end.
