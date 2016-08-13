unit LauncherStartup;

interface

uses Task, ProgressBar, System.Generics.Collections, IdHTTP, System.SysUtils,
Vcl.Forms, ShellApi, Winapi.Windows, Vcl.Dialogs, Vcl.Controls, Vcl.StdCtrls, LaunchHandler;

type
  TUpdateTask = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      constructor Create;
  end;

var
HasFinishedStartup : Boolean;

function getStartupTasks : TList<TTask>;
function getStartupPostTasks : TList<TTask>;
procedure onStartupHasFinished;
procedure onPostStartupHasFinished;

implementation

uses DatabaseConnection, VanillaUtils, IconUtils, InstanceUtils, JavaUtils,
AccountUtils, CoreLoader, StringUtils, DownloadUtils, LauncherSettings,
ZipUtils, ForgeUtils, ModUtils, ModpackUtils, Cauldron, SpongeForge, ResourcePackUtils, CommandUtils,
Overview, URLProtocolUtils, Logger, Setup;

constructor TUpdateTask.Create;
begin
  inherited Create('Searching for updates', True);
end;

procedure TUpdateTask.runTask(Bar : TCMDProgressBar);
var
HTTP : TIdHTTP;
ServerVersion, Changelog : String;
DownloadTask : TDownloadTask;
begin
  //Command
  CommandUtils.loadLauncherCommands;

  HTTP := TIdHTTP.Create;
  CloseLauncher := False;
  try
    ServerVersion := HTTP.Get('http://creativemd.de/service/version_new.php?name=' + ProgramName);
  except
    on E: Exception do
      ServerVersion := '';
  end;
  Bar.StartStep(1);
  if ServerVersion <> '' then
    if StringUtils.isHigher(ProgramVersion, ServerVersion) then
    begin
      DownloadTask := TDownloadTask.Create('http://creativemd.bplaced.net/downloads/CMDUpdate.exe', ProgramFolder + 'CMDUpdate.exe');
      DownloadTask.downloadFile(Bar);
      DownloadTask.Destroy;
      ShellExecute(Application.Handle, 'open', PChar(ProgramFolder + 'CMDUpdate.exe'), nil, nil, SW_NORMAL);
      CloseLauncher := True;
    end
    else
    begin
      Self.Log.log('No new version available! Newest version=' + ServerVersion);
      ChangeLog := HTTP.Get('http://creativemd.de/service/changelog_new.php?name=' + ProgramName);
      LauncherSettings.Changelog := Explode(ChangeLog, '<br>');
    end;
  Bar.FinishStep;
end;

procedure onStartupHasFinished;
var
Tasks : TList<TTask>;
i : Integer;
begin
  HasFinishedStartup := False;
  if not CloseLauncher then
  begin
    OverviewF.BackgroundTask := TBackgroundTaskManager.Create(nil, OverviewF.BackgroundBar);
    Tasks := LauncherStartup.getStartupPostTasks;
    for i := 0 to Tasks.Count-1 do
      OverviewF.BackgroundTask.addTask(Tasks[i]);
  end;
end;

procedure onPostStartupHasFinished;
begin
  HasFinishedStartup := True;
  //ShowMessage('Test');
  if ProgramSettings.getBoolean('protocol-enabled') then
  begin
    if not checkIfProtocol('cmdlauncher', 'CMDLauncher', ProgramFile) then
      if not registerProtocol('cmdlauncher', 'CMDLauncher', ProgramFile) then
      begin
        //If it's not working!
        with CreateMessageDialog('Could not install url protocol.' + sLineBreak + 'Please run it as admin to install this feature!', mtInformation, mbOKCancel) do
          try
            Width := 310;
            Height := 140;
            TButton(FindComponent('Ok')).Caption := 'Run as admin';
            TButton(FindComponent('Ok')).Width := 100;
            TButton(FindComponent('Cancel')).Left := TButton(FindComponent('Cancel')).Left + 20;
            if ShowModal = mrOk then
            begin
              ShellExecute(Application.Handle, 'runas', PChar(ProgramFolder + 'URLRegister.exe'), nil, nil, SW_NORMAL);
            end;
          finally
            Free;
          end;
      end
      else
        Logger.MainLog.log('Sucessfully registed cmdlauncher:// protocol!')
    else
      Logger.MainLog.log('cmdlauncher:// protocol is ready to be used!');
  end
  else
    Logger.MainLog.log('Skipped loading cmdlauncher:// protocol!');

  if (AccountUtils.MinecraftAccounts.Count = 0) then
    SetupAssistant.Show;
end;

function getStartupPostTasks : TList<TTask>;
begin
  Result := TList<TTask>.Create;
  Result.add(ModUtils.TFullLoadMod.Create);
  Result.add(ModpackUtils.TFullLoadModpack.Create);
  Result.add(InstanceUtils.TFinishLoadInstance.Create);
end;

function getStartupTasks : TList<TTask>;
begin
  Result := TList<TTask>.Create;
  Result.Add(TConnect.Create);
  Result.Add(TUpdateTask.Create);
  Result.Add(TLoadMV.Create);
  Result.Add(TLoadForge.Create);
  Result.Add(TLoadCauldron.Create);
  Result.Add(TLoadSpongeForge.Create);
  Result.Add(TLoadMod.Create);
  Result.Add(TLoadModpack.Create);
  Result.Add(TLoadIcon.Create);
  Result.Add(TLoadJava.Create);
  Result.Add(TLoadResourcePacks.Create);
  Result.Add(TLoadInstance.Create);
  Result.Add(TLoadAccount.Create);
end;

end.
