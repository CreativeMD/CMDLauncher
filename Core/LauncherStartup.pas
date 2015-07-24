unit LauncherStartup;

interface

uses Task, ProgressBar, System.Generics.Collections, IdHTTP, System.SysUtils,
Vcl.Forms, ShellApi, Winapi.Windows;

type
  TUpdateTask = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      constructor Create;
  end;

var
CloseLauncher : Boolean;

function getStartupTasks : TList<TTask>;
function getStartupPostTasks : TList<TTask>;

implementation

uses DatabaseConnection, VanillaUtils, IconUtils, InstanceUtils, JavaUtils,
AccountUtils, CoreLoader, StringUtils, DownloadUtils, LauncherSettings,
ZipUtils, ForgeUtils, ModUtils, ModpackUtils, Cauldron;

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
      DownloadTask := TDownloadTask.Create('http://creativemd.bplaced.net/downloads/CMDUpdate.exe', ProgramFolder + 'Update.exe');
      DownloadTask.downloadFile(Bar);
      DownloadTask.Destroy;
      ShellExecute(Application.Handle, 'open', PChar(ProgramFolder + 'Update.exe'), nil, nil, SW_NORMAL);
      CloseLauncher := True;
    end
    else
    begin
      ChangeLog := HTTP.Get('http://creativemd.de/service/changelog_new.php?name=' + ProgramName);
      LauncherSettings.Changelog := Explode(ChangeLog, '<br>');
    end;
  Bar.FinishStep;
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
  Result.Add(TLoadMod.Create);
  Result.Add(TLoadModpack.Create);
  Result.Add(TLoadIcon.Create);
  Result.Add(TLoadJava.Create);
  Result.Add(TLoadInstance.Create);
  Result.Add(TLoadAccount.Create);
end;

end.
