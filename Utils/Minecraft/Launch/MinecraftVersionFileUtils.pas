unit MinecraftVersionFileUtils;

interface

uses LaunchTaskUtils, ProgressBar, InstanceUtils, DownloadUtils, System.Classes,
System.SysUtils, MinecraftLaunchCommand, Task, superobject;

type
  TDownloadVersion = class(TTask)
    protected
      MCVersion : string;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      constructor Create(Command : TMinecraftLaunch); overload;
      constructor Create(MCVersion : string); overload;
  end;
  TDownloadServerVersion = class(TLaunchTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      InstanceFolder : String;
      constructor Create(InstanceFolder : string; Command : TMinecraftLaunch);
  end;

implementation

uses
StringUtils, CoreLoader, DatabaseConnection, VanillaUtils;

constructor TDownloadServerVersion.Create(InstanceFolder : string; Command : TMinecraftLaunch);
begin
  inherited Create('Download minecraft server files', Command, False);
  Self.InstanceFolder := InstanceFolder;
end;

procedure TDownloadServerVersion.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
FileName : string;
json : ISuperObject;
begin
  FileName := DownloadFolder + 'versions\' + Command.getMCVersion + '\' + Command.getMCVersion + '.json';
  if FileExists(FileName) then
  begin
    json := TSuperObject.ParseFile(FileName, true);
    json := json.O['downloads'].O['server'];
    DownloadTask := TDownloadTask.Create(json.S['url'], InstanceFolder + '\minecraft_server.' + Command.getMCVersion + '.jar', False);
    DownloadTask.setLog(Self.Log);
    DownloadTask.downloadFile(Bar);
  end
  else
    Self.Log.log('Could not find "' + Command.getMCVersion + '.json"!');
  Command.SpecialArguments.Add('-jar');
  Command.SpecialArguments.Add('"minecraft_server.' + Command.getMCVersion + '.jar"');
  //Bar.FinishStep;
end;

constructor TDownloadVersion.Create(MCVersion : String);
begin
  inherited Create('Downloading minecraft files', True);
  Self.MCVersion := MCVersion;
end;

constructor TDownloadVersion.Create(Command : TMinecraftLaunch);
begin
  Self.Create(Command.getMCVersion);
end;

procedure TDownloadVersion.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
FileName : string;
MinecraftVersion : TMinecraftVersion;
json : ISuperObject;
begin
  MinecraftVersion := VanillaUtils.getMinecraftVersion(MCVersion);

  if MinecraftVersion <> nil then
  begin
    FileName := DownloadFolder + 'versions\' + MCVersion + '\' + MCVersion + '.json';
    DownloadTask := TDownloadTask.Create(MinecraftVersion.URL, FileName, False);
    DownloadTask.setLog(Self.Log);
    if DownloadTask.downloadFile(nil) then
    begin
      json := TSuperObject.ParseFile(FileName, true);
      json := json.O['downloads'].O['client'];
      DownloadTask := TDownloadTask.Create(json.S['url'], DownloadFolder + 'versions\' + MCVersion + '\' + MCVersion + '.jar', False);
      DownloadTask.setLog(Self.Log);
      DownloadTask.downloadFile(Bar);
    end
    else
      Self.Log.log('Could not download version info file of "' + MCVersion + '"!');

  end
  else
    Self.Log.log('Could not download version "' + MCVersion + '"!');
end;

end.
