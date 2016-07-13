unit MinecraftVersionFileUtils;

interface

uses LaunchTaskUtils, ProgressBar, InstanceUtils, DownloadUtils, System.Classes,
System.SysUtils, MinecraftLaunchCommand, Task;

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
StringUtils, CoreLoader, DatabaseConnection;

constructor TDownloadServerVersion.Create(InstanceFolder : string; Command : TMinecraftLaunch);
begin
  inherited Create('Download minecraft server files', Command, False);
  Self.InstanceFolder := InstanceFolder;
end;

procedure TDownloadServerVersion.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
DownloadFiles : TStringList;
FileName : string;
i: Integer;
begin
  DownloadFiles := TStringList.Create;
  DownloadFiles.Add('http://s3.amazonaws.com/Minecraft.Download/versions/' + Command.MCVersion + '/minecraft_server.' + Command.MCVersion + '.jar');
  //DownloadFiles.Add('http://s3.amazonaws.com/Minecraft.Download/versions/' + Command.MCVersion + '/' + Command.MCVersion + '.json');

  if DataBaseConnection.online then
  begin
    for i := 0 to DownloadFiles.Count-1 do
    begin
      FileName := InstanceFolder + ExtractUrlFileName(DownloadFiles[i]);
      DownloadTask := TDownloadTask.Create(DownloadFiles[i], FileName, False);
      DownloadTask.setLog(Self.Log);
      DownloadTask.downloadFile(Bar);
    end;
  end;
  Command.SpecialArguments.Add('-jar');
  Command.SpecialArguments.Add('minecraft_server.' + Command.MCVersion + '.jar');
  //Bar.FinishStep;
end;

constructor TDownloadVersion.Create(MCVersion : String);
begin
  inherited Create('Downloading minecraft files', True);
  Self.MCVersion := MCVersion;
end;

constructor TDownloadVersion.Create(Command : TMinecraftLaunch);
begin
  Self.Create(Command.MCVersion);
end;

procedure TDownloadVersion.runTask(Bar : TCMDProgressBar);
var
DownloadTask : TDownloadTask;
DownloadFiles : TStringList;
FileName : string;
i: Integer;
begin
  DownloadFiles := TStringList.Create;
  DownloadFiles.Add('http://s3.amazonaws.com/Minecraft.Download/versions/' + MCVersion + '/' + MCVersion + '.jar');
  DownloadFiles.Add('http://s3.amazonaws.com/Minecraft.Download/versions/' + MCVersion + '/' + MCVersion + '.json');

  if Bar <> nil then
    Bar.StartStep(DownloadFiles.Count);

  for i := 0 to DownloadFiles.Count-1 do
  begin
    FileName := DownloadFolder + 'versions\' + MCVersion + '\' + ExtractUrlFileName(DownloadFiles[i]);
    DownloadTask := TDownloadTask.Create(DownloadFiles[i], FileName, False);
    DownloadTask.setLog(Self.Log);
    DownloadTask.downloadFile(nil);
    if Bar <> nil then
      Bar.StepPos := i+1;
  end;

  if Bar <> nil then
    Bar.FinishStep;
end;

end.
