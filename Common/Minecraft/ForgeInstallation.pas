unit ForgeInstallation;

interface

uses MinecraftLaunchCommand, LaunchTaskUtils, ForgeUtils, JavaUtils, ProgressBar,
Task, System.SysUtils, superobject, System.Classes, Winapi.Windows, FileUtils,
SortingUtils, VanillaUtils, AccountUtils, InstanceUtils;

type
  TForgeLaunch = class(TVanillaLaunch)
    Forge : TForge;
    constructor Create(Java : TJava; Forge : TForge; Instance : TInstance; LoginData : TLoginData); overload;
    constructor Create(Java : TJava; MC : String; Instance : TInstance; LoginData : TLoginData); overload;
  end;
  TInstallServerForge = class(TLaunchTask)
    InstanceFolder : String;
    procedure runTask(Bar : TCMDProgressBar); override;
    constructor Create(InstanceFolder : String; Command : TForgeLaunch);
  end;
  TInstallForge = class(TLaunchTask)
    procedure runTask(Bar : TCMDProgressBar); override;
    constructor Create(Command : TForgeLaunch);
  end;

implementation

uses MinecraftVersionFileUtils, CoreLoader, DownloadUtils, ZipUtils;

constructor TForgeLaunch.Create(Java : TJava; Forge : TForge; Instance : TInstance; LoginData : TLoginData);
begin
  inherited Create(Java, Forge.MV, Instance, LoginData);
  Self.Forge := Forge;
end;

constructor TForgeLaunch.Create(Java : TJava; MC : String; Instance : TInstance; LoginData : TLoginData);
begin
  inherited Create(Java, MC, Instance, LoginData);
  Self.Forge := nil;
end;

procedure TInstallServerForge.runTask(Bar : TCMDProgressBar);
var
FName, MCVersion, UUID : string;
DownloadTask : TDownloadTask;
DownloadServer : TDownloadServerVersion;
begin
  MCVersion := TForgeLaunch(Command).Forge.MV;
  UUID := TForgeLaunch(Command).Forge.UUID;
  Command.MCVersion := MCVersion;

  DownloadServer := TDownloadServerVersion.Create(InstanceFolder, Command);
  DownloadServer.setLog(Log);
  Task.runTask(DownloadServer, nil);
  Command.SpecialArguments.Clear;

  FName := MCVersion + '-' + UUID;
  if TForgeLaunch(Command).Forge.Branch <> '' then
    FName := FName + '-' + TForgeLaunch(Command).Forge.Branch;
  DownloadTask := TDownloadTask.Create('http://files.minecraftforge.net/maven/net/minecraftforge/forge/' + FName + '/forge-' + FName + '-universal.jar',
  InstanceFolder + 'forge-' + FName + '-universal.jar', False);
  DownloadTask.setLog(Log);
  DownloadTask.downloadFile(Bar);
  Command.SpecialArguments.Add('-jar');
  Command.SpecialArguments.Add('forge-' + FName + '-universal.jar');
end;

constructor TInstallServerForge.Create(InstanceFolder : String; Command : TForgeLaunch);
begin
  inherited Create('Installing Forge Server', Command, True);
  Self.InstanceFolder := InstanceFolder;
end;

constructor TInstallForge.Create(Command : TForgeLaunch);
begin
  inherited Create('Installing Forge', Command, True);
end;

procedure TInstallForge.runTask(Bar : TCMDProgressBar);
var
DownloadMinecraft : TDownloadVersion;
DownloadTask : TDownloadTask;
UnZipper : TExtractZip;
ForgeJarFile, ForgeJsonFile, VanillaJsonFile, VanillaJarFile, MCVersion, UUID, FName: String;
ForgeJFile, VanillaJFile : ISuperObject;
LibArrayVanilla, LibArrayForge : TSuperArray;
i : Integer;
FileStream : TFileStream;
begin
  ForgeJarFile := TForgeLaunch(Command).Forge.getJarFileName;
  ForgeJsonFile := TForgeLaunch(Command).Forge.getJsonFileName;
  MCVersion := TForgeLaunch(Command).Forge.MV;
  UUID := TForgeLaunch(Command).Forge.UUID;
  Bar.StartStep(7);
  if not FileExists(ForgeJarFile) then
  begin
    DownloadMinecraft := TDownloadVersion.Create(Command);
    DownloadMinecraft.setLog(Log);
    Task.runTask(DownloadMinecraft, nil);
    Bar.StepPos := 1;
    Log.log('Downloaded Vanilla Minecraft');
    VanillaJarFile := DownloadFolder + 'versions\' + MCVersion + '\' + MCVersion + '.jar';
    ForceDirectories(ExtractFilePath(ForgeJarFile));
    if FileExists(VanillaJarFile) then
      CopyFile(PWideChar(VanillaJarFile), PWideChar(ForgeJarFile), False);
    Bar.StepPos := 2;
  end;

  VanillaJsonFile := DownloadFolder + 'versions\' + MCVersion + '\' + MCVersion + '.json';
  if not FileExists(ForgeJsonFile) then
  begin
      FName := MCVersion + '-' + UUID;
      if TForgeLaunch(Command).Forge.Branch <> '' then
        FName := FName + '-' + TForgeLaunch(Command).Forge.Branch;
      DownloadTask := TDownloadTask.Create('http://files.minecraftforge.net/maven/net/minecraftforge/forge/' + FName + '/forge-' + FName + '-universal.jar',
      DownloadFolder + 'forge-universal.jar');
      DownloadTask.setLog(Log);
      if DownloadTask.downloadFile(nil) then
      begin
        Bar.StepPos := 3;
        ForceDirectories(TempFolder);
        UnZipper := TExtractZip.Create(DownloadFolder + 'forge-universal.jar', TempFolder);
        UnZipper.setLog(Self.Log);
        UnZipper.Exclude.ListType := ltWhite;
        UnZipper.Exclude.Add('version.json');
        UnZipper.runTask(nil);
        UnZipper.Destroy;
        Bar.StepPos := 4;
        if FileExists(TempFolder + 'version.json') then
          RenameFile(TempFolder + 'version.json', ForgeJsonFile);
        DeleteFolder(TempFolder);
        Bar.StepPos := 5;

      if FileExists(VanillaJsonFile) and FileExists(ForgeJsonFile) then
      begin
        ForgeJFile := TSuperObject.ParseFile(ForgeJsonFile, false);
        if ForgeJFile.S['inheritsFrom'] = MCVersion then
        begin
          VanillaJFile := TSuperObject.ParseFile(VanillaJsonFile, false);
          LibArrayVanilla := VanillaJFile.A['libraries'];
          LibArrayForge := ForgeJFile.A['libraries'];
          for i := 0 to LibArrayVanilla.Length-1 do
            LibArrayForge.Add(LibArrayVanilla[i]);
          FileStream := TFileStream.Create(ForgeJsonFile, fmCreate);
          ForgeJFile.SaveTo(FileStream, True);
          FileStream.Destroy;
        end;
      end;
    end;
    if FileExists(DownloadFolder + 'forge-universal.jar') then
      DeleteFile(PWideChar(DownloadFolder + 'forge-universal.jar'));
  end;
  Bar.FinishStep;
  Command.MCVersion := MCVersion + '-Forge' + UUID;
end;


end.
