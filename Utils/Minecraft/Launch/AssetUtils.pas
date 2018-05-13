unit AssetUtils;

interface

uses LaunchTaskUtils, VanillaUtils, InstanceUtils, ProgressBar, superobject,
System.SysUtils, DownloadUtils, MinecraftLaunchCommand;

type
  TLoadAssets = class(TLaunchTask)
    LaunchTyp : TLaunchTyp;
    CustomJsonPath : String;
    constructor Create(Command : TMinecraftLaunch; LaunchTyp : TLaunchTyp);
    procedure runTask(Bar : TCMDProgressBar); override;
  end;

implementation

uses DatabaseConnection, CoreLoader, MinecraftVersionFileUtils, Task;

constructor TLoadAssets.Create(Command : TMinecraftLaunch; LaunchTyp : TLaunchTyp);
begin
  inherited Create('Loading Assets', Command, False);
  Self.LaunchTyp := LaunchTyp;
  CustomJsonPath := '';
end;

procedure TLoadAssets.runTask(Bar : TCMDProgressBar);
var
Assets,AssetsO : ISuperObject;
AssetsFile, FileName, AssetsIndex, VersionFile, InheritsFileName : string;
DownloadTask : TDownloadTask;
DownloadVanilla : TDownloadVersion;
item: TSuperObjectIter;
zahlAssets : Integer;
json : ISuperObject;
begin
  AssetsIndex := Command.getMCVersion;
  if CustomJsonPath <> '' then
    VersionFile := CustomJsonPath
  else
    VersionFile := DownloadFolder + 'versions\' + Command.getMCVersion + '\' + Command.getMCVersion + '.json';

  if FileExists(VersionFile) then
  begin
    Assets := TSuperObject.ParseFile(VersionFile, true);
    if Assets.S['assets'] <> '' then
      AssetsIndex := Assets.S['assets']
    else if Assets.S['inheritsFrom'] <> '' then
    begin
      DownloadVanilla := TDownloadVersion.Create(Assets.S['inheritsFrom']);
      Task.runTask(DownloadVanilla, nil);
      InheritsFileName := DownloadFolder + 'versions\' + Assets.S['inheritsFrom'] + '\' + Assets.S['inheritsFrom'] + '.json';
      if FileExists(InheritsFileName) then
      begin
        AssetsO := TSuperObject.ParseFile(InheritsFileName, true);
        if AssetsO.S['assets'] <> '' then
          AssetsIndex := AssetsO.S['assets'];
      end;
    end;
  end;

  if LaunchTyp = ltOld then
    AssetsIndex := 'legacy';

  AssetsFile := DownloadFolder + 'assets\indexes\' + AssetsIndex + '.json';
  Command.Replacements.Add('${assets_index_name}', AssetsIndex);

  if DatabaseConnection.online then
  begin
    FileName := DownloadFolder + 'versions\' + Command.getMCVersion + '\' + Command.getMCVersion + '.json';
    if FileExists(FileName) then
    begin
      json := TSuperObject.ParseFile(FileName, true);
      json := json.O['assetIndex'];
      DownloadTask := TDownloadTask.Create(json.S['url'], AssetsFile, False);
      DownloadTask.setLog(Self.Log);
      DownloadTask.downloadFile(nil);
      DownloadTask.Destroy;
    end
    else
      Self.Log.log('Could not find "' + Command.getMCVersion + '.json"!');

  end;

  if FileExists(AssetsFile) then
  begin
    Assets := TSuperObject.ParseFile(AssetsFile, true);
    AssetsO := Assets.O['objects'];

    Bar.StartStep(AssetsO.AsString.CountChar('}')+1);
    zahlAssets := 0;
    if ObjectFindFirst(AssetsO, item) then
    repeat
      if LaunchTyp = ltNew then
        FileName := DownloadFolder + 'assets\objects\' + Copy(item.val.S['hash'], 1, 2) + '\' + item.val.S['hash']
      else
        FileName := DownloadFolder + 'assets\virtual\legacy\' + item.key.Replace('/', '\');

      if DatabaseConnection.online then
      begin
        DownloadTask := TDownloadTask.Create('http://resources.download.minecraft.net/' + Copy(item.val.S['hash'], 1, 2) + '/' + item.val.S['hash'], FileName, False);
        DownloadTask.setLog(Self.Log);
        DownloadTask.downloadFile(nil);
        DownloadTask.Destroy;
      end;

      Bar.StepPos := zahlAssets;
      zahlAssets := zahlAssets + 1;
    until not ObjectFindNext(item);
    ObjectFindClose(item);
  end;
  Bar.FinishStep;
end;


end.
