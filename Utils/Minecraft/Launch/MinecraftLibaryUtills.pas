unit MinecraftLibaryUtills;

interface

uses LaunchTaskUtils, InstanceUtils, ProgressBar, System.SysUtils, superobject,
DownloadUtils, FileUtils, System.Classes, StringUtils, MinecraftLaunchCommand;

type
  TDownLoadLibary = class(TLaunchTask)
    IsServer : Boolean;
    CustomJsonPath, CustomLibaryFolder : String;
    constructor Create(Command : TMinecraftLaunch);
    procedure runTask(Bar : TCMDProgressBar); override;
    function getDJarFileEnd(LibaryString : String) : string; virtual;
    function getDownload(LibaryString : String) : String; virtual;
  end;
  TDownloadFLibary = class(TDownLoadLibary)
    function getDJarFileEnd(LibaryString : String) : string; override;
    function getDownload(LibaryString : String) : String; override;
  end;

implementation

uses CoreLoader, ZipUtils, DatabaseConnection;

function TDownloadFLibary.getDownload(LibaryString : String) : String;
var
LibInfo : TStringList;
begin
  Result := '';
  LibInfo := Explode(LibaryString, ':');
  if LibInfo.Count = 3 then
  begin
    if LibInfo[0].StartsWith('com.typesafe') then
      Result := 'http://central.maven.org/maven2/';
  end;
end;

function TDownloadFLibary.getDJarFileEnd(LibaryString : String) : string;
var
LibInfo : TStringList;
begin
  Result := '';
  LibInfo := Explode(LibaryString, ':');
  if LibInfo.Count = 3 then
  begin
    if LibInfo[1] = 'forge' then
      Result := '-universal';
  end;
  Result := Result + '.jar';
end;

constructor TDownLoadLibary.Create(Command : TMinecraftLaunch);
begin
  inherited Create('Downloading libraries', Command, False);
  IsServer := False;
  CustomJsonPath := '';
  CustomLibaryFolder := '';
end;

function TDownLoadLibary.getDJarFileEnd(LibaryString : String) : string;
begin
  Result := '.jar';
end;

function TDownLoadLibary.getDownload(LibaryString : String) : String;
begin
  Result := '';
end;

procedure TDownLoadLibary.runTask(Bar : TCMDProgressBar);
var
JsonFile, Natives, Extract : ISuperObject;
LibArray, ExcludeArray, rules, Arguments : TSuperArray;
JsonFileName, LibFileName, LibDownloadLink, NativeFolder, Libs, Argument : String;
i, j : Integer;
DownloadTask : TDownloadTask;
LibInfo : TStringList;
Extractor : TExtractZip;
CanDownload : Boolean;
begin
  if CustomJsonPath <> '' then
    JsonFileName := CustomJsonPath
  else
    JsonFileName := DownloadFolder + 'versions\' + Command.getMCVersion + '\' + Command.getMCVersion + '.json';
  if FileExists(JsonFileName) then
  begin
    NativeFolder := DownloadFolder + 'versions\' + Command.getMCVersion + '\natives';
    if DirectoryExists(NativeFolder) and DataBaseConnection.online then
      DeleteFolder(NativeFolder);

    ForceDirectories(NativeFolder);

    JsonFile := TSuperObject.ParseFile(JsonFileName, True);

    LibArray := JsonFile.A['libraries'];

    if not IsServer then
    begin
      Command.SpecialArguments.Add('-Djava.library.path="' + NativeFolder + '"');
      Command.SpecialArguments.Add('-cp');
    end;

    Libs := '';

    Bar.StartStep(LibArray.Length);

    for i := 0 to LibArray.Length-1 do
    begin
      LibInfo := Explode(LibArray[i].S['name'], ':');

      if LibInfo.Count = 3 then
      begin
        if LibArray[i].S['url'] <> '' then
          LibDownloadLink := LibArray[i].S['url']
        else
          LibDownloadLink := 'https://libraries.minecraft.net/';

        if getDownload(LibArray[i].S['name']) <> '' then
          LibDownloadLink := getDownload(LibArray[i].S['name']);

        LibDownloadLink := LibDownloadLink + LibInfo[0].Replace('.', '/') + '/' + LibInfo[1] + '/' + LibInfo[2] + '/' + LibInfo[1] + '-' + LibInfo[2];;


        LibFileName := DownloadFolder + 'libraries\';
        if CustomLibaryFolder <> '' then
          LibFileName := CustomLibaryFolder;
        LibFileName := LibFileName + LibInfo[0].Replace('.', '\') + '\' +
        LibInfo[1] + '\' + LibInfo[2] + '\' + LibInfo[1] + '-' + LibInfo[2];

        Natives := LibArray[i].O['natives'];

        CanDownload := True;

        if Natives <> nil then
        begin
          if Natives.S['windows'] <> '' then
          begin
            LibDownloadLink := LibDownloadLink + '-' + Natives.S['windows'].Replace('${arch}', Command.getArch);
            LibFileName := LibFileName + '-' + Natives.S['windows'].Replace('${arch}', Command.getArch);
          end
          else
            CanDownload := False;
        end;

        LibDownloadLink := LibDownloadLink + getDJarFileEnd(LibArray[i].S['name']);
        LibFileName := LibFileName + '.jar';

        rules := LibArray[i].A['rules'];
        if rules <> nil then
        begin
          for j := 0 to rules.Length-1 do
          begin
            if rules[j].S['action'] = 'allow' then
            begin
              if rules[j].O['os'] <> nil then
                CanDownload := rules[j].O['os'].S['name'] = 'windows';
            end
            else if rules[j].S['action'] = 'disallow' then
              if rules[j].O['os'] <> nil then
                CanDownload := rules[j].O['os'].S['name'] <> 'windows';
          end;
        end;

        if IsServer then
        begin
          CanDownload := LibArray[i].B['serverreq'];
        end;

        DownloadTask := TDownloadTask.Create(LibDownloadLink, LibFileName, False);
        DownloadTask.setLog(Self.Log);
        if CanDownload and DownloadTask.downloadFile(nil) then
        begin
          Extract := LibArray[i].O['extract'];
          if (Extract <> nil) then
          begin
            ExcludeArray := Extract.A['exclude'];
            Extractor := TExtractZip.Create(LibFileName, NativeFolder);
            Extractor.setLog(Self.Log);
            for j := 0 to ExcludeArray.Length-1 do
              Extractor.Exclude.Add(string(ExcludeArray.S[j]));
            Extractor.runTask(nil);
            Extractor.Destroy;
          end
          else
          begin
            if FileExists(LibFileName) then
              Libs := Libs + '"' + LibFileName + '";'
            else
              Self.Log.log('Could not load ' + LibFileName.Replace(DownloadFolder + 'libraries\', ''));
          end;
        end;
        if not FileExists(LibFileName) and CanDownload then
          Self.Log.log('Could not load ' + LibFileName.Replace(DownloadFolder + 'libraries\', ''));
        DownloadTask.Destroy;
      end;
      Bar.StepPos := i;
    end;
    if not IsServer then
    begin
      Command.SpecialArguments.Add(Libs + '"' + DownloadFolder + 'versions\' + Command.getMCVersion + '\' + Command.getMCVersion + '.jar";');
      Command.SpecialArguments.Add(JsonFile.S['mainClass']);
      if JsonFile.O['arguments'] <> nil then
      begin
        Arguments := JsonFile.O['arguments'].A['game'];
        for j := 0 to Arguments.Length-1 do
        begin
          Argument := Arguments.S[j];
          if (Argument <> '') and (not Argument.StartsWith('{')) then
            Command.SpecialArguments.Add(Argument);
        end;
      end
      else
        Command.SpecialArguments.Add(JsonFile.S['minecraftArguments']);
    end;
  end;
  Bar.FinishStep;
end;

end.
