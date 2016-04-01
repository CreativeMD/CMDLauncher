unit CoreLoader;

interface

uses System.SysUtils, SaveFileUtils, mysql, JclShell, ShlObj, Vcl.Dialogs, System.Classes;

procedure LoadCore(args : TStringList);

var
ProgramFolder, ProgramFile, AssetsFolder, TempFolder, InstanceFolder, LibFolder,
DownloadFolder, MinecraftFolder, CommunicationFile : String;
ProgramSettings : TSaveFile;

const
ProgramName : String = 'CMDLauncher';
ProgramVersion : String = '2.6.7';

implementation

uses Logger, LauncherStartup, FileListener, StringUtils;

procedure LoadCore(args : TStringList);
begin
  ProgramFile := ParamStr(0);
  ProgramFolder := ExtractFilePath(ProgramFile);
  AssetsFolder := ProgramFolder + 'Assets\';
  InstanceFolder := ProgramFolder + 'Instances\';
  ForceDirectories(InstanceFolder);
  LibFolder := ProgramFolder + 'Lib\';
  DownloadFolder := ProgramFolder + 'Download\';
  TempFolder := DownloadFolder + 'Temp\';
  CommunicationFile := TempFolder + 'internal.cfg';
  MinecraftFolder := GetSpecialFolderLocation(CSIDL_APPDATA) + '\.minecraft\';
  MainLog := TLog.Create;
  ProgramSettings := TSaveFile.Create(ProgramFolder + 'CMDLauncher.cfg');
  ProgramSettings.setString('version', ProgramVersion);
  try
    mysql.libmysql_fast_load(PChar(LibFolder + 'libmysql.dll'));
  except
    on E: Exception do
    begin
      ShowMessage('Could not load libmysql.dll! Please extract the whole zip!');
      CloseLauncher := True;
    end;
  end;

  try
    FileListener.createListener;
  except
    on E: Exception do
    begin
      MainLog.log('Failed to create listener!');
    end;
  end;

  if args.Count > 0 then
    MainLog.log('Launching CMDLauncher with following args ' + Implode(args, ' '));
end;

end.
