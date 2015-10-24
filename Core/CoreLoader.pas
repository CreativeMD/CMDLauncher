unit CoreLoader;

interface

uses System.SysUtils, SaveFileUtils, mysql, JclShell, ShlObj, Vcl.Dialogs;

procedure LoadCore;

var
ProgramFolder, AssetsFolder, TempFolder, InstanceFolder, LibFolder,
DownloadFolder, MinecraftFolder : String;
ProgramSettings : TSaveFile;

const
ProgramName : String = 'CMDLauncher';
ProgramVersion : String = '2.6.0';

implementation

uses Logger, LauncherStartup;

procedure LoadCore;
begin
  ProgramFolder := ExtractFilePath(ParamStr(0));
  AssetsFolder := ProgramFolder + 'Assets\';
  InstanceFolder := ProgramFolder + 'Instances\';
  ForceDirectories(InstanceFolder);
  LibFolder := ProgramFolder + 'Lib\';
  DownloadFolder := ProgramFolder + 'Download\';
  TempFolder := DownloadFolder + 'Temp\';
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
end;

end.
