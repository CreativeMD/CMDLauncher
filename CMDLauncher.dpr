program CMDLauncher;

uses
  Vcl.Forms,
  ceflib,
  Overview in 'Forms\Overview.pas' {OverviewF},
  CoreLoader in 'Core\CoreLoader.pas',
  Logger in 'Core\Logger.pas',
  SaveFileUtils in 'Utils\SaveFileUtils.pas',
  StringUtils in 'Utils\StringUtils.pas',
  Task in 'Utils\Task\Task.pas',
  LoadingForm in 'Forms\LoadingForm.pas' {LoadingScreen},
  LauncherStartup in 'Core\LauncherStartup.pas',
  DatabaseConnection in 'Core\DatabaseConnection.pas',
  Settings in 'Forms\Settings.pas' {SettingsForm},
  ForgeUtils in 'Utils\Minecraft\ForgeUtils.pas',
  InstanceUtils in 'Utils\Minecraft\InstanceUtils.pas',
  VanillaUtils in 'Utils\Minecraft\VanillaUtils.pas',
  ModpackUtils in 'Utils\Minecraft\ModpackUtils.pas',
  ModUtils in 'Utils\Minecraft\ModUtils.pas',
  mysql in 'Libaries\mysql.pas',
  IconUtils in 'Utils\IconUtils.pas',
  FileUtils in 'Utils\FileUtils.pas',
  SettingUtils in 'Utils\SettingUtils.pas',
  LauncherSettings in 'Common\LauncherSettings.pas',
  superobject in 'Libaries\superobject.pas',
  superxmlparser in 'Libaries\superxmlparser.pas',
  DownloadUtils in 'Utils\DownloadUtils.pas',
  GraphicHintLib in 'Libaries\GraphicHintLib.pas',
  InstanceSettings in 'Common\InstanceSettings.pas',
  AccountUtils in 'Utils\AccountUtils.pas',
  IconPicker in 'Forms\IconPicker.pas' {IconPick},
  MinecraftStartup in 'Common\Minecraft\MinecraftStartup.pas',
  Console in 'Forms\Console.pas' {ConsoleF},
  AssetUtils in 'Utils\Minecraft\Launch\AssetUtils.pas',
  LaunchTaskUtils in 'Utils\Minecraft\Launch\LaunchTaskUtils.pas',
  MinecraftVersionFileUtils in 'Utils\Minecraft\Launch\MinecraftVersionFileUtils.pas',
  MinecraftLibaryUtills in 'Utils\Minecraft\Launch\MinecraftLibaryUtills.pas',
  ZipUtils in 'Utils\ZipUtils.pas',
  MinecraftLaunchCommand in 'Common\Minecraft\MinecraftLaunchCommand.pas',
  JavaUtils in 'Utils\JavaUtils.pas',
  LoginForm in 'Forms\LoginForm.pas' {LoginF},
  OfflineForm in 'Forms\OfflineForm.pas' {OfflineF},
  ServerUtils in 'Utils\Minecraft\ServerUtils.pas',
  ForgeInstallation in 'Common\Minecraft\ForgeInstallation.pas',
  SortingUtils in 'Utils\SortingUtils.pas',
  Vcl.Themes,
  Vcl.Styles,
  System.Classes,
  FTBUtils in 'Utils\Minecraft\External\FTBUtils.pas',
  TechnicUtils in 'Utils\Minecraft\External\TechnicUtils.pas',
  ResourcePackUtils in 'Utils\Minecraft\Custom\ResourcePackUtils.pas',
  ShaderPackUtils in 'Utils\Minecraft\Custom\ShaderPackUtils.pas',
  ModSettings in 'Common\Minecraft\ModSettings.pas',
  ModSelectForm in 'Forms\Minecraft\ModSelectForm.pas' {ModSelectF},
  FileDownload in 'Forms\Minecraft\FileDownload.pas' {DownloaderF},
  ModpackSettings in 'Common\Minecraft\ModpackSettings.pas',
  Cauldron in 'Utils\Minecraft\Special\Cauldron.pas',
  SideUtils in 'Utils\Minecraft\SideUtils.pas',
  ConsoleServer in 'Forms\ConsoleServer.pas' {ConsoleServerF},
  CustomSettings in 'Utils\Settings\CustomSettings.pas',
  SnapshotUtils in 'Utils\Minecraft\SnapshotUtils.pas',
  LauncherException in 'Forms\LauncherException.pas' {ErrorDialog},
  Bukkit in 'Utils\Minecraft\Special\Bukkit.pas',
  SpongeForge in 'Utils\Minecraft\Special\SpongeForge.pas',
  BuildUtils in 'Utils\Minecraft\BuildUtils.pas',
  ImportMinecraft in 'Forms\Minecraft\ImportMinecraft.pas' {Importer},
  CommandUtils in 'Utils\CommandUtils.pas',
  ShaderPackSelect in 'Forms\Minecraft\ShaderPackSelect.pas' {frmShaderPackSelect},
  FileListener in 'Common\FileListener.pas',
  URLProtocolUtils in 'Utils\URLProtocolUtils.pas',
  System.SysUtils,
  Vcl.Dialogs,
  ImportUtils in 'Utils\ImportUtils.pas',
  CreativeMD in 'Forms\CreativeMD.pas' {Credit},
  LaunchHandler in 'Core\LaunchHandler.pas',
  FontUtils in 'Utils\FontUtils.pas',
  Setup in 'Forms\Setup.pas' {SetupAssistant},
  ResourcePackSelect in 'Forms\Minecraft\ResourcePackSelect.pas' {ResourceSelect};

{$R *.res}
var
i : Integer;
args : TStringList;
isOpened : Boolean;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  args := TStringList.Create;
  for i := 1 to PARAMCOUNT do
    args.Add(ParamStr(i));
  CoreLoader.LoadCore(args);

  if args.Count > 0 then
  begin
    ForceDirectories(ExtractFilePath(CommunicationFile));
    args.SaveToFile(CommunicationFile);
  end;
  args.Destroy;

  try
    isOpened := processExists(ProgramFile);
  except
    on E : Exception do
    begin
      Logger.MainLog.log(E.Message);
      Logger.MainLog.log(E.StackTrace);
      isOpened := False;
    end;
  end;

  if isOpened then
  begin
    Application.Terminate;
  end
  else
  begin
    ceflib.CefResourcesDirPath := LibFolder + 'Chrome\';
    ceflib.CefLocalesDirPath := LibFolder + 'Chrome\locales\';
    ceflib.CefLibrary := LibFolder + 'Chrome\libcef.dll';
    //ceflib.CefLogFile := LibFolder + 'Chrome\log.txt';
    ceflib.CefLogSeverity := TCefLogSeverity.LOGSEVERITY_ERROR;
    ceflib.CefSingleProcess := False;
    ceflib.CefBrowserSubprocessPath := ProgramFolder + 'CMDChromeBrowser.exe';

    TStyleManager.TrySetStyle('Windows10');
  Application.Title := 'CMDLauncher';
    Application.CreateForm(TOverviewF, OverviewF);
  Application.CreateForm(TLoadingScreen, LoadingScreen);
  Application.CreateForm(TCredit, Credit);
  Application.CreateForm(TSetupAssistant, SetupAssistant);
  Application.Run;
  end;

end.
