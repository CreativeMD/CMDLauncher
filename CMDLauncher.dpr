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
  FTBUtils in 'Utils\Minecraft\External\FTBUtils.pas',
  TechnicUtils in 'Utils\Minecraft\External\TechnicUtils.pas',
  ResourcePackUtils in 'Utils\Minecraft\Custom\ResourcePackUtils.pas',
  ShaderPackUtils in 'Utils\Minecraft\Custom\ShaderPackUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  CoreLoader.LoadCore;
  ceflib.CefResourcesDirPath := LibFolder + 'Chrome\';
  ceflib.CefLocalesDirPath := LibFolder + 'Chrome\locales\';
  ceflib.CefLibrary := LibFolder + 'Chrome\libcef.dll';
  Application.Title := 'CMDLauncher';
  Application.CreateForm(TOverviewF, OverviewF);
  Application.CreateForm(TLoadingScreen, LoadingScreen);
  Application.Run;
end.
