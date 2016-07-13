program CMDUpdate;

uses
  Vcl.Forms,
  System.Classes,
  Main in 'Main.pas' {MainF},
  superobject in 'Libaries\superobject.pas',
  Task in 'Utils\Task\Task.pas',
  StringUtils in 'Utils\StringUtils.pas',
  SaveFileUtils in 'Utils\SaveFileUtils.pas',
  LaunchHandler in 'Core\LaunchHandler.pas',
  Logger in 'Core\Logger.pas',
  DatabaseConnection in 'Core\DatabaseConnection.pas',
  CoreLoaderUpdate in 'Core\CoreLoaderUpdate.pas';

{$R *.res}
var
i : Integer;
args : TStringList;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  args := TStringList.Create;
  for i := 1 to PARAMCOUNT do
    args.Add(ParamStr(i));
  CoreLoaderUpdate.LoadCore(args);

  Application.CreateForm(TMainF, MainF);
  Application.Run;
end.
