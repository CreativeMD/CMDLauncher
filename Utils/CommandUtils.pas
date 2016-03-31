unit CommandUtils;

interface

uses System.Generics.Collections, StringUtils, System.Classes, Vcl.Forms, System.SysUtils, Vcl.Dialogs, Vcl.FileCtrl,
JavaUtils;

type
TProcessCommand = reference to function(args : TStringList) : String;
TCommand = class
  Process : TProcessCommand;
  Name : String;
  constructor Create(Name : string; Process : TProcessCommand);
end;

procedure loadLauncherCommands;
procedure registerCommand(Command : TCommand);
function processCommand(Command : String) : String; overload;
function processCommand(Args : TStringList) : String; overload;

var
Commands : TList<TCommand>;

implementation
uses Overview, Task;

function processCommand(Args : TStringList) : String;
var
i : Integer;
begin
  if Args.Count > 0 then
  begin
    for i := 0 to Commands.Count-1 do
      if Commands[i].Name = Args[0] then
        Exit(Commands[i].Process(Args));
  end;
  Exit('Could not find command!');
end;

function processCommand(Command : String) : String;
begin
  Result := processCommand(Explode(Command.ToLower, ' '));
end;

procedure registerCommand(Command : TCommand);
begin
  Commands.Add(Command);
end;

procedure loadLauncherCommands;
begin
  Commands := TList<TCommand>.Create;
  registerCommand(TCommand.Create('tasks', function(args : TStringList) : String
  var
    i: Integer;begin
    Result :=  'Found ' + IntToStr(MultiTasks.Count) + ' Tasks';
    for i := 0 to MultiTasks.Count-1 do
      Result := sLineBreak + MultiTasks[i].ClassName;
  end));
  registerCommand(TCommand.Create('reload', function(args : TStringList) : String
     begin
     Result := 'Reloading Launcher';
    OverviewF.lblRetryClick(OverviewF.lblRetry);
  end));
  registerCommand(TCommand.Create('help', function(args : TStringList) : String

  var
    i: Integer;begin
    Result := '';
    for i := 0 to Commands.Count-1 do
    begin
      if i > 0 then
        Result := Result + ',';
      Result := Result + Commands[i].Name;
    end;
  end));
  registerCommand(TCommand.Create('exit', function(args : TStringList) : String
  begin
    Result := 'Terminating ...';
    Application.Terminate;
  end));
  registerCommand(TCommand.Create('terminate', function(args : TStringList) : String
  begin
    Result := 'Terminating ...';
    Application.Terminate;
  end));
  registerCommand(TCommand.Create('test', function(args : TStringList) : String
  begin
    ShowMessage('Hello World!');
    Result := 'Hello World!';
  end));
  registerCommand(TCommand.Create('java-create', function(args : TStringList) : String
  var
    directory : string;
    options : TSelectDirOpts;
    Java : TJava;
  begin
    if (args.Count > 1) and ((args[1] = '64') or (args[1] = '32')) then
    begin
      if SelectDirectory(directory, options, 0) then
      begin
        Java := TJava.Create(ExtractLastFolder(directory), directory + '\bin\javaw', args[1] = '64');
        JavaVersions.Add(Java);
        Result := 'Created Java Version -> ' + Java.Title;
      end;
    end
    else
      Result := 'java-create <32/64>';
  end));
end;

constructor TCommand.Create(Name : string; Process : TProcessCommand);
begin
  Self.Name := Name;
  Self.Process := Process;
end;

end.
