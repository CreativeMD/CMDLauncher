unit CommandUtils;

interface

uses System.Generics.Collections, StringUtils, System.Classes, Vcl.Forms, System.SysUtils;

type
TProcessCommand = reference to function(args : TStringList) : String;
TCommand = class
  Process : TProcessCommand;
  Name : String;
  constructor Create(Name : string; Process : TProcessCommand);
end;

procedure loadLauncherCommands;
procedure registerCommand(Command : TCommand);
function processCommand(Command : String) : String;

var
Commands : TList<TCommand>;

implementation
uses Overview;

function processCommand(Command : String) : String;
var
Args : TStringList;
  i: Integer;
begin
  Args := Explode(Command.ToLower, ' ');
  if Args.Count > 0 then
  begin
    for i := 0 to Commands.Count-1 do
      if Commands[i].Name = Args[0] then
        Exit(Commands[i].Process(Args));
  end;
  Exit('Could not find command!');
end;

procedure registerCommand(Command : TCommand);
begin
  Commands.Add(Command);
end;

procedure loadLauncherCommands;
begin
  Commands := TList<TCommand>.Create;
  registerCommand(TCommand.Create('reload', function(args : TStringList) : String
  begin
    Result := 'Reloading Launcher';
    OverviewF.lblRetryClick(OverviewF.lblRetry);
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
end;

constructor TCommand.Create(Name : string; Process : TProcessCommand);
begin
  Self.Name := Name;
  Self.Process := Process;
end;

end.
