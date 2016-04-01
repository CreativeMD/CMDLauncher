unit FileListener;

interface

uses Logger, System.Classes, Vcl.Forms, System.SysUtils, IdURI;

type
  TInteralCommunicator = class(TThread)
    args : TStringList;
    procedure Execute; override;
    procedure onReceiveMessage;
  end;

procedure createListener;


var
Communicator : TInteralCommunicator;

implementation

uses CoreLoader, CommandUtils, StringUtils, Overview, LauncherStartup;

procedure createListener;
begin
  Communicator := TInteralCommunicator.Create;
  //Communicator.Start;
end;

procedure TInteralCommunicator.onReceiveMessage;
var
  i: Integer;
  command : String;
begin
  MainLog.log('Received new interal messages');
  MainLog.log(args);
  for i := 0 to args.Count-1 do
  begin
    command := TIdURI.URLDecode(args[i]).Replace('cmdlauncher://', '');
    if command.EndsWith('/') then
      command := command.Substring(0, Length(command)-1);

    MainLog.log(CommandUtils.processCommand(command));
  end;

end;

procedure TInteralCommunicator.Execute;
var
args : TStringList;
begin
  while not Application.Terminated do
  begin
    if LoadedLauncher and (OverviewF <> nil) and (OverviewF.BackgroundTask <> nil) and (not Assigned(OverviewF.BackgroundTask.CurrentTask)) then
    begin
      if not HasFinishedStartup then
        onPostStartupHasFinished;
      if FileExists(CommunicationFile) then
      begin
        args := TStringList.Create;
        args.LoadFromFile(CommunicationFile);
        DeleteFile(CommunicationFile);
        Self.args := args;
        Synchronize(onReceiveMessage);
        args.Destroy;

      end;
    end;
    Sleep(1000);
  end;
end;

end.
