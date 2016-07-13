unit CoreLoaderUpdate;

interface

uses System.SysUtils, Vcl.Dialogs, System.Classes, LaunchHandler;

procedure LoadCore(args : TStringList);

var
ProgramFolder, ProgramFile : String;

const
ProgramName : String = 'CMDUpdate';
ProgramVersion : String = '1.0.0';

implementation

uses Logger, StringUtils, DatabaseConnection;

procedure LoadCore(args : TStringList);
begin
  ProgramFile := ParamStr(0);
  ProgramFolder := ExtractFilePath(ProgramFile);

  MainLog := TLog.Create;

  if args.Count > 0 then
    MainLog.log('Launching CMDUpdate with following args ' + Implode(args, ' '));

  DatabaseConnection.online := DatabaseConnection.IsConnected;
end;

end.
