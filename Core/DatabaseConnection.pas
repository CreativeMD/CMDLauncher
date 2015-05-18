unit DatabaseConnection;

interface

uses Task, ProgressBar, mysql, Winapi.WinInet, Winapi.Windows, System.SysUtils;

type
  TConnect = class(TTask)
    constructor Create;
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
  end;

function CheckUrl(url:string):boolean;

var
online : Boolean;

implementation

uses Logger, Overview;

function CheckUrl(url:string):boolean;
var
hSession, hfile: hInternet;
dwindex,dwcodelen :dword;
dwcode:array[1..20] of char;
res : pchar;
begin
  if pos('http://',url.ToLower)=0 then
    url := 'http://'+url;
  Result := false;
  hSession := InternetOpen('InetURL:/1.0',
  INTERNET_OPEN_TYPE_PRECONFIG,nil, nil, 0);
  if assigned(hsession) then
  begin
    hfile := InternetOpenUrl(hsession,pchar(url),nil,0,INTERNET_FLAG_RELOAD,0);
    dwIndex := 0;
    dwCodeLen := 10;
    HttpQueryInfo(hfile, HTTP_QUERY_STATUS_CODE,@dwcode, dwcodeLen, dwIndex);
    res := pchar(@dwcode);
    result:= (res ='200') or (res ='302');
    if assigned(hfile) then
      InternetCloseHandle(hfile);
    InternetCloseHandle(hsession);
  end;
end;

constructor TConnect.Create;
begin
  inherited Create('Connecting to Database', False);
end;

procedure TConnect.runTask(Bar : TCMDProgressBar);
var
origin : cardinal;
begin
  Bar.StartStep(1);
  origin :=
    INTERNET_CONNECTION_MODEM +
    INTERNET_CONNECTION_LAN +
    INTERNET_CONNECTION_PROXY;
  DatabaseConnection.online := InternetGetConnectedState(@origin,0);
  if DatabaseConnection.online then
  begin
    Logger.Log.log('Connected to Database successfully! Launcher is running in online mode!');
    OverviewF.lblNotify.Caption := 'Launcher is running in online mode.';
    OverviewF.lblNotify.Hint := 'You have fully access to all elements of this launcher.';
    OverviewF.lblNotify.Left := OverviewF.lblRetry.Left;
    OverviewF.lblRetry.Visible := False;
  end
  else
  begin
    Logger.Log.log('Failed to connect to Database! Launcher is running in offline mode!');
    OverviewF.lblNotify.Caption := 'Launcher is running in offline mode.';
    OverviewF.lblNotify.Hint := 'Some elements of this launcher are unaccessible.';
    OverviewF.lblNotify.Left := OverviewF.lblRetry.Left + OverviewF.lblRetry.Width + 5;
    OverviewF.lblRetry.Visible := True;
  end;
  Bar.FinishStep;
end;

end.
