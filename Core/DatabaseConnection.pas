unit DatabaseConnection;

interface

uses Task, ProgressBar, mysql, Winapi.WinInet, Winapi.Windows, System.SysUtils, Vcl.Forms;

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
  try
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
  except
    on E: Exception do
      Exit(False);
  end;
end;

constructor TConnect.Create;
begin
  inherited Create('Connecting to Database', False);
end;

function IsConnected: Boolean;
const
  // Local system has a valid connection to the Internet, but it might or might
  // not be currently connected.
  INTERNET_CONNECTION_CONFIGURED = $40;

  // Local system uses a local area network to connect to the Internet.
  INTERNET_CONNECTION_LAN = $02;

  // Local system uses a modem to connect to the Internet
  INTERNET_CONNECTION_MODEM = $01;

  // Local system is in offline mode.
  INTERNET_CONNECTION_OFFLINE = $20;

  // Local system uses a proxy server to connect to the Internet
  INTERNET_CONNECTION_PROXY = $04;

  // Local system has RAS installed.
  INTERNET_RAS_INSTALLED = $10;

var
  InetState: DWORD;
  hHttpSession, hReqUrl: HInternet;
begin
  Result:= InternetGetConnectedState(@InetState, 0);
  if (
    Result
    and
    (
      InetState and INTERNET_CONNECTION_CONFIGURED
        = INTERNET_CONNECTION_CONFIGURED)
    ) then
  begin
    // so far we ONLY know there's a valid connection. See if we can grab some
    // known URL ...
    hHttpSession:= InternetOpen(
      PChar(Application.Title), // this line is the agent string
      INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0
    );
    try
      hReqUrl:= InternetOpenURL(
        hHttpSession,
        PChar('http://www.example.com'{ the URL to check }),
        nil,
        0,
        0,
        0
      );
      Result := hReqUrl <> nil;
      InternetCloseHandle(hReqUrl);
    finally
      InternetCloseHandle(hHttpSession);
    end;
  end
  else
    if (
      InetState and INTERNET_CONNECTION_OFFLINE = INTERNET_CONNECTION_OFFLINE
    ) then
      Result := False; // we know for sure we are offline.
end;

procedure TConnect.runTask(Bar : TCMDProgressBar);
begin
  Bar.StartStep(1);
  DatabaseConnection.online := IsConnected;
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
