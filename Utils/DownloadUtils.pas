unit DownloadUtils;

interface

uses Task, ProgressBar, idHttp, IdSSLOpenSSL, System.Classes, IdException,
IdExceptionCore, System.SysUtils, IdComponent, idGlobal, IdZLibCompressorBase,
IdBaseComponent, IdSSL, IdSSLOpenSSLHeaders, IdCTypes;

type
  TDownloadTask = class(TTask)
    private
      Bar : TCMDProgressBar;
      DownloadLink, DownloadPath : String;
      ForceDownload : Boolean;
      MaxWork, LastPercent : Integer;
      HTTP: TIdHTTP;
      procedure OnStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
      function downloadExternally : Boolean;
    public
      constructor Create(DownloadLink, DownloadPath : String; ForceDownload : Boolean = True);
      function downloadFile(Bar : TCMDProgressBar) : Boolean;
      procedure IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
      procedure IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
      procedure IdHTTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  end;

implementation

uses Logger, DatabaseConnection, FileUtils, FileDownload;

const
download_tries : Integer = 5;

procedure TDownloadTask.IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  LastPercent := 0;
  if Bar <> nil then
    Bar.StartStep(AWorkCountMax);

  MaxWork := AWorkCountMax;
end;

procedure TDownloadTask.IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
var
NewPercent : Integer;
begin
  if Bar <> nil then
    Bar.StepPos := AWorkCount;

  if MaxWork > 0 then
  begin
    NewPercent := Round(AWorkCount/MaxWork*100);
    if NewPercent <> LastPercent then
    begin
      LastPercent := NewPercent;
      Log.logLastLine('Downloading ' + ExtractFileName(DownloadPath) + ' ' + IntToStr(LastPercent) + '%');
    end;
  end;
end;

procedure TDownloadTask.IdHTTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if Bar <> nil then
    Bar.FinishStep;
end;

procedure TDownloadTask.OnStatusInfoEx(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT;
  const AType, AMsg: String);
begin
  SSL_set_tlsext_host_name(AsslSocket, HTTP.Request.Host);
end;

procedure TDownloadTask.runTask(Bar : TCMDProgressBar);
var
Tries : Integer;
FileStream : TFileStream;
Socket : TIdSSLIOHandlerSocketOpenSSL;
begin
  if not ForceDownload and (FileExists(DownloadPath)) then
  begin
    if FileIsReadOnly(DownloadPath) then
    begin
      Log.log(ExtractFileName(DownloadPath) + ' is in readonly mode');
      Exit;
    end;

    if GetSizeOfFile(DownloadPath) > 0 then
      Exit ;
  end;

  if not DataBaseConnection.online then
    Exit;

  Http := TIdHTTP.Create;
  Http.OnWorkBegin := IdHTTPWorkBegin;
  Http.OnWork := IdHTTPWork;
  Http.OnWorkEnd := IdHTTPWorkEnd;
  Http.Request.UserAgent := 'Mozilla/5.0';

  Self.Bar := Bar;
  Socket := TIdSSLIOHandlerSocketOpenSSL.Create(Http);
  Socket.OnStatusInfoEx := Self.OnStatusInfoEx;
  Socket.SSLOptions.Method := sslvSSLv23;
  Socket.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_1, sslvTLSv1];
  Http.IOHandler := Socket;
  ForceDirectories(ExtractFilePath(DownloadPath));
  FileStream := TFileStream.Create(DownloadPath, fmCreate);

  Logger.MainLog.log('Downloading FileName=' + DownloadPath + ';DownloadLink=' + DownloadLink);

  Tries := 0;
  repeat
    try
      Self.Log.log('Downloading ' + ExtractFileName(DownloadPath));
      Http.HandleRedirects := True;
      Http.Get(DownloadLink, FileStream);
      Tries := 10;
      FileStream.Destroy;
      FileStream := nil;
    except
      on E : EIdException do
      begin
        if (E is EIdConnClosedGracefully) or (E is EIdSilentException)
        or (E is EIdNotAllBytesSent) or (E is EIdConnectException)then
        begin
          Self.Log.logLastLine('Could not download file=' + DownloadPath + ' DownloadLink=' + DownloadLink  + ' Exception=' + E.Message + ' ' + InttoStr(Tries) + ' tries so far!');
          Tries := Tries + 1;
        end
        else if E.Message = 'Socket Error # 10054'#$D#$A'Connection reset by peer.' then
        begin
          FileStream.Destroy;
          FileStream := nil;
          if downloadExternally then
            Tries := 10
          else
            Tries := 6;
        end
        else
          Tries := 6;
      end
      else
        Tries := 6;

    end;
  until (Tries > 5);

  if Assigned(FileStream) then
    FileStream.Destroy;
  
  if Tries = 6 then
  begin
    if FileExists(DownloadPath) then
      DeleteFile(DownloadPath);
    Self.Log.logLastLine('Failed to fulfill the download task file=' + DownloadPath + ' DownloadLink=' + DownloadLink);
  end;

  if Tries = 10 then
  begin
    Self.Log.logLastLine('Downloaded ' + ExtractFileName(DownloadPath) + ' successfully');
  end;

  Http.Free;
end;

function TDownloadTask.downloadExternally : Boolean;
var
Downloader : TDownloaderF;
DResult : TDownloadR;
begin
  Downloader := TDownloaderF.Create(nil);
  Downloader.DownloadBar.StartProcess(1);
  Downloader.lblProgress.Caption := 'Downloading ' + ExtractFileName(DownloadPath);
  DResult := Downloader.downloadItem(TDownloadItem.Create(DownloadLink, DownloadPath, True, False));
  if DResult = drCancel then
  begin
    Downloader.Destroy;
    Exit(False);
  end;
  if DResult = drFail then
  begin
    Downloader.Destroy;
    Exit(False);
  end;

  if DResult = drSuccess then
  begin
    Downloader.Destroy;
    Exit(True);
  end;
end;

constructor TDownloadTask.Create(DownloadLink, DownloadPath : String; ForceDownload : Boolean = True);
begin
  inherited Create(ExtractFileName(DownloadPath), True);
  Self.DownloadLink := DownloadLink;
  Self.DownloadPath := DownloadPath;
  Self.ForceDownload := ForceDownload;
end;

function TDownloadTask.downloadFile(Bar : TCMDProgressBar) : Boolean;
begin
  runTask(Bar);
  Result := FileExists(DownloadPath);
end;

end.
