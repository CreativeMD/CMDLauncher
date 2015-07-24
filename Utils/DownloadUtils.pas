unit DownloadUtils;

interface

uses Task, ProgressBar, idHttp, IdSSLOpenSSL, System.Classes, IdException,
IdExceptionCore, System.SysUtils, IdComponent;

type
  TDownloadTask = class(TTask)
    private
      Bar : TCMDProgressBar;
      DownloadLink, DownloadPath : String;
      ForceDownload : Boolean;
      MaxWork, LastPercent : Integer;
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      constructor Create(DownloadLink, DownloadPath : String; ForceDownload : Boolean = True);
      function downloadFile(Bar : TCMDProgressBar) : Boolean;
      procedure IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
      procedure IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
      procedure IdHTTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  end;

implementation

uses Logger, DatabaseConnection, FileUtils;

const
download_tries : Integer = 5;

procedure TDownloadTask.IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  LastPercent := 0;
  if Bar <> nil then
    Bar.StartStep(AWorkCountMax)
  else
    MaxWork := AWorkCountMax;
end;

procedure TDownloadTask.IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
var
NewPercent : Integer;
begin
  if Bar <> nil then
    Bar.StepPos := AWorkCount
  else if MaxWork > 0 then
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

procedure TDownloadTask.runTask(Bar : TCMDProgressBar);
var
Http : TIdHTTP;
Tries : Integer;
FileStream : TFileStream;
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
  Http.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  ForceDirectories(ExtractFilePath(DownloadPath));
  FileStream := TFileStream.Create(DownloadPath, fmCreate);
  Tries := 0;
  repeat
    try
      Self.Log.log('Downloading ' + ExtractFileName(DownloadPath));
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
end;

constructor TDownloadTask.Create(DownloadLink, DownloadPath : String; ForceDownload : Boolean = True);
begin
  inherited Create(ExtractFilePath(DownloadPath), True);
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
