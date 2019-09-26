unit FileDownload;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, cefvcl, ceflib,
  Task, Generics.Collections, ModUtils, ForgeUtils, ProgressBar, System.UITypes, StringUtils,
  System.IOUtils, System.Types, LoadingForm, SideUtils;

type
  TDownloadR = (drSuccess, drFail, drCancel, drNotFinished);
  TDownloadItem = class
    URL, FileName : String;
    IgnoreFileName, DownloadToTempFolder : Boolean;
    constructor Create(URL, FileName : String; IgnoreFileName : Boolean = False; DownloadToTempFolder : Boolean = True);
    function isFileName(SuggestedFileName : string): Boolean;
    function getFileName : String;
  end;
  TDownloaderF = class(TForm)
    btnCancel: TButton;
    btnSkip: TButton;
    btnRedo: TButton;
    btnHelp: TButton;
    lblProgress: TLabel;
    DownloadBar: TCMDProgressBar;
    chrmDownloader: TChromium;
    lblFile: TLabel;
    procedure btnRedoClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure chrmDownloadBrowserBeforeDownload(Sender: TObject;
      const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure chrmDownloadBrowserDownloadUpdated(Sender: TObject;
      const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
    procedure chrmDownloaderBeforeBrowse(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean; out Result: Boolean);
    procedure chrmDownloaderBeforeUnloadDialog(Sender: TObject;
      const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback; out Result: Boolean);
  private
    { Private-Deklarationen }
  public
    ForceReload : Boolean;
    DownloadResult : TDownloadR;
    Item : TDownloadItem;
    //Item : TPair<TMod, TModVersion>;
    //ModObj : TModInstallObj;
    Progress : TLoadingScreen;
    procedure loadPage;
    function cancelIt : Boolean;
    function downloadItem(DownloadItem : TDownloadItem) : TDownloadR;
    //function downloadModVersion(ModObj : TModInstallObj; Item : TPair<TMod, TModVersion>) : TDownloadR;
  end;


implementation

{$R *.dfm}

uses CoreLoader;

constructor TDownloadItem.Create(URL, FileName : String; IgnoreFileName : Boolean = False; DownloadToTempFolder : Boolean = True);
begin
  Self.URL := URL;
  Self.FileName := FileName;
  Self.IgnoreFileName := IgnoreFileName;
  Self.DownloadToTempFolder := DownloadToTempFolder;
end;

function TDownloadItem.isFileName(SuggestedFileName : string): Boolean;
begin
  Result := IgnoreFileName or (SuggestedFileName.Replace(' ', '').Replace('''', '') = FileName.Replace(' ', ''));
end;

function TDownloadItem.getFileName;
begin
  if DownloadToTempFolder then
    Result := TempFolder + FileName
  else
    Result := FileName;
end;

function TDownloaderF.cancelIt : Boolean;
begin
  Result := MessageDlg('Do you really want to cancel all mods?',mtConfirmation, mbOKCancel, 0) = mrOK;
end;

function TDownloaderF.downloadItem(DownloadItem : TDownloadItem) : TDownloadR;
var
timeToWait : Integer;
WindowHandle : HWND;
begin
  DownloadResult := drNotFinished;
  Self.Item := DownloadItem;

  lblFile.Caption := Item.FileName;

  Progress := nil;

  loadPage;

  while (DownloadResult = drNotFinished) and not Application.Terminated do
  begin
    if ForceReload then
    begin
      timeToWait := 1000;
      while timeToWait > 0 do
      begin
        Application.ProcessMessages;
        timeToWait := timeToWait - 1;
        Sleep(1);
      end;
      loadPage;
    end;
    Application.ProcessMessages;
    Sleep(1);
  end;

  Result := DownloadResult;

  repeat
    WindowHandle := FindWindow('CefBrowserWindow', '');
    if IsWindow(WindowHandle) then
      DestroyWindow(WindowHandle);
    Application.ProcessMessages;
  until not IsWindow(WindowHandle);
end;

{function TModDownloaderF.downloadModVersion(ModObj : TModInstallObj; Item : TPair<TMod, TModVersion>) : TDownloadR;
var
timeToWait : Integer;
WindowHandle : HWND;
begin
  DownloadResult := drNotFinished;
  Self.ModObj := ModObj;
  Self.Item := Item;
  'http://launcher.creativemd.de/service/moddownloadservice.php?modid=' + IntToStr(Item.Key.ID)
  + '&versionID=' + Item.Value.Name + '&url=' + ModObj.DownloadLink

  Progress := nil;

  loadPage;

  while (DownloadResult = drNotFinished) and not Application.Terminated do
  begin
    if ForceReload then
    begin
      timeToWait := 1000;
      while timeToWait > 0 do
      begin
        Application.ProcessMessages;
        timeToWait := timeToWait - 1;
        Sleep(1);
      end;
      loadPage;
    end;
    Application.ProcessMessages;
    Sleep(1);
  end;

  Result := DownloadResult;

  repeat
    WindowHandle := FindWindow('CefBrowserWindow', '');
    if IsWindow(WindowHandle) then
      DestroyWindow(WindowHandle);
    Application.ProcessMessages;
  until not IsWindow(WindowHandle);

  //FindWindow()
  //EnumWindows(@EnumerateWindows, 0);
end;        }

procedure TDownloaderF.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if cancelIt then
    DownloadResult := drCancel;
  CanClose := False;
end;

procedure TDownloaderF.loadPage;
begin
  chrmDownloader.Load(Self.Item.URL);
  ForceReload := False;
end;


procedure TDownloaderF.btnCancelClick(Sender: TObject);
begin
  if cancelIt then
    DownloadResult := drCancel;
end;

procedure TDownloaderF.btnRedoClick(Sender: TObject);
begin
  loadPage;
end;

procedure TDownloaderF.btnSkipClick(Sender: TObject);
begin
  if MessageDlg('Do you really want to cancel the download of this file?' + sLineBreak,mtError, mbOKCancel, 0) = mrOK then
    DownloadResult := drFail;
end;

procedure TDownloaderF.chrmDownloadBrowserBeforeDownload(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
begin
  if not Item.isFileName(suggestedName) then
    ShowMessage('Invalid file! ' + suggestedName + ' does not match to ' + Item.FileName + '.' + sLineBreak
    + 'You do have the option to skip this mod or cancel all mods.')
  else
  begin
    if FileExists(Item.getFileName) then
      DeleteFile(Item.getFileName);
    callback.Cont(Item.getFileName, False);
  end;
end;

procedure TDownloaderF.chrmDownloadBrowserDownloadUpdated(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Progress = nil then
  begin
    Progress := TLoadingScreen.Create(Self);
    Progress.Position := poOwnerFormCenter;
    Progress.lblTask.Caption := 'Downloading ' + Item.FileName + ' ...';
    Progress.lblLog.Caption := '0 / ' + IntToStr(downloadItem.TotalBytes) + ' Bytes (0%)';
    Progress.TaskProgress.StartProcess(1);
    Progress.TaskProgress.StartStep(downloadItem.TotalBytes);
    Progress.Show;
    //Progress.SetFocus;
  end;
  if downloadItem.IsInProgress then
  begin
    Progress.TaskProgress.StepPos := downloadItem.ReceivedBytes;
    Progress.lblLog.Caption := IntToStr(downloadItem.ReceivedBytes) + ' / ' + IntToStr(downloadItem.TotalBytes) + ' Bytes (' + IntToStr(round(downloadItem.ReceivedBytes / downloadItem.TotalBytes * 100)) + '%)';
  end
  else
  begin
    Application.ProcessMessages;
    Progress.TaskProgress.FinishStep;
    DownloadResult := drSuccess;
  end;
end;

procedure TDownloaderF.chrmDownloaderBeforeBrowse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; isRedirect: Boolean; out Result: Boolean);
var
URL : String;

begin
  if String(request.Url).Contains('://www.dropbox.com') then
  begin
    Url := string(request.Url).Replace('dl=0', 'dl=1');

    if string(request.Url).Contains('dl=0') then
    begin
      //request.Url := URL;
      //isRedirect := True;
      frame.LoadUrl(Url);
      //frame.LoadRequest(request);
      Result := True;
    end;
  end;
end;

procedure TDownloaderF.chrmDownloaderBeforeUnloadDialog(Sender: TObject;
  const browser: ICefBrowser; const messageText: ustring; isReload: Boolean;
  const callback: ICefJsDialogCallback; out Result: Boolean);
begin
  if messageText = 'You are about to be redirected.'#$A'In order to reach your destination link, please click to stay on the page and then click our Skip Ad button.' then
  begin
    callback.Cont(True, 'Ok');
    //Result := True;
    ForceReload := True;
  end;
end;

end.
