unit ModDownload;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, cefvcl, ceflib,
  Task, Generics.Collections, ModUtils, ForgeUtils, ProgressBar, System.UITypes, StringUtils,
  System.IOUtils, System.Types, LoadingForm;

type
  TDownloadR = (drSuccess, drFail, drCancel, drNotFinished);
  TModDownloaderF = class(TForm)
    btnCancel: TButton;
    btnSkip: TButton;
    btnRedo: TButton;
    btnHelp: TButton;
    lblProgress: TLabel;
    DownloadBar: TCMDProgressBar;
    chrmDownloader: TChromium;
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
    Item : TPair<TMod, TModVersion>;
    ModObj : TModInstallObj;
    Progress : TLoadingScreen;
    procedure loadPage;
    function cancelIt : Boolean;
    function downloadModVersion(ModObj : TModInstallObj; Item : TPair<TMod, TModVersion>) : TDownloadR;
  end;
  TDownloadMods = class(TTask)
    protected
      ModsFolder : String;
      isServer : Boolean;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      Mods : TDictionary<TMod, TModVersion>;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; isServer : Boolean);
  end;
  TModCleaning = class(TTask)
    protected
      ModsFolders : TStringList;
      isServer : Boolean;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      Mods : TDictionary<TMod, TModVersion>;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolders : TStringList; isServer : Boolean); overload;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; isServer : Boolean); overload;
  end;

implementation

{$R *.dfm}

uses CoreLoader;

constructor TModCleaning.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolders : TStringList; isServer : Boolean);
begin
  inherited Create('Clean Mods', True);
  Self.ModsFolders := ModsFolders;
  Self.isServer := isServer;
  Self.Mods := TDictionary<TMod, TModVersion>.Create(Mods);
end;

constructor TModCleaning.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; isServer : Boolean);
begin
  Self.ModsFolders := TStringList.Create;
  Self.ModsFolders.Add(ModsFolder);
  Create(Mods, Self.ModsFolders, isServer);
end;

procedure TModCleaning.runTask(Bar : TCMDProgressBar);
var
Files : TStringList;
i, j: Integer;
Item : TPair<TMod, TModVersion>;
isFileOfMod : Boolean;
begin
  for i := 0 to ModsFolders.Count-1 do
  begin
    if DirectoryExists(ModsFolders[i]) then
    begin
      Files := ArrayToList(TDirectory.GetFiles(ModsFolders[i]));

      Bar.StartStep(Files.Count);
      for j := 0 to Files.Count-1 do
      begin
        isFileOfMod := False;

        for Item in Mods do
          if Item.Value.isModFile(Files[j].Replace(ModsFolders[i], '').Replace('\', '/')) then
          begin
            isFileOfMod := True;
            Break;
          end;

        if not isFileOfMod then
        begin
          DeleteFile(Files[j]);
          Self.Log.log('Deleted ' + Files[j].Replace(ModsFolders[i], '').Replace('\', '/'));
        end;

        Bar.StepPos := j;
      end;
    end;
  end;
  Bar.FinishStep;
end;

function TModDownloaderF.cancelIt : Boolean;
begin
  Result := MessageDlg('Do you really want to cancel all mods?',mtConfirmation, mbOKCancel, 0) = mrOK;
end;

procedure WindowInfo(hWnd: HWND; List: TStrings);
var
  TheClassName: array[0..255] of char;
  TheInstance: cardinal;
begin
  GetClassName(hWnd, TheClassName, 255);
  TheInstance := GetWindowLong(hWnd, GWL_HINSTANCE);
  if (TheInstance <> 0) then begin
    if (TheInstance = hInstance) then
    begin
      if string(TheClassName).Contains('CefBrowserWindow') then
      begin
        DestroyWindow(hWnd);
      end;
    end;
  end;
end;


function EnumerateChildWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
begin
  WindowInfo(hWnd, nil);
  EnumChildWindows(hWnd, @EnumerateChildWindows, 0);
  Result := TRUE;
end;

function EnumerateWindows(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
begin
  Result := TRUE;
  WindowInfo(hWnd, nil);
  EnumChildWindows(hWnd, @EnumerateChildWindows, 0);
end;

function TModDownloaderF.downloadModVersion(ModObj : TModInstallObj; Item : TPair<TMod, TModVersion>) : TDownloadR;
var
timeToWait : Integer;
WindowHandle : HWND;
begin
  DownloadResult := drNotFinished;
  Self.ModObj := ModObj;
  Self.Item := Item;

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
  until not IsWindow(WindowHandle);

  //FindWindow()
  //EnumWindows(@EnumerateWindows, 0);
end;

procedure TModDownloaderF.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if cancelIt then
    DownloadResult := drCancel;
  CanClose := False;
end;

procedure TModDownloaderF.loadPage;
begin
  chrmDownloader.Load('http://launcher.creativemd.de/service/moddownloadservice.php?modid=' + IntToStr(Item.Key.ID)
  + '&versionID=' + Item.Value.Name + '&url=' + ModObj.DownloadLink);
  ForceReload := False;
end;

constructor TDownloadMods.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; isServer : Boolean);
begin
  inherited Create('Downloading Mods', True);
  Self.Mods := TDictionary<TMod, TModVersion>.Create(Mods);
  Self.ModsFolder := ModsFolder;
  Self.sync := True;
  Self.isServer := isServer;
end;

procedure TDownloadMods.runTask(Bar : TCMDProgressBar);
var
Downloader : TModDownloaderF;
Item : TPair<TMod, TModVersion>;
DResult : TDownloadR;
i : Integer;
NeedInstallation, IsModValid : Boolean;
begin
  NeedInstallation := False;
  for Item in Mods do
  begin
    if not Item.Value.isInstalled(ModsFolder) then
    begin
      NeedInstallation := True;
    end;
  end;

  if NeedInstallation then
  begin
    Downloader := TModDownloaderF.Create(nil);
    Downloader.Show;
    Downloader.DownloadBar.StartProcess(Mods.Count);
    Bar.StartStep(Mods.Count);
    for Item in Mods do
    begin
      IsModValid := True;
      if isServer and not Item.Key.isServerCompatible then
        IsModValid := False;

      if not isServer and not Item.Key.isClientCompatible then
        IsModValid := False;

      if IsModValid and not Item.Value.isInstalled(ModsFolder) then
      begin
        for i := 0 to Item.Value.Files.Count-1 do
        begin
          if not Item.Value.Files[i].isInstalled(ModsFolder) then
          begin
            Downloader.lblProgress.Caption := IntToStr(Bar.StepPos+1) + '/' + IntToStr(Mods.Count) + ' Mods';
            DResult := Downloader.downloadModVersion(Item.Value.Files[i], Item);
            if DResult = drCancel then
            begin
              if Downloader.Progress <> nil then
                Downloader.Progress.Destroy;
              Downloader.Destroy;
              Exit;
            end;
            if DResult = drFail then
            begin
              if Downloader.Progress <> nil then
                Downloader.Progress.Destroy;
              Self.Log.log('Failed to download mod! ' + Item.Key.Title);
            end;

            if DResult = drSuccess then
            begin
              Downloader.Progress.lblTask.Caption := 'Installing Mod';
              Application.ProcessMessages;
              Item.Value.Files[i].installObj(TempFolder, ModsFolder);
              Downloader.Progress.Destroy;
            end;
            Downloader.chrmDownloader.ReCreateBrowser('');
            //Downloader.chrmDownloader := TChromium.Create(Downloader);
            //Downloader.chrmDownloader.Parent := Downloader;
          end;
        end;

      end;
      Downloader.DownloadBar.FinishStep;
      Bar.StepPos := Bar.StepPos + 1;
    end;
    Downloader.Destroy;
  end;
  Bar.FinishStep;

end;

procedure TModDownloaderF.btnCancelClick(Sender: TObject);
begin
  if cancelIt then
    DownloadResult := drCancel;
end;

procedure TModDownloaderF.btnRedoClick(Sender: TObject);
begin
  loadPage;
end;

procedure TModDownloaderF.btnSkipClick(Sender: TObject);
begin
  if MessageDlg('Do you really want to cancel the download of this mod?' + sLineBreak,mtError, mbOKCancel, 0) = mrOK then
    DownloadResult := drFail;
end;

procedure TModDownloaderF.chrmDownloadBrowserBeforeDownload(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
begin
  if string(suggestedName).Replace(' ', '').Replace('''', '') <> ModObj.DFileName.Replace(' ', '') then
    ShowMessage('Invalid file! ' + suggestedName + ' does not match to ' + ModObj.DFileName + '.' + sLineBreak
    + 'You do have the option to skip this mod or cancel all mods.')
  else
  begin
    callback.Cont(TempFolder + ModObj.DFileName, False);
  end;
end;

procedure TModDownloaderF.chrmDownloadBrowserDownloadUpdated(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
begin
  if Progress = nil then
  begin
    Progress := TLoadingScreen.Create(Self);
    Progress.Position := poOwnerFormCenter;
    Progress.lblTask.Caption := 'Downloading ' + ModObj.DFileName + ' ...';
    Progress.lblLog.Caption := '0 / ' + IntToStr(downloadItem.TotalBytes) + ' Bytes (0%)';
    Progress.TaskProgress.StartProcess(1);
    Progress.TaskProgress.StartStep(downloadItem.TotalBytes);
    Progress.Show;
    //Progress.SetFocus;
  end;
  if not downloadItem.IsComplete then
  begin
    Progress.TaskProgress.StepPos := downloadItem.ReceivedBytes;
    Progress.lblLog.Caption := IntToStr(downloadItem.ReceivedBytes) + ' / ' + IntToStr(downloadItem.TotalBytes) + ' Bytes (' + IntToStr(round(downloadItem.ReceivedBytes / downloadItem.TotalBytes * 100)) + '%)';
  end
  else
  begin
    Progress.TaskProgress.FinishStep;
    DownloadResult := drSuccess;
  end;
end;

procedure TModDownloaderF.chrmDownloaderBeforeBrowse(Sender: TObject;
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

procedure TModDownloaderF.chrmDownloaderBeforeUnloadDialog(Sender: TObject;
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
