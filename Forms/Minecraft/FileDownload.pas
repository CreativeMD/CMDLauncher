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
    IgnoreFileName : Boolean;
    constructor Create(URL, FileName : String; IgnoreFileName : Boolean = False);
    function isFileName(SuggestedFileName : string): Boolean;
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
  TDownloadMods = class(TTask)
    protected
      ModsFolder : String;
      Side : TSide;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      Mods : TDictionary<TMod, TModVersion>;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; Side : TSide);
      procedure addMod(PMod : TPair<TMod, TModVersion>);
  end;
  TModCleaning = class(TTask)
    protected
      ModsFolders : TStringList;
      Side : TSide;
      procedure runTask(Bar : TCMDProgressBar); override;
    public
      Mods : TDictionary<TMod, TModVersion>;
      Exclude : TStringList;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolders : TStringList; Side : TSide); overload;
      constructor Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; Side : TSide); overload;
      procedure addMod(PMod : TPair<TMod, TModVersion>);
  end;

implementation

{$R *.dfm}

uses CoreLoader;

constructor TDownloadItem.Create(URL, FileName : String; IgnoreFileName : Boolean = False);
begin
  Self.URL := URL;
  Self.FileName := FileName;
  Self.IgnoreFileName := IgnoreFileName;
end;

function TDownloadItem.isFileName(SuggestedFileName : string): Boolean;
begin
  Result := IgnoreFileName or (SuggestedFileName.Replace(' ', '').Replace('''', '') = FileName.Replace(' ', ''));
end;

constructor TModCleaning.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolders : TStringList;  Side : TSide);
var
Item : TPair<TMod, TModVersion>;
begin
  inherited Create('Clean Mods', True);
  Self.Exclude := TStringList.Create;
  Self.ModsFolders := ModsFolders;
  Self.Side := Side;
  Self.Mods := TDictionary<TMod, TModVersion>.Create;
  for Item in Mods do
  begin
    addMod(Item);
  end;
end;

procedure TModCleaning.addMod(PMod : TPair<TMod, TModVersion>);
begin
  if PMod.Key.ModType.isCompatible(Side) and not Self.Mods.ContainsKey(PMod.Key) then
      Self.Mods.Add(PMod.Key, PMod.Value);
end;

constructor TModCleaning.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String;  Side : TSide);
begin
  Self.ModsFolders := TStringList.Create;
  Self.ModsFolders.Add(ModsFolder);
  Create(Mods, Self.ModsFolders, Side);
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
          if Item.Value.isModFile(Files[j].Replace(ModsFolders[i], '').Replace('\', '/'), Side) then
          begin
            isFileOfMod := True;
            Break;
          end;

        if not isFileOfMod and not Exclude.Contains(Files[j].Replace(ModsFolders[i], '').Replace('\', '/')) then
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

procedure TDownloadMods.addMod(PMod : TPair<TMod, TModVersion>);
begin
  if PMod.Key.ModType.isCompatible(Side) and not Self.Mods.ContainsKey(PMod.Key) then
      Self.Mods.Add(PMod.Key, PMod.Value);
end;

constructor TDownloadMods.Create(Mods : TDictionary<TMod, TModVersion>; ModsFolder : String; Side : TSide);
var
Item : TPair<TMod, TModVersion>;
begin
  inherited Create('Downloading Mods', True, False);
  Self.ModsFolder := ModsFolder;
  Self.sync := True;
  Self.Side := Side;
  Self.Mods := TDictionary<TMod, TModVersion>.Create;
  for Item in Mods do
  begin
    addMod(Item);
  end;

end;

procedure TDownloadMods.runTask(Bar : TCMDProgressBar);
var
Downloader : TDownloaderF;
Item : TPair<TMod, TModVersion>;
DResult : TDownloadR;
i : Integer;
NeedInstallation, IsModValid : Boolean;
begin
  NeedInstallation := False;
  for Item in Mods do
  begin
    if not Item.Value.isInstalled(ModsFolder, Side) then
    begin
      NeedInstallation := True;
    end;
  end;

  if NeedInstallation then
  begin
    Downloader := TDownloaderF.Create(nil);
    //Downloader.chrmDownloader.
    Downloader.Show;
    Downloader.DownloadBar.StartProcess(Mods.Count);
    Bar.StartStep(Mods.Count);
    for Item in Mods do
    begin
      IsModValid := True;

      if IsModValid and not Item.Value.isInstalled(ModsFolder, Side) then
      begin
        for i := 0 to Item.Value.Files.Count-1 do
        begin
          if not Item.Value.Files[i].isInstalled(ModsFolder, Side) and Item.Value.Files[i].SideType.isCompatible(Side) then
          begin
            Self.Log.log('Downloading ' + Item.Key.Title);
            Downloader.lblProgress.Caption := IntToStr(Bar.StepPos+1) + '/' + IntToStr(Mods.Count) + ' Mods';
            DResult := Downloader.downloadItem(TDownloadItem.Create('http://launcher.creativemd.de/service/downloadservice.php?id=' + IntToStr(Item.Key.ID) + '&versionID=' + IntToStr(Item.Value.ID) + '&cat=mod&url=' + Item.Value.Files[i].DownloadLink, Item.Value.Files[i].DFileName));
            if DResult = drCancel then
            begin
              if Downloader.Progress <> nil then
                Downloader.Progress.Destroy;
              Downloader.Destroy;
              Self.Log.log('Canceled mod download ');
              Exit;
            end;
            if DResult = drFail then
            begin
              if Downloader.Progress <> nil then
                Downloader.Progress.Destroy;
              Self.Log.logLastLine('Failed to download mod! ' + Item.Key.Title);
            end;

            if DResult = drSuccess then
            begin
              Downloader.Progress.lblTask.Caption := 'Installing Mod';
              Application.ProcessMessages;
              Item.Value.Files[i].installObj(TempFolder, ModsFolder, Side);
              Downloader.Progress.Destroy;
              Self.Log.logLastLine('Downloaded ' + Item.Key.Title);
            end;
            //Downloader.chrmDownloader.ReCreateBrowser('');
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
    callback.Cont(TempFolder + Item.FileName, False);
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
