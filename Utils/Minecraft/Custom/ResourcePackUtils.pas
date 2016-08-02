unit ResourcePackUtils;

interface

uses Task, System.Generics.Collections, VanillaUtils, ProgressBar, superobject, System.Classes, DownloadUtils, System.SysUtils,
SettingUtils, SaveFileUtils, ceflib, Vcl.Controls, StringUtils, cefvcl, InstanceUtils, Vcl.Dialogs, Winapi.Windows,
Vcl.Forms;

type
  TLoadResourcePacks = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;
  TResourcePackVersion = class;
  TResourcePack = class
    private
      FID : Integer;
      FName : String;
    public
      Versions : TList<TResourcePackVersion>;
      constructor Create(Json : ISuperObject);
      function getVersion(Name, MC : string) : TResourcePackVersion;
      property ID : Integer read FID;
      property Name : String read FName;
  end;
  TResourcePackVersion = class
    private
      FID : Integer;
      FName, FFileName, FIFileName, FURL : String;
      FMC : TStringList;
    public
      constructor Create(Json : ISuperObject);
      function getMinecraftVersion : TList<TMinecraftVersion>;
      property MC : TStringList read FMC;
      property FileName : String read FFileName;
      property IFileName : String read FIFileName;
      property URL : String read FURL;
      property ID : Integer read FID;
      property Name : String read FName;
  end;
  TResourcepackSelect = class(TSetting)
    Folder : String;
    ResourcePack : TResourcePack;
    constructor Create(Name, Title, Folder : String);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
    procedure chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    function isFilled : Boolean; override;
    procedure downloadRVersion(ResourceSelect : TForm);
  end;


function getResourcePackByID(ID : Integer) : TResourcePack;

var
ResourcePacks : TList<TResourcePack>;

implementation

uses CoreLoader, DatabaseConnection, ResourcePackSelect, FileDownload;

function getResourcePackByID(ID : Integer) : TResourcePack;
var
  i: Integer;
begin
  for i := 0 to ResourcePacks.Count-1 do
    if ResourcePacks[i].ID = ID then
      Exit(ResourcePacks[i]);
  Exit(nil);
end;

constructor TLoadResourcePacks.Create;
begin
  inherited Create('Loading ResourcePacks', True, False);
end;

procedure TLoadResourcePacks.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
ResourcePack : TResourcePack;
JsonFile : ISuperObject;
JsonArray : TSuperArray;
I: Integer;
FileString : TStringList;
FileJsonData : WideString;
begin
  FileName := DownloadFolder + 'resourcepack.json';
  ResourcePacks := TList<TResourcePack>.Create;
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/resourcepackservice.php',
  FileName, True);

  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
  end;

  if FileExists(FileName) and (VanillaUtils.MinecraftVersions <> nil) then
  begin
    FileString := TStringList.Create;
    FileString.LoadFromFile(FileName);
    FileJsonData := '';
    for i := 0 to FileString.Count-1 do
      FileJsonData := FileJsonData + Trim(FileString[i]).Replace(#$FEFF, '');
    JsonFile := TSuperObject.ParseString(PWideChar(FileJsonData), False);
    JsonArray := JsonFile.AsArray;
    for I := 0 to JsonArray.Length-1 do
    begin
      ResourcePack := TResourcePack.Create(JsonArray.O[i]);
      if ResourcePack <> nil then
        ResourcePacks.Add(ResourcePack);
    end;
  end;
end;

constructor TResourcePack.Create(Json : ISuperObject);
var
VersionArray : TSuperArray;
i: Integer;
Version : TResourcePackVersion;
begin
  VersionArray := Json.A['versions'];
  Versions := TList<TResourcePackVersion>.Create;
  for i := 0 to VersionArray.Length-1 do
  begin
    Version := TResourcePackVersion.Create(VersionArray.O[i]);
    if Version <> nil then
      Versions.Add(Version);
  end;
  FID := Json.I['id'];
  FName := Json.S['name'];
end;

function TResourcePack.getVersion(Name, MC : string) : TResourcePackVersion;
var
  i: Integer;
begin
  for i := 0 to Versions.Count-1 do
    if (Versions[i].Name = Name) and (Versions[i].MC.Contains(MC)) then
      Exit(Versions[i]);
  Exit(nil);
end;

constructor TResourcePackVersion.Create(Json : ISuperObject);
var
MCArray : TSuperArray;
  i: Integer;
begin
  MCArray := Json.A['mc'];
  FMC := TStringList.Create;
  if MCArray <> nil then
    for i := 0 to MCArray.Length-1 do
      FMC.Add(MCArray.S[i]);
  FFileName := Json.S['filename'];
  FIFileName := Json.S['ifilename'];
  FURL := Json.S['url'];
  FID := Json.I['id'];
  FName := Json.S['name'];
end;

function TResourcePackVersion.getMinecraftVersion : TList<TMinecraftVersion>;
var
MCVersion : TMinecraftVersion;
  i: Integer;
begin
  Result := TList<TMinecraftVersion>.Create;
  for i := 0 to MC.Count-1 do
  begin
    MCVersion := VanillaUtils.getMinecraftVersion(MC[i]);
    if MCVersion <> nil then
      Result.Add(MCVersion);
  end;

end;

constructor TResourcepackSelect.Create(Name, Title, Folder : String);
begin
  inherited Create(Name, Title, True);
  Self.Folder := Folder;
  setHideTitle;
end;

function TResourcepackSelect.isFilled : Boolean;
begin
  Result := True;
end;

procedure TResourcepackSelect.chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
data : TStringList;
ResourcePackSelect : TResourceSelect;
  I: Integer;
begin
  if string(url).Contains('#add') then
  begin
    data := Explode(url, '#add');
    if (data.Count = 2) and isStringNumber(data[1]) then
    begin
      ResourcePack := getResourcePackByID(StrToInt(data[1]));
      if ResourcePack <> nil then
      begin
        ResourcePackSelect := TResourceSelect.Create(nil);
        for I := 0 to ResourcePack.Versions.Count-1 do
          ResourcePackSelect.lstVersions.Items.AddObject('(' + Implode(ResourcePack.Versions[i].MC, '-', False) + ')-' + ResourcePack.Versions[i].Name, ResourcePack.Versions[i]);
        ResourcePackSelect.ResourceSelect := Self;
        ResourcePackSelect.ShowModal;
      end;
    end;
  end;
end;

procedure TResourcepackSelect.downloadRVersion(ResourceSelect : TForm);
var
ResourcePackSelect : TResourceSelect;
ResourcePackVersion : TResourcePackVersion;
Downloader : TDownloaderF;
downloadItem : TDownloadItem;
begin
  ResourcePackSelect := TResourceSelect(ResourceSelect);
  if ResourcePackSelect.lstVersions.ItemIndex <> -1 then
  begin
    ResourcePackVersion := TResourcePackVersion(ResourcePackSelect.lstVersions.Items.Objects[ResourcePackSelect.lstVersions.ItemIndex]);
    if ResourcePackVersion <> nil then
    begin
      if not FileExists(Folder + 'resourcepacks\' + ResourcePackVersion.IFileName) then
      begin
        Downloader := TDownloaderF.Create(nil);
        Downloader.Show;
        downloadItem := TDownloadItem.Create('http://launcher.creativemd.de/service/downloadservice.php?id=' + IntToStr(ResourcePack.ID) + '&versionID=' + IntToStr(ResourcePackVersion.ID) + '&cat=resourcepack&url=' + ResourcePackVersion.URL, ResourcePackVersion.FileName);

        //downloadItem := TDownloadItem.Create(ResourcePackVersion.URL, ResourcePackVersion.FileName);
        TThread.CreateAnonymousThread(procedure
        begin
          if Downloader.downloadItem(downloadItem) = drSuccess then
          begin
            ForceDirectories(ExtractFilePath(Folder + 'resourcepacks\' + ResourcePackVersion.FileName));
            RenameFile(TempFolder + ResourcePackVersion.FileName, Folder + 'resourcepacks\' + ResourcePackVersion.IFileName);
          end;
          try
            Downloader.Progress.Destroy;
          except
            on E : Exception do
          end;
          //Downloader.DestroyComponents;
          //Downloader.Close;
          Downloader.Destroy;
        end).Start;

      end
      else
        ShowMessage('Already downloaded!');
    end;
  end;
end;

procedure TResourcepackSelect.createControl(x, y : Integer; Parent : TWinControl);
var
Chromium : TChromium;
begin
  Chromium := TChromium.Create(Parent);
  Chromium.Parent := Parent;
  Chromium.Left := SettingUtils.xOffset;
  Chromium.Top := y;
  Chromium.OnAddressChange := chrmModsAddressChange;
  Chromium.Align := alClient;
  Chromium.Load('http://launcher.creativemd.de/index.php?cat=resourcepack&launcher=yes');
  //Chromium.Visible := False;
  Controls.Add(Chromium);
end;

procedure TResourcepackSelect.destroyControl;
begin
  TChromium(Controls[0]).Destroying;
  Controls[0] := nil;
end;

function TResourcepackSelect.getUUID : string;
begin
  Result := 'resourcepackselect';
end;

procedure TResourcepackSelect.SaveToFile(SaveFile : TSaveFile);
begin

end;

procedure TResourcepackSelect.LoadFromFile(SaveFile : TSaveFile);
begin
  
end;


end.
