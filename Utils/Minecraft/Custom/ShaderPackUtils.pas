unit ShaderPackUtils;

interface

uses Task, ProgressBar, superobject, System.Classes, VanillaUtils, System.Generics.Collections, DownloadUtils, StringUtils,
DatabaseConnection, CoreLoader, System.SysUtils, cefvcl, SettingUtils, SaveFileUtils, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
ceflib, Winapi.Windows;

type
  TLoadShaderpacks = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;
  TShaderpackVersion = class;
  TShaderpack = class
    private
      FID : Integer;
      FName : String;
    public
      Versions : TList<TShaderpackVersion>;
      constructor Create(Json : ISuperObject);
      function getVersion(Name, MC : string) : TShaderpackVersion;
      property ID : Integer read FID;
      property Name : String read FName;
  end;
  TShaderpackVersion = class
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
  TShaderpackSelect = class(TSetting)
    Folder : String;
    Shaderpack : TShaderpack;
    constructor Create(Name, Title, Folder : String);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
    procedure chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    function isFilled : Boolean; override;
    procedure downloadRVersion(ShaderPackSelect : TForm);
  end;

function getShaderpackByID(ID : Integer) : TShaderpack;

var
Shaderpacks : TList<TShaderpack>;

implementation

uses ShaderPackSelect, FileDownload;

function getShaderpackByID(ID : Integer) : TShaderpack;
var
  i: Integer;
begin
  for i := 0 to Shaderpacks.Count-1 do
    if Shaderpacks[i].ID = ID then
      Exit(Shaderpacks[i]);
  Exit(nil);
end;

constructor TLoadShaderpacks.Create;
begin
  inherited Create('Loading Shaderpacks', True, False);
end;

procedure TLoadShaderpacks.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
Shaderpack : TShaderpack;
JsonFile : ISuperObject;
JsonArray : TSuperArray;
I: Integer;
FileString : TStringList;
FileJsonData : WideString;
begin
  FileName := DownloadFolder + 'shaderpack.json';
  Shaderpacks := TList<TShaderpack>.Create;
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/shaderpackservice.php',
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
      Shaderpack := TShaderpack.Create(JsonArray.O[i]);
      if Shaderpack <> nil then
        Shaderpacks.Add(Shaderpack);
    end;
  end;
end;

constructor TShaderpack.Create(Json : ISuperObject);
var
VersionArray : TSuperArray;
i: Integer;
Version : TShaderpackVersion;
begin
  VersionArray := Json.A['versions'];
  Versions := TList<TShaderpackVersion>.Create;
  for i := 0 to VersionArray.Length-1 do
  begin
    Version := TShaderpackVersion.Create(VersionArray.O[i]);
    if Version <> nil then
      Versions.Add(Version);
  end;
  FID := Json.I['id'];
  FName := Json.S['name'];
end;

function TShaderpack.getVersion(Name, MC : string) : TShaderpackVersion;
var
  i: Integer;
begin
  for i := 0 to Versions.Count-1 do
    if (Versions[i].Name = Name) and (Versions[i].MC.Contains(MC)) then
      Exit(Versions[i]);
  Exit(nil);
end;

constructor TShaderpackVersion.Create(Json : ISuperObject);
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

function TShaderpackVersion.getMinecraftVersion : TList<TMinecraftVersion>;
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

constructor TShaderpackSelect.Create(Name, Title, Folder : String);
begin
  inherited Create(Name, Title, True);
  Self.Folder := Folder;
  setHideTitle;
end;

function TShaderpackSelect.isFilled : Boolean;
begin
  Result := True;
end;

procedure TShaderpackSelect.chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
data : TStringList;
ShaderpackSelectForm : TfrmShaderPackSelect;
begin
  if string(url).Contains('#add') then
  begin
    data := Explode(url, '#add');
    if (data.Count = 2) and isStringNumber(data[1]) then
    begin
      Shaderpack := getShaderpackByID(StrToInt(data[1]));
      if Shaderpack <> nil then
      begin
        ShaderpackSelectForm := TfrmShaderPackSelect.Create(nil);
        ShaderpackSelectForm.ShaderpackSelect := Self;
        ShaderpackSelectForm.ShowModal;
      end;
    end;
  end;
end;

procedure TShaderpackSelect.downloadRVersion(ShaderPackSelect : TForm);
var
ShaderpackSelectForm : TfrmShaderPackSelect;
ShaderpackVersion : TShaderpackVersion;
Downloader : TDownloaderF;
downloadItem : TDownloadItem;
begin
  ShaderpackSelectForm := TfrmShaderPackSelect(ShaderPackSelect);
  if ShaderpackSelectForm.lstVersions.ItemIndex <> -1 then
  begin
    ShaderpackVersion := TShaderpackVersion(ShaderpackSelectForm.lstVersions.Items.Objects[ShaderpackSelectForm.lstVersions.ItemIndex]);
    if ShaderpackVersion <> nil then
    begin
      if not FileExists(Folder + 'shaderpacks\' + ShaderpackVersion.IFileName) then
      begin
        Downloader := TDownloaderF.Create(nil);
        Downloader.Show;
        downloadItem := TDownloadItem.Create('http://launcher.creativemd.de/service/downloadservice.php?id=' + IntToStr(Shaderpack.ID) + '&versionID=' + IntToStr(ShaderpackVersion.ID) + '&cat=shaderpack&url=' + ShaderpackVersion.URL, ShaderpackVersion.FileName);
        TThread.CreateAnonymousThread(procedure
        begin
          if Downloader.downloadItem(downloadItem) = drSuccess then
          begin
            ForceDirectories(ExtractFilePath(Folder + 'shaderpacks\' + ShaderpackVersion.FileName));
            RenameFile(TempFolder + ShaderpackVersion.FileName, Folder + 'shaderpacks\' + ShaderpackVersion.IFileName);
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

procedure TShaderpackSelect.createControl(x, y : Integer; Parent : TWinControl);
var
Chromium : TChromium;
begin
  Chromium := TChromium.Create(Parent);
  Chromium.Parent := Parent;
  Chromium.Left := SettingUtils.xOffset;
  Chromium.Top := y;
  Chromium.OnAddressChange := chrmModsAddressChange;
  Chromium.Align := alClient;
  Chromium.Load('http://launcher.creativemd.de/index.php?cat=shaderpack&launcher=yes');
  //Chromium.Visible := False;
  Controls.Add(Chromium);
end;

procedure TShaderpackSelect.destroyControl;
begin
  TChromium(Controls[0]).Destroying;
  Controls[0] := nil;
end;

function TShaderpackSelect.getUUID : string;
begin
  Result := 'shaderpackselect';
end;

procedure TShaderpackSelect.SaveToFile(SaveFile : TSaveFile);
begin

end;

procedure TShaderpackSelect.LoadFromFile(SaveFile : TSaveFile);
begin

end;

end.

