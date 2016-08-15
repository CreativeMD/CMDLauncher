unit Cauldron;

interface

uses ForgeUtils, Task, ProgressBar, System.Generics.Collections, superobject, VanillaUtils, System.Classes, DownloadUtils,
System.SysUtils, StringUtils, SettingUtils, MinecraftLaunchCommand, ForgeInstallation, LaunchTaskUtils, ZipUtils, SaveFileUtils,
Vcl.StdCtrls, Vcl.Controls, JavaUtils, AccountUtils, FileDownload, SideUtils, InstanceUtils, ModpackUtils;

type
TCauldron = class
  private
    FID : Integer;
    FVersion, FMC, FURL, FFile : String;
  public
    constructor Create(Json : ISuperObject);
    property ID : Integer read FID;
    property MC : String read FMC;
    property URL : String read FURL;
    property ServerFile : String read FFile;
    property Version : String read FVersion;
    function getMinecraftVersion : TMinecraftVersion;
end;
TCauldronInstance = class(TForgeInstance)
  protected
    procedure Load(SaveFile : TSaveFile); override;
    procedure Save(SaveFile : TSaveFile); override;
  public
    Cauldron : TCauldron;
    function getUUID : String; override;
    function getSettings : TList<TSetting>; override;
    function getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>; override;
    function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
end;
TLoadCauldron = class(TTask)
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
    public
     constructor Create;
  end;
TCauldronSelect = class(TForgeSelect)
  protected
      procedure onChanged(Sender: TObject); override;
  public
    constructor Create(Name, Title : String);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    function getMCVersion : String; override;
end;
TInstallServerCauldron = class(TLaunchTask)
  Instance : TCauldronInstance;
  procedure runTask(Bar : TCMDProgressBar); override;
  constructor Create(Command : TMinecraftLaunch; Instance : TCauldronInstance);
end;
TCauldronLaunch = class(TForgeLaunch)
  procedure buildListener(InstanceObject : TObject); override;
  function createWindow(InstanceObject : TObject; out Listener : TStrings) : TCMDProgressBar; override;
end;

function getCauldronByVersion(Version : string) : TCauldron;

var
CauldronList : TList<TCauldron>;
CauldronMCList : TStringList;
implementation

uses CoreLoader, DatabaseConnection, ModSettings, ConsoleServer, Console;

function getCauldronByVersion(Version : string) : TCauldron;
var
i : Integer;
begin
  for I := 0 to CauldronList.Count-1 do
    if CauldronList[i].Version = Version then
      Exit(CauldronList[i]);
  Exit(nil);
end;

constructor TCauldronSelect.Create(Name, Title : String);
begin
  inherited Create(Name, Title);
  Self.Items := CauldronMCList;
  Self.Online := CauldronMCList.Count = 0;
end;

procedure TCauldronSelect.createControl(x, y : Integer; Parent : TWinControl);
var
Cauldron : TCauldron;
begin
  inherited createControl(x, y, Parent);

  Cauldron := getCauldronByVersion(Value);
  if Cauldron <> nil then
  begin
    TComboBox(Controls[0]).ItemIndex := Items.IndexOf(Cauldron.MC);
    onChanged(Controls[0]);
    TComboBox(Controls[1]).ItemIndex := TComboBox(Controls[1]).Items.IndexOf(Value);
  end
  else
    onChanged(Controls[0]);
end;

function TCauldronSelect.getMCVersion : String;
var
TempCauldron : TCauldron;
begin
  TempCauldron := getCauldronByVersion(Value);
  Result := '';
  if TempCauldron <> nil then
    Result := TempCauldron.MC;
end;

procedure TCauldronSelect.onChanged(Sender : TObject);
var
mcversion : string;
i : Integer;
Strings : TStringList;
begin
  inherited onChanged(Sender);
  if Controls.Count = 2 then
  begin
    mcversion := TComboBox(Controls[0]).Text;
    TComboBox(Controls[1]).Clear;
    Strings := TStringList.Create;
    if mcversion <> '' then
      for i := 0 to CauldronList.Count-1 do
        if CauldronList[i].MC = mcversion then
          Strings.Add(CauldronList[i].Version);
    Strings.Sort;
    for i := 0 to Strings.Count-1 do
      TComboBox(Controls[1]).Items.Add(Strings[Strings.Count-1-i]);
  end;
end;

procedure TInstallServerCauldron.runTask(Bar : TCMDProgressBar);
var
Cauldron : TCauldron;
DownloadTask : TDownloadTask;
Extract : TExtractZip;
ZipFile : String;
begin
  Cauldron := Instance.Cauldron;
  if not FileExists(Instance.getInstanceFolder + Cauldron.ServerFile) then
  begin
    Command.MCVersion := Cauldron.MC;
    Command.SpecialArguments.Clear;

    ZipFile := Instance.getInstanceFolder + 'cauldron.zip';
    DownloadTask := TDownloadTask.Create(Cauldron.URL, ZipFile, False);
    DownloadTask.setLog(Log);
    DownloadTask.downloadFile(Bar);

    if FileExists(ZipFile) then
    begin
      Self.Log.log('Extracting cauldron.zip');
      Extract := TExtractZip.Create(ZipFile, Instance.getInstanceFolder);
      Extract.setLog(Self.Log);
      Extract.runTask(nil);
      Self.Log.logLastLine('Extracted cauldron.zip');
      DeleteFile(ZipFile);
    end;
  end;

  Command.SpecialArguments.Add('-jar');
  Command.SpecialArguments.Add(Cauldron.ServerFile);
  Bar.FinishStep;
end;

constructor TInstallServerCauldron.Create(Command : TMinecraftLaunch; Instance : TCauldronInstance);
begin
  inherited Create('Installing Cauldron Server', Command, True);
  Self.Instance := Instance;
end;

constructor TCauldron.Create(Json : ISuperObject);
begin
  FID := Json.I['id'];
  FVersion := Json.S['version'];
  FMC := Json.S['mc'];
  FURL := Json.S['url'];
  FFile := Json.S['server_file'];
end;

function TCauldron.getMinecraftVersion : TMinecraftVersion;
begin
  Result := VanillaUtils.getMinecraftVersion(MC);
end;

procedure TCauldronInstance.Load(SaveFile : TSaveFile);
begin
  inherited Load(SaveFile);
  Cauldron := getCauldronByVersion(SaveFile.getString('cauldron'));
  Side := TServer;
end;

procedure TCauldronInstance.Save(SaveFile : TSaveFile);
begin
  inherited Save(SaveFile);
  SaveFile.setString('cauldron', Cauldron.Version);
end;

function TCauldronInstance.getUUID : String;
begin
  Result := 'Cauldron';
end;

function TCauldronInstance.getSettings : TList<TSetting>;
var
i: Integer;
CauldronSelect : TCauldronSelect;
begin
  Result := inherited getSettings;
  CauldronSelect := TCauldronSelect.Create('cauldron', 'Cauldron');
  for i := 0 to Result.Count-1 do
    if Result[i] is TForgeSelect then
    begin
      Result[i].Destroy;
      Result[i] := CauldronSelect;
    end
    else if Result[i] is TEnhancedModSelect then
      TEnhancedModSelect(Result[i]).ForgeSelect := CauldronSelect;
end;

function TCauldronInstance.getStartupTasks(MinecraftComand : TMinecraftLaunch) : TList<TTask>;
begin
  Result := TList<TTask>.Create;
  //Result := inherited getStartupTasks(MinecraftComand);
  Result.Add(TInstallServerCauldron.Create(MinecraftComand, Self));
  if not Custom then
    Result.Add(TModCleaning.Create(Mods, getInstanceFolder + 'mods\', Side));

  Result.Add(TDownloadMods.Create(Mods, getInstanceFolder + 'mods\', Side));
end;

function TCauldronInstance.getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch;
begin
  Result := nil;
  if Cauldron <> nil then
    Result := TCauldronLaunch.Create(Java, Cauldron.MC, Self, LoginData);
end;

function TCauldronLaunch.createWindow(InstanceObject : TObject; out Listener : TStrings) : TCMDProgressBar;

begin
  Console := TConsoleServerF.Create(nil);
  Console.Caption := TInstance(InstanceObject).Title;
  TConsoleServerF(Console).Instance := TInstance(InstanceObject);
  Listener := TConsoleServerF(Console).mmoLog.Lines;
  Console.Show;
  Result := TConsoleServerF(Console).ProgressBar;
  TConsoleServerF(Console).edtConsole.Visible := False;
  TConsoleServerF(Console).btnEnter.Visible := False;
end;

procedure TCauldronLaunch.buildListener(InstanceObject : TObject);
var
Instance : TInstance;
begin
  Instance := TInstance(InstanceObject);
  TConsoleServerF(Console).Instance := Instance;
  TConsoleServerF(Console).Launching := TLaunching.Create('"' + Java.Path + '"', getLaunchCommand,
  Instance.getInstanceFolder, procedure(const Line: AnsiString)
      begin
          if string(Line) <> '' then
            TConsoleServerF(Console).mmoLog.Lines.Append(String(Line));
      end);
  TConsoleServerF(Console).ProgressBar.Visible := False;
  TConsoleServerF(Console).edtConsole.Visible := True;
  TConsoleServerF(Console).btnEnter.Visible := True;
  TConsoleServerF(Console).mmoLog.Lines.Add('"' + Java.Path + '"');
  TConsoleServerF(Console).mmoLog.Lines.AddStrings(getLaunchCommandList);
  TConsoleServerF(Console).Launching.OnClosed := TConsoleServerF(Console).onClosed;
end;

constructor TLoadCauldron.Create;
begin
  inherited Create('Loading Cauldron', False, False);
end;

procedure TLoadCauldron.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
Cauldron : TCauldron;
JsonFile : ISuperObject;
JsonArray : TSuperArray;
I: Integer;
FileString : TStringList;
FileJsonData : WideString;
begin
  FileName := DownloadFolder + 'cauldron.json';
  CauldronList := TList<TCauldron>.Create;
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/cauldronservice.php',
  FileName, True);
  CauldronMCList := TStringList.Create;

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
      Cauldron := TCauldron.Create(JsonArray.O[i]);
      if not CauldronMCList.Contains(Cauldron.MC) then
        CauldronMCList.Add(Cauldron.MC);
      CauldronList.Add(Cauldron);
    end;
  end;
end;

end.
