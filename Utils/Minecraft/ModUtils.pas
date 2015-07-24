unit ModUtils;

interface

uses InstanceUtils, System.Generics.Collections, superobject, System.Types,
System.SysUtils, Winapi.Windows, Task, Progressbar,
System.Classes, JclStringConversions, SideUtils;

type
  TModInstallObj = class abstract
    public
      SideType : TSideType;
      DownloadLink, DFileName : String;
      constructor Create(DownloadLink, DFileName : String; SideType : TSideType);
      function installObj(TempFolder : String; Folder : String) : Boolean; virtual; abstract;
      function isInstalled(Folder : String) : Boolean; virtual; abstract;
      function isFile(FileName : String) : Boolean; virtual; abstract;
  end;
  TModFile = class(TModInstallObj)
    private
      FileName : String;
    public
      constructor Create(Json : ISuperObject); overload;
      constructor Create(DownloadLink, DFileName, FileName : String; SideType : TSideType); overload;
      function installObj(TempFolder : String; Folder : String) : Boolean; override;
      function isInstalled(Folder : String) : Boolean; override;
      function isFile(FileName : String) : Boolean; override;
  end;
  TModArchive = class(TModInstallObj)
    private
      InstallObjs : TList<TModInstallObj>;
    public
      constructor Create(Json : ISuperObject);
      function installObj(TempFolder : String; Folder : String) : Boolean; override;
      function isInstalled(Folder : String) : Boolean; override;
      function isFile(FileName : String) : Boolean; override;
  end;
  TModVersion = class
    private
      FID : Integer;
      FName, FMin, FMax : String;
      InstallObjs : TList<TModInstallObj>;
    public
      constructor Create(Json : ISuperObject);
      function isInstalled(Folder : String) : Boolean;
      function isForgeValid(Version : string) : Boolean;
      function isModFile(FileName : string) : Boolean;
      function getMCVersion : String;
      property ID : Integer read FID;
      property Name : String read FName;
      property Min : String read FMin;
      property Max : String read FMax;
      property Files : TList<TModInstallObj> read InstallObjs;
  end;
  TMod = class
    private
      FID : Integer;
      Versions : TList<TModVersion>;
      FTitle : String;
      Required : TList<Integer>;
      FModType : TSideType;
      constructor Create(Json : ISuperObject);
    public
      function getVersionByID(ID : Integer) : TModVersion;
      function getVersionByName(Name : String) : TModVersion;
      function getValidVersionsMC(MC : String) : TList<TModVersion>;
      function getValidVersions(Version : String) : TList<TModVersion>;
      function isInstalled(Instance : TInstance) : Boolean;
      function isServerCompatible : Boolean;
      function isClientCompatible : Boolean;
      procedure loadMod(Json : ISuperObject);
      function hasLoaded : Boolean;
      property ModType : TSideType read FModType;
      property Title : String read FTitle;
      property ID : Integer read FID;
  end;
  TLoadMod = class(TTask)
    procedure runTask(Bar : TCMDProgressBar); override;
    constructor Create;
  end;
  TFullLoadMod = class(TLoadMod)
    procedure runTask(Bar : TCMDProgressBar); override;
  end;

function getModByID(ID : Integer) : TMod;
function getModByName(Name : String) : TMod;
function createMod(Json : ISuperObject) : TMod;
function createModObj(Json : ISuperObject) : TModInstallObj;

var
ModList : TList<TMod>;
ModsLoaded : Boolean;
implementation

uses ForgeUtils, FileUtils, ZipUtils, StringUtils, DownloadUtils, CoreLoader,
DatabaseConnection, Logger;

function getModByName(Name : String) : TMod;
var
  i: Integer;
begin
  for i := 0 to ModList.Count-1 do
    if ModList[i].Title = Name then
      Exit(ModList[i]);
  Exit(nil);
end;

function getModByID(ID : Integer) : TMod;
var
  i: Integer;
begin
  for i := 0 to ModList.Count-1 do
    if ModList[i].ID = ID then
      Exit(ModList[i]);
  Exit(nil);
end;

function createMod(Json : ISuperObject) : TMod;
begin
  try
    Exit(TMod.Create(Json));
  except
    on E : Exception do
      Exit(nil);
  end;
end;

function createModObj(Json : ISuperObject) : TModInstallObj;
begin      
  try   
    if Json.A['subfiles'] <> nil then
      Result := TModArchive.Create(Json)
    else
      Result := TModFile.Create(Json);
  except
    on E : Exception do
      Result := nil;
  end;
end;

procedure TFullLoadMod.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
JsonFile : ISuperObject;
ModArray : TSuperArray;
i: Integer;
TempMod : TMod;
FileString : TStringList;
FileJsonData : WideString;
begin
  FileName := DownloadFolder + 'mod.json';
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/modservice.php?type=full',FileName, True);
  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(Bar);
  end;
  if FileExists(FileName) then
  begin
    FileString := TStringList.Create;
    FileString.LoadFromFile(FileName);
    FileJsonData := '';
    for i := 0 to FileString.Count-1 do
      FileJsonData := FileJsonData + FileString[i];
    JsonFile := TSuperObject.ParseString(PWideChar(FileJsonData), False);
    ModArray := JsonFile.AsArray;
    for i := 0 to ModArray.Length-1 do
    begin
      TempMod := getModByID(ModArray.O[i].I['id']);
      if TempMod <> nil then
        TempMod.loadMod(ModArray.O[i])
      else
        Self.Log.log('Failed to load "' + ModArray.O[i].S['title'] + '"');
    end;
  end;
  ModsLoaded := True;
  Self.Log.log('Loaded ' + InttoStr(ModList.Count) + ' mods');
end;

{function ReadSO(const aFileName: string): ISuperObject;
var
  input: TFileStream;
  output: TStringStream;
begin
  input := TFileStream.Create(aFileName, fmOpenRead, fmShareDenyWrite);
  try
     output := TStringStream.Create('');
     try
       output.CopyFrom(input, input.Size);
       Result := TSuperObject.ParseString(PWideChar(UTF8ToUTF16(output.DataString)), true, true);
     finally
       output.Free;
     end;

  finally
    input.Free;
  end;
end;    }

procedure TLoadMod.runTask(Bar : TCMDProgressBar);
var
FileName : String;
DownloadTask : TDownloadTask;
JsonFile : ISuperObject;
ModArray : TSuperArray;
i: Integer;
TempMod : TMod;
FileString : TStringList;
FileJsonData : WideString;
begin
  ModsLoaded := False;
  ModList := TList<TMod>.Create;
  FileName := DownloadFolder + 'modlite.json';
  DownloadTask := TDownloadTask.Create('http://launcher.creativemd.de/service/modservice.php',FileName, True);
  if DatabaseConnection.online then
  begin
    DownloadTask.downloadFile(nil);
  end;
  if FileExists(FileName) then
  begin
    FileString := TStringList.Create;
    FileString.LoadFromFile(FileName);
    FileJsonData := '';
    for i := 0 to FileString.Count-1 do
      FileJsonData := FileJsonData + FileString[i];
    JsonFile := TSuperObject.ParseString(PWideChar(FileJsonData), False); //.ParseFile(FileName, False);
    ModArray := JsonFile.AsArray;
    for i := 0 to ModArray.Length-1 do
    begin
      TempMod := createMod(ModArray.O[i]);

      if TempMod <> nil then
        ModList.Add(TempMod)
      else
        Self.Log.log('Failed to load mod!');
    end;
  end;
  Self.Log.log('Pre-Loaded ' + InttoStr(ModList.Count) + ' mods');
end;

constructor TLoadMod.Create;
begin
  inherited Create('Loading Mods', False, False);
end;

constructor TMod.Create(Json : ISuperObject);
var
RequiredArray : TSuperArray;
  i: Integer;
begin
  FTitle := Json.S['title'];
  FID := Json.I['id'];
  Required := TList<Integer>.Create;
  RequiredArray := Json.A['require'];
  if RequiredArray <> nil then
  begin
    for i := 0 to RequiredArray.Length-1 do
    begin
      try
        Required.Add(RequiredArray.I[i]);
      except
        on E : Exception do
          Logger.Log.log('Failed to add required mod!');

      end;
    end;
  end;
  if Json.A['versions'] <> nil then
    Self.loadMod(Json);
end;

function TMod.hasLoaded : Boolean;
begin
  Result := Versions <> nil;
end;

procedure TMod.loadMod(Json : ISuperObject);
var
VArray : TSuperArray;
i: Integer;
ModVersion : TModVersion;
begin
  Versions := TList<TModVersion>.Create;
  FModType := parseSideType(Json.S['type']);
  VArray := Json.A['versions'];
  for i := 0 to VArray.Length-1 do
  begin
    ModVersion := TModVersion.Create(VArray[i]);
    if ModVersion <> nil then
      Versions.Add(ModVersion);
  end;
end;

function TMod.getVersionByID(ID : Integer) : TModVersion;
var
i: Integer;
begin
  Result := nil;
  for i := 0 to Versions.Count-1 do
    if Versions[i].ID = ID then
      Exit(Versions[i]);
end;

function TMod.getVersionByName(Name : String) : TModVersion;
var
i: Integer;
begin
  Result := nil;
  for i := 0 to Versions.Count-1 do
    if Versions[i].Name = Name then
      Exit(Versions[i]);
end;

function TMod.getValidVersionsMC(MC : String) : TList<TModVersion>;
var
i: Integer;
begin
  Result := TList<TModVersion>.Create;
  for i := 0 to Versions.Count-1 do
    if Versions[i].getMCVersion = MC then
      Result.Add(Versions[i]);
end;

function TMod.getValidVersions(Version : String) : TList<TModVersion>;
var
  i: Integer;
begin
  Result := TList<TModVersion>.Create;
  for i := 0 to Versions.Count-1 do
    if Versions[i].isForgeValid(Version) then
      Result.Add(Versions[i]);
end;

function TMod.isServerCompatible : Boolean;
begin
  Result := (ModType = Universal) or (ModType = OnlyServer);
end;

function TMod.isClientCompatible : Boolean;
begin
  Result := (ModType = Universal) or (ModType = OnlyClient);
end;

function TMod.isInstalled(Instance : TInstance) : Boolean;
var
i : Integer;
Versions : TList<TModVersion>;
begin
  if Instance is TForgeInstance then
  begin
    Versions := getValidVersions(TForgeInstance(Instance).Forge.UUID);
    for i := 0 to Versions.Count-1 do
      if Versions[i].isInstalled(Instance.getInstanceFolder + 'mods\') then
        Exit(True);
  end;
  Exit(False);
end;

constructor TModVersion.Create(Json : ISuperObject);
var
InstallArray : TSuperArray;
i : Integer;
InstallObj : TModInstallObj;
begin
  FID := StrToInt(Json.S['id']);
  FName := Json.S['name'];
  FMin := Json.S['minforge'];
  FMax := Json.S['maxforge'];
  InstallObjs := TList<TModInstallObj>.Create;
  InstallArray := Json.A['files'];
  if InstallArray <> nil then
  begin
    for i := 0 to InstallArray.Length-1 do
    begin
      InstallObj := createModObj(InstallArray[i]);
      if InstallObj <> nil then
        InstallObjs.Add(InstallObj);
    end;
  end;
end;

function TModVersion.getMCVersion : String;
var
Forge : TForge;
begin
  Forge := ForgeUtils.getForgeByUUID(Min);
  Result := '';
  if Forge <> nil then
    Result := Forge.MV;
end;

function TModVersion.isForgeValid(Version : string) : Boolean;
begin
  Result := isHigher(Min, Version) and (isHigher(Version, Max) or (Version = Max));
end;

function TModVersion.isModFile(FileName : string) : Boolean;
var
i : Integer;
begin
  for i := 0 to InstallObjs.Count-1 do
    if InstallObjs[i].isFile(FileName) then
      Exit(True);
  Exit(false);
end;

function TModVersion.isInstalled(Folder : String) : Boolean;
var
i : Integer;
begin
  for i := 0 to InstallObjs.Count-1 do
    if not InstallObjs[i].isInstalled(Folder) then
      Exit(False);
  Exit(True);
end;

constructor TModFile.Create(Json : ISuperObject);
begin
  inherited Create(Json.S['url'], Json.S['filename'], parseSideType(Json.S['type']));
  Self.FileName := Json.S['ifilename'];
  if Self.FileName = '' then
    Self.FileName := Self.DFileName;
end;

constructor TModFile.Create(DownloadLink, DFileName, FileName : String; SideType : TSideType);
begin
  inherited Create(DownLoadLink, DFileName, SideType);
  Self.FileName := FileName;
  if Self.FileName = '' then
    Self.FileName := Self.DFileName;
end;

function TModFile.installObj(TempFolder : String; Folder : String) : Boolean;
begin
  Result := False;
  if FileExists(TempFolder + DFileName) then
  begin
    ForceDirectories(ExtractFilePath(Folder + FileName.Replace('/', '\')));
    Result := RenameFile(TempFolder + DFileName, Folder + FileName.Replace('/', '\'));
  end;
end;

function TModFile.isInstalled(Folder : String) : Boolean;
var
FilePath  : String;
begin
  FilePath := Folder + FileName;
  if GetSizeOfFile(FilePath) = 0 then
    DeleteFile(PWideChar(FilePath));
  Result := FileExists(PWideChar(FilePath));
end;

function TModFile.isFile(FileName : String) : Boolean;
begin
  Result := Self.FileName = FileName;
end;

constructor TModArchive.Create(Json : ISuperObject);
var
InstallArray : TSuperArray;
i : Integer;
//InstallObj : TModInstallObj;
begin
  inherited Create(Json.S['url'], Json.S['filename'], parseSideType(Json.S['type']));
  InstallObjs := TList<TModInstallObj>.Create;
  InstallArray := Json.A['subfiles'];
  if InstallArray <> nil then
  begin
    for i := 0 to InstallArray.Length-1 do
    begin
      InstallObjs.Add(TModFile.Create('', InstallArray.O[i].S['afilename'], InstallArray.O[i].S['filename'], parseSideType(InstallArray.O[i].S['type'])));
      {InstallObj := createModObj(InstallArray[i]);
      if InstallObj <> nil then
        InstallObjs.Add(InstallObj); }
    end;
  end;
end;

function TModArchive.installObj(TempFolder : String; Folder : String) : Boolean;
var
Zipper : TExtractZip;
NewTempFolder : String;
i: Integer;
begin
  Result := False;
  if FileExists(TempFolder + DFileName) then
  begin
    NewTempFolder := TempFolder + 'CMDTemp\';
    ForceDirectories(NewTempFolder);
    Zipper := TExtractZip.Create(TempFolder + DFileName, NewTempFolder);
    Zipper.runTask(nil);
    Result := True;
    for i := 0 to InstallObjs.Count-1 do
      if not InstallObjs[i].installObj(NewTempFolder, Folder) then
        Result := False;
    DeleteFolder(NewTempFolder, nil);
    DeleteFile(PWideChar(TempFolder + DFileName));
  end;
end;

function TModArchive.isFile(FileName : String) : Boolean;
var
i : Integer;
begin
  for i := 0 to InstallObjs.Count-1 do
    if InstallObjs[i].isFile(FileName) then
      Exit(True);
  Exit(false);
end;

function TModArchive.isInstalled(Folder : String) : Boolean;
var
i : Integer;
begin
  for i := 0 to InstallObjs.Count-1 do
    if not InstallObjs[i].isInstalled(Folder) then
      Exit(False);
  Exit(True);
end;

constructor TModInstallObj.Create(DownloadLink, DFileName : String; SideType : TSideType);
begin
  Self.DownloadLink := DownloadLink;
  Self.DFileName := DFileName;
  Self.SideType := SideType;
end;


end.
