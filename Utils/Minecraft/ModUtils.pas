unit ModUtils;

interface

uses InstanceUtils, System.Generics.Collections, superobject, System.Types,
System.SysUtils, Winapi.Windows, Task;

type
  TModInstallObj = class abstract
    private
      DownloadLink, DFileName : String;
    public
      constructor Create(DownloadLink, DFileName : String);
      function installObj(TempFolder : String; Instance : TInstance) : Boolean; virtual; abstract;
      function isInstalled(Instance : TInstance) : Boolean; virtual; abstract;
  end;
  TModFile = class(TModInstallObj)
    private
      FileName : String;
    public
      constructor Create(Json : ISuperObject);
      function installObj(TempFolder : String; Instance : TInstance) : Boolean; override;
      function isInstalled(Instance : TInstance) : Boolean; override;
  end;
  TModArchive = class(TModInstallObj)
    private
      InstallObjs : TList<TModInstallObj>;
    public
      constructor Create(Json : ISuperObject);
      function installObj(TempFolder : String; Instance : TInstance) : Boolean; override;
      function isInstalled(Instance : TInstance) : Boolean; override;
  end;
  TModVersion = class
    private
      ID, Min, Max : String;
      InstallObjs : TList<TModInstallObj>;
    public
      constructor Create(Json : ISuperObject);
      function isInstalled(Instance : TInstance) : Boolean;
      function isForgeValid(Version : string) : Boolean;
  end;
  TMod = class
    private
      Versions : TList<TModVersion>;
      Title : String;
      constructor Create(Json : ISuperObject);
    public
      function getValidVersions(Version : String) : TList<TModVersion>;
      function isInstalled(Instance : TInstance) : Boolean;
      procedure loadMod(Json : ISuperObject);
  end;
  TLoadMod = class(TTask)
  
  end;

function createMod(Json : ISuperObject) : TMod;
function createModObj(Json : ISuperObject) : TModInstallObj;

implementation

uses ForgeUtils, FileUtils, ZipUtils, StringUtils;

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
    if Json.B['archive'] then
      Result := TModArchive.Create(Json)
    else
      Result := TModFile.Create(Json);
  except
    on E : Exception do
      Result := nil;
  end;
end;

constructor TMod.Create(Json : ISuperObject);
begin
  Title := Json.S['title'];
  if Json.A['versions'] <> nil then
    Self.loadMod(Json);
end;

procedure TMod.loadMod(Json : ISuperObject);
var
VArray : TSuperArray;
i: Integer;
ModVersion : TModVersion;
begin
  Versions := TList<TModVersion>.Create;
  VArray := Json.A['versions'];
  for i := 0 to VArray.Length-1 do
  begin
    ModVersion := TModVersion.Create(VArray[i]);
    if ModVersion <> nil then
      Versions.Add(ModVersion);
  end;
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

function TMod.isInstalled(Instance : TInstance) : Boolean;
var
i : Integer;
Versions : TList<TModVersion>;
begin
  if Instance is TForgeInstance then
  begin
    Versions := getValidVersions(TForgeInstance(Instance).Forge.UUID);
    for i := 0 to Versions.Count-1 do
      if Versions[i].isInstalled(Instance) then
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
  ID := Json.S['id'];
  Min := Json.S['min'];
  Max := Json.S['max'];
  InstallObjs := TList<TModInstallObj>.Create;
  InstallArray := Json.A['obj'];
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

function TModVersion.isForgeValid(Version : string) : Boolean;
begin
  Result := isHigher(Min, Version) and (isHigher(Version, Max) or (Version = Max));
end;

function TModVersion.isInstalled(Instance : TInstance) : Boolean;
var
i : Integer;
begin
  for i := 0 to InstallObjs.Count-1 do
    if not InstallObjs[i].isInstalled(Instance) then
      Exit(False);
  Exit(True);
end;

constructor TModFile.Create(Json : ISuperObject);
begin
  inherited Create(Json.S['url'], Json.S['download-filename']);
  Self.FileName := Json.S['filename'];
end;

function TModFile.installObj(TempFolder : String; Instance : TInstance) : Boolean;
begin
  Result := False;
  if FileExists(TempFolder + DFileName) then
    Result := RenameFile(TempFolder + DFileName, Instance.getInstanceFolder + 'mods\' + FileName);
end;

function TModFile.isInstalled(Instance : TInstance) : Boolean;
var
FilePath  : String;
begin
  FilePath := Instance.getInstanceFolder + 'mods\' + FileName;
  if GetSizeOfFile(FilePath) = 0 then
    DeleteFile(PWideChar(FilePath));
  Result := FileExists(PWideChar(FilePath));
end;

constructor TModArchive.Create(Json : ISuperObject);
var
InstallArray : TSuperArray;
i : Integer;
InstallObj : TModInstallObj;
begin
  inherited Create(Json.S['url'], Json.S['download-filename']);
  InstallObjs := TList<TModInstallObj>.Create;
  InstallArray := Json.A['obj'];
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

function TModArchive.installObj(TempFolder : String; Instance : TInstance) : Boolean;
var
Zipper : TExtractZip;
NewTempFolder : String;
i: Integer;
begin
  Result := False;
  if FileExists(TempFolder + DFileName) then
  begin
    NewTempFolder := TempFolder + 'CMDTemp\';
    Zipper := TExtractZip.Create(TempFolder + DFileName, NewTempFolder);
    Zipper.runTask(nil);
    Result := True;
    for i := 0 to InstallObjs.Count-1 do
      if not InstallObjs[i].installObj(NewTempFolder, Instance) then
        Result := False;
    DeleteFolder(NewTempFolder, nil);
    DeleteFile(PWideChar(TempFolder + DFileName));
  end;
end;

function TModArchive.isInstalled(Instance : TInstance) : Boolean;
var
i : Integer;
begin
  for i := 0 to InstallObjs.Count-1 do
    if not InstallObjs[i].isInstalled(Instance) then
      Exit(False);
  Exit(True);
end;

constructor TModInstallObj.Create(DownloadLink, DFileName : String);
begin
  Self.DownloadLink := DownloadLink;
  Self.DFileName := DFileName;
end;


end.
