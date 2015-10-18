unit TechnicUtils;

interface

uses InstanceUtils, ForgeUtils, Generics.Collections, superobject, SaveFileUtils, JavaUtils, AccountUtils,
Task, MinecraftLaunchCommand;

type
  TTechnicModpackVersion = class
    private
      FModpackName, FVersionName, FURL, FArchiveDownload : String;
      Forge : TForge;
    public
      constructor Create(ModpackName, VersionName, FURL : String); overload;
      constructor Create(Json : ISuperObject); overload;
      procedure saveToJson(Json : ISuperObject);
  end;
  TTechnicInstance = class(TForgeInstance)
    private
      FVersion : TTechnicModpackVersion;
    protected
      procedure Save(SaveFile : TSaveFile); override;
    public
      procedure LoadPost(SaveFile : TSaveFile); override;
      function getUUID : String; override;
      function getStartupTasks(MinecrafComand : TMinecraftLaunch) : TList<TTask>; override;
      function getCommand(Java : TJava; LoginData : TLoginData) : TMinecraftLaunch; override;
      property Version : TTechnicModpackVersion read FVersion;
  end;

function getTechnicModpack(ModpackName, VersionName : String) : TTechnicModpackVersion;

var
LoadedTechnicModpacks : TList<TTechnicModpackVersion>;

implementation

procedure TTechnicInstance.Save(SaveFile : TSaveFile);
var
Json : ISuperObject;
begin
  inherited Save(SaveFile);
  if Version <> nil then
  begin
    Json := TSuperObject.Create;
    Version.saveToJson(Json);
    SaveFile.setJson('modpackTechnicV', Json);
  end;
end;

function TTechnicInstance.getUUID : String;
begin
  Result := 'Technic';
end;

end.
