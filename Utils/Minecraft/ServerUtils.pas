unit ServerUtils;

interface

uses SettingUtils, System.Generics.Collections, System.IOUtils, InstanceUtils,
System.Classes, System.Types, System.SysUtils, SaveFileUtils;

type
  TEulaOption = class(TCheckOption)
    EulaSaveFile : TSaveFile;
    constructor Create(Name, Title : String; Default : Boolean; EulaSaveFile : TSaveFile; Online : Boolean = False);
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;

function getStandardServerSettings(Instance : TInstance) : TList<TSetting>;

implementation

constructor TEulaOption.Create(Name, Title : String; Default : Boolean; EulaSaveFile : TSaveFile; Online : Boolean = False);
begin
  inherited Create(Name, Title, Default, Online);
  Self.EulaSaveFile := EulaSaveFile;
end;

procedure TEulaOption.SaveToFile(SaveFile : TSaveFile);
begin
  inherited SaveToFile(EulaSaveFile);
end;

procedure TEulaOption.LoadFromFile(SaveFile : TSaveFile);
begin
  inherited LoadFromFile(EulaSaveFile);
end;

function getStandardServerSettings(Instance : TInstance) : TList<TSetting>;
var
Folders, Difficulty : TStringList;
Directories : TStringDynArray;
i: Integer;
begin
  Result := TList<TSetting>.Create;
  Directories := TDirectory.GetDirectories(Instance.getInstanceFolder);
  Folders := TStringList.Create;
  for i := 0 to Length(Directories)-1 do
  begin
    if FileExists(Directories[i] + 'level.dat') then
      Folders.Add(Directories[i].Replace(Instance.getInstanceFolder, ''));
  end;
  Result.Add(TTextSelectSetting.Create('level-name', 'World', Folders, 'world').setNotNeedFill);
  Result.Add(TEulaOption.Create('eula', 'EULA', False, TSaveFile.Create(Instance.getInstanceFolder + 'eula.txt')));
  Result.Add(TCheckOption.Create('online-mode', 'Online', True));
  Difficulty := TStringList.Create;
  Difficulty.Add('Peaceful');
  Difficulty.Add('Easy');
  Difficulty.Add('Normal');
  Difficulty.Add('Hard');
  Result.Add(TSelectSetting.Create('difficulty', 'Difficulty', Difficulty, 'Peaceful').setSaveNumber(True));
  { TODO 2 : Add more Settings }
end;

end.
