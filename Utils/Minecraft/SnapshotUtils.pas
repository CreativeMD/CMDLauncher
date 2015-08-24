unit SnapshotUtils;

interface

uses VanillaUtils, System.Generics.Collections, SettingUtils, System.Classes;

type
  TSnapshotInstance = class(TVanillaInstance)
    function getUUID : String; override;
    function getSettings : TList<TSetting>; override;
  end;
implementation

function TSnapshotInstance.getUUID : String;
begin
  Result := 'Snapshot';
end;

function TSnapshotInstance.getSettings : TList<TSetting>;
var
Selected : String;
Items : TStringList;
  i: Integer;
isOnline : Boolean;
begin
  Result := TList<TSetting>.Create;
  Items := TStringList.Create;
  isOnline := True;
  if MinecraftVersions <> nil then
  begin
    for i := 0 to MinecraftVersions.Count-1 do
      if (MinecraftVersions[i].ReleaseTyp = mvSnapshot) and (MinecraftVersions[i].LaunchTyp = ltNew) then
        Items.Add(MinecraftVersions[i].UUID);
    Selected := Items[0];
    isOnline := False;
  end
  else
    Selected := '';

  Result.Add(TSelectSetting.Create('mcversion', 'Version', Items, Selected, isOnline));
end;

end.
