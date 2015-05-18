unit ModpackUtils;

interface

uses ForgeUtils, System.Generics.Collections, Task, ModUtils;

type
  TModpack = class
    private
      FForge : TForge;
      ModpackID, Version : String;
      Mods : TDictionary<TMod, TModVersion>;
    public

  end;
  TLoadModpack = class(TTask)

  end;
  TModPackInstance = class(TForgeInstance)
    Modpack : TModpack;
  end;

var
ModPacks : TList<TModpack>;

implementation

end.
