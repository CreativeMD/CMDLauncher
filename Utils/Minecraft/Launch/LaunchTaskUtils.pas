unit LaunchTaskUtils;

interface

uses Task, InstanceUtils, MinecraftLaunchCommand;

type
  TLaunchTask = class abstract(TTask)
    Command : TMinecraftLaunch;
    constructor Create(Title : String; Command : TMinecraftLaunch; Online : Boolean = False);
  end;

implementation

constructor TLaunchTask.Create(Title : String; Command : TMinecraftLaunch; Online : Boolean = False);
begin
  inherited Create(Title, Online, False);
  Self.Command := Command;
end;

end.
