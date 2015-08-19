unit SideUtils;

interface

uses System.SysUtils;

type
  TSideType = (OnlyClient, OnlyServer, Universal);
  TSide = (TClient, TServer);
  TSideTypeHelper = record helper for TSideType
    function isCompatible(Side : TSide) : Boolean;
    function toString : String;
  end;
  TSideHelper = record helper for TSide
    function isCompatible(SideType : TSideType) : Boolean;
    function toString : String;
    function isServer : Boolean;
    function isClient : Boolean;
  end;

function parseSide(Input : string) : TSide;
function parseSideType(Input : string) : TSideType;

implementation

function parseSide(Input : string) : TSide;
begin
  if Input.ToLower = 'server' then
    Exit(TServer)
  else
    Exit(TClient);
end;

function parseSideType(Input : string) : TSideType;
begin
  if Input.ToLower = 'only client' then
    Exit(OnlyClient)
  else  if Input.ToLower = 'only server' then
    Exit(OnlyServer)
  else
    Exit(Universal);
end;

function TSideHelper.isCompatible(SideType : TSideType) : Boolean;
begin
  Exit(SideType.isCompatible(Self));
end;

function TSideHelper.toString : String;
begin
  if Self = TServer then
    Exit('Server')
  else
    Exit('Client');
end;

function TSideHelper.isServer : Boolean;
begin
  Result := Self = TServer;
end;

function TSideHelper.isClient : Boolean;
begin
  Result := Self = TClient;
end;

function TSideTypeHelper.toString : String;
begin
  if Self = OnlyClient then
    Exit('Only Client')
  else if Self = OnlyServer then
    Exit('Only Server')
  else
    Exit('Universal');
end;

function TSideTypeHelper.isCompatible(Side : TSide) : Boolean;
begin
  if Self = Universal then
    Exit(True);
  if Side = TClient then
    Exit(Self = OnlyClient);
  if Side = TServer then
    Exit(Self = OnlyServer);
end;

end.
