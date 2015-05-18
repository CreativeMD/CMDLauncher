unit MinecraftLaunchCommand;

interface

uses System.Classes, System.Generics.Collections, JavaUtils, System.SysUtils;

type
  TMinecraftLaunch = class abstract
    Java : TJava;
    MCVersion : String;
    Replacements : TDictionary<string, string>;
    SpecialArguments : TStringList;
    constructor Create(Java : TJava; mcversion : String);
    function getArch : String;
    function is64Bit : Boolean;
    function getLaunchCommand : String; overload;
    function getLaunchCommandList : TStringList; overload;
  end;

implementation

uses StringUtils;

function TMinecraftLaunch.getLaunchCommand : String;
var
  i: Integer;
  Key : String;
begin
  Result := '';
  for i := 0 to SpecialArguments.Count-1 do
    Result := Result + ' ' + SpecialArguments[i];
  for i := 0 to Replacements.Count-1 do
  begin
    Key := Replacements.Keys.ToArray[i];
    Result := Result.Replace(Key, Replacements.Items[Key]);
  end;
end;

function TMinecraftLaunch.getLaunchCommandList : TStringList;
var
  i, j: Integer;
  Key : String;
begin
  Result := TStringList.Create;
  for i := 0 to SpecialArguments.Count-1 do
    Result.Add(SpecialArguments[i].Replace(';', ';' + sLineBreak));
  for i := 0 to Replacements.Count-1 do
  begin
    Key := Replacements.Keys.ToArray[i];
    for j := 0 to Result.Count-1 do
    begin
      Result[j] := Result[j].Replace(Key, Replacements.Items[Key]);
    end;
  end;
end;

constructor TMinecraftLaunch.Create(Java : TJava; mcversion : String);
begin
  Replacements := TDictionary<string, string>.Create;
  SpecialArguments := TStringList.Create;
  Self.MCVersion := mcversion;
  Self.Java := Java;
end;

function TMinecraftLaunch.getArch : String;
begin
  if is64Bit then
    Result := '64'
  else
    Result := '32';
end;

function TMinecraftLaunch.is64Bit : Boolean;
begin
  Result := Java.is64;
end;

end.
