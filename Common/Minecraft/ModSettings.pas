unit ModSettings;

interface

uses SettingUtils, ModUtils, System.SysUtils, System.Classes, Vcl.Controls, SaveFileUtils,
Generics.Collections, Vcl.StdCtrls, ForgeUtils, ModpackUtils;

type
  TCustomNotify = function() : String of object;
  TModSelect = class(TSetting<TStringList>)
    private
      FCustomNotification : TCustomNotify;
    public
      isOptional, isServer : Boolean;
      Mods : TDictionary<TMod, TModVersion>;
      ForgeSelect : TForgeSelect;
      SelectedMC : String;
      ModpackVersion : TModpackVersion;
      constructor Create(Name, Title : String; isOptional, isServer : Boolean; ForgeSelect : TForgeSelect);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
      procedure onButtonClicked(Sender : TObject);
      function updateForge : Boolean;
      property CustomNotification : TCustomNotify read FCustomNotification write FCustomNotification;
  end;

implementation

uses ModSelectForm, StringUtils, Logger;

function TModSelect.updateForge : Boolean;
begin
  SelectedMC := '';
  if Assigned(FCustomNotification) then
    SelectedMC := FCustomNotification
  else if ForgeSelect <> nil then
    SelectedMC := ForgeSelect.getMCVersion;
  Result := SelectedMC <> '';
end;

procedure TModSelect.onButtonClicked(Sender : TObject);
var
ModSelect : TModSelectF;
modpack : Integer;
TempMods : TDictionary<TMod, TModVersion>;
begin
  if updateForge and ModUtils.ModsLoaded then
  begin
    ModSelect := TModSelectF.Create(nil);
    modpack := -1;
    if ModpackVersion <> nil then
      modpack := ModpackVersion.ID;
    TempMods := ModSelect.selectMods(SelectedMC, Mods, isOptional, isServer, modpack);
    if TempMods <> nil then
      Mods := TempMods;
  end;
end;

constructor TModSelect.Create(Name, Title : String; isOptional, isServer : Boolean; ForgeSelect : TForgeSelect);
begin
  inherited Create(Name, Title, TStringList.Create, True);
  Mods := TDictionary<TMod, TModVersion>.Create;
  Self.isOptional := isOptional;
  Self.isServer := isServer;
  Self.SelectedMC := '';
  Self.ForgeSelect := ForgeSelect;
  Self.CustomNotification := nil;
  updateForge;
end;

procedure TModSelect.createControl(x, y : Integer; Parent : TWinControl);
var
Button : TButton;
begin
  Button := TButton.Create(Parent);
  Button.Parent := Parent;
  Button.Left := x;
  Button.Top := y;
  Button.Caption := 'Edit Mods';
  Button.OnClick := onButtonClicked;
  Controls.Add(Button);
end;

procedure TModSelect.destroyControl;
begin

end;

function TModSelect.getUUID : string;
begin
  Result := 'modselect';
end;

procedure TModSelect.SaveToFile(SaveFile : TSaveFile);
var
Item : TPair<TMod, TModVersion>;
begin
  Value := TStringList.Create;
  for Item in Mods do
    Value.Add(InttoStr(Item.Key.ID) + ':' + IntToStr(Item.Value.ID));
  SaveFile.setStringList(Name, Value);
end;

procedure TModSelect.LoadFromFile(SaveFile : TSaveFile);
var
i: Integer;
Item : TPair<TMod, TModVersion>;
Splits : TStringList;
begin
  Value := SaveFile.getStringList(Name);
  for i := 0 to Value.Count-1 do
  begin
    Splits := Explode(Value[i], ':');
    if Splits.Count = 2 then
    begin
      try
      Item.Key := ModUtils.getModByID(StrtoInt(Splits[0]));
      except
        on E: Exception do
        begin
          Logger.Log.log('Failed to load a mod from data! data=' + Value[i]);
        end;
      end;
      if Item.Key <> nil then
      begin
        Item.Value := Item.Key.getVersionByID(StrToInt(Splits[1]));
        if Item.Value <> nil then
          Mods.Add(Item.Key, Item.Value);
      end;
    end;
  end;
end;

end.
