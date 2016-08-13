unit ModpackSettings;

interface

uses SettingUtils, SaveFileUtils, Vcl.Controls, cefvcl, ceflib, Vcl.StdCtrls, System.Classes, StringUtils, System.SysUtils,
ModpackUtils;

type
TModpackSelect = class(TSetting<Integer>)
  ModpackVersion : Integer;
  function isFilled : Boolean; override;
  constructor Create(Name, Title : String);
  procedure createControl(x, y : Integer; Parent : TWinControl); override;
  procedure destroyControl; override;
  function getUUID : string; override;
  procedure SaveToFile(SaveFile : TSaveFile); override;
  procedure LoadFromFile(SaveFile : TSaveFile); override;
  procedure chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
  function getItems : TStringList;
  function getMCVersion : String;
end;

implementation

uses ForgeUtils;

constructor TModpackSelect.Create(Name, Title : String);
begin
  inherited Create(Name, Title, -1, True);
  //HasTitle := False;
  setHideTitle;
end;

function TModpackSelect.isFilled : Boolean;
var
Modpack : TModpack;
begin
  Result := False;
  Modpack := ModpackUtils.getModPackByID(Value);
  if Modpack <> nil then
    if ModpackVersion = -1 then
      Exit(True)
    else
      Exit(True);
end;

function TModpackSelect.getMCVersion : String;
var
Modpack : TModpack;
ModpackV : TModpackVersion;
begin
  Modpack := ModpackUtils.getModPackByID(Value);
  if Modpack <> nil then
  begin
    if ModpackVersion = -1 then
      ModpackV := Modpack.getNewestVersion
    else
      ModpackV := Modpack.getVersionByID(ModpackVersion);
    if ModpackV <> nil then
      Exit(ForgeUtils.getForgeByUUID(ModpackV.Forge).MV);
  end;
  Exit('');
end;

procedure TModpackSelect.chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
data : TStringList;
Modpack : TModpack;
begin
  if string(url).Contains('#add') then
  begin
    data := Explode(url, '#add');
    if (data.Count = 2) and isStringNumber(data[1]) then
    begin
      Modpack := ModpackUtils.getModPackByID(StrToInt(data[1]));
      if Modpack <> nil then
      begin
        Value := StrToInt(data[1]);
        ModpackVersion := -1;
        if Controls.Count > 0 then
        begin
          TComboBox(Controls[0]).Items.Clear;
          TComboBox(Controls[0]).Items.AddStrings(getItems);
          TComboBox(Controls[0]).ItemIndex := TComboBox(Controls[0]).Items.IndexOf('newest');

          TLabel(Controls[2]).Caption := Modpack.Name;
        end;
      end;
    end;
  end;
end;

function TModpackSelect.getItems : TStringList;
var
Modpack : TModpack;
  i: Integer;
begin
  Result := TStringList.Create;
  Modpack := ModpackUtils.getModPackByID(Value);
  if Modpack <> nil then
  begin
    if Modpack.Versions.Count > 0 then
      Result.Add('newest');
    for i := 0 to Modpack.Versions.Count-1 do
      Result.Add(Modpack.Versions[i].Name);
  end;
end;

procedure TModpackSelect.createControl(x, y : Integer; Parent : TWinControl);
var
Chromium : TChromium;
ComboBox : TComboBox;
Modpack : TModpack;
ModpackV : TModpackVersion;
TitleLabel : TLabel;
begin
  ComboBox := TComboBox.Create(Parent);
  ComboBox.Parent := Parent;
  ComboBox.Left := x;

  ComboBox.Items := getItems;
  ComboBox.Style := csOwnerDrawFixed;

  Controls.Add(ComboBox);

  Chromium := TChromium.Create(Parent);
  Chromium.Parent := Parent;
  Chromium.Left := SettingUtils.xOffset;

  Chromium.OnAddressChange := chrmModsAddressChange;
  Chromium.Anchors := [akLeft,akRight,akTop];
  Chromium.Height := 600;
  Chromium.Width := Parent.Width-4-Chromium.Left;
  Chromium.Load('http://launcher.creativemd.de/index.php?cat=modpack&launcher=yes');
  //Chromium.Visible := False;
  Controls.Add(Chromium);

  TitleLabel := TLabel.Create(Parent);
  TitleLabel.Parent := Parent;
  TitleLabel.Caption := 'Nothing selected';
  TitleLabel.Left := x;
  TitleLabel.Top := y;
  TitleLabel.Font.Size := 20;


  ComboBox.Top := TitleLabel.Top+TitleLabel.Height+4;
  Chromium.Top := ComboBox.Top+ComboBox.Height+SettingUtils.SpaceY;

  Controls.Add(TitleLabel);

  Modpack := ModpackUtils.getModPackByID(Value);
  if Modpack <> nil then
  begin
    TitleLabel.Caption := Modpack.Name;
    if ModpackVersion = -1 then
      ComboBox.ItemIndex := TComboBox(Controls[0]).Items.IndexOf('newest')
    else
    begin
      ModpackV := Modpack.getVersionByID(ModpackVersion);
      if ModpackV <> nil then
        ComboBox.ItemIndex := TComboBox(Controls[0]).Items.IndexOf(ModpackV.Name);
    end;
  end;
end;

procedure TModpackSelect.destroyControl;
var
Modpack : TModpack;
ModpackV : TModpackVersion;
begin
  Modpack := ModpackUtils.getModPackByID(Value);
  if Modpack <> nil then
  begin
    ModpackV := Modpack.getVersionByName(TComboBox(Controls[0]).Text);
    if ModpackV <> nil then
      ModpackVersion := ModpackV.ID;
  end;
  TChromium(Controls[1]).Destroying;
end;

function TModpackSelect.getUUID : string;
begin
  Result := 'modpackselect';
end;

procedure TModpackSelect.SaveToFile(SaveFile : TSaveFile);
begin
  SaveFile.setInteger(name, Value);
  SaveFile.setInteger(name + 'v', ModpackVersion);
end;

procedure TModpackSelect.LoadFromFile(SaveFile : TSaveFile);
begin
  if SaveFile.hasKey(name) then
  begin
    Value := SaveFile.getInteger(name);
    ModpackVersion := -1;
    if SaveFile.hasKey(Name + 'v') then
      ModpackVersion := SaveFile.getInteger(Name + 'v');
  end;
end;


end.
