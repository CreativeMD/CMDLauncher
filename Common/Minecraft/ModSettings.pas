unit ModSettings;

interface

uses SettingUtils, ModUtils, System.SysUtils, System.Classes, Vcl.Controls, SaveFileUtils,
Generics.Collections, Vcl.StdCtrls, ForgeUtils, ModpackUtils, Vcl.ComCtrls, cefvcl, ceflib, System.Types,
Vcl.Graphics;

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
  TEnhancedModSelect = class(TSetting<TStringList>, IForgeUpdateSetting)
    private
      FCustomNotification : TCustomNotify;
    public
      isOptional, isServer : Boolean;
      Mods : TDictionary<TMod, TModVersion>;
      ForgeSelect : TForgeSelect;
      SelectedMC : String;
      ModpackVersion : TModpackVersion;
      constructor Create(Name, Title : String; isOptional, isServer : Boolean; ForgeSelect : TForgeSelect);
      procedure refreshList;
      procedure reloadURL;
      procedure updateErrorLabel;
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
      function updateForge : Boolean;
      property CustomNotification : TCustomNotify read FCustomNotification write FCustomNotification;
      function AddMod(PMod : TMod; PVersion : TModVersion = nil) : Boolean;
      function ContainsMod(PMod : TMod) : Boolean;
      function getMod(ID : Integer) : TPair<TMod, TModVersion>;
      function getMods : TList<TMod>;
      procedure onButtonClicked(Sender : TObject);
      procedure OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
      procedure onForgeChanges(ForgeSelect : TForgeSelect);
  end;

implementation

uses ModSelectForm, StringUtils, Logger;

function TEnhancedModSelect.updateForge : Boolean;
begin
  SelectedMC := '';
  if Assigned(FCustomNotification) then
    SelectedMC := FCustomNotification
  else if ForgeSelect <> nil then
    SelectedMC := ForgeSelect.getMCVersion;
  Result := SelectedMC <> '';
end;

constructor TEnhancedModSelect.Create(Name, Title : String; isOptional, isServer : Boolean; ForgeSelect : TForgeSelect);
begin
  inherited Create(Name, Title, TStringList.Create, True);
  Mods := TDictionary<TMod, TModVersion>.Create;
  Self.isOptional := isOptional;
  Self.isServer := isServer;
  Self.SelectedMC := '';
  Self.ForgeSelect := ForgeSelect;
  Self.CustomNotification := nil;
  updateForge;
  setHideTitle;
  _AddRef;
end;

procedure TEnhancedModSelect.refreshList;
var
  i: Integer;
  r: TRect;
  ComboBox : TComboBox;
  lvMods : TListView;
begin
  lvMods := TListView(Controls[0]);
  for i := 0 to lvMods.Items.Count-1 do
  begin
    if lvMods.Items.Item[i].Data <> nil then
    begin
      ComboBox := TComboBox(lvMods.Items.Item[i].Data);
      r := lvMods.Items.Item[i].DisplayRect(drBounds);
      r.Left  := r.Left + lvMods.columns[0].Width;
      r.Right := r.Left + lvMods.columns[1].Width;
      ComboBox.BoundsRect := r;
    end;
  end;
end;

function TEnhancedModSelect.ContainsMod(PMod : TMod) : Boolean;
var
  i: Integer;
  lvMods : TListView;
begin
  lvMods := TListView(Controls[0]);
  for i := 0 to lvMods.Items.Count-1 do
  begin
    if lvMods.Items.Item[i].Caption = PMod.Title then
      Exit(True);
  end;
  Exit(False);
end;

procedure TEnhancedModSelect.updateErrorLabel;
var
  i, j: Integer;
  Mods : TList<TMod>;
  PMod, RequiredMod : TMod;
  Required : String;
begin
  Required := '';
  Mods := getMods;
  for i := 0 to Mods.Count-1 do
  begin
    PMod := Mods[i];
    if PMod <> nil then
    begin
      for j := 0 to PMod.Required.Count-1 do
      begin
        RequiredMod := getModByID(PMod.Required[j]);
        if not Mods.Contains(RequiredMod) then
        begin
          Required := Required + 'Missing "' + RequiredMod.Title + '" required by "' + PMod.Title + '". ';
        end;
      end;
    end;
  end;


  TLabel(Controls[3]).Caption := Required;
end;

function TEnhancedModSelect.AddMod(PMod : TMod; PVersion : TModVersion = nil) : Boolean;
var
ComboBox : TComboBox;
r: TRect;
Versions : TList<TModVersion>;
i : Integer;
  lvMods : TListView;
begin
  lvMods := TListView(Controls[0]);
  Result := False;
  if not ContainsMod(PMod) then
  begin
    with lvMods.Items.Add do
    begin
      Caption := PMod.Title;

      r := DisplayRect(drBounds);
      r.Left  := r.Left + lvMods.columns[0].Width;
      r.Right := r.Left + lvMods.columns[1].Width;
      ComboBox := TComboBox.Create(lvMods);
      ComboBox.Parent := lvMods;
      ComboBox.BoundsRect := r;
      ComboBox.SendToBack;
      ComboBox.Style := csOwnerDrawFixed;
      ComboBox.Sorted := True;
      Versions := PMod.getValidVersionsMC(SelectedMC);
      for i := 0 to Versions.Count-1 do
        ComboBox.Items.Add(Versions[i].Name);
      if PVersion = nil then
      begin
        if ComboBox.Items.Count > 0 then
          ComboBox.ItemIndex := ComboBox.Items.Count-1;
          //Checked := False;
      end
      else
      begin
        ComboBox.ItemIndex := ComboBox.Items.IndexOf(PVersion.Name);
        //Checked := True;
      end;
      Data := ComboBox;
    end;
    updateErrorLabel;
    Result := True;
  end;
end;

function TEnhancedModSelect.getMod(ID : Integer) : TPair<TMod, TModVersion>;
var
Key : TMod;
Value : TModVersion;
lvMods : TListView;
begin
  lvMods := TListView(Controls[0]);
  Key := ModUtils.getModByName(lvMods.Items.Item[ID].Caption);
  if Key <> nil then
  begin
    Value := Key.getVersionByName(TComboBox(lvMods.Items.Item[ID].Data).Text, SelectedMC);
    Exit(TPair<TMod, TModVersion>.Create(Key, Value));
  end;
  Exit(TPair<TMod, TModVersion>.Create(nil, nil));
end;

function TEnhancedModSelect.getMods : TList<TMod>;
var
lvMods : TListView;
PMod : TMod;
  i: Integer;
begin
  Result := TList<TMod>.Create;
  lvMods := TListView(Controls[0]);
  for i := 0 to lvMods.Items.Count-1 do
  begin
    PMod := ModUtils.getModByName(lvMods.Items.Item[i].Caption);
    if PMod <> nil then
      Result.Add(PMod);
  end;
end;

procedure TEnhancedModSelect.OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
data : TStringList;
TempMod : TMod;
begin
  if string(url).Contains('#add') then
  begin
    data := Explode(url, '#add');
    if (data.Count = 2) and isStringNumber(data[1]) then
    begin
      TempMod := ModUtils.getModByID(StrToInt(data[1]));
      if TempMod <> nil then
        AddMod(TempMod);
    end;
  end;
end;


procedure TEnhancedModSelect.onButtonClicked(Sender : TObject);
var
i : Integer;
lvMods : TListView;
begin
  lvMods := TListView(Controls[0]);
  i := 0;
  while i < lvMods.Items.Count do
  begin
    if lvMods.Items.Item[i].Selected then
    begin
      TComboBox(lvMods.Items.Item[i].Data).Destroy;
      lvMods.Items.Delete(i);
    end
    else
      i := i + 1;
  end;
  updateErrorLabel;
  refreshList;
end;

procedure TEnhancedModSelect.onForgeChanges(ForgeSelect : TForgeSelect);
var
ComboBox : TComboBox;
Versions : TList<TModVersion>;
Item : TPair<TMod, TModVersion>;
i,j : Integer;
lvMods : TListView;
begin
  if Controls.Count = 0 then
    Exit;
  lvMods := TListView(Controls[0]);
  updateForge;

  for i := 0 to lvMods.Items.Count-1 do
  begin
    if lvMods.Items.Item[i].Data <> nil then
    begin
      ComboBox := TComboBox(lvMods.Items.Item[i].Data);
      Item := getMod(i);
      ComboBox.Clear;
      Versions := Item.Key.getValidVersionsMC(SelectedMC);
      for j := 0 to Versions.Count-1 do
        ComboBox.Items.Add(Versions[j].Name);
      if Item.Value = nil then
      begin
        if ComboBox.Items.Count > 0 then
          ComboBox.ItemIndex := ComboBox.Items.Count-1;
      end
      else
      begin
        ComboBox.ItemIndex := ComboBox.Items.IndexOf(Item.Value.Name);
      end;
    end;
    reloadURL;
  end;


end;

procedure TEnhancedModSelect.reloadURL;
var
chrmMods : TChromium;
modpack : Integer;
url : String;
Item : TPair<TMod, TModVersion>;
begin
  chrmMods := TChromium(Controls[2]);

  url := 'http://launcher.creativemd.de/index.php?cat=mod&launcher=yes&mc=' + SelectedMC;
  if isOptional then
    url := url + '&optional=yes';
  if isServer then
    url := url + '&type=server'
  else
    url := url + '&type=client';

  modpack := -1;
  if updateForge and ModUtils.ModsLoaded and (ModpackVersion <> nil) then
    modpack := ModpackVersion.ID;
  if modpack <> -1 then
    url := url + '&modpack=' + IntToStr(modpack);

  for Item in Mods do
    AddMod(Item.Key, Item.Value);
  //chrmMods.Load(url);
  chrmMods.Load(url);
end;

procedure TEnhancedModSelect.createControl(x, y : Integer; Parent : TWinControl);
var
ModsListView : TListView;
chrmMods : TChromium;
DeleteButton : TButton;
TempList : TImageList;
ErrorLabel : TLabel;
begin
  ErrorLabel := TLabel.Create(Parent);
  ErrorLabel.Parent := Parent;
  ErrorLabel.Font.Color := clRed;
  ErrorLabel.Caption := '';
  ErrorLabel.Left := x;
  ErrorLabel.Top := y;

  y := y + ErrorLabel.Height + SpaceY;

  ModsListView := TListView.Create(Parent);
  ModsListView.Parent := Parent;
  ModsListView.Left := x;
  ModsListView.Top := y;
  ModsListView.Width := 226;
  ModsListView.Height := 542;
  ModsListView.ViewStyle := vsReport;
  ModsListView.Anchors := [akLeft,akTop];
  with ModsListView.Columns.Add do
  begin
    Caption := 'Name';
    Width := 111;
    AutoSize := True;
  end;
  with ModsListView.Columns.Add do
  begin
    Caption := 'Version';
    Width := 111;
    AutoSize := True;
  end;
  ModsListView.DoubleBuffered := True;
  ModsListView.MultiSelect := True;
  ModsListView.Name := 'lvMods';
  ModsListView.RowSelect := True;
  ModsListView.ReadOnly := True;

  TempList := TImageList.Create(Parent);
  //TempList.Parent := Parent;
  TempList.Height := 24;
  TempList.Width := 1;
  ModsListView.SmallImages := TempList;

  Controls.Add(ModsListView);

  DeleteButton := TButton.Create(Parent);
  DeleteButton.Parent := Parent;
  DeleteButton.Left := x;
  DeleteButton.Top := ModsListView.Top + 4 + ModsListView.Height;
  DeleteButton.Caption := 'Delete';
  DeleteButton.OnClick := onButtonClicked;

  Controls.Add(DeleteButton);

  chrmMods := TChromium.Create(Parent);
  chrmMods.Parent := Parent;
  chrmMods.Left := x+ModsListView.Width+4;
  chrmMods.Top := y;
  chrmMods.Height := DeleteButton.Top+DeleteButton.Height-y;//411;
  chrmMods.Width := Parent.Width-8-chrmMods.Left;
  chrmMods.Anchors := [akLeft,akTop,akRight];
  chrmMods.OnAddressChange := OnAddressChange;

  Controls.Add(chrmMods);

  Controls.Add(ErrorLabel);

  updateForge;
  reloadURL;

end;

procedure TEnhancedModSelect.destroyControl;
var
Item : TPair<TMod, TModVersion>;
i: Integer;
lvMods : TListView;
begin
  lvMods := TListView(Controls[0]);
  Mods := TDictionary<TMod, TModVersion>.Create;
  for i := 0 to lvMods.Items.Count-1 do
  begin
    Item := getMod(i);
    if Item.Key <> nil then
      Mods.Add(Item.Key, Item.Value);
  end;
end;

function TEnhancedModSelect.getUUID : string;
begin
  Result := 'modselect';
end;

procedure TEnhancedModSelect.SaveToFile(SaveFile : TSaveFile);
var
Item : TPair<TMod, TModVersion>;
begin
  Value := TStringList.Create;
  for Item in Mods do
    Value.Add(InttoStr(Item.Key.ID) + ':' + IntToStr(Item.Value.ID));
  SaveFile.setStringList(Name, Value);
end;

procedure TEnhancedModSelect.LoadFromFile(SaveFile : TSaveFile);
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
          Logger.MainLog.log('Failed to load a mod from data! data=' + Value[i]);
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
          Logger.MainLog.log('Failed to load a mod from data! data=' + Value[i]);
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
