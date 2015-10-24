unit ModSelectForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, cefvcl, Vcl.ComCtrls,
  ceflib, StringUtils, ModUtils, Generics.Collections, InstanceUtils,
  Vcl.ImgList;

type
  TModSelectF = class(TForm)
    lvMods: TListView;
    chrmMods: TChromium;
    btnSave: TButton;
    btnDelete: TButton;
    btnCancel: TButton;
    lblTime: TLabel;
    TempList: TImageList;
    procedure chrmModsAddressChange(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring);
    procedure btnDeleteClick(Sender: TObject);

    public
      onlyOptional, isServer : Boolean;
      SelectedMC : String;
      function selectMods(SelectedMC : String; Mods : TDictionary<TMod, TModVersion>; onlyOptional, isServer : Boolean; modpack : Integer = -1) : TDictionary<TMod, TModVersion>;
      function AddMod(PMod : TMod; PVersion : TModVersion = nil) : Boolean;
      function ContainsMod(PMod : TMod) : Boolean;
      function getMod(ID : Integer) : TPair<TMod, TModVersion>;
      procedure refreshList;
  end;

implementation
uses  CoreLoader;

{$R *.dfm}

procedure TModSelectF.refreshList;
var
  i: Integer;
  r: TRect;
  ComboBox : TComboBox;
begin
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

function TModSelectF.ContainsMod(PMod : TMod) : Boolean;
var
  i: Integer;
begin
  for i := 0 to lvMods.Items.Count-1 do
  begin
    if lvMods.Items.Item[i].Caption = PMod.Title then
      Exit(True);
  end;
  Exit(False);
end;

function TModSelectF.AddMod(PMod : TMod; PVersion : TModVersion = nil) : Boolean;
var
ComboBox : TComboBox;
r: TRect;
Versions : TList<TModVersion>;
i : Integer;
begin
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
    Result := True;
  end;
end;

procedure TModSelectF.btnDeleteClick(Sender: TObject);
var
i : Integer;
begin
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
  refreshList;
end;

procedure TModSelectF.chrmModsAddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
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

function TModSelectF.getMod(ID : Integer) : TPair<TMod, TModVersion>;
var
Key : TMod;
Value : TModVersion;
begin
  Key := ModUtils.getModByName(lvMods.Items.Item[ID].Caption);
  if Key <> nil then
  begin
    Value := Key.getVersionByName(TComboBox(lvMods.Items.Item[ID].Data).Text);
    if Value <> nil then
      Exit(TPair<TMod, TModVersion>.Create(Key, Value));
  end;
  Exit(TPair<TMod, TModVersion>.Create(nil, nil));
end;

function TModSelectF.selectMods(SelectedMC : String; Mods : TDictionary<TMod, TModVersion>; onlyOptional, isServer : Boolean; modpack : Integer = -1) : TDictionary<TMod, TModVersion>;
var
Item : TPair<TMod, TModVersion>;
i: Integer;
url : String;
begin
  Self.SelectedMC := SelectedMC;
  Self.onlyOptional := onlyOptional;
  Self.isServer := isServer;

  url := 'http://launcher.creativemd.de/index.php?cat=mod&launcher=yes&mc=' + SelectedMC;
  if onlyOptional then
    url := url + '&optional=yes';
  if isServer then
    url := url + '&type=server';
  if modpack <> -1 then
    url := url + '&modpack=' + IntToStr(modpack);
  chrmMods.Load(url);

  for Item in Mods do
    AddMod(Item.Key, Item.Value);

  if Self.ShowModal = mrOk then
  begin
    Result := TDictionary<TMod, TModVersion>.Create;
    for i := 0 to lvMods.Items.Count-1 do
    begin
      Item := getMod(i);
      if Item.Key <> nil then
        Result.Add(Item.Key, Item.Value);
    end;
  end
  else
    Exit(nil);
end;

end.
