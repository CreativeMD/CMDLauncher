unit ResourcePackSelect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ResourcePackUtils, System.Generics.Collections;

type
  TResourceSelect = class(TForm)
    btnSave: TButton;
    lstVersions: TListBox;
    btnCancel: TButton;
    cbbMinecraft: TComboBox;
    cbbResolution: TComboBox;
    lblMinecraft: TLabel;
    lblResolution: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbbMinecraftChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    ResourceSelect : TResourcepackSelect;
    procedure reloadList;
  end;

implementation

uses VanillaUtils, StringUtils;

{$R *.dfm}

procedure TResourceSelect.btnSaveClick(Sender: TObject);
begin
  ResourceSelect.downloadRVersion(Self);
  ModalResult := mrOk;
end;

procedure TResourceSelect.reloadList;
var
  I: Integer;
begin
  lstVersions.Clear;
  for I := 0 to ResourceSelect.ResourcePack.Versions.Count-1 do
    if ((cbbMinecraft.Text = '') or ResourceSelect.ResourcePack.Versions[i].MC.Contains(cbbMinecraft.Text)) and ((cbbResolution.Text = '') or (ResourceSelect.ResourcePack.Versions[i].getResolution = cbbResolution.Text)) then
      lstVersions.Items.AddObject('[' + Implode(ResourceSelect.ResourcePack.Versions[i].MC, ',', False) + ']-' + ResourceSelect.ResourcePack.Versions[i].Name, ResourceSelect.ResourcePack.Versions[i]);
end;

procedure TResourceSelect.cbbMinecraftChange(Sender: TObject);
begin
  reloadList;
end;

procedure TResourceSelect.FormShow(Sender: TObject);
var
  i,j: Integer;
  MinecraftVersions : TList<TMinecraftVersion>;
begin
  for i := 0 to ResourceSelect.ResourcePack.Versions.Count-1 do
  begin
    MinecraftVersions := ResourceSelect.ResourcePack.Versions[i].getMinecraftVersion;
    for j := 0 to MinecraftVersions.Count-1 do
      if cbbMinecraft.Items.IndexOf(MinecraftVersions[j].UUID) = -1 then
        cbbMinecraft.Items.Add(MinecraftVersions[j].UUID);

    if cbbResolution.Items.IndexOf(ResourceSelect.ResourcePack.Versions[i].getResolution) = -1 then
      cbbResolution.Items.Add(ResourceSelect.ResourcePack.Versions[i].getResolution);
  end;

  reloadList;
end;

end.
