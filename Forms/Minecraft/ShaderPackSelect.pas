unit ShaderPackSelect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections, ShaderPackUtils;

type
  TfrmShaderPackSelect = class(TForm)
    btnSave: TButton;
    lstVersions: TListBox;
    btnCancel: TButton;
    cbbMinecraft: TComboBox;
    lblMinecraft: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbbMinecraftChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    ShaderPackSelect : TShaderpackSelect;
    procedure reloadList;
  end;

implementation

uses VanillaUtils, StringUtils;

{$R *.dfm}

procedure TfrmShaderPackSelect.btnSaveClick(Sender: TObject);
begin
  ShaderPackSelect.downloadRVersion(Self);
  ModalResult := mrOk;
end;

procedure TfrmShaderPackSelect.reloadList;
var
  I: Integer;
begin
  lstVersions.Clear;
  for I := 0 to ShaderPackSelect.Shaderpack.Versions.Count-1 do
    if (cbbMinecraft.Text = '') or ShaderPackSelect.Shaderpack.Versions[i].MC.Contains(cbbMinecraft.Text) then
      lstVersions.Items.AddObject('[' + Implode(ShaderPackSelect.Shaderpack.Versions[i].MC, ',', False) + ']-' + ShaderPackSelect.Shaderpack.Versions[i].Name, ShaderPackSelect.Shaderpack.Versions[i]);
end;

procedure TfrmShaderPackSelect.cbbMinecraftChange(Sender: TObject);
begin
  reloadList;
end;

procedure TfrmShaderPackSelect.FormShow(Sender: TObject);
var
  i,j: Integer;
  MinecraftVersions : TList<TMinecraftVersion>;
begin
  for i := 0 to ShaderPackSelect.Shaderpack.Versions.Count-1 do
  begin
    MinecraftVersions := ShaderPackSelect.Shaderpack.Versions[i].getMinecraftVersion;
    for j := 0 to MinecraftVersions.Count-1 do
      if cbbMinecraft.Items.IndexOf(MinecraftVersions[j].UUID) = -1 then
        cbbMinecraft.Items.Add(MinecraftVersions[j].UUID);
  end;

  reloadList;
end;

end.
