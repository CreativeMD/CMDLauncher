unit ResourcePackSelect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ResourcePackUtils;

type
  TResourceSelect = class(TForm)
    btnSave: TButton;
    lstVersions: TListBox;
    btnCancel: TButton;
    procedure btnSaveClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    ResourceSelect : TResourcepackSelect;
  end;

implementation

{$R *.dfm}

procedure TResourceSelect.btnSaveClick(Sender: TObject);
begin
  ResourceSelect.downloadRVersion(Self);
  ModalResult := mrOk;
end;

end.
