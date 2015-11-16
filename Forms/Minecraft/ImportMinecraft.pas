unit ImportMinecraft;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TImporter = class(TForm)
    btnCancel: TButton;
    edtURL: TEdit;
    btnSave: TButton;
    rbURL: TRadioButton;
    ImportSelection: TGroupBox;
    rbFile: TRadioButton;
    lblFile: TLabel;
    btnFile: TButton;
    dlgOpenFile: TOpenDialog;
    procedure btnFileClick(Sender: TObject);
    procedure edtURLChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TImporter.btnFileClick(Sender: TObject);
begin
  rbFile.Checked := True;
  if dlgOpenFile.Execute then
  begin
    lblFile.Caption := dlgOpenFile.FileName;
  end;
end;

procedure TImporter.edtURLChange(Sender: TObject);
begin
  rbURL.Checked := True;
end;

end.
