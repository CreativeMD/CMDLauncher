unit OfflineForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TOfflineF = class(TForm)
    edtName: TEdit;
    lblOfflineName: TLabel;
    btnPlay: TButton;
    btnCancel: TButton;
    procedure edtNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TOfflineF.edtNameKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 then
  begin
    btnPlay.Click;
    Key := #0;
  end;
end;

end.
