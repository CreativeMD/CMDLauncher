unit LoginForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TLoginF = class(TForm)
    edtName: TEdit;
    edtPassword: TEdit;
    btnLogin: TButton;
    lblName: TLabel;
    lblPassword: TLabel;
    chkPassword: TCheckBox;
    btnCancel: TButton;
    btnOffline: TButton;
    lblError: TLabel;
    procedure edtNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TLoginF.edtNameKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 then
  begin
    btnLogin.Click;
    Key := #0;
  end;
end;

end.
