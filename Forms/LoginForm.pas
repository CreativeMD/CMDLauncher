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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

end.
