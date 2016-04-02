unit CreativeMD;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Vcl.StdCtrls, ShellApi;

type
  TCredit = class(TForm)
    imgAvatar: TImage;
    lblText: TLabel;
    lblTitle: TLabel;
    lblAutor: TLabel;
    btnClose: TButton;
    lblVersion: TLabel;
    lblWebsite: TLabel;
    lblWebsiteLink: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure lblWebsiteLinkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Credit: TCredit;

implementation

{$R *.dfm}

procedure TCredit.btnCloseClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TCredit.lblWebsiteLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(lblWebsiteLink.Caption), '', '', SW_SHOWNORMAL);
end;

end.
