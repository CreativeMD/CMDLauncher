unit ModDownloadProgress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.GIFImg, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.StdCtrls;

type
  TModProgress = class(TForm)
    imgLoading: TImage;
    pbDownload: TProgressBar;
    lblStatus: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TModProgress.FormShow(Sender: TObject);
begin
  (imgLoading.Picture.Graphic as TGIFImage).AnimateLoop := glEnabled;
  (imgLoading.Picture.Graphic as TGIFImage).Animate := True;
end;

end.
