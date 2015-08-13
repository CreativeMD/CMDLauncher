unit LoadingForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ProgressBar,
  Vcl.Imaging.GIFImg, Vcl.ExtCtrls;

type
  TLoadingScreen = class(TForm)
    TaskProgress: TCMDProgressBar;
    lblTask: TLabel;
    lblLog: TLabel;
    imgLoading: TImage;
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  LoadingScreen: TLoadingScreen;

implementation

{$R *.dfm}

procedure TLoadingScreen.FormShow(Sender: TObject);
begin
  (imgLoading.Picture.Graphic as TGIFImage).AnimateLoop := glEnabled;
  (imgLoading.Picture.Graphic as TGIFImage).Animate := True;
end;

end.
