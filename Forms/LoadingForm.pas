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

uses Overview, CoreLoader;

{$R *.dfm}

procedure TLoadingScreen.FormShow(Sender: TObject);
begin
  SetWindowPos(Application.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);

  imgLoading.Picture.LoadFromFile(ProgramFolder + 'Assets\CustomIcons\default.gif');
  (imgLoading.Picture.Graphic as TGIFImage).AnimateLoop := glEnabled;
  (imgLoading.Picture.Graphic as TGIFImage).Animate := True;
  Left := OverviewF.Left + OverviewF.Width div 2 - Self.Width div 2;
  Top := OverviewF.Top + OverviewF.Height div 2 - Self.Height div 2;
end;

end.
