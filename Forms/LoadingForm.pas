unit LoadingForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ProgressBar;

type
  TLoadingScreen = class(TForm)
    TaskProgress: TCMDProgressBar;
    lblTask: TLabel;
    lblLog: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  LoadingScreen: TLoadingScreen;

implementation

{$R *.dfm}

end.
