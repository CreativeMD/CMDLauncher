unit CMDChromeMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cefvcl;

type
  TMain = class(TForm)
    chrmMain: TChromium;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

end.
