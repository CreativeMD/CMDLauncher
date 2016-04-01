unit URLProtocolRegister;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

type
  TResult = class(TForm)
    btnok: TButton;
    lblFeature: TLabel;
    imgFailure: TImage;
    procedure btnokClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Result: TResult;
  Succied : Boolean;

implementation

{$R *.dfm}



procedure TResult.btnokClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
