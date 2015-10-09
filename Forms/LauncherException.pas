unit LauncherException;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ClipBrd;

type
  TErrorDialog = class(TForm)
    mmoLog: TMemo;
    lblError: TLabel;
    btnCopy: TButton;
    mmoDescription: TMemo;
    lblWhat: TLabel;
    btnTerminate: TButton;
    procedure btnTerminateClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

uses CoreLoader;

{$R *.dfm}

procedure TErrorDialog.btnCopyClick(Sender: TObject);
begin
  Clipboard.AsText := ProgramName + ' v' + ProgramVersion + sLineBreak + mmoLog.Text + sLineBreak + mmoDescription.Text;
end;

procedure TErrorDialog.btnTerminateClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
