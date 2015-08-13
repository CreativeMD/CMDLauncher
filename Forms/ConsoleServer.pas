unit ConsoleServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ProgressBar,
  InstanceUtils, InstanceSettings, JvExStdCtrls, Console, System.UITypes;

type
  TConsoleServerF = class(TForm)
    btnTerminate: TButton;
    btnSettings: TButton;
    ProgressBar: TCMDProgressBar;
    mmoLog: TMemo;
    edtConsole: TEdit;
    btnEnter: TButton;
    procedure btnTerminateClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnEnterClick(Sender: TObject);
    procedure edtConsoleKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private-Deklarationen }
  public
    Launching : TLaunching;
    Instance : TInstance;
    procedure onClosed;
  end;

implementation

{$R *.dfm}

procedure TConsoleServerF.onClosed;
begin
  btnTerminate.Enabled := False;
end;

procedure TConsoleServerF.btnEnterClick(Sender: TObject);
var LineToWrite : Ansistring;
    BytesWritten : Cardinal;
begin
  LineToWrite := AnsiString(edtConsole.Text + sLineBreak);
  WriteFile(Launching.hWriteLine, LineToWrite[1],Length(LineToWrite),BytesWritten,nil);
  edtConsole.Text := '';
end;

procedure TConsoleServerF.btnSettingsClick(Sender: TObject);
begin
  InstanceSettings.loadInstanceSettings(Instance);
end;

procedure TConsoleServerF.btnTerminateClick(Sender: TObject);
var
button : Integer;
begin
  if btnTerminate.Caption = 'Close' then
    Self.Destroy
  else
    if Launching <> nil then
    begin
      button := MessageDlg('Do you really want to close the server without saving?',mtError, mbOKCancel, 0);
      if button = mrOk then
      begin
        TerminateProcess(Launching.piProcess.hProcess, WM_DESTROY);
        Launching.Terminate;
        btnTerminate.Enabled := False;
      end;
    end;
end;

procedure TConsoleServerF.edtConsoleKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 then
  begin
    btnEnter.Click;
    Key := #0;
  end;
end;

procedure TConsoleServerF.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
button: Integer;
begin
  if Assigned(Launching) and (Launching <> nil) and not Launching.Finished then
  begin
    button := MessageDlg('Do you really want to close the server without saving?',mtError, mbOKCancel, 0);
    if button = mrOk then
    begin
      CanClose := True;
      TerminateProcess(Launching.piProcess.hProcess, WM_DESTROY);
      Launching.Terminate;
    end
    else if button = mrCancel then
    begin
      CanClose := False;
    end;
  end;
end;

end.
