unit ConsoleServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ProgressBar,
  InstanceUtils, InstanceSettings, JvExStdCtrls, Console, System.UITypes, shellapi;

type
  TConsoleServerF = class(TForm)
    btnTerminate: TButton;
    btnSettings: TButton;
    ProgressBar: TCMDProgressBar;
    mmoLog: TMemo;
    edtConsole: TEdit;
    btnEnter: TButton;
    btnOpenFolder: TButton;
    procedure btnTerminateClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnEnterClick(Sender: TObject);
    procedure edtConsoleKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnOpenFolderClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Launching : TLaunching;
    Instance : TInstance;
    CloseAfterTerminate : Boolean;
    procedure onClosed;
  end;

implementation

{$R *.dfm}

procedure TConsoleServerF.onClosed;
begin
  btnTerminate.Enabled := False;
  if CloseAfterTerminate then
    Self.Destroy;
end;

procedure TConsoleServerF.btnEnterClick(Sender: TObject);
var LineToWrite : Ansistring;
    BytesWritten : Cardinal;
begin
  LineToWrite := AnsiString(edtConsole.Text + sLineBreak);
  WriteFile(Launching.hWriteLine, LineToWrite[1],Length(LineToWrite),BytesWritten,nil);
  edtConsole.Text := '';
end;

procedure TConsoleServerF.btnOpenFolderClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,PChar('explore'),PChar(Instance.getInstanceFolder),nil,nil,SW_NORMAL);
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
      button := MessageDlg('Do you really want to SAVE the server before closing it?',mtWarning, mbYesNoCancel, 0);
      if button = mrYes then
        begin
          edtConsole.Text := 'stop';
          btnEnter.Click;
        end;
      if button = mrNo then
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
    button := MessageDlg('Do you really want to SAVE the server before closing it?',mtWarning, mbYesNoCancel, 0);
    if button = mrYes then
    begin
      edtConsole.Text := 'stop';
      btnEnter.Click;
      CanClose := False;
      CloseAfterTerminate := True;
    end
    else if button = mrNo then
    begin
      TerminateProcess(Launching.piProcess.hProcess, WM_DESTROY);
      Launching.Terminate;
      btnTerminate.Enabled := False;
    end
    else if button = mrCancel then
    begin
      CanClose := False;
    end;
  end;
end;

end.
