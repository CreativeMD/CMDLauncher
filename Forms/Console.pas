unit Console;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ProgressBar,
  InstanceUtils, InstanceSettings, JvExStdCtrls, shellapi;

type
  TConsoleF = class;
  TArg<T> = reference to procedure(const Arg: T);
  TLaunching = class(TThread)
    ACommand, AParameters, Folder: String;
    CallBack: TArg<AnsiString>;
    piProcess: TProcessInformation;
    hReadLine: THandle;
    hWriteLine : THandle;
    OnClosed : procedure of object;
    constructor Create(ACommand, AParameters, Folder: String; CallBack: TArg<AnsiString>);
    procedure Execute; override;
  end;
  TConsoleF = class(TForm)
    btnTerminate: TButton;
    btnSettings: TButton;
    ProgressBar: TCMDProgressBar;
    mmoLog: TMemo;
    btnOpenFolder: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnTerminateClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Launching : TLaunching;
    Instance : TInstance;
    procedure onClosed;
  end;

implementation

{$R *.dfm}

procedure TConsoleF.onClosed;
begin
  btnTerminate.Enabled := False;
end;

constructor TLaunching.Create(ACommand, AParameters, Folder: String; CallBack: TArg<AnsiString>);
begin
  inherited Create;
  Self.ACommand := ACommand;
  Self.AParameters := AParameters;
  Self.Folder := Folder;
  Self.CallBack := CallBack;
end;

procedure TLaunching.Execute;
//const
//    CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  suiStartup: TStartupInfo;
  //pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  //dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWord;
  CurLine : Ansistring;
  Zeichen : AnsiChar;
  hRead: THandle;
  hWrite : THandle;
begin
  FillChar(saSecurity, SizeOf(TSecurityAttributes), 0);
  saSecurity.nLength := sizeof(SECURITY_ATTRIBUTES);
  saSecurity.bInheritHandle := TRUE;

  CreatePipe(hReadLine, hWriteLine, @saSecurity, 0);

  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
  begin
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      SetHandleInformation(hRead, HANDLE_FLAG_INHERIT, 0);
      //suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.hStdInput := hReadLine;
      //suiStartup.dwXSize := 854;
      //suiStartup.dwYSize := 480;
      suiStartup.dwFlags := STARTF_USESTDHANDLES;

      if CreateProcess(nil, pChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, PChar(Folder), suiStartup, piProcess) then
      begin
          CloseHandle(hWrite);
          CurLine := '';
          while ReadFile(hRead, Zeichen, 1, dRead, nil) do
          begin
            if dRead > 0 then
            begin
              if Zeichen = #$D then
              begin
                CallBack(CurLine);
                CurLine := '';
                Continue;
              end;
              if Zeichen = #$A then
                Continue;
              CurLine := CurLine + Zeichen;
            end;
          end;
          WaitForSingleObject(piProcess.hThread,INFINITE);
          WaitForSingleObject(piProcess.hProcess,INFINITE);
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
      end;
      //CloseHandle(hRead);
      //CloseHandle(hWrite);
  end;
  OnClosed;
  Terminate;
end;

procedure TConsoleF.btnOpenFolderClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,PChar('explore'),PChar(Instance.getInstanceFolder),nil,nil,SW_NORMAL);
end;

procedure TConsoleF.btnSettingsClick(Sender: TObject);
begin
  InstanceSettings.loadInstanceSettings(Instance);
end;

procedure TConsoleF.btnTerminateClick(Sender: TObject);
begin
  if btnTerminate.Caption = 'Close' then
    Self.Destroy
  else
    if Launching <> nil then
    begin
      TerminateProcess(Launching.piProcess.hProcess, WM_DESTROY);
      Launching.Terminate;
      btnTerminate.Enabled := False;
    end;
end;

procedure TConsoleF.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Launching) then
  begin
    Launching.Terminate;
  end;
end;

end.
