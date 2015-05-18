unit Console;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ProgressBar,
  InstanceUtils, InstanceSettings, JvExStdCtrls;

type
  TConsoleF = class;
  TArg<T> = reference to procedure(const Arg: T);
  TLaunching = class(TThread)
    ACommand, AParameters, Folder: String;
    Consol : TConsoleF;
    CallBack: TArg<PAnsiChar>;
    piProcess: TProcessInformation;
    constructor Create(ACommand, AParameters, Folder: String; CallBack: TArg<PAnsiChar>);
    procedure Execute; override;
  end;
  TConsoleF = class(TForm)
    btnTerminate: TButton;
    btnSettings: TButton;
    ProgressBar: TCMDProgressBar;
    mmoLog: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnTerminateClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Launching : TLaunching;
    Instance : TInstance;
  end;

implementation

{$R *.dfm}

constructor TLaunching.Create(ACommand, AParameters, Folder: String; CallBack: TArg<PAnsiChar>);
begin
  inherited Create;
  Self.ACommand := ACommand;
  Self.AParameters := AParameters;
  Self.Folder := Folder;
  Self.CallBack := CallBack;
end;

procedure TLaunching.Execute;
const
    CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWord;
  dRunning: DWord;
begin
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := True;
  saSecurity.lpSecurityDescriptor := nil;

  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
  begin
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      //suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      //suiStartup.dwXSize := 854;
      //suiStartup.dwYSize := 480;
      suiStartup.dwFlags := STARTF_USESTDHANDLES;
      if CreateProcess(nil, pChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, PChar(Folder), suiStartup, piProcess) then
      begin
          repeat
              dRunning := WaitForSingleObject(piProcess.hProcess, 0);
              repeat
                  if dRunning <> 0 then
                  begin
                    dRead := 0;
                    ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                    pBuffer[dRead] := #0;

                    //OemToAnsi(pBuffer, pBuffer);
                    //Unicode support by Lars Fosdal
                    OemToCharA(pBuffer, dBuffer);
                    CallBack(dBuffer);
                  end;
              until (dRead < CReadBuffer);
              //Sleep(1);
          until dRunning = 0;
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
      end;
      CloseHandle(hRead);
      CloseHandle(hWrite);
  end;
  Consol.btnTerminate.Enabled := False;
  Terminate;
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
