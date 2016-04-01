program URLRegister;

uses
  Vcl.Forms,
  URLProtocolRegister in 'URLProtocolRegister.pas' {Result},
  URLProtocolUtils in 'Utils\URLProtocolUtils.pas',
  System.SysUtils,
  System.Classes;

{$R *.res}
var
ProgramFolder : String;
CommunicationFile : TStringList;
begin
  ProgramFolder := ExtractFilePath(ParamStr(0));
  if FileExists(ProgramFolder + 'CMDLauncher.exe') then
    Succied := registerProtocol('cmdlauncher', 'CMDLauncher', ProgramFolder + 'CMDLauncher.exe');
  if Succied then
  begin
    CommunicationFile := TStringList.Create;
    CommunicationFile.Add('reload');
    CommunicationFile.SaveToFile(ProgramFolder + 'Download\Temp\internal.cfg');
    Application.Terminate;
  end
  else
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TResult, Result);
    Application.Run;
  end;
end.
