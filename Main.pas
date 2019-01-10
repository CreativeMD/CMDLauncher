unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, ProgressBar, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, StringUtils, SaveFileUtils, ShellApi, Task, DownloadUtils,
  System.Generics.Collections, CoreLoaderUpdate, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL;

type
  TMainF = class(TForm)
    mmoChangelog: TMemo;
    lblProgrammName: TLabel;
    btnUpdate: TButton;
    tmrUpdate: TTimer;
    btnCancel: TButton;
    UpdateProgress: TCMDProgressBar;
    lblVersion: TLabel;
    HTTP: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    procedure FormShow(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;
  TUpdateTaskManager = class(TTaskManager)
    function isEndless : Boolean; override;
  end;

const
CountdownTime = 10;
ProgramVersion : string = '1.2.0';

var
  MainF: TMainF;
  UpdateProgramVersion, ProgramName : String;
  //StartDownloading : Boolean;
  UpdateTaskManager : TUpdateTaskManager;

implementation

uses Logger;

{$R *.dfm}

function TUpdateTaskManager.isEndless : Boolean;
begin
  Result := False;
end;

procedure TMainF.btnCancelClick(Sender: TObject);
begin
  tmrUpdate.Enabled := False;
  btnCancel.Enabled := False;
  btnUpdate.Caption := 'Update';
end;

procedure TMainF.btnUpdateClick(Sender: TObject);
var
ServerVersion : String;
Files, TempFiles : TStringList;
i: Integer;
Tasks : TList<TTask>;
begin
  btnUpdate.Caption := 'Updating';
  tmrUpdate.Enabled := False;
  btnUpdate.Enabled := False;
  btnCancel.Enabled := False;
  try
    ServerVersion := HTTP.Get('http://creativemd.de/service/version_new.php?name=' + ProgramName);
  except
    on E: Exception do
      ServerVersion := '';
  end;

  if ServerVersion <> '' then
  begin
    if StringUtils.isHigher(UpdateProgramVersion, ServerVersion) then
    begin
      Files := TStringList.Create;
      Files.AddStrings(Explode(HTTP.Get('http://creativemd.de/service/files_new.php?name=' + ProgramName + '&version=' + UpdateProgramVersion), ';'));
      TempFiles := Explode(HTTP.Get('http://creativemd.de/service/files_new.php?name=' + ProgramName), ';');
      for i := 0 to TempFiles.Count-1 do
      begin
        if Files.IndexOf(TempFiles[i]) = -1 then
        begin
          if not FileExists(ProgramFolder + TempFiles[i].Replace('/', '\')) then
            Files.Add(TempFiles[i])
        end;
      end;
      Tasks := TList<TTask>.Create;
      for i := 0 to Files.Count-1 do
        Tasks.Add(TDownloadTask.Create('http://creativemd.bplaced.net/downloads/' + ProgramName + '/' + Files[i].Replace('\', '/'), ProgramFolder + Files[i].Replace('/', '\')));
      UpdateTaskManager := TUpdateTaskManager.Create(Tasks, Self.UpdateProgress);

      //UpdateTaskManager.Start;
      while UpdateTaskManager.isActive do
        Application.ProcessMessages;
    end;
    ShellExecute(Application.Handle, 'open', PChar(ProgramFolder + ProgramName + '.exe'), nil, nil, SW_NORMAL);
    Application.Terminate;
  end;
end;

procedure TMainF.FormCreate(Sender: TObject);
var
ChangeLog : String;
Programs : TArray<string>;
j : Integer;
begin
  MainLog.Listener.Add(mmoChangelog.Lines);
  lblVersion.Caption := string(lblVersion.Caption).Replace('${program_version}', ProgramVersion);
  //StartDownloading := False;

  try
    Programs := HTTP.Get('https://creativemd.de/service/programs.php').Split([';']);
  except
    on E: Exception do
      Programs := TArray<string>.Create();
  end;
  for j := 0 to Length(Programs)-1 do
  begin
    if FileExists(ProgramFolder + Programs[j] + '.exe') then
    begin
      ProgramName := Programs[j];
      lblProgrammName.Caption := ProgramName;
      UpdateProgramVersion := TSaveFile.Create(ProgramFolder + ProgramName + '.cfg').getString('version');
      ChangeLog := HTTP.Get('http://creativemd.de/service/changelog_new.php?name=' + ProgramName + '&version=' + UpdateProgramVersion);
      mmoChangelog.Lines.AddStrings(Explode(ChangeLog, '<br>'));
      break;
    end;
  end;
end;

procedure TMainF.FormShow(Sender: TObject);
begin
  tmrUpdate.Enabled := True;
end;

procedure TMainF.tmrUpdateTimer(Sender: TObject);
begin
  tmrUpdate.Tag := tmrUpdate.Tag + 1;
  if tmrUpdate.Tag < CountdownTime then
  begin
    btnUpdate.Caption := 'Update (' + IntToStr(CountdownTime - tmrUpdate.Tag) + ')';
  end
  else
    btnUpdateClick(btnUpdate);
end;

end.
