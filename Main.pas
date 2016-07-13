unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, ProgressBar, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, StringUtils, SaveFileUtils, ShellApi, Task, DownloadUtils,
  System.Generics.Collections, CoreLoaderUpdate;

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

var
  MainF: TMainF;
  UpdateProgramVersion : String;
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
  FileStream : TFileStream;
Tasks : TList<TTask>;
begin
  btnUpdate.Caption := 'Updating';
  tmrUpdate.Enabled := False;
  btnUpdate.Enabled := False;
  btnCancel.Enabled := False;

  try
    ServerVersion := HTTP.Get('http://creativemd.de/service/version_new.php?name=CMDLauncher');
  except
    on E: Exception do
      ServerVersion := '';
  end;

  if ServerVersion <> '' then
  begin
    if StringUtils.isHigher(UpdateProgramVersion, ServerVersion) then
    begin
      Files := TStringList.Create;
      Files.AddStrings(Explode(HTTP.Get('http://creativemd.de/service/files_new.php?name=CMDLauncher&version=' + UpdateProgramVersion), ';'));
      TempFiles := Explode(HTTP.Get('http://creativemd.de/service/files_new.php?name=CMDLauncher'), ';');
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
        Tasks.Add(TDownloadTask.Create('http://creativemd.bplaced.net/downloads/CMDLauncher/' + Files[i].Replace('\', '/'), ProgramFolder + Files[i].Replace('/', '\')));
      UpdateTaskManager := TUpdateTaskManager.Create(Tasks, Self.UpdateProgress);

      //UpdateTaskManager.Start;
      while UpdateTaskManager.isActive do
        Application.ProcessMessages;
      {UpdateProgress.StartProcess(Files.Count);
      //StartDownloading := True;
      for i := 0 to Files.Count-1 do
      begin
        mmoChangelog.Lines.Add('Downloading ' + Files[i] + ' ...');
        ForceDirectories(ExtractFilePath(ExePfad + Files[i].Replace('/', '\')));
        FileStream := TFileStream.Create(ExePfad + Files[i].Replace('/', '\'), fmCreate);
        try
        HTTP.Get('http://creativemd.bplaced.net/downloads/CMDLauncher/' + Files[i].Replace('\', '/'), FileStream);
        except
          on E: Exception do
            mmoChangelog.Lines.Add('Failed to download file=' + Files[i].Replace('/', '\') + ' url=' + 'http://creativemd.bplaced.net/downloads/CMDLauncher/' + Files[i].Replace('\', '/'));

        end;
        FileStream.Destroy;
      end;   }
    end;
    ShellExecute(Application.Handle, 'open', PChar(ProgramFolder + 'CMDLauncher.exe'), nil, nil, SW_NORMAL);
    Application.Terminate;
  end;
end;

procedure TMainF.FormCreate(Sender: TObject);
var
ChangeLog : String;
begin
  MainLog.Listener.Add(mmoChangelog.Lines);
  lblVersion.Caption := string(lblVersion.Caption).Replace('${program_version}', ProgramVersion);
  //StartDownloading := False;
  if FileExists(ProgramFolder + 'CMDLauncher.cfg') then    { TODO : Redesign finding program to update }
  begin
    UpdateProgramVersion := TSaveFile.Create(ProgramFolder + 'CMDLauncher.cfg').getString('version');
    lblProgrammName.Caption := 'CMDLauncher';
    ChangeLog := HTTP.Get('http://creativemd.de/service/changelog_new.php?name=CMDLauncher&version=' + UpdateProgramVersion);
    mmoChangelog.Lines.AddStrings(Explode(ChangeLog, '<br>'));
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
