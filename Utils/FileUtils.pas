unit FileUtils;

interface

uses ProgressBar, System.SysUtils, System.IOUtils, System.Types, Winapi.Windows,
JclFileUtils, Task, TlHelp32, PsAPI, Vcl.Dialogs;

type
TDeleteFolder = class(TTask)
  protected
    Path : string;
    procedure runTask(Bar : TCMDProgressBar); override;
  public
    constructor Create(Title, Path : String);
end;
TRenameFolder = class(TTask)
  protected
    OldPath, NewPath : string;
    procedure runTask(Bar : TCMDProgressBar); override;
  public
    constructor Create(Title, OldPath, NewPath : String);
end;
TCopyFolder = class(TTask)
  protected
    Path, CopyPath : string;
    procedure runTask(Bar : TCMDProgressBar); override;
  public
    constructor Create(Title, Path, CopyPath : String);
end;

procedure RenameFolder(OldPath, NewPath : string; ProgressBar : TCMDProgressBar = nil);
procedure DeleteFolder(Path : string; ProgressBar : TCMDProgressBar = nil);
procedure CopyFolder(Path, CopyPath : string; ProgressBar : TCMDProgressBar = nil);
function GetSizeOfFile(const Filename: string): Int64;
function processExists(exeFileName: string): Boolean;
function GetPathFromPID(const PID: cardinal): string;

implementation

uses Logger;

function GetPathFromPID(const PID: cardinal): string;
var
  hProcess: THandle;
  path: array[0..MAX_PATH - 1] of char;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, PID);
  if hProcess <> 0 then
    try
      if GetModuleFileNameEx(hProcess, 0, path, MAX_PATH) = 0 then
        RaiseLastOSError;
      result := path;
    finally
      CloseHandle(hProcess)
    end
  //else
    //RaiseLastOSError;
end;

function processExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while Integer(ContinueLoop) <> 0 do
  begin
   // MainLog.log(GetPathFromPID(FProcessEntry32.th32ProcessID));
    try
      if (FProcessEntry32.th32ProcessID <> GetCurrentProcessId) and (UpperCase(GetPathFromPID(FProcessEntry32.th32ProcessID)) =
        UpperCase(ExeFileName)) then
      begin
        Result := True;
        Break;
      end;
    except

    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

function GetSizeOfFile(const Filename: string): Int64;
var
 sr : TSearchRec;
begin
  if FindFirst(fileName, faAnyFile, sr ) <> 0 then
    Exit(-1);
  try
    result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow);
  finally
    System.SysUtils.FindClose(sr) ;
  end;
end;

constructor TDeleteFolder.Create(Title, Path : String);
begin
  inherited Create(Title, False);
  Self.Path := Path;
end;

procedure TDeleteFolder.runTask(Bar : TCMDProgressBar);
begin
  DeleteFolder(Path, Bar);
end;

constructor TRenameFolder.Create(Title, OldPath, NewPath : String);
begin
  inherited Create(Title, False);
  Self.OldPath := OldPath;
  Self.NewPath := NewPath;
end;

procedure TRenameFolder.runTask(Bar : TCMDProgressBar);
begin
  RenameFolder(OldPath, NewPath, Bar);
end;

constructor TCopyFolder.Create(Title, Path, CopyPath : String);
begin
  inherited Create(Title, False);
  Self.Path := Path;
  Self.CopyPath := CopyPath;
end;

procedure TCopyFolder.runTask(Bar : TCMDProgressBar);
begin
  CopyFolder(Path, CopyPath, Bar);
end;

procedure RenameFolder(OldPath, NewPath : string; ProgressBar : TCMDProgressBar = nil);
var
Files : TStringDynArray;
  i: Integer;
  FileName : String;
begin
  Files := TDirectory.GetFiles(OldPath, '*', TSearchOption.soAllDirectories);

  if ProgressBar <> nil then
    ProgressBar.StartStep(Length(Files));

  for i := 0 to Length(Files)-1 do
  begin
    FileName := NewPath + '\' + Files[i].Replace(OldPath, '');
    ForceDirectories(ExtractFilePath(FileName));
    RenameFile(Files[i], FileName);
    if ProgressBar <> nil then
      ProgressBar.StepPos := i;
  end;

  DeleteDirectory(OldPath, False);

  if ProgressBar <> nil then
    ProgressBar.FinishStep;

end;

procedure DeleteFolder(Path : string; ProgressBar : TCMDProgressBar = nil);
var
Files : TStringDynArray;
  i: Integer;
begin
  Files := TDirectory.GetFiles(Path, '*', TSearchOption.soAllDirectories);

  if ProgressBar <> nil then
    ProgressBar.StartStep(Length(Files));

  for i := 0 to Length(Files)-1 do
  begin
    if not DeleteFile(PChar(Files[i])) then
      Logger.MainLog.log('Could not delete file: ' + Files[i]);
    if ProgressBar <> nil then
      ProgressBar.StepPos := i;
  end;

  DeleteDirectory(Path, False);

  if ProgressBar <> nil then
    ProgressBar.FinishStep;
end;

procedure CopyFolder(Path, CopyPath : string; ProgressBar : TCMDProgressBar = nil);
var
Files : TStringDynArray;
  i: Integer;
begin
  Files := TDirectory.GetFiles(Path, '*', TSearchOption.soAllDirectories);

  if ProgressBar <> nil then
    ProgressBar.StartStep(Length(Files));

  for i := 0 to Length(Files)-1 do
  begin
    ForceDirectories(ExtractFilePath(CopyPath + Files[i].Replace(Path, '')));
    CopyFile(PWideChar(Files[i]), PWideChar(CopyPath + Files[i].Replace(Path, '')), False);
    if ProgressBar <> nil then
      ProgressBar.StepPos := i;
  end;

  if ProgressBar <> nil then
    ProgressBar.FinishStep;
end;

end.
