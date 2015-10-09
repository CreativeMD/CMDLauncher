unit FileUtils;

interface

uses ProgressBar, System.SysUtils, System.IOUtils, System.Types, Winapi.Windows,
JclFileUtils, Task;

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

implementation

uses Logger;

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
begin
  Files := TDirectory.GetFiles(OldPath, '*', TSearchOption.soAllDirectories);

  if ProgressBar <> nil then
    ProgressBar.StartStep(Length(Files));

  for i := 0 to Length(Files)-1 do
  begin
    RenameFile(Files[i], NewPath + '\' + Files[i].Replace(OldPath, ''));
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
