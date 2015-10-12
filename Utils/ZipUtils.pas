unit ZipUtils;

interface

uses Task, System.Classes, ProgressBar, AbZipper, AbUnzper, System.SysUtils,
AbArcTyp, System.Types, System.IOUtils, SortingUtils;

type
  TExtractZip = class(TTask)
    ZipFile, ExtractFolder : String;
    Exclude : TWhiteBlackList;
    constructor Create(ZipFile, ExtractFolder : String);
    procedure runTask(Bar : TCMDProgressBar); override;
  end;
  TArchiveFolder = class(TTask)
    ZipFile, ArchiveFolder : String;
    Exclude : TWhiteBlackList;
    constructor Create(ZipFile, ArchiveFolder : String);
    procedure runTask(Bar : TCMDProgressBar); override;
  end;
implementation

uses FileUtils;

constructor TExtractZip.Create(ZipFile, ExtractFolder : String);
begin
  inherited Create('Extracting Zip', False);
  Self.ZipFile := ZipFile;
  Self.ExtractFolder := ExtractFolder;
  Self.Exclude := TWhiteBlackList.Create;
end;

procedure TExtractZip.runTask(Bar : TCMDProgressBar);
var
UnZipper : TAbUnZipper;
i: Integer;
canExtract : Boolean;
begin
  Self.Log.log('Extracting ' + ExtractFileName(ZipFile) + ' 0%');
  UnZipper := TAbUnZipper.Create(nil);
  UnZipper.OpenArchive(ZipFile);
  UnZipper.ExtractOptions := [eoCreateDirs,eoRestorePath];
  UnZipper.BaseDirectory := ExtractFolder;
  if Bar <> nil then
    Bar.StartStep(UnZipper.Count);
  for i := 0 to UnZipper.Count-1 do
  begin
    canExtract := Exclude.isValid(UnZipper.Items[i].FileName);
    if canExtract then
      UnZipper.ExtractAt(i, '');
    if Bar <> nil then
      Bar.StepPos := i;
    Self.Log.logLastLine('Extracting ' + ExtractFileName(ZipFile) + ' ' + IntToStr(Round(i/UnZipper.Count*100)) + '%');
  end;
  Self.Log.logLastLine('Extracted ' + ExtractFileName(ZipFile));
  UnZipper.CloseArchive;
  UnZipper.Destroy;
  if Bar <> nil then
    Bar.FinishStep;
end;

constructor TArchiveFolder.Create(ZipFile, ArchiveFolder : String);
begin
  inherited Create('Creating Zip', False);
  Self.ZipFile := ZipFile;
  Self.ArchiveFolder := ArchiveFolder;
  Self.Exclude := TWhiteBlackList.Create;
end;

procedure TArchiveFolder.runTask(Bar : TCMDProgressBar);
var
Zipper : TAbZipper;
Files : TStringDynArray;
i: Integer;
FileStream : TFileStream;
FileName : String;
canArchive : Boolean;
begin
  Self.Log.log('Zipping ' + ExtractFileName(ZipFile) + ' 0%');
  Zipper := TAbZipper.Create(nil);
  Zipper.AutoSave := True;
  Zipper.FileName := ZipFile;
  Files := TDirectory.GetFiles(ArchiveFolder, '*', TSearchOption.soAllDirectories);
  if FileExists(ArchiveFolder) then
    DeleteFile(ArchiveFolder);
  if Bar <> nil then
    Bar.StartStep(Length(Files));
  for i := 0 to Length(Files)-1 do
  begin
    FileName := Files[i].Replace(ArchiveFolder, '');
    canArchive := Exclude.isValid(FileName);
    if canArchive then
    begin
      FileStream := TFileStream.Create(Files[I], fmOpenRead);
      Zipper.AddFromStream(FileName.Replace('\', '/'), FileStream);
      FileStream.Destroy;
    end;
    if Bar <> nil then
      Bar.StepPos := i;
    Self.Log.logLastLine('Zipping ' + ExtractFileName(ZipFile) + ' ' + IntToStr(Round(i/Length(Files)*100)));
  end;
  Zipper.Save;
  Zipper.CloseArchive;
  Zipper.Destroy;
  if Bar <> nil then
    Bar.FinishStep;
  Self.Log.logLastLine('Zipped ' + ExtractFileName(ZipFile));
end;

end.
