unit ImportUtils;

interface

uses SaveFileUtils, System.Classes, System.SysUtils, Vcl.Dialogs, Vcl.Forms, System.IOUtils, Winapi.Windows, System.UITypes,
System.Generics.Collections;

procedure import;
function importModpack(URL : String) : Boolean;
function importFile(FileName : String) : Boolean;
procedure openTempInstance(Name : String);

implementation

uses Overview, ImportMinecraft, ModpackUtils, CoreLoader, InstanceUtils, FileUtils, StringUtils, InstanceSettings,
VanillaUtils, DownloadUtils, Task;

procedure openTempInstance;
var
TemporaryInstance : TInstance;
begin
  TemporaryInstance := TVanillaInstance.Create(Name);
  loadInstanceSettings(TemporaryInstance, true);
  TemporaryInstance.Destroy;
end;

function importModpack(URL : String) : Boolean;
var
Name : String;
Modpack : TModpack;
SaveFile : TSaveFile;
data : TStringList;
begin
  Name := OverviewF.getNextUnusedInstanceFolder;
  SaveFile := TSaveFile.Create(InstanceFolder + Name + '\' + InstanceUtils.SaveFileName);
  Modpack := nil;
  if string(url).Contains('#add') then
  begin
    data := Explode(url, '#add');
    if (data.Count = 2) and isStringNumber(data[1]) then
    begin
      Modpack := ModpackUtils.getModPackByID(StrToInt(data[1]));
    end;
  end;
  if Modpack = nil then
  begin
    ShowMessage('Failed to load modpack!');
    Exit(False);
  end
  else
  begin
    SaveFile.setString('uuid', 'Modpack');
    SaveFile.setInteger('modpack', Modpack.ID);
    SaveFile.setInteger('modpackV', -1);
    openTempInstance(Name);
    Exit(True);
  end;
end;

function importFile(FileName : String) : Boolean;
var
Name : String;
DownloadTask : TDownloadTask;
Tasks : TList<TTask>;
begin
  Name := OverviewF.getNextUnusedInstanceFolder;
  if FileName.Contains('http://') or FileName.Contains('https://') then
  begin
    DownloadTask := TDownloadTask.Create(FileName, InstanceFolder + Name + '\' + InstanceUtils.SaveFileName);
    Tasks := TList<TTask>.Create;
    Tasks.Add(DownloadTask);
    OverviewF.runForegroundTasks(Tasks);
    if FileExists(InstanceFolder + Name + '\' + InstanceUtils.SaveFileName) then
    begin
      openTempInstance(Name);
      Exit(True);
    end
    else
      Exit(False);
  end;
  if FileExists(FileName) then
  begin

    ForceDirectories(InstanceFolder + Name + '\');
    if CopyFile(PWideChar(FileName), PWideChar(InstanceFolder + Name + '\' + InstanceUtils.SaveFileName), False) then
    begin
      openTempInstance(Name);
      Exit(True);
    end;
  end;
  Exit(False);
end;

procedure import;
var
Importer : TImporter;
begin
  Importer := TImporter.Create(nil);
  if Importer.ShowModal = mrOk then
  begin
    if Importer.rbURL.Checked then
    begin
      importModpack(Importer.edtURL.Text)
    end
    else
    begin
      importFile(Importer.lblFile.Caption);
    end;
  end;
end;

end.
