unit Overview;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ImgList,
  ProgressBar, Task, System.Generics.Collections, IdBaseComponent, IdComponent,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  Vcl.Menus, GraphicHintLib, cefvcl, ceflib, InstanceUtils, StringUtils,
  JvExForms, JvCustomItemViewer, JvImageListViewer, JvExStdCtrls, JvButton,
  JvCtrls, JvListComb, System.UITypes, System.Types, System.Win.TaskbarCore,
  Vcl.Taskbar, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnCtrls, JvBackgrounds, JvMenus,
  JvExComCtrls, JvHeaderControl, JvStatusBar, JvSpeedbar, Vcl.ExtCtrls,
  JvExExtCtrls, JvExtComponent, JvToolBar, JvListView, ShellApi,
  Vcl.Themes, superobject;

const
  WM_AFTER_SHOW = WM_USER + 300;
type
  TBackgroundTaskManager = class(TTaskManager)
    function isEndless : Boolean; override;
    procedure TaskFinishedEvent; override;
    procedure TaskStartEvent; override;
    procedure NoTaskFoundEvent; override;
  end;
  TOverviewF = class(TForm)
    lvInstances: TListView;
    HeaderControl: THeaderControl;
    lblVersion: TLabel;
    HeaderIcons: TImageList;
    InstanceIcons: TImageList;
    lblRetry: TLabel;
    lblNotify: TLabel;
    BackgroundBar: TCMDProgressBar;
    lblBackgroundTask: TLabel;
    pmInstance: TPopupMenu;
    Edit1: TMenuItem;
    Remove1: TMenuItem;
    N1: TMenuItem;
    Copy1: TMenuItem;
    Launch1: TMenuItem;
    N2: TMenuItem;
    Export1: TMenuItem;
    SaveAsModpack1: TMenuItem;
    pmGenerell: TPopupMenu;
    AddInstance1: TMenuItem;
    LaunchOffline1: TMenuItem;
    Import1: TMenuItem;
    N3: TMenuItem;
    Taskbar: TTaskbar;
    BackgroundImage: TJvBackground;
    StateIcons: TImageList;
    OpenFolder1: TMenuItem;
    N4: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure lblRetryClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure HeaderControlSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure HeaderControlMouseEnter(Sender: TObject);
    procedure HeaderControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lvInstancesClick(Sender: TObject);
    procedure lvInstancesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvInstancesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Edit1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Launch1Click(Sender: TObject);
    procedure lvInstancesDblClick(Sender: TObject);
    procedure AddInstance1Click(Sender: TObject);
    procedure LaunchOffline1Click(Sender: TObject);
    procedure lvInstancesAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure OpenFolder1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Groups : TStringList;
    Icons : TStringList;
    BackgroundTask : TBackgroundTaskManager;
    procedure loadInstances;
    procedure runForegroundTasks(Tasks : TList<TTask>);
    procedure showHint;
    function getSectionIndex : Integer;
    function getSelectedInstance : TInstance;
    function getInstanceByName(Name : String) : TInstance;
  end;
  TForegroundTaskManager = class(TTaskManager)
    function isEndless : Boolean; override;
  end;
var
  OverviewF: TOverviewF;
  LoadedLauncher : Boolean;
  SectionTitles : TStringList;

implementation

{$R *.dfm}

uses CoreLoader, LoadingForm, Logger, LauncherStartup, LauncherSettings,
InstanceSettings, FileUtils, MinecraftStartup, JavaUtils, ModUtils;

function TForegroundTaskManager.isEndless : Boolean;
begin
  Result := False;
end;

function TBackgroundTaskManager.isEndless : Boolean;
begin
  Result := True;
end;

procedure TBackgroundTaskManager.TaskStartEvent;
begin
  OverviewF.lblBackgroundTask.Caption := CurrentTask.Title;
end;

procedure TBackgroundTaskManager.TaskFinishedEvent;
begin
  OverviewF.lblBackgroundTask.Caption := CurrentTask.Title;
end;

procedure TBackgroundTaskManager.NoTaskFoundEvent;
begin
  OverviewF.lblBackgroundTask.Caption := 'Nothing to do';
end;

procedure TOverviewF.loadInstances;
var
i: Integer;
begin
  SendMessage(lvInstances.Handle, WM_SETREDRAW, WPARAM(False), 0);
  lvInstances.Clear;
  lvInstances.Groups.Clear;
  with lvInstances.Groups.Add do
  begin
    Header := 'Ungrouped';
    State := [lgsNormal,lgsNoHeader];
    GroupID := 0;
  end;

  if Groups <> nil then
    Groups.Clear
  else
    Groups := TStringList.Create;

  for i := 0 to Instances.Count-1 do
  begin
    if Groups.IndexOf(Instances[i].Group) = -1 then
    begin
      if Instances[i].Group <> '' then
      begin
        with lvInstances.Groups.Add do
        begin
          Header := Instances[i].Group;
          State := [lgsNormal,lgsCollapsible];
          GroupID := Groups.Count+1;
        end;
        Groups.Add(Instances[i].Group);
      end;
    end;
    with lvInstances.Items.Add do
    begin
      Caption := Instances[i].Title;
      ImageIndex := Icons.IndexOf(Instances[i].IconName);
      GroupID := Groups.IndexOf(Instances[i].Group)+1;
      if Instances[i].InstanceTyp = IClient then
        StateIndex := 0
      else
        StateIndex := 1;
    end;
  end;
  SendMessage(lvInstances.Canvas.Handle, WM_SETREDRAW, WPARAM(True), 0);
  lvInstances.Repaint;
end;

procedure TOverviewF.lvInstancesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  lvInstances.OnClick(lvInstances);
end;

procedure TOverviewF.lvInstancesClick(Sender: TObject);
begin
  if lvInstances.Selected <> nil then
  begin
    HeaderControl.Sections.Items[1].ImageIndex := 8;
    HeaderControl.Sections.Items[2].ImageIndex := 1;
    HeaderControl.Sections.Items[3].ImageIndex := 2;
  end
  else
  begin
    HeaderControl.Sections.Items[1].ImageIndex := 9;
    HeaderControl.Sections.Items[2].ImageIndex := 6;
    HeaderControl.Sections.Items[3].ImageIndex := 7;
  end;
end;

procedure TOverviewF.lvInstancesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
PopPoint : TPoint;
begin
  PopPoint := ClientToScreen(lvInstances.ClientToParent(MousePos, OverviewF));
  if lvInstances.Selected <> nil then
    pmInstance.Popup(PopPoint.X, PopPoint.Y)
  else
    pmGenerell.Popup(PopPoint.X, PopPoint.Y);
end;

procedure TOverviewF.lvInstancesAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var
  Bmp: TIcon;
  Instance : TInstance;
  Rect : TRect;
begin
  DefaultDraw := True;
  if Stage = cdPostPaint then
  begin
    Bmp := TIcon.Create;
    Instance := getInstanceByName(Item.Caption);
    if Instance <> nil then
    begin
      if Instance.InstanceTyp = IClient then
        StateIcons.GetIcon(0, Bmp)
      else
        StateIcons.GetIcon(1, Bmp);
      Rect := Item.DisplayRect(drIcon);
      lvInstances.Canvas.Draw(Rect.Right - Rect.Width div 2 + InstanceIcons.Width div 2 - Bmp.Width, Rect.Bottom-Bmp.Height, Bmp);
    end;
  end;
end;

procedure TOverviewF.lvInstancesDblClick(Sender: TObject);
begin
  if lvInstances.Selected <> nil then
    Launch1Click(pmInstance);
end;

procedure TOverviewF.OpenFolder1Click(Sender: TObject);
begin
  if lvInstances.Selected <> nil then
    ShellExecute(Application.Handle,PChar('explore'),PChar(getSelectedInstance.getInstanceFolder),nil,nil,SW_NORMAL);
end;

procedure TOverviewF.Remove1Click(Sender: TObject);
var
Tasks : TList<TTask>;
Instance : TInstance;
begin
  if lvInstances.Selected <> nil then
    if MessageDlg('Do you really want to delete ' + getSelectedInstance.Title,mtCustom, [mbYes,mbNo], 0) = mrYes then
    begin
      Tasks := TList<TTask>.Create;
      Tasks.Add(TDeleteFolder.Create('Delete Instance', InstanceFolder + getSelectedInstance.Title));
      Instance := getSelectedInstance;
      Instances.Remove(Instance);
      Instance.Destroy;
      runForegroundTasks(Tasks);
      loadInstances;
    end;
end;

procedure TOverviewF.FormShow(Sender: TObject);
begin
  if not LoadedLauncher then
  begin
    LoadedLauncher := True;
    PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
  end;
end;

function TOverviewF.getSectionIndex : Integer;
var
 i, left, xPos : Integer;
begin
  Result := -1;
  xPos := ScreenToClient(Mouse.CursorPos).X;
  left := 0;
  for i := 0 to HeaderControl.Sections.Count-1 do
  begin
    if (left <= xPos) and (left+HeaderControl.Sections[i].Width >= xPos) then
      Result := i;
    left := left + HeaderControl.Sections[i].Width;
  end;
end;

procedure TOverviewF.ShowHint;
var
index : Integer;
begin
  index := getSectionIndex;

  if index <> -1 then
  begin
    HeaderControl.Hint := SectionTitles[index];
    Application.ActivateHint(Mouse.CursorPos);
  end;
end;

procedure TOverviewF.HeaderControlMouseEnter(Sender: TObject);
begin
  showHint;
end;

procedure TOverviewF.HeaderControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  showHint;
end;

function TOverviewF.getInstanceByName(Name : String) : TInstance;
var
i : Integer;
begin
  for i := 0 to InstanceUtils.Instances.Count-1 do
    if InstanceUtils.Instances[i].Title = Name then
      Exit(InstanceUtils.Instances[i]);
  Exit(nil);
end;

function TOverviewF.getSelectedInstance : TInstance;
begin
  Result := nil;
  if lvInstances.Selected <> nil then
    Result := getInstanceByName(lvInstances.Selected.Caption);
end;

procedure TOverviewF.HeaderControlSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  case Section.ID of
    0: //create
    begin
      InstanceSettings.loadInstanceSettings(nil);
    end;
    1: //Launch
    begin
      lvInstancesDblClick(lvInstances);
    end;
    2: //edit
    begin
      Edit1Click(pmInstance);
    end;
    3: //remove
    begin
      Remove1Click(pmInstance);
    end;
    4: //Settings
    begin
      LauncherSettings.openSettings('');
    end;
    5: //Reload
    begin
      runForegroundTasks(LauncherStartup.getStartupTasks);
    end;
    6: //Accounts
    begin
      LauncherSettings.openSettings('Accounts');
    end;
    7: //User
    begin

    end;
  end;
end;

procedure TOverviewF.Launch1Click(Sender: TObject);
begin
  if lvInstances.Selected <> nil then
    launchInstance(getSelectedInstance);
end;

procedure TOverviewF.LaunchOffline1Click(Sender: TObject);
begin
  if lvInstances.Selected <> nil then
    launchInstance(getSelectedInstance, False);
end;

procedure TOverviewF.lblRetryClick(Sender: TObject);
begin
  runForegroundTasks(LauncherStartup.getStartupTasks);
end;

procedure TOverviewF.runForegroundTasks(Tasks : TList<TTask>);
var
TaskManager : TForegroundTaskManager;
begin
  TaskManager := TForegroundTaskManager.Create(Tasks, LoadingScreen.TaskProgress);
  Self.Enabled := False;
  LoadingScreen.Show;
  Taskbar.ProgressState := TTaskBarProgressState.Normal;
  Taskbar.ProgressMaxValue := Tasks.Count;
  while TaskManager.isActive do
  begin
    if TaskManager.CurrentTask <> nil then
    begin
      LoadingScreen.lblTask.Caption := TaskManager.CurrentTask.Title + ' ...';
      LoadingScreen.lblLog.Caption := Logger.Log.getLastLog;
    end;
    Taskbar.ProgressValue := LoadingScreen.TaskProgress.StepIndex;
    Application.ProcessMessages;
  end;
  Self.Enabled := True;
  LoadingScreen.Hide;

  Taskbar.ProgressState := TTaskBarProgressState.None;

  if LauncherStartup.CloseLauncher then
    Application.Terminate
  else
  begin
    BackgroundTask := TBackgroundTaskManager.Create(nil, BackgroundBar);
    BackgroundTask.addTask(ModUtils.TFullLoadMod.Create);
  end;
end;

procedure TOverviewF.AddInstance1Click(Sender: TObject);
begin
  InstanceSettings.loadInstanceSettings(nil);
end;

procedure TOverviewF.Copy1Click(Sender: TObject);
var
Settings : TInstanceSetting;
begin
  Settings := InstanceSettings.loadInstanceSettings(getSelectedInstance);
  Settings.copyOf := Settings.Instance.Title;
  Settings.Instance := nil;
end;

procedure TOverviewF.Edit1Click(Sender: TObject);
begin
  if lvInstances.Selected <> nil then
    InstanceSettings.loadInstanceSettings(getSelectedInstance);
end;

procedure TOverviewF.FormCreate(Sender: TObject);
var
i: Integer;
begin
  HintWindowClass := TGraphicHintWindow;
  Application.ShowHint := False;
  Application.ShowHint := True;
  lblVersion.Caption := ProgramVersion;
  LoadedLauncher := False;
  SectionTitles := TStringList.Create;
  for i := 0 to HeaderControl.Sections.Count-1 do
  begin
    SectionTitles.Add(HeaderControl.Sections.Items[i].Text);
    HeaderControl.Sections.Items[i].Text := '';
  end;
  if not ProgramSettings.hasKey('style') then
    ProgramSettings.setString('style', 'Light');

  TStyleManager.TrySetStyle(ProgramSettings.getString('style'), False);
end;

procedure TOverviewF.WmAfterShow(var Msg: TMessage);
begin
  lvInstances.OnClick(lvInstances);
  runForegroundTasks(LauncherStartup.getStartupTasks);
end;

end.
