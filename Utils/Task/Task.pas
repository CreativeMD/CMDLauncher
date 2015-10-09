unit Task;

interface

uses Generics.Collections, ProgressBar, System.Classes, Vcl.Forms, System.SysUtils,
Logger;

type
  TTaskManager = class;
  TTaskEvent = (teStart, teFinished, teStartTask, teFinishedTask);
  TTaskNotify = procedure(Manager : TTaskManager; Event : TTaskEvent);
  TTask = class
    protected
      Log : TLog;
      procedure runTask(Bar : TCMDProgressBar); virtual; abstract;
    public
      Title : String;
      online, sync, multitasking : Boolean;
      constructor Create(Title : String; online : Boolean = False; multitasking : Boolean = True);
      procedure setLog(Log : TLog);
  end;
  TTaskManager = class abstract(TThread)
    private
      Tasks : TList<TTask>;
      ProgressBar : TCMDProgressBar;
      FActive : Boolean;
      FLog : TLog;
      procedure SetLog(Value : TLog);
    public
      CurrentTask : TTask;
      TaskEvent : TTaskNotify;
      constructor Create(Tasks : TList<TTask>; ProgressBar : TCMDProgressBar);
      procedure runTask;
      procedure Execute; override;
      function isEndless : Boolean; virtual; abstract;
      procedure StartEvent; virtual;
      procedure TaskFinishedEvent; virtual;
      procedure TaskStartEvent; virtual;
      procedure FinishedEvent; virtual;
      procedure NoTaskFoundEvent; virtual;
      procedure addTask(Task : TTask);
      property isActive : Boolean read FActive;
      property Log : TLog read FLog write SetLog;
  end;

procedure runTask(Task : TTask; Bar : TCMDProgressBar);

var
MultiTasks : TList<TClass>;

implementation

uses DatabaseConnection, LauncherStartup;

procedure runTask(Task : TTask; Bar : TCMDProgressBar);
begin
  Task.runTask(Bar);
end;

constructor TTask.Create(Title : String; online : Boolean = False; multitasking : Boolean = True);
begin
  Self.Title := Title;
  Self.online := online;
  Self.multitasking := multitasking;
  Self.Log := Logger.MainLog;
  Self.sync := false;
end;

procedure TTask.setLog(Log : TLog);
begin
  Self.Log := Log;
end;

procedure TTaskManager.StartEvent;
begin
  if Assigned(Self.TaskEvent) then
    TaskEvent(Self, teStart);
end;

procedure TTaskManager.TaskStartEvent;
begin
  if Assigned(Self.TaskEvent) then
    TaskEvent(Self, teStartTask);
end;

procedure TTaskManager.TaskFinishedEvent;
begin
  if Assigned(Self.TaskEvent) then
    TaskEvent(Self, teFinishedTask);
end;

procedure TTaskManager.FinishedEvent;
begin
  if Assigned(Self.TaskEvent) then
    TaskEvent(Self, teFinished);
end;

procedure TTaskManager.NoTaskFoundEvent;
begin

end;

procedure TTaskManager.addTask(Task : TTask);
begin
  if isEndless then
  begin
    Task.Log := Self.Log;
    Tasks.Add(Task);
  end;
end;

procedure TTaskManager.SetLog(Value : TLog);
var
i : Integer;
begin
  Self.FLog := Value;
  for i := 0 to Tasks.Count-1 do
    Tasks[i].Log := Self.FLog;
end;

constructor TTaskManager.Create(Tasks : TList<TTask>; ProgressBar : TCMDProgressBar);
var
  i: Integer;
begin
  inherited Create;
  Self.ProgressBar := ProgressBar;
  Self.Tasks := Tasks;
  if Self.Tasks = nil then
    Self.Tasks := TList<TTask>.Create;
  Self.FLog := Logger.MainLog;
  for i := 0 to Self.Tasks.Count-1 do
    Self.Tasks[i].Log := Self.FLog;
  TaskEvent := niL;
  FActive := True;
end;

procedure TTaskManager.runTask;
begin
  if DatabaseConnection.online or (not CurrentTask.online) then
  begin
    Self.FLog.log('==== ' + CurrentTask.Title + ' ====');
    CurrentTask.runTask(ProgressBar)
  end
  else
  begin
    Self.FLog.log('Could not run task ' + CurrentTask.Title + ' because launcher is in offline mode!');
    Self.ProgressBar.FinishStep;
  end;
end;

procedure TTaskManager.Execute;
var
before, after, dif : Integer;
TimeString : String;
Wait : Boolean;
begin
  if MultiTasks = nil then
    MultiTasks := TList<TClass>.Create;

  if not isEndless then
    ProgressBar.StartProcess(Tasks.Count);

  StartEvent;

  while (isEndless or (Tasks.Count > 0)) and (not Application.Terminated) do
  begin

    Wait := False;
    if LauncherStartup.CloseLauncher then
    begin
      Terminate;
    end;

    if Tasks.Count > 0 then
    begin
      CurrentTask := Tasks[0];

      if not CurrentTask.multitasking then
      begin
        while (not LauncherStartup.CloseLauncher) and (MultiTasks.Contains(CurrentTask.ClassType)) do
        begin
          if LauncherStartup.CloseLauncher then
            Terminate;
          if not Wait then
          begin
            Self.FLog.log('Wait for the other task to finish!');
            Wait := True;
          end;
        end;
        MultiTasks.Add(CurrentTask.ClassType);
      end;

      before := GetTickCount;
      try
        TaskStartEvent;
        if isEndless then
          ProgressBar.StartProcess(1);

        if CurrentTask.sync then
          Synchronize(runTask)
        else
          runTask;
        //Synchronize();
        TaskFinishedEvent;
      except
        on E: Exception do
        begin
          Self.FLog.log('An error occoured: ' + E.Message + '. Please report this to the author!');
          Self.ProgressBar.FinishStep;
        end;
      end;
      after := GetTickCount;
      dif := after - before;
      TimeString := '';
      if dif < 1000 then
        TimeString := IntToStr(dif) + ' ms'
      else if dif < 60000 then
        TimeString := FloatToStr(dif/1000) + ' s'
      else
        TimeString := FloatToStr(dif/60000) + ' min';

      Self.FLog.log('Finished task in ' + TimeString);
      if not CurrentTask.multitasking then
      begin
        MultiTasks.Remove(CurrentTask.ClassType);
      end;
      Tasks[0].Destroy;
      Tasks.Delete(0);
    end
    else
      Synchronize(NoTaskFoundEvent);
    if isEndless then
      Sleep(1);
  end;
  FinishedEvent;
  FActive := False;
end;


end.
