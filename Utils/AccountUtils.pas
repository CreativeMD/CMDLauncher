unit AccountUtils;

interface

uses System.Generics.Collections, SettingUtils, Vcl.Controls, SaveFileUtils,
Vcl.ComCtrls, Task, ProgressBar, System.Classes, Vcl.StdCtrls, IdHTTP, superobject,
IdSSLOpenSSL, System.SysUtils, Vcl.Dialogs, System.UITypes, LoginForm;

type
  TLoginData = class
    private
      Failed, Offline : Boolean;
      Name : String;
    public
      Error, Session, UUID, clientToken : String;
      function hasFailed : Boolean;
      function isOffline : Boolean;
      function getName : String;
  end;
  TAccount = class
    protected
      FLoginName, FPassword, FMinecraftName, UUID : String;
      LastLogin : TLoginData;

    public
      SavePassword : Boolean;
      constructor Create(LoginName, Password, MinecraftName : String); overload;
      function SaveToFile : String;
      function doLogin : TLoginData;
      function Equals(O : TObject) : Boolean; override;
      property LoginName : string read FLoginName;
      property MinecraftName : string read FMinecraftName;
      function createToken : TLoginData;
      function refreshToken : TLoginData;
  end;
  TAccountSetting = class(TSetting<string>)
    constructor Create(Name, Title : String);
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
    procedure onItemChecked(Sender : TObject; Item : TListItem);
    procedure onButtonClicked(Sender : TObject);
  end;
  TLoadAccount = class(TTask)
    constructor Create;
    procedure runTask(Bar : TCMDProgressBar); override;
  end;

var
MinecraftAccounts : TList<TAccount>;

function CreateFromFile(Input : String) : TAccount;
function encodestr(Str: String): String;
function decodestr(Str: String): String;
procedure saveAccounts;
function login(Account : TAccount; LoginForm : TLoginF = nil; Online : Boolean = True) : TLoginData;
function getSelectedAccount : TAccount;

implementation

uses CoreLoader, StringUtils, DatabaseConnection, DownloadUtils,
OfflineForm, Logger;

function getSelectedAccount : TAccount;
var
Name : string;
  i: Integer;
begin
  Name := ProgramSettings.getString('selacc');
  for i := 0 to MinecraftAccounts.Count-1 do
    if MinecraftAccounts[i].FLoginName = Name then
      Exit(MinecraftAccounts[i]);
  Exit(nil);
end;

function TAccount.createToken : TLoginData;
var
HTTP : TIdHTTP;
Parameters : TStringList;
LoginFile, ParameterFile : TFileStream;
LoginS : ISuperObject;
begin
  Result := TLoginData.Create;
  HTTP := TIdHTTP.Create;
  LoginFile := nil;
  ParameterFile := nil;
  try
    HTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    Parameters := TStringList.Create;
    Parameters.Add('{');
    Parameters.Add('"agent":{');
    Parameters.Add('    "name": "Minecraft",');
    Parameters.Add('    "version": "1"');
    Parameters.Add('},');
    Parameters.Add('    "username": "' + FLoginName + '",');
    Parameters.Add('    "password": "' + FPassword + '"');
    if LastLogin <> nil then
      Parameters.Add('    ,"clientToken": "' + LastLogin.clientToken + '"');
    Parameters.Add('}');
    Parameters.SaveToFile(DownloadFolder + 'request.json');
    LoginFile := TFileStream.Create(DownloadFolder + 'login.json', fmCreate);
    ParameterFile := TFileStream.Create(DownloadFolder + 'request.json', fmOpenRead);
    HTTP.Post('https://authserver.mojang.com/authenticate', ParameterFile, LoginFile);
    ParameterFile.Free;
    LoginFile.Free;
  except
    on E: Exception do
    begin
      ParameterFile.Free;
      LoginFile.Free;
      Result.Error := 'Bad Login';
      Result.Failed := True;
    end;
  end;

  LoginS := TSuperObject.ParseFile(DownloadFolder + 'login.json', true);
  if (LoginS <> nil) and (LoginS.S['errorMessage'] <> '') then
  begin
    Result.Error := LoginS.S['errorMessage'];
    Result.Failed := True;
  end
  else if not Result.Failed then
  begin

    Result.Session := LoginS.S['accessToken'];
    Result.clientToken := LoginS.S['clientToken'];
    FMinecraftName := LoginS.O['selectedProfile'].S['name'];
    Result.UUID := LoginS.O['selectedProfile'].S['id'];
    UUID := Result.UUID;
    Result.Name := FMinecraftName;
    LastLogin := Result;
    DeleteFile(DownloadFolder + 'login.json');
    DeleteFile(DownloadFolder + 'request.json');
    if getSelectedAccount = nil then
      ProgramSettings.setString('selacc', FLoginName);
    SaveAccounts;
    Exit(Result);
  end;
  DeleteFile(DownloadFolder + 'login.json');
  DeleteFile(DownloadFolder + 'request.json');
end;

function TAccount.refreshToken : TLoginData;
var
HTTP : TIdHTTP;
Parameters : TStringList;
RefreshFile, ParameterFile : TFileStream;
LoginS : ISuperObject;
begin
  Result := TLoginData.Create;
  HTTP := TIdHTTP.Create;
  RefreshFile := nil;
  ParameterFile := nil;
  try
    HTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    ParameterFile.Free;
    RefreshFile.Free;

    Parameters := TStringList.Create;
    Parameters.Add('{"accessToken":"' + LastLogin.Session + '","clientToken":"' + LastLogin.clientToken + '"}');
    Parameters.SaveToFile(DownloadFolder + 'requestRefresh.json');
    RefreshFile := TFileStream.Create(DownloadFolder + 'refresh.json', fmCreate);
    ParameterFile := TFileStream.Create(DownloadFolder + 'requestRefresh.json', fmOpenRead);

    HTTP.Post('https://authserver.mojang.com/refresh', ParameterFile, RefreshFile);
    ParameterFile.Free;
    RefreshFile.Free;
  except
    on E: Exception do
    begin
      ParameterFile.Free;
      RefreshFile.Free;
      Result.Error := 'Bad Login';
      Result.Failed := True;
      Logger.MainLog.log('Failed to refresh token!!!');
    end;
  end;

  LoginS := TSuperObject.ParseFile(DownloadFolder + 'refresh.json', true);
  if (LoginS <> nil) and (LoginS.S['errorMessage'] <> '') then
  begin
    Result.Error := LoginS.S['errorMessage'];
    Result.Failed := True;
  end
  else if not Result.Failed then
  begin
    LastLogin.Session := LoginS.S['accessToken'];
    Result := LastLogin;
  end;
  DeleteFile(DownloadFolder + 'refresh.json');
  DeleteFile(DownloadFolder + 'requestRefresh.json');
end;

function login(Account : TAccount; LoginForm : TLoginF = nil; Online : Boolean = True) : TLoginData;
var
OfflineForm :  TOfflineF;
PlayOffline : Boolean;
ModalResult : Integer;
begin
  Result := nil;
  if (Account.FPassword <> '') and (DatabaseConnection.online) and (Online) then
  begin
    if Account.LastLogin <> nil then
      Result := Account.refreshToken;

    if (Result = nil) or Result.Failed then
      Result := Account.createToken;
  end;

  if Result = nil then
    Result := TLoginData.Create;

  PlayOffline := False;

  if (DatabaseConnection.online) and (Online) and Result.Failed then
  begin
    if LoginForm = nil then
      LoginForm := TLoginF.Create(nil);
    LoginForm.lblError.Caption := Result.Error;
    LoginForm.edtName.Text := Account.FLoginName;
    LoginForm.edtPassword.Text := Account.FPassword;
    ModalResult := LoginForm.ShowModal;

    if ModalResult = mrOk then
    begin
      Account.FLoginName := LoginForm.edtName.Text;
      Account.FPassword := LoginForm.edtPassword.Text;
      Account.SavePassword := LoginForm.chkPassword.Checked;
      Result := login(Account, LoginForm, True);
      //if Assigned(LoginForm) then
        //FreeAndNil(LoginForm);
      Result.Offline := False;
      Result.Name := Account.MinecraftName;
      Exit(Result);
    end;

    if Assigned(LoginForm) then
      FreeAndNil(LoginForm);

    if ModalResult = mrCancel then
    begin
      Result.Failed := True;
      Exit(Result);
    end;

    if ModalResult = mrYes then
      PlayOffline := True;
  end;

  if PlayOffline or not DatabaseConnection.online or not Online then
  begin
    OfflineForm := TOfflineF.Create(nil);
    OfflineForm.edtName.Text := Account.MinecraftName;
    ModalResult := OfflineForm.ShowModal;
    if ModalResult = mrOk then
    begin
      Result.Name := OfflineForm.edtName.Text;
      Result.Offline := True;
      Result.Failed := False;
    end;

    OfflineForm.Destroy;

    if ModalResult = mrCancel then
      Result.Failed := True;
  end;
end;

procedure saveAccounts;
var
Accounts : TStringList;
  i: Integer;
begin
  Accounts := TStringList.Create;
  for i := 0 to MinecraftAccounts.Count-1 do
    Accounts.Add(MinecraftAccounts[i].SaveToFile);
  ProgramSettings.setStringList('acc', Accounts);
end;

function encodestr(Str: String): String;
var I: Cardinal;
begin
  Result:=Str;
  for I:=1 to length(Result) do
  Result[I]:=Chr(Ord(Result[I])+1);
end;

function decodestr(Str: String): String;
var I: Cardinal;
begin
  Result:=Str;
  for I:=1 to length(Result) do
  Result[I]:=Chr(Ord(Result[I])-1);
end;

function CreateFromFile(Input : String) : TAccount;
var
Data : TStringList;
begin
  Result := nil;
  Data := Explode(Input, '&Break');
  if Data.Count = 3 then
    Result := TAccount.Create(Data[0], decodestr(Data[1]), Data[2]);
  if Data.Count = 2 then
    Result := TAccount.Create(Data[0], '', Data[1]);
end;

constructor TLoadAccount.Create;
begin
  inherited Create('Loading Accounts', False);
end;

procedure TLoadAccount.runTask(Bar : TCMDProgressBar);
var
Accounts : TStringList;
Account : TAccount;
i: Integer;
begin
  MinecraftAccounts := TList<TAccount>.Create;
  Accounts := ProgramSettings.getStringList('acc');
  Bar.StartStep(Accounts.Count);
  for i := 0 to Accounts.Count-1 do
  begin
    Account := CreateFromFile(Accounts[i]);
    if (Account <> nil) and (Account.FLoginName <> '') then
      MinecraftAccounts.Add(Account);
    Bar.StepPos := i;
  end;
  Bar.FinishStep;
end;

constructor TAccountSetting.Create(Name, Title : String);
begin
  inherited Create(Name, Title, '', True);
  setHideTitle;
end;

procedure TAccountSetting.onButtonClicked(Sender : TObject);
var
LoginForm : TLoginF;
Account, TempAccount : TAccount;
i : Integer;
begin
  if Sender is TListView then
  begin
    Controls[2].Enabled := TListView(Sender).Selected <> nil;
    Controls[3].Enabled := TListView(Sender).Selected <> nil;
  end
  else
  begin
    if (TButton(Sender).Caption = 'Add') or (TButton(Sender).Caption = 'Edit') then
    begin
      LoginForm := TLoginF.Create(nil);
      LoginForm.btnOffline.Enabled := False;
      Account := nil;
      TempAccount := nil;
      if TButton(Sender).Caption = 'Edit' then
      begin
        for i := 0 to MinecraftAccounts.Count-1 do
          if MinecraftAccounts[i].FLoginName = TListView(Controls[0]).Selected.Caption then
            Account := MinecraftAccounts[i];
        if Account <> nil then
        begin
          LoginForm.edtName.Text := Account.FLoginName;
          LoginForm.edtPassword.Text := Account.FPassword;
          LoginForm.chkPassword.Checked := Account.SavePassword;
        end;
        TempAccount := Account;
      end;
      if LoginForm.ShowModal = mrOk then
      begin
        Account := TAccount.Create(LoginForm.edtName.Text, LoginForm.edtPassword.Text, '');
        Account.SavePassword := LoginForm.chkPassword.Checked;
        if not login(Account, LoginForm).Failed then
        begin
          if TempAccount <> nil then
          begin
            TempAccount.FMinecraftName := Account.FMinecraftName;
            TempAccount.FLoginName := Account.FLoginName;
            TempAccount.FPassword := Account.FPassword;
            TempAccount.SavePassword := Account.SavePassword;
            TListView(Controls[0]).Selected.Caption := TempAccount.FLoginName;
            TListView(Controls[0]).Selected.SubItems[0] := TempAccount.FMinecraftName;
            if getSelectedAccount <> nil then
              TListView(Controls[0]).Selected.Checked := TempAccount.MinecraftName = getSelectedAccount.MinecraftName;
            TListView(Controls[0]).Selected.Checked := False;
            Account.Destroy;
          end
          else
          begin
            MinecraftAccounts.Add(Account);
            with TListView(Controls[0]).Items.Add do
            begin
              Caption := Account.FLoginName;
              SubItems.Add(Account.FMinecraftName);
              if getSelectedAccount <> nil then
                Checked := Account.LoginName = getSelectedAccount.LoginName
              else
                Checked := False;
            end;
          end;
        end;
      end;
    end;
    if TButton(Sender).Caption = 'Remove' then
    begin
      Account := nil;
      for i := 0 to MinecraftAccounts.Count-1 do
        if MinecraftAccounts[i].FLoginName = TListView(Controls[0]).Selected.Caption then
          Account := MinecraftAccounts[i];
      if MessageDlg('Do you really want to delete this account', mtCustom, [mbYes,mbNo], 0) = mrYes then
      begin
        MinecraftAccounts.Remove(Account);
        TListView(Controls[0]).Items.Delete(TListView(Controls[0]).Selected.Index);
      end;
      Account.Destroy;
    end;
  end;
end;

procedure TAccountSetting.onItemChecked(Sender : TObject; Item : TListItem);
var
i : Integer;
begin
  if Item.Checked then
  begin
    for i := 0 to TListView(Sender).Items.Count-1 do
      if TListView(Sender).Items[i] <> Item then
        TListView(Sender).Items[i].Checked := False;
  end;
end;

procedure TAccountSetting.createControl(x, y : Integer; Parent : TWinControl);
var
ListView : TListView;
i: Integer;
Add, Edit, Remove : TButton;
begin
  ListView := TListView.Create(Parent);
  ListView.Parent := Parent;
  ListView.Top := y;
  ListView.Left := x;
  //ListView.GroupView := True;
  ListView.ViewStyle := vsReport;
  ListView.Checkboxes := True;
  ListView.OnItemChecked := onItemChecked;
  with ListView.Columns.Add do
  begin
    Caption := 'Name/E-Mail';
    AutoSize := True;
  end;
  with ListView.Columns.Add do
  begin
    Caption := 'MinecraftName';
    AutoSize := True;
  end;
  ListView.Realign;
  for i := 0 to MinecraftAccounts.Count-1 do
  begin
    with ListView.Items.Add do
    begin
      if MinecraftAccounts[i].FLoginName = Value then
        Checked := True;
      Caption := MinecraftAccounts[i].FLoginName;
      SubItems.Add(MinecraftAccounts[i].FMinecraftName);
    end;
  end;

  ListView.OnClick := onButtonClicked;
  ListView.ReadOnly := True;
  ListView.RowSelect := True;
  ListView.Width := Parent.Width-x*2;
  ListView.Anchors := [akLeft, akTop, akRight, akBottom];
  ListView.Height := Parent.Height - 80;
  Controls.Add(ListView);

  Add := TButton.Create(Parent);
  Add.Parent := Parent;
  Add.Caption := 'Add';
  Add.Anchors := [akBottom, akLeft];
  Add.Top := ListView.Top + ListView.Height + 10;
  Add.Left := x;
  Add.OnClick := onButtonClicked;
  Controls.Add(Add);

  Edit := TButton.Create(Parent);
  Edit.Parent := Parent;
  Edit.Caption := 'Edit';
  Edit.Anchors := [akBottom, akLeft];
  Edit.Top := ListView.Top + ListView.Height + 10;
  Edit.Left := x + 100;
  Edit.Enabled := False;
  Edit.OnClick := onButtonClicked;
  Controls.Add(Edit);

  Remove := TButton.Create(Parent);
  Remove.Parent := Parent;
  Remove.Caption := 'Remove';
  Remove.Anchors := [akBottom, akLeft];
  Remove.Top := ListView.Top + ListView.Height + 10;
  Remove.Left := x + 200;
  Remove.Enabled := False;
  Remove.OnClick := onButtonClicked;
  Controls.Add(Remove);
end;

procedure TAccountSetting.destroyControl;
var
ListView : TListView;
  i: Integer;
begin
  Value := '';
  ListView := TListView(Controls[0]);
  for i := 0 to ListView.Items.Count-1 do
    if ListView.Items[i].Checked then
      Value := ListView.Items[i].Caption;
end;

function TAccountSetting.getUUID : string;
begin
  Result := 'accounts';
end;

procedure TAccountSetting.SaveToFile(SaveFile : TSaveFile);
begin
  SaveFile.setString('selacc', Value);
  saveAccounts;
end;

procedure TAccountSetting.LoadFromFile(SaveFile : TSaveFile);
begin
  if SaveFile.hasKey('selacc') then
    Value := SaveFile.getString('selacc');
end;

function TLoginData.hasFailed : Boolean;
begin
  Result := Failed;
end;

function TLoginData.isOffline : Boolean;
begin
  Result := Offline;
end;

function TAccount.doLogin : TLoginData;
begin
  Result := login(Self);
end;

function TLoginData.getName : String;
begin
  Result := Name;
end;

constructor TAccount.Create(LoginName, Password, MinecraftName : String);
begin
  Self.FLoginName := LoginName;
  Self.FPassword := Password;
  Self.FMinecraftName := MinecraftName;
  Self.SavePassword := True;
  Self.LastLogin := nil;
  Self.UUID := '';
end;

function TAccount.Equals(O : TObject) : Boolean;
begin
  Result := False;
  if O is TAccount then
    Result := Self.LoginName = TAccount(O).LoginName;
end;

function TAccount.SaveToFile : String;
begin
  if SavePassword then
    Result := FLoginName + '&Break' + encodestr(FPassword) + '&Break' + FMinecraftName
  else
    Result := FLoginName + '&Break' + FMinecraftName;
end;

end.
