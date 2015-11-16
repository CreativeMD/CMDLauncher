unit Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  JvExComCtrls, JvComCtrls, Vcl.ImgList, SettingUtils, JvListView, System.Generics.Collections,
  System.UITypes;

const
  WM_CLOSE = WM_USER + 305;
type
  TSettingsForm = class(TForm)
    grpSetting: TGroupBox;
    btnSave: TButton;
    btnCancel: TButton;
    ilIcon: TImageList;
    lvSettings: TJvListView;
    ScrollBox: TScrollBox;
    procedure lvSettingsItemClick(Sender: TObject; Item: TListItem;
      SubItemIndex, X, Y: Integer);
    procedure lvSettingsKeyPress(Sender: TObject; var Key: Char);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSaveClick(Sender: TObject);
    procedure WmClose(var Msg: TMessage); message WM_CLOSE;
  private
    { Private-Deklarationen }
  public
    SettingGroup : TSettingGroupList;
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TSettingsForm.WmClose(var Msg: TMessage);
begin
  Close;
end;

procedure TSettingsForm.btnCancelClick(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_CLOSE, 0, 0);
end;

procedure TSettingsForm.btnSaveClick(Sender: TObject);
begin
  if SettingGroup.Save then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
end;

procedure TSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if SettingGroup.CurrentPage <> nil then
    SettingGroup.CurrentPage.unloadPage(grpSetting, ScrollBox);
  SettingGroup.onCancel;
  SettingGroup.Destroy;
  Self.Destroy;
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
i, button: Integer;
Settings : TList<TSetting>;
NeedSave : Boolean;
begin
  NeedSave := False;
  Settings := SettingGroup.getAllSettings;
  for i := 0 to Settings.Count-1 do
    if Settings[i].IsEdited then
      NeedSave := True;

  if NeedSave then
  begin
    button := MessageDlg('Do you want to save?',mtCustom, [mbYes,mbNo,mbCancel], 0);
    if button = mrYes then
    begin
      if not SettingGroup.Save then
        CanClose := False
    end
    else if button = mrCancel then
      CanClose := False;
  end;
end;

procedure TSettingsForm.lvSettingsItemClick(Sender: TObject; Item: TListItem;
  SubItemIndex, X, Y: Integer);
begin
  if lvSettings.Selected <> nil then
    SettingGroup.loadPage(lvSettings.Selected.Caption);
end;

procedure TSettingsForm.lvSettingsKeyPress(Sender: TObject; var Key: Char);
begin
  if lvSettings.Selected <> nil then
    SettingGroup.loadPage(lvSettings.Selected.Caption);
end;

end.
