unit Setup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvgTab, Vcl.ComCtrls, JvExComCtrls,
  JvComCtrls, Vcl.Tabs, JvComponentBase, JvTabBar, MetropolisUI.Tile,
  JvExControls, JvgButton, Vcl.StdCtrls, JvButton, JvTransparentButton,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.ToolWin, ProgressBar, JvLabel, Winapi.ShellAPI,
  Task, System.Generics.Collections, AccountUtils;

type
  TSetupAssistant = class(TForm)
    imgAvatar: TImage;
    lblTitle: TLabel;
    lblText: TLabel;
    pgcPages: TPageControl;
    tsIntroduction: TTabSheet;
    tsMinecraftAccount: TTabSheet;
    btnContinueIntro: TButton;
    pb: TCMDProgressBar;
    btnIntroduction: TJvTransparentButton;
    btnAccount: TJvTransparentButton;
    btnJava: TJvTransparentButton;
    tsJava: TTabSheet;
    cbbJava: TComboBox;
    lblWarning64Bit: TJvLabel;
    lblWarning64BitText: TLabel;
    lblWebsiteLink: TLabel;
    btnSaveJava: TButton;
    btnRefreshJava: TButton;
    btnLogin: TButton;
    lblName: TLabel;
    lblPassword: TLabel;
    edtName: TEdit;
    edtPassword: TEdit;
    lblAccountText: TLabel;
    lblBuyMinecraft: TLabel;
    lblFailed: TLabel;
    procedure btnIntroductionClick(Sender: TObject);
    procedure btnContinueIntroClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblWebsiteLinkClick(Sender: TObject);
    procedure btnRefreshJavaClick(Sender: TObject);
    procedure edtNameKeyPress(Sender: TObject; var Key: Char);
    procedure btnLoginClick(Sender: TObject);
    procedure btnSaveJavaClick(Sender: TObject);
  private
    { Private declarations }
  public
    SavedAccount : TAccount;
    procedure changeTab(Index : Integer);
    procedure refreshAndSelectJava;
  end;

var
  SetupAssistant: TSetupAssistant;

implementation

uses JavaUtils, CoreLoader, Overview;

{$R *.dfm}

function IsWow64: Boolean;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var SystemInfo : TSystemInfo;
begin
  GetNativeSystemInfo(SystemInfo);
  Result := SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64;
end;

procedure TSetupAssistant.changeTab(Index : Integer);
begin
  pgcPages.TabIndex := Index;
  if pb.StepIndex > Index then
    pb.StartProcess(3);
  while pb.StepIndex < Index do
    pb.FinishStep;
end;

procedure TSetupAssistant.edtNameKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 then
  begin
    btnLogin.Click;
    Key := #0;
  end;
end;

procedure TSetupAssistant.btnContinueIntroClick(Sender: TObject);
begin
  changeTab(pgcPages.TabIndex+1);
end;

procedure TSetupAssistant.btnIntroductionClick(Sender: TObject);
begin
  changeTab((Sender as TControl).Tag);
end;

procedure TSetupAssistant.btnLoginClick(Sender: TObject);
var
Account : TAccount;
LoginData : TLoginData;
begin
  Account := TAccount.Create(edtName.Text, edtPassword.Text, '');

  LoginData := Account.createToken;
  if not LoginData.hasFailed then
  begin
    changeTab(pgcPages.TabIndex+1);
    SavedAccount := Account;
    lblFailed.Visible := False;
  end
  else
  begin
    Account.Destroy;
    lblFailed.Visible := True;
  end;
end;

procedure TSetupAssistant.btnRefreshJavaClick(Sender: TObject);
var
Tasks : TList<TTask>;
begin
  Tasks := TList<TTask>.Create;
  Tasks.Add(TLoadJava.Create);
  OverviewF.runForegroundTasks(Tasks);
  refreshAndSelectJava;
end;

procedure TSetupAssistant.btnSaveJavaClick(Sender: TObject);
begin
  if SavedAccount <> nil then
  begin
    AccountUtils.MinecraftAccounts.Add(SavedAccount);
    AccountUtils.saveAccounts;
    ProgramSettings.setString('selacc', SavedAccount.LoginName);
  end;

  if cbbJava.ItemIndex <> -1 then
  begin
    ProgramSettings.setString('javaversion', cbbJava.Text);
  end;

  Self.Hide;
end;

function StringListCompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List[Index2],List[Index1]);
end;

procedure TSetupAssistant.refreshAndSelectJava;
var
  i: Integer;
  SelectedJava : TJava;
  Sorted : TStringList;
begin
  cbbJava.Clear;
  Sorted := TStringList.Create;
  for i := 0 to JavaVersions.Count-1 do
    Sorted.Add(JavaVersions[i].getCompleteName);

  //Sorted.Sort;
  Sorted.CustomSort(StringListCompareStrings);
  cbbJava.Clear;
  cbbJava.Items.AddStrings(Sorted);
  Sorted.Destroy;

  cbbJava.ItemIndex := cbbJava.Items.IndexOf(ProgramSettings.getString('javaversion'));

  if (cbbJava.ItemIndex = -1) and (cbbJava.Items.Count > 0) then
  begin
    cbbJava.ItemIndex := 0;
    for i := 0 to cbbJava.Items.Count-1 do //just take anything if it's 64-Bit
    begin
      if getJavaByTitle(cbbJava.Items[i]).is64 then
      begin
        cbbJava.ItemIndex := i;
        break;
      end;
    end;
  end;

  SelectedJava := getJavaByTitle(cbbJava.Text);
  if SelectedJava = nil then
  begin
    lblWarning64BitText.Caption := 'No Java found. Please install Java ';
    if IsWOW64 then
      lblWarning64BitText.Caption := lblWarning64BitText.Caption + '64'
    else
      lblWarning64BitText.Caption := lblWarning64BitText.Caption + '32';
    lblWarning64BitText.Caption := lblWarning64BitText.Caption + '-Bit.' + sLineBreak + 'Use the link below and refresh the list afterwards.';
    lblWebsiteLink.Visible := True;
    lblWarning64Bit.Visible := True;
    lblWarning64BitText.Visible := True;
  end
  else if not SelectedJava.is64 and IsWOW64 then
  begin
    lblWarning64BitText.Caption := 'Could not find a 64-Bit Java version.' + sLineBreak
    + 'It''s highly recommended to download a 64 bit version first:';
    lblWebsiteLink.Visible := True;
    lblWarning64Bit.Visible := True;
    lblWarning64BitText.Visible := True;
  end
  else
  begin
    lblWebsiteLink.Visible := False;
    lblWarning64Bit.Visible := False;
    lblWarning64BitText.Visible := False;
  end;
end;

procedure TSetupAssistant.FormShow(Sender: TObject);
begin
  pb.StartProcess(3);

  refreshAndSelectJava;

end;

procedure TSetupAssistant.lblWebsiteLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar((Sender as TLabel).Caption), '', '', SW_SHOWNORMAL);
end;

end.
