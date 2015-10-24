unit SettingUtils;

interface

uses Vcl.Controls, System.Generics.Collections, Vcl.Forms, Vcl.StdCtrls, SaveFileUtils,
System.Classes, JvListView, System.SysUtils, Vcl.Dialogs, JvGroupBox, System.Types, Vcl.Graphics,
Winapi.Windows;

const
standardChars = 'abcdefghijklmnopqrstuvwzxy1234567890- .!|/&%()[]ß';
type
  TSetting = class abstract(TInterfacedObject)
    protected
      NeedFill : Boolean;
      function isFilled : Boolean; virtual;
      function isFilledValid : Boolean; virtual;
    public
      Name, Title : string;
      Online, HasTitle, IsEdited : Boolean;
      Controls : TList<TControl>;
      constructor Create(Name, Title : String; Online : Boolean = False);
      procedure createControl(x, y : Integer; Parent : TWinControl); virtual; abstract;
      procedure destroyControl; virtual; abstract;
      function getUUID : string; virtual; abstract;
      procedure SaveToFile(SaveFile : TSaveFile); virtual; abstract;
      procedure LoadFromFile(SaveFile : TSaveFile); virtual; abstract;
      function canSave : String; virtual;
      function canSaveArgs : TStringList; virtual;
      function getFormName : String;
      function getHeight : Integer;
      function setNotNeedFill : TSetting;
      function setHideTitle : TSetting;
  end;
  TSetting<T> = class abstract(TSetting)
    Value : T;
    constructor Create(Name, Title : String; default : T; Online : Boolean = False);
  end;
  TStringSetting = class(TSetting<string>)
    protected
      Width, MinimalLength: Integer;
      function isFilled : Boolean; override;
      function isFilledValid : Boolean; override;
      procedure onChanged(Sender: TObject); virtual;
    public
      OnlyContains : String;
      constructor Create(Name, Title : String; Default : string; Online : Boolean = False; OnlyContains : string = standardChars);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
      function setWidth(Width : Integer) : TStringSetting;
      function setOnlyNumbers : TStringSetting;
      function setMinimalLength(Length : Integer) : TStringSetting;
  end;
  TSelectSetting = class(TSetting<string>)
    protected
      SaveNumber : Boolean;
      function isFilled : Boolean; override;
      procedure onChanged(Sender: TObject); virtual;
      procedure drawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    public
      Items : TStringList;
      constructor Create(Name, Title : String; Items : TStringList; Default : string = ''; Online : Boolean = False);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
      function setSaveNumber(SaveNumber : Boolean) : TSelectSetting;
  end;
  TTextSelectSetting = class(TSelectSetting)
    protected
      OnlyContains : String;
      function isFilled : Boolean; override;
      function isFilledValid : Boolean; override;
    public
      constructor Create(Name, Title : String; Items : TStringList; Default : string = ''; Online : Boolean = False; OnlyContains : string = standardChars);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      function getUUID : string; override;
  end;
  TStringListSetting = class(TSetting<TStringList>)
    protected
      function isFilled : Boolean; override;
      procedure onChanged(Sender: TObject); virtual;
    public
      Items : TStringList;
      constructor Create(Name, Title : String; Default : TStringList = nil; Online : Boolean = False);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;
  {TIntegerSetting = class(TSetting<Integer>)

  end; }
  TCheckOption = class(TSetting<Boolean>)
    protected
      procedure onChanged(Sender: TObject); virtual;
    public
      constructor Create(Name, Title : String; Default : Boolean; Online : Boolean = False);
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
  end;
  TSettingGroupList = class;
  TSettingPage = class
    private
      procedure loadSettings(Settings : TList<TSetting>; Parent : TWinControl; GroupList : TSettingGroupList);
      procedure unloadSettings(Settings : TList<TSetting>; Parent : TWinControl);
    public
      Title, Icon : string;
      Settings : TList<TSetting>;
      Labels : TList<TLabel>;
      Groups : TList<TGroupBox>;
      constructor Create(Title, Icon : String; Settings : TList<TSetting> = nil);
      procedure AddSetting(Setting : TSetting);
      procedure loadPage(GroupBox : TGroupBox; Parent : TWinControl; GroupList : TSettingGroupList);
      procedure unloadPage(GroupBox : TGroupBox; Parent : TWinControl);
  end;
  TSettingGroup = class
    Title : string;
    Pages : TList<TSettingPage>;
    constructor Create(Title : String; Pages : TList<TSettingPage> = nil);
    procedure AddPage(Page : TSettingPage);
  end;
  TOnBeforeSaving = procedure(GroupList : TSettingGroupList);
  TSettingGroupList = class
    Title : string;
    SettingForm : TForm;
    SaveFile : TSaveFile;
    Groups : TList<TSettingGroup>;
    Icons : TStringList;
    CurrentPage : TSettingPage;
    constructor Create(Title, IconPath : String; SaveFile : TSaveFile; Selected : String = ''; Groups : TList<TSettingGroup> = nil);
    destructor Destroy; override;
    procedure hideGroups;
    procedure loadPage(Title : string);
    procedure refreshPage;
    function Save : Boolean;
    function getErrors : TStringList; virtual;
    function getAllSettings : TList<TSetting>;
    function getNormalGroupedAllSettings : TList<TSetting>;
    function getSetting(Name : String) : TSetting;
    procedure onBeforeSaving(GroupList : TSettingGroupList); virtual;
    procedure onSaved(GroupList : TSettingGroupList); virtual;
  end;
  IExpandableSetting = interface  ['{82F1F81A-A408-448B-A194-DCED9A7E4FF7}']
    function getExpandedSettings : TList<TSetting>;
    function getGroupTitle : string;
    procedure setGroupList(GroupList : TSettingGroupList);
    function createCustomBox(Parent : TWinControl) : TGroupBox;
  end;
  TExpand = class(TSetting, IExpandableSetting)
    protected
      GroupList : TSettingGroupList;
      function getExpandedSettings : TList<TSetting>;
      function getGroupTitle : string;
      procedure setGroupList(GroupList : TSettingGroupList);
      function createCustomBox(Parent : TWinControl) : TGroupBox; virtual;
      function isFilled : Boolean; override;
      function isFilledValid : Boolean; override;
    public
      Settings : TList<TSetting>;
      constructor Create(Name, Title : String; Online : Boolean = False);
      destructor Destroy; override;
      procedure createControl(x, y : Integer; Parent : TWinControl); override;
      procedure destroyControl; override;
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
      function canSaveArgs : TStringList; override;
  end;
  TCheckExpand = class(TExpand, IExpandableSetting)
    protected
      procedure onChanged(Sender: TObject);
      function isFilled : Boolean; override;
      function isFilledValid : Boolean; override;
      function createCustomBox(Parent : TWinControl) : TGroupBox; override;
    public
      Checked : Boolean;
      constructor Create(Name, Title : String; Default : Boolean; Online : Boolean = False);
      function getUUID : string; override;
      procedure SaveToFile(SaveFile : TSaveFile); override;
      procedure LoadFromFile(SaveFile : TSaveFile); override;
      function canSaveArgs : TStringList; override;
  end;

const
SpaceY : Integer = 10;
xOffset : Integer = 10;
xLabelSpace : Integer = 10;

implementation

uses Settings, IconUtils, StringUtils, DatabaseConnection, BuildUtils;



function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
begin
  Result := Assigned(Instance) and Instance.GetInterface(IID, Intf);
end;

constructor TSetting.Create(Name, Title : String; Online : Boolean = False);
begin
  Self.Name := Name;
  Self.Title := Title;
  if Self.Title = '' then
    setHideTitle;
  Self.Online := Online;
  Self.HasTitle := True;
  Controls := TList<TControl>.Create;
  IsEdited := False;
  NeedFill := True;
end;

function TSetting.isFilled : Boolean;
begin
  Result := True;
end;

function TSetting.isFilledValid : Boolean;
begin
  Result := True;
end;

function TSetting.canSaveArgs : TStringList;
var
Error : String;
ExSetting : IExpandableSetting;
SubSettings : TList<TSetting>;
i: Integer;
begin
  Result := TStringList.Create;

  if System.SysUtils.Supports(Self.ClassType, IExpandableSetting) then
  begin
    Supports(Self, IExpandableSetting, ExSetting);
    SubSettings := ExSetting.getExpandedSettings;
    for i := 0 to SubSettings.Count-1 do
    begin
      Error := SubSettings[i].canSave;
      if Error <> '' then
        Result.Add(Error);
    end;
  end;
  Error := canSave;
  if Error <> '' then
    Result.Add(Error);
end;

function TSetting.canSave : String;
begin
  if (Self.Online) and (not DatabaseConnection.online) then
    Exit('');
  Result := '';
  if NeedFill then
  begin
    if not isFilled then
      Result := Title + ' is not filled'
    else if not isFilledValid then
      Result := Title + ' is not filled valid!';
  end
  else
    if isFilled then
      if not isFilledValid then
        Result := Title + ' is not filled valid!';
end;

function TSetting.getFormName : String;
begin
  Result := getUUID + Name;
end;

function TSetting.getHeight : Integer;
var
  i, min, max: Integer;
begin
  min := -1;
  max := 0;
  for i := 0 to Controls.Count-1 do
    if Controls[i] is TControl then
    begin
      if (TControl(Controls[i]).Top < min) or (min = -1) then
        min := TControl(Controls[i]).Top;
      if (TControl(Controls[i]).Top+TControl(Controls[i]).Height) > max then
        max := (TControl(Controls[i]).Top+TControl(Controls[i]).Height);
    end;

  Result := max - min;
  if Result < 0 then
    Result := 0;
end;

function TSetting.setNotNeedFill : TSetting;
begin
  Self.NeedFill := False;
  Result := Self;
end;

function TSetting.setHideTitle : TSetting;
begin
  Self.HasTitle := False;
  Result := Self;
end;

constructor TSetting<T>.Create(Name, Title : String; default : T; Online : Boolean = False);
begin
  inherited Create(Name, Title, Online);
  Value := default;
end;

constructor TSettingPage.Create(Title, Icon : String; Settings : TList<TSetting> = nil);
begin
  Self.Title := Title;
  Self.Icon := Icon;
  if Settings = nil then
    Settings := TList<TSetting>.Create;
  Self.Settings := Settings;
  Self.Labels := TList<TLabel>.Create;
  Self.Groups := TList<TGroupBox>.Create;
end;

procedure TSettingPage.AddSetting(Setting : TSetting);
begin
  if Setting <> nil then
  begin
    Self.Settings.Add(Setting);
    Setting.FRefCount := 1;
  end;
end;

procedure TSettingPage.loadSettings(Settings : TList<TSetting>; Parent : TWinControl; GroupList : TSettingGroupList);
var
current_heigt, offset : Integer;
i, j, tempheight: Integer;
TempLabel : TLabel;
GroupBox : TGroupBox;
SubSettings : TList<TSetting>;
ExSetting : IExpandableSetting;
begin
  if Parent is TGroupBox then
    current_heigt := SpaceY*2
  else
    current_heigt := SpaceY;
  for i := 0 to Settings.Count-1 do
  begin
    offset := xOffset;
    //TempLabel := nil;
    if Settings[i].HasTitle then
    begin
      TempLabel := TLabel.Create(Parent);
      TempLabel.Parent := Parent;
      TempLabel.Left := xOffset;
      TempLabel.Top := current_heigt;
      TempLabel.Caption := Settings[i].Title + ':';
      TempLabel.AutoSize := True;
      offset := TempLabel.Left + TempLabel.Width + xLabelSpace;
      Labels.Add(TempLabel);
    end;

    if System.SysUtils.Supports(Settings[i].ClassType, IExpandableSetting) then
    begin
      Supports(Settings[i], IExpandableSetting, ExSetting);
      ExSetting.setGroupList(GroupList);
    end;

    Settings[i].createControl(offset, current_heigt, Parent);
    if (not DatabaseConnection.online) and (Settings[i].Online) then
      for j := 0 to Settings[i].Controls.Count-1 do
        Settings[i].Controls[j].Enabled := False;

    //if Settings[i].HasTitle then
      //TempLabel.Top := TempLabel.Top + Settings[i].getHeight div 2 - TempLabel.Height div 2;
    current_heigt := current_heigt + Settings[i].getHeight + SpaceY;

    if System.SysUtils.Supports(Settings[i].ClassType, IExpandableSetting) then
    begin
      Supports(Settings[i], IExpandableSetting, ExSetting);
      GroupBox := ExSetting.createCustomBox(Parent);
      if GroupBox = nil then
        GroupBox := TGroupBox.Create(Parent);
      GroupBox.Parent := Parent;
      GroupBox.Left := xOffset;
      GroupBox.Top := current_heigt;
      GroupBox.Width := Parent.Width-xOffset*2-2;
      GroupBox.Anchors := [akLeft, akRight, akTop];


      GroupBox.Caption := ExSetting.getGroupTitle;


      SubSettings := ExSetting.getExpandedSettings;
      loadSettings(SubSettings, GroupBox, GroupList);

      tempheight := current_heigt;
      for j := 0 to SubSettings.Count-1 do
        current_heigt := current_heigt + SubSettings[j].getHeight + SpaceY;
      GroupBox.Height := current_heigt - tempheight + SpaceY*2;
      Groups.Add(GroupBox);
      current_heigt := current_heigt + SpaceY;
    end;
  end;
end;

procedure TSettingPage.unloadSettings(Settings : TList<TSetting>; Parent : TWinControl);
var
i, j : Integer;
ExSetting : IExpandableSetting;
begin
  for i := 0 to Settings.Count-1 do
  begin
    if System.SysUtils.Supports(Settings[i].ClassType, IExpandableSetting) then
    begin
      Supports(Settings[i], IExpandableSetting, ExSetting);
      unloadSettings(ExSetting.getExpandedSettings, Parent);
    end;
    Settings[i].destroyControl;
    for j := 0 to Settings[i].Controls.Count-1 do
      if Settings[i].Controls[j] <> nil then
        Settings[i].Controls[j].Destroy;
    Settings[i].Controls.Clear;
  end;
end;

procedure TSettingPage.loadPage(GroupBox : TGroupBox; Parent : TWinControl; GroupList : TSettingGroupList);
begin
  GroupBox.Caption := Title.Replace('&', '&&');
  loadSettings(Settings, Parent, GroupList);
end;

procedure TSettingPage.unloadPage(GroupBox : TGroupBox; Parent : TWinControl);
var
  i: Integer;
begin
  for i := 0 to Labels.Count-1 do
    if Labels[i] <> nil then
      Labels[i].Destroy;
  Labels.Clear;
  unloadSettings(Settings, Parent);
  for i := 0 to Groups.Count-1 do
    Groups[i].Destroy;
  Groups.Clear;
end;

constructor TSettingGroup.Create(Title : String; Pages : TList<TSettingPage> = nil);
begin
  Self.Title := Title;
  if Pages = nil then
    Pages := TList<TSettingPage>.Create;
  Self.Pages := Pages;
end;

procedure TSettingGroup.AddPage(Page : TSettingPage);
begin
  if Page <> nil then
    Pages.Add(Page);
end;

destructor TSettingGroupList.Destroy;
var
i, j, h : Integer;
begin
  for i := 0 to Groups.Count-1 do
  begin
    for j := 0 to Groups[i].Pages.Count-1 do
    begin
      for h := 0 to Groups[i].Pages[j].Settings.Count-1 do
      begin
        Groups[i].Pages[j].Settings[h].FRefCount := 0;
        Groups[i].Pages[j].Settings[h].Destroy;
      end;
      Groups[i].Pages[j].Settings.Destroy;
      Groups[i].Pages[j].Destroy;
    end;
    Groups[i].Pages.Destroy;
    Groups[i].Destroy;
  end;
  Groups.Destroy;
  inherited Destroy;
end;

constructor TSettingGroupList.Create(Title, IconPath : String; SaveFile : TSaveFile; Selected : String = ''; Groups : TList<TSettingGroup> = nil);
var
  i, j, h, index, foundindex: Integer;
  FirstPage : TSettingPage;
begin
  Self.Title := Title;
  Self.SettingForm := TSettingsForm.Create(nil);
  Self.Icons := IconUtils.LoadIcons(IconPath, TSettingsForm(SettingForm).ilIcon);
  Self.Groups := Groups;
  Self.SaveFile := SaveFile;

  foundindex := -1;
  index := 0;
  CurrentPage := nil;
  FirstPage := nil;
  for i := 0 to Groups.Count-1 do
    for j := 0 to Groups[i].Pages.Count-1 do
    begin
      if (FirstPage = nil) or (Groups[i].Pages[j].Title = Selected) then
      begin
        FirstPage := Groups[i].Pages[j];
        foundindex := index;
      end;
      if SaveFile <> nil then
        for h := 0 to Groups[i].Pages[j].Settings.Count-1 do
          Groups[i].Pages[j].Settings[h].LoadFromFile(SaveFile);
      index := index + 1;
    end;

  if FirstPage <> nil then
  begin
    loadPage(FirstPage.Title);
    with TSettingsForm(SettingForm) do
    begin
      lvSettings.ViewStyle := TJvViewStyle.vsIcon;
      lvSettings.ViewStyle := TJvViewStyle.vsTile;
      lvSettings.TileViewProperties.TileSizeKind := tskAutoSize;
      lvSettings.TileViewProperties.TileSizeKind := tskFixedWidth;
      lvSettings.TileViewProperties.TileSize.Width := 100;
      lvSettings.TileViewProperties.TileSize.Width := 166;
      SettingGroup := Self;
      Caption := Self.Title;
      for i := 0 to Groups.Count-1 do
      begin
        lvSettings.Groups.Add.Header := Groups[i].Title;
        for j := 0 to Groups[i].Pages.Count-1 do
          with lvSettings.Items.Add do
          begin
            Caption := Groups[i].Pages[j].Title;
            GroupID := i;
            ImageIndex := Icons.IndexOf(Groups[i].Pages[j].Icon);
          end;
      end;
    end;
    SettingForm.Show;
    TSettingsForm(SettingForm).lvSettings.ItemIndex := foundindex;
  end;
end;

procedure TSettingGroupList.loadPage(Title : string);
var
i, j : Integer;
Page : TSettingPage;
begin
  Page := nil;
  for i := 0 to Groups.Count-1 do
      for j := 0 to Groups[i].Pages.Count-1 do
        if Groups[i].Pages[j].Title = Title then
          Page := Groups[i].Pages[j];

  if (Page <> nil) and (Page <> CurrentPage) then
  begin
    if CurrentPage <> nil then
      CurrentPage.unloadPage(TSettingsForm(SettingForm).grpSetting, TWinControl(TSettingsForm(SettingForm).grpSetting.Controls[0]));

    CurrentPage := Page;
    Page.loadPage(TSettingsForm(SettingForm).grpSetting, TWinControl(TSettingsForm(SettingForm).grpSetting.Controls[0]), Self);
  end;
end;

procedure TSettingGroupList.hideGroups;
begin
  TSettingsForm(SettingForm).lvSettings.Visible := False;
  TSettingsForm(SettingForm).grpSetting.Caption := '';
  TSettingsForm(SettingForm).grpSetting.Left := 10;
  TSettingsForm(SettingForm).grpSetting.Width := SettingForm.Width - 35;
end;

function TSettingGroupList.getNormalGroupedAllSettings : TList<TSetting>;
var
i, j, h: Integer;
begin
  Result := TList<TSetting>.Create;
  for i := 0 to Groups.Count-1 do
    for j := 0 to Groups[i].Pages.Count-1 do
      for h := 0 to Groups[i].Pages[j].Settings.Count-1 do
        Result.Add(Groups[i].Pages[j].Settings[h]);
end;

function TSettingGroupList.getAllSettings : TList<TSetting>;
var
i, j, h: Integer;
Setting : TSetting;
ExSetting : IExpandableSetting;
begin
  Result := TList<TSetting>.Create;
  for i := 0 to Groups.Count-1 do
    for j := 0 to Groups[i].Pages.Count-1 do
      for h := 0 to Groups[i].Pages[j].Settings.Count-1 do
      begin
        Setting := Groups[i].Pages[j].Settings[h];
        Result.Add(Setting);
        if System.SysUtils.Supports(Setting.ClassType, IExpandableSetting) then
        begin
          Supports(Setting, IExpandableSetting, ExSetting);
          ExSetting.setGroupList(Self);
          Result.AddRange(ExSetting.getExpandedSettings);
        end;
      end;
end;

function TSettingGroupList.getSetting(Name : String) : TSetting;
var
i, j, h: Integer;
begin
  for i := 0 to Groups.Count-1 do
    for j := 0 to Groups[i].Pages.Count-1 do
      for h := 0 to Groups[i].Pages[j].Settings.Count-1 do
        if Groups[i].Pages[j].Settings[h].Name = Name then
          Exit(Groups[i].Pages[j].Settings[h]);
  Exit(nil);
end;

procedure TSettingGroupList.onBeforeSaving(GroupList : TSettingGroupList);
begin

end;

procedure TSettingGroupList.onSaved(GroupList : TSettingGroupList);
begin

end;

procedure TSettingGroupList.refreshPage;
begin
  CurrentPage.unloadPage(TSettingsForm(SettingForm).grpSetting, TWinControl(TSettingsForm(SettingForm).grpSetting.Controls[0]));
  CurrentPage.loadPage(TSettingsForm(SettingForm).grpSetting, TWinControl(TSettingsForm(SettingForm).grpSetting.Controls[0]), Self);
end;

function TSettingGroupList.Save : Boolean;
var
i: Integer;
Settings, Grouped : TList<TSetting>;
Errors : TStringList;
begin
  Result := False;
  Grouped := getNormalGroupedAllSettings;
  Settings := getAllSettings;
  Errors := TStringList.Create;

  refreshPage;

  for i := 0 to Grouped.Count-1 do
    Errors.AddStrings(Grouped[i].canSaveArgs);

  Errors.AddStrings(getErrors);

  if Errors.Count = 0 then
  begin
    onBeforeSaving(Self);
    if SaveFile <> nil then
    begin
      for i := 0 to Settings.Count-1 do
      begin
        if (DatabaseConnection.online) or (not Settings[i].Online) then
        begin
          Settings[i].SaveToFile(SaveFile);
          Settings[i].IsEdited := False;
        end;
      end;
      Result := True;
      onSaved(Self);
    end;
  end
  else
  begin
    //Show Message
    ShowMessage('Errors: ' + Implode(Errors, sLineBreak));
    Result := False;
  end;
end;

function TSettingGroupList.getErrors : TStringList;
begin
  Result := TStringList.Create;
end;

constructor TStringSetting.Create(Name, Title : String; Default : string; Online : Boolean = False; OnlyContains : string = standardChars);
begin
  inherited Create(Name, Title, Default, Online);
  Self.Width := 200;
  Self.OnlyContains := OnlyContains;
  Self.MinimalLength := 3;
end;

function TStringSetting.isFilled : Boolean;
begin
  Result := Value <> '';
end;

function TStringSetting.isFilledValid : Boolean;
begin
  Result := (Value <> '');
  if (Self.OnlyContains <> '') and not StringUtils.onlyContains(Value, Self.OnlyContains) then
    Result := False;
  if Length(Value) < MinimalLength then
    Result := False;
end;

procedure TStringSetting.createControl(x, y : Integer; Parent : TWinControl);
var
Edit : TEdit;
begin
  Edit := TEdit.Create(Parent);
  Edit.Parent := Parent;
  Edit.Left := X;
  Edit.Top := Y;
  Edit.Text := Value;
  Edit.OnChange := onChanged;
  Edit.Width := Width;
  Controls.Add(Edit);
end;

procedure TStringSetting.onChanged(Sender: TObject);
begin
  Self.IsEdited := True;
end;

procedure TStringSetting.destroyControl;
var
Edit : TEdit;
begin
  if Controls[0] is TEdit then
  begin
    Edit := TEdit(Controls[0]);
    Value := Edit.Text;
  end;
end;

function TStringSetting.getUUID : string;
begin
  Result := 'edt';
end;

procedure TStringSetting.SaveToFile(SaveFile : TSaveFile);
begin
  SaveFile.setString(Name, Value);
end;

function TStringSetting.setWidth(Width : Integer) : TStringSetting;
begin
  Self.Width := Width;
  Exit(Self);
end;

function TStringSetting.setOnlyNumbers : TStringSetting;
begin
  Self.OnlyContains := '0123456789';
  Exit(Self);
end;

function TStringSetting.setMinimalLength(Length : Integer) : TStringSetting;
begin
  Self.MinimalLength := Length;
  Exit(Self);
end;

procedure TStringSetting.LoadFromFile(SaveFile : TSaveFile);
begin
  if SaveFile.hasKey(Name) then
    Value := SaveFile.getString(Name);
end;

constructor TSelectSetting.Create(Name, Title : String; Items : TStringList; Default : string = ''; Online : Boolean = False);
begin
  inherited Create(Name, Title, Default, Online);
  Self.Items := Items;
  Self.SaveNumber := False;
end;

procedure TSelectSetting.createControl(x, y : Integer; Parent : TWinControl);
var
ComboBox : TComboBox;
begin
  ComboBox := TComboBox.Create(Parent);
  ComboBox.Parent := Parent;
  ComboBox.Left := x;
  ComboBox.Top := y;
  ComboBox.Items.AddStrings(Items);
  ComboBox.ItemIndex := Items.IndexOf(Value);
  ComboBox.Style := csOwnerDrawFixed;
  ComboBox.OnDrawItem := drawItem;
  ComboBox.OnChange := onChanged;
  Controls.Add(ComboBox);
end;

procedure TSelectSetting.drawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  if Controls.Count > 0 then
  begin
    with TComboBox(Control) do
    begin
      if TObject(Items.Objects[Index]) <> nil then
      begin
        Canvas.Brush.Color := TBuildTypeObject(Items.Objects[Index]).BuildType.getColor;
        Canvas.Font.Color := clBlack;
      end;

      Canvas.FillRect(Rect);
      if Index >= 0 then
        if odComboBoxEdit in State then
          Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, Items[Index]) //Visual state of the text in the edit control
        else
          Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]); //Visual state of the text(items) in the deployed list
    end;
  end;

end;

procedure TSelectSetting.onChanged(Sender: TObject);
begin
  Self.IsEdited := True;
end;

procedure TSelectSetting.destroyControl;
begin
  if Controls[0] is TComboBox then
    Value := TComboBox(Controls[0]).Text;
end;

function TSelectSetting.getUUID : string;
begin
  Result := 'sel';
end;

procedure TSelectSetting.SaveToFile(SaveFile : TSaveFile);
begin
  if SaveNumber then
    SaveFile.setInteger(Name, Items.IndexOf(Value))
  else
    SaveFile.setString(Name, Value);
end;

procedure TSelectSetting.LoadFromFile(SaveFile : TSaveFile);
var
index : Integer;
begin
  if SaveFile.hasKey(Name) then
    if SaveNumber then
    begin
      index := SaveFile.getInteger(Name);
      if (index >= 0) and (index < Items.Count) then
        Value := Items[index];
    end
    else
      Value := SaveFile.getString(Name);
end;

function TSelectSetting.setSaveNumber(SaveNumber : Boolean) : TSelectSetting;
begin
  Self.SaveNumber := SaveNumber;
  Result := Self;
end;

function TSelectSetting.isFilled : Boolean;
begin
  Result := Value <> '';
end;

constructor TTextSelectSetting.Create(Name, Title : String; Items : TStringList; Default : string = ''; Online : Boolean = False; OnlyContains : string = standardChars);
begin
  inherited Create(Name, Title, Items, Default, Online);
  Self.OnlyContains := OnlyContains;
end;

procedure TTextSelectSetting.createControl(x, y : Integer; Parent : TWinControl);
begin
  inherited createControl(x, y, Parent);
  TComboBox(Controls[0]).Style := csDropDown;
  TComboBox(Controls[0]).Text := Value;
end;

function TTextSelectSetting.getUUID : string;
begin
  Result := 'selTex';
end;

function TTextSelectSetting.isFilled : Boolean;
begin
  Result := Value <> '';
end;

function TTextSelectSetting.isFilledValid : Boolean;
begin
  Result := (Value <> '');
  if not StringUtils.onlyContains(Value, Self.OnlyContains) then
    Result := False;
  if Length(Value) < 3 then
    Result := False;
end;

constructor TStringListSetting.Create(Name, Title : String; Default : TStringList = nil; Online : Boolean = False);
begin
  inherited Create(Name, Title, Default, Online);
  if Default = nil then
    Value := TStringList.Create;
end;

procedure TStringListSetting.createControl(x, y : Integer; Parent : TWinControl);
var
Memo : TMemo;
begin
  Memo := TMemo.Create(Parent);
  Memo.Parent := Parent;
  Memo.Left := x;
  Memo.Top := y;
  Memo.Lines.Clear;
  Memo.Lines.AddStrings(Value);
  Memo.Width := 200;
  Memo.Height := 80;
  Memo.ScrollBars := ssVertical;
  Memo.OnChange := onChanged;
  Controls.Add(Memo);
end;

procedure TStringListSetting.onChanged(Sender: TObject);
begin
  Self.IsEdited := True;
end;

procedure TStringListSetting.destroyControl;
begin
  Value.Clear;
  Value.AddStrings(TMemo(Controls[0]).Lines);
end;

function TStringListSetting.getUUID : string;
begin
  Result := 'strs';
end;

procedure TStringListSetting.SaveToFile(SaveFile : TSaveFile);
begin
  SaveFile.setStringList(Name, Value);
end;

procedure TStringListSetting.LoadFromFile(SaveFile : TSaveFile);
begin
  if SaveFile.hasKey(Name) then
    Value := SaveFile.getStringList(Name);
end;

function TStringListSetting.isFilled : Boolean;
begin
  Result := Value.Text <> '';
end;

constructor TCheckOption.Create(Name, Title : String; Default : Boolean; Online : Boolean = False);
begin
  inherited Create(Name, Title, Default, Online);
end;

procedure TCheckOption.createControl(x, y : Integer; Parent : TWinControl);
var
CheckBox : TCheckBox;
begin
  CheckBox := TCheckBox.Create(Parent);
  CheckBox.Parent := Parent;
  CheckBox.Left := x;
  CheckBox.Top := Y;
  CheckBox.Checked := Value;
  CheckBox.OnClick := onChanged;
  Controls.Add(CheckBox);
end;

procedure TCheckOption.onChanged(Sender: TObject);
begin
  Self.IsEdited := True;
end;

procedure TCheckOption.destroyControl;
var
CheckBox : TCheckBox;
begin
  if Controls[0] is TCheckBox then
  begin
    CheckBox := TCheckBox(Controls[0]);
    Value := CheckBox.Checked;
  end;
end;

function TCheckOption.getUUID : string;
begin
  Result := 'chb';
end;

procedure TCheckOption.SaveToFile(SaveFile : TSaveFile);
begin
  SaveFile.setBoolean(Name, Value);
end;

procedure TCheckOption.LoadFromFile(SaveFile : TSaveFile);
begin
  if SaveFile.hasKey(Name) then
    Value := SaveFile.getBoolean(Name);
end;

constructor TExpand.Create(Name, Title : String; Online : Boolean = False);
begin
  inherited Create(Name, Title, Online);
  setHideTitle;
  Settings := TList<TSetting>.Create;
  GroupList := nil;
end;

function TExpand.getExpandedSettings : TList<TSetting>;
begin
  Result := Settings;
end;

function TExpand.getGroupTitle : string;
begin
  Result := Title;
end;

procedure TExpand.setGroupList(GroupList : TSettingGroupList);
begin
  Self.GroupList := GroupList;
end;

function TExpand.createCustomBox(Parent : TWinControl) : TGroupBox;
begin
  Result := nil;
end;

function TExpand.isFilled : Boolean;
var
i : Integer;
begin
  for i := 0 to Settings.Count-1 do
    if not Settings[i].isFilled and Settings[i].NeedFill then
      Exit(False);
  Exit(True);
end;

function TExpand.isFilledValid : Boolean;
var
i : Integer;
begin
  for i := 0 to Settings.Count-1 do
    if not Settings[i].isFilledValid then
      Exit(False);
  Exit(True);
end;

function TExpand.canSaveArgs : TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to Settings.Count-1 do
    Result.AddStrings(Settings[i].canSaveArgs);
end;

procedure TExpand.createControl(x, y : Integer; Parent : TWinControl);
begin

end;

procedure TExpand.destroyControl;
begin

end;

destructor TExpand.Destroy;
var
  i: Integer;
begin
  for i := 0 to Settings.Count-1 do
    Settings[i].Destroy;
  Settings.Destroy;
  inherited Destroy;
end;

function TExpand.getUUID : string;
begin
  Result := 'Expand';
end;

procedure TExpand.SaveToFile(SaveFile : TSaveFile);
var
i : Integer;
begin
  for i := 0 to Settings.Count-1 do
    Settings[i].SaveToFile(SaveFile);
end;

procedure TExpand.LoadFromFile(SaveFile : TSaveFile);
var
i : Integer;
begin
  for i := 0 to Settings.Count-1 do
    Settings[i].LoadFromFile(SaveFile);
end;

constructor TCheckExpand.Create(Name, Title : String; Default : Boolean; Online : Boolean = False);
begin
  inherited Create(Name, Title, Online);
  Checked := Default;
end;

function TCheckExpand.getUUID : string;
begin
  Result := 'CExpand';
end;

procedure TCheckExpand.SaveToFile(SaveFile : TSaveFile);
begin
  inherited SaveToFile(SaveFile);
  SaveFile.setBoolean(Name, Checked);
end;

procedure TCheckExpand.LoadFromFile(SaveFile : TSaveFile);
begin
  inherited LoadFromFile(SaveFile);
  if SaveFile.hasKey(Name) then
    Checked := SaveFile.getBoolean(Name);
end;

procedure TCheckExpand.onChanged(Sender: TObject);
begin
  IsEdited := True;
  Checked := TJvGroupBox(Sender).Checked;
end;

function TCheckExpand.isFilled : Boolean;
begin
  if Checked then
    Exit(inherited isFilled);
  Exit(True);
end;

function TCheckExpand.isFilledValid : Boolean;
begin
  if Checked then
    Exit(inherited isFilledValid);
  Exit(True);
end;

function TCheckExpand.canSaveArgs : TStringList;
begin
  if Checked then
    Exit(inherited canSaveArgs);
  Exit(TStringList.Create);
end;

function TCheckExpand.createCustomBox(Parent : TWinControl) : TGroupBox;
var
JvGroupBox : TJvGroupBox;
begin
  JvGroupBox := TJvGroupBox.Create(Parent);

  JVGroupBox.Checkable := True;
  JvGroupBox.Checked := Checked;
  JvGroupBox.OnCheckBoxClick := onChanged;
  Result := JvGroupBox;
end;


end.
