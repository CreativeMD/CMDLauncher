unit IconUtils;

interface

uses Task, Progressbar, System.Classes, Vcl.Controls, Vcl.Imaging.pngimage,
Vcl.Graphics, Vcl.ImgList, System.IOUtils, System.SysUtils, StringUtils,
JvImageListViewer, SettingUtils, SaveFileUtils, JvCtrls;

type
  TIconPicker = class(TSetting<string>)
  protected
    ImageList : TImageList;
    Icons : TStringList;
    function isFilled : Boolean; override;
  public
    constructor Create(Name, Title : String; ImageList : TImageList; Icons : TStringList; Online : Boolean = False; Default : string = '');
    procedure createControl(x, y : Integer; Parent : TWinControl); override;
    procedure destroyControl; override;
    function getUUID : string; override;
    procedure SaveToFile(SaveFile : TSaveFile); override;
    procedure LoadFromFile(SaveFile : TSaveFile); override;
    procedure onButtonClicked(Sender : TObject);
  end;
  TLoadIcon = class(TTask)
    constructor Create;
    protected
      procedure runTask(Bar : TCMDProgressBar); override;
  end;


function LoadIcons(IconPath : String; ImageList : TImageList) : TStringList;

var
LauncherIcons, InstanceIcons : string;

implementation

uses CoreLoader, Logger, Overview, IconPicker;

constructor TIconPicker.Create(Name, Title : String; ImageList : TImageList; Icons : TStringList; Online : Boolean = False; Default : string = '');
begin
  inherited Create(Name, Title, Default, Online);
  Self.ImageList := ImageList;
  Self.Icons := Icons;
end;

procedure TIconPicker.createControl(x, y : Integer; Parent : TWinControl);
var
JVBtn : TJvImgBtn;
begin
  JVBtn := TJvImgBtn.Create(Parent);
  JVBtn.Parent := Parent;
  JVBtn.Left := x;
  JVBtn.Top := y;
  JVBtn.Images := ImageList;
  JVBtn.ImageIndex := Icons.IndexOf(Value);
  JVBtn.Caption := Value;
  JVBtn.OnClick := onButtonClicked;
  JVBtn.Width := 150;
  JVBtn.Height := 45;
  Controls.Add(JVBtn);
end;

procedure TIconPicker.onButtonClicked(Sender : TObject);
var
IconPicker : TIconPick;
begin
  IconPicker := TIconPick.Create(nil);
  IconPicker.ImageListViewer.Images := ImageList;
  IconPicker.ImageListViewer.SelectedIndex := Icons.IndexOf(Value);
  if IconPicker.ShowModal = mrOk then
  begin
    TJvImgBtn(Controls[0]).ImageIndex := IconPicker.ImageListViewer.SelectedIndex;
    if IconPicker.ImageListViewer.SelectedIndex = -1 then
      Value := ''
    else
      Value := Icons[IconPicker.ImageListViewer.SelectedIndex];
    TJvImgBtn(Controls[0]).Caption := Value;
  end;
  IconPicker.Destroy;
end;

procedure TIconPicker.destroyControl;
begin
  if Controls[0] is TJvImgBtn then
  begin
    if TJvImgBtn(Controls[0]).ImageIndex = -1 then
      Value := ''
    else
      Value := Icons[TJvImgBtn(Controls[0]).ImageIndex];
  end;
end;

function TIconPicker.getUUID : string;
begin
  Result := 'ipick';
end;

procedure TIconPicker.SaveToFile(SaveFile : TSaveFile);
begin
  SaveFile.setString(Name, Value);
end;

procedure TIconPicker.LoadFromFile(SaveFile : TSaveFile);
begin
  if SaveFile.hasKey(Name) then
    Value := SaveFile.getString(Name);
end;

function TIconPicker.isFilled : Boolean;
begin
  Result := Value <> '';
end;

constructor TLoadIcon.Create;
begin
  inherited Create('Loading Icons');
  Self.sync := True;
end;

procedure TLoadIcon.runTask(Bar : TCMDProgressBar);
begin
  Bar.StartStep(1);
  Self.Log.log('Starting loading icons ...');
  LauncherIcons := AssetsFolder + 'LanucherIcons\';
  InstanceIcons := AssetsFolder + 'Icons\';
  OverviewF.Icons := LoadIcons(InstanceIcons, OverviewF.InstanceIcons);
  Bar.FinishStep;
end;

function LoadIcons(IconPath : String; ImageList : TImageList) : TStringList;
var
pngbmp: TPngImage;
bmp: TBitmap;
  i: Integer;
Images : TStringList;
begin
  Result := TStringList.Create;
  if IconPath = '' then
    Exit(Result);
  Images := ArrayToList(TDirectory.GetFiles(IconPath, '*.png', TSearchOption.soAllDirectories));
  ImageList.Masked:=false;
  ImageList.ColorDepth:=cd32bit;
  for i := 0 to Images.Count-1 do
  begin
    try
      pngbmp:=TPNGImage.Create;
      pngbmp.LoadFromFile(Images[i]);
      bmp:=TBitmap.Create;
      pngbmp.AssignTo(bmp);
      // ====================================================
      // Important or else it gets alpha blended into the list! After Assign
      // AlphaFormat is afDefined which is OK if you want to draw 32 bit bmp
      // with alpha blending on a canvas but not OK if you put it into
      // ImageList -- it will be way too dark!
      // ====================================================
      bmp.AlphaFormat:=afIgnored;
      ImageList.Add(bmp, nil);
      Result.Add(Images[i].Replace(IconPath, ''));
    except
      on E : Exception do
        Logger.Log.log('Could not load image: ' + Images[i]);
    end;
  end;
  Logger.Log.log('Loaded ' + IntToStr(Images.Count) + ' Icon(s)');
end;

end.
