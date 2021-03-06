unit CustomSettings;

interface

uses SettingUtils, Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, System.SysUtils, FileCtrl;

type
TSelectDirSetting = class(TStringSetting)
  constructor Create(Name, Title : String; Default : string = ''; Online : Boolean = False);
  procedure createControl(x, y : Integer; Parent : TWinControl); override;
  function getUUID : string; override;
  procedure onButtonClicked(Sender : TObject);
end;

implementation

constructor TSelectDirSetting.Create(Name, Title : String; Default : string = ''; Online : Boolean = False);
begin
  inherited Create(Name, Title, Default, Online, '');
  setWidth(190);
end;

procedure TSelectDirSetting.onButtonClicked(Sender : TObject);
var
failed : Boolean;
directory : string;
options : TSelectDirOpts;
begin
  if (Sender as TButton).Caption = 'Select Dir' then
  begin
    failed := False;
    try
      with TFileOpenDialog.Create(nil) do
      begin
        try
          Options := [fdoPickFolders];
          if Execute then
            TEdit(Controls[0]).Text := FileName + '\';
        finally
          Free;
        end;
      end;
    except
      on E: Exception do
      begin
        failed := True;
      end;
    end;
    if failed then
    begin
      directory := '';
      if SelectDirectory(directory, options, 0) then
        TEdit(Controls[0]).Text := directory + '\';
    end;
  end
  else
    TEdit(Controls[0]).Text := '';
end;

procedure TSelectDirSetting.createControl(x, y : Integer; Parent : TWinControl);
var
Button : TButton;
begin
  inherited createControl(x, y, Parent);
  Button := TButton.Create(Parent);
  Button.Parent := Parent;
  Button.Left := x+Controls[0].Width+4;
  Button.Top := y;
  Button.Caption := 'Select Dir';
  Button.OnClick := onButtonClicked;
  Controls.Add(Button);
  Button := TButton.Create(Parent);
  Button.Parent := Parent;
  Button.Left := x+Controls[0].Width+Controls[1].Width+4;
  Button.Top := y;
  Button.Caption := 'Reset Dir';
  Button.OnClick := onButtonClicked;
  Controls.Add(Button);
end;

function TSelectDirSetting.getUUID : string;
begin
  Result := 'selectdir';
end;

end.
