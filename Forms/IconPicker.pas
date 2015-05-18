unit IconPicker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, JvExForms,
  JvCustomItemViewer, JvImageListViewer;

type
  TIconPick = class(TForm)
    ImageListViewer: TJvImageListViewer;
    btnSave: TButton;
    btnCancel: TButton;
    procedure ImageListViewerClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TIconPick.ImageListViewerClick(Sender: TObject);
begin
  ImageListViewer.SelectedIndex := -1;
end;

end.
