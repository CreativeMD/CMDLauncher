unit LogForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TLogFile = class(TForm)
    mmoLog: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  LogFile: TLogFile;

implementation

uses Logger;

{$R *.dfm}

procedure TLogFile.FormShow(Sender: TObject);
begin
  mmoLog.Lines.Assign(Logger.Log.Content);
end;

end.
