unit URLProtocolUtils;

interface

uses System.Win.Registry, Winapi.Windows, Vcl.Dialogs;

procedure RegisterProtocol(const Name, description, ExecuteStr: string);


var
CreatedProtocol : Boolean;

implementation

procedure RegisterProtocol(const Name, description, ExecuteStr: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if reg.OpenKey(Name, True) then
    begin
    try
      //ShowMessage('Created!');
      reg.Writestring('', 'URL:' + Name +' (' + description + ')');
      reg.WriteInteger('EditFlags', 2);
      reg.WriteString('Source Filter', '');
      reg.WriteString('URL Protocol', '');
      reg.OpenKey('shell', True);
      reg.OpenKey('open', True);
      reg.OpenKey('command', True);
      reg.Writestring('', ExecuteStr);
    finally
      reg.CloseKey;
    end;
    end
    else
      CreatedProtocol := False;
  finally
    reg.Free;
  end;
end;

end.
