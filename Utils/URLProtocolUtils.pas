unit URLProtocolUtils;

interface

uses System.Win.Registry, Winapi.Windows, Vcl.Dialogs, System.SysUtils;

function registerProtocol(const Name, description, ProgramFile : string) : Boolean;
function checkIfProtocol(const Name, description, ProgramFile : String) : Boolean;

implementation

function checkIfProtocol(const Name, description, ProgramFile : String) : Boolean;
var
  reg: TRegistry;
begin
  Result := False;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if reg.OpenKeyReadOnly(Name) then
    begin
      try
        if (reg.ReadString('') = 'URL:' + Name +' (' + description + ')')
          and (reg.ReadInteger('EditFlags') = 2)
          and (reg.OpenKeyReadOnly('shell'))
          and (reg.OpenKeyReadOnly('open'))
          and (reg.OpenKeyReadOnly('command'))
          and (reg.ReadString('') = ProgramFile) then
        begin
          Result := True;
        end
      except

      end;
      Reg.CloseKey;
    end;
  except

  end;
  Reg.Free;
end;

function registerProtocol(const Name, description, ProgramFile: string) : Boolean;
var
  reg: TRegistry;
begin
  Result := False;
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
        reg.Writestring('', ProgramFile);
        Result := True;
      finally
        reg.CloseKey;
      end;
    end;
  finally
    reg.Free;
  end;
end;

end.
