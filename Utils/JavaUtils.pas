unit JavaUtils;

interface

uses Task, ProgressBar, System.Generics.Collections, Winapi.Windows, System.Classes,
System.Win.Registry, System.SysUtils;

type
  TJava = class
    Title, Path : String;
    is64 : Boolean;
    function getArch : string;
    constructor Create(Title, Path : string; is64 : Boolean);
    function getCompleteName : String;
  end;
  TLoadJava = class(TTask)
    procedure runTask(Bar : TCMDProgressBar); override;
    constructor Create;
  end;

var
JavaVersions : TList<TJava>;
SelectedJava : string;

function getSelectedJava : TJava;
function getJavaByTitle(Title : String) : TJava;
function getJavaVersions : TStringList;

implementation

uses CoreLoader;

function getJavaVersions : TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to JavaVersions.Count-1 do
    Result.Add(JavaVersions[i].getCompleteName);
  Result.Sort;
end;

function getSelectedJava : TJava;
begin
  Result := getJavaByTitle(SelectedJava);
end;

function getJavaByTitle(Title : String) : TJava;
var
  I: Integer;
begin
  for I := 0 to JavaVersions.Count-1 do
    if (JavaVersions[I].Title = Title) or (JavaVersions[I].getCompleteName = Title) then
      Exit(JavaVersions[I]);
  Exit(nil);
end;

constructor TJava.Create(Title, Path : string; is64 : Boolean);
begin
  Self.Title := Title;
  Self.Path := Path;
  Self.is64 := is64;
end;

function TJava.getCompleteName : String;
begin
  Result := Self.Title + ' - ' + Self.getArch + 'bit';
end;

function TJava.getArch : string;
begin
  if is64 then
    Result := '64'
  else
    Result := '32';
end;

procedure searchInFolder(Reg : TRegistry; is64 : Boolean; Prefix : String);
var
TempVersions : TStringList;
  i: Integer;
  Path : string;
begin
  TempVersions := TStringList.Create;
  Reg.GetKeyNames(TempVersions);
  Path := Reg.CurrentPath;
  Reg.CloseKey;
  for i := 0 to TempVersions.Count-1 do
    if (Length(TempVersions[i]) > 3) and (TempVersions[i] <> '') then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Reg.OpenKeyReadOnly(Path);
      Reg.OpenKeyReadOnly(TempVersions[i]);
      JavaVersions.Add(TJava.Create(Prefix + TempVersions[i], Reg.ReadString('JavaHome') + '\bin\javaw.exe', is64));
      Reg.CloseKey;
    end;
end;

function IsWow64: Boolean;
  type
    TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: Cardinal; var Res: BOOL
  ): BOOL; stdcall;
  var
    IsWow64Result: BOOL; // Result from IsWow64Process
    IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
  begin
    // Try to load required function from kernel32
    IsWow64Process := GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process');
    if Assigned(IsWow64Process) then
    begin
      // Function is implemented: call it
      if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
        raise Exception.Create('IsWow64: bad process handle');
      // Return result of function
      Result := IsWow64Result;
    end
    else
      // Function not implemented: can't be running on Wow64
      Result := False;
  end;

procedure TLoadJava.runTask(Bar : TCMDProgressBar);
var
  Reg: TRegistry;
const KEY_WOW64_64KEY = $0100;
begin
  JavaVersions := TList<TJava>.Create;

  SelectedJava := ProgramSettings.getString('javaversion');

  if IsWow64 then
    Reg := TRegistry.Create(KEY_ALL_ACCESS or KEY_WOW64_64KEY)
  else
    Reg := TRegistry.Create;
  with Reg do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\JavaSoft\Java Runtime Environment') then
      begin   
        searchInFolder(Reg, True, 'jre');
      end;
      CloseKey;

      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\JavaSoft\Java Development Kit') then
      begin   
        searchInFolder(Reg, True, 'jdk');
      end;
      CloseKey;
      
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\Wow6432Node\JavaSoft\Java Runtime Environment') then
      begin
        searchInFolder(Reg, False, 'jre');
      end;
      CloseKey;
      
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\Wow6432Node\JavaSoft\Java Development Kit') then
      begin   
        searchInFolder(Reg, False, 'jdk');
      end;
      CloseKey;
    finally
      Free;
    end;
  end;
  Bar.FinishStep;
end;

constructor TLoadJava.Create;
begin
  inherited Create('Loading Java versions');
end;

end.
