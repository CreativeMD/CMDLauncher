unit SaveFileUtils;

interface

uses System.Classes, StringUtils, System.SysUtils, superobject;

type
  TSaveFile = class
    private
      FileName : String;
      Splitter : String;
    public
      constructor Create(FileName : String; Splitter : String); overload;
      constructor Create(FileName : String); overload;
      function getFile : TStringList;
      function getIndex(Key : String) : Integer;
      function hasKey(Key : string) : Boolean;

      function getString(Key : String) : String;
      function getInteger(Key : String) : Integer;
      function getBoolean(Key : String) : Boolean;
      function getStringList(Key : String) : TStringList;
      function getJson(Key : String) : ISuperObject;
      function getArray(Key : String) : TSuperArray;

      procedure setString(Key, Value : String);
      procedure setInteger(Key : String; Value : Integer);
      procedure setBoolean(Key : String; Value : Boolean);
      procedure setStringList(Key : String; Value : TStringList);
      procedure setJson(Key : String; Value : ISuperObject);
      procedure setArray(Key : String; Value : TSuperArray);
  end;

const
  SplitReplacement : String = '&Split';
implementation

constructor TSaveFile.Create(FileName : String; Splitter : String);
begin
  Self.FileName := FileName;
  Self.Splitter := Splitter;
end;

constructor TSaveFile.Create(FileName : String);
begin
  Create(FileName, '=');
end;

function TSaveFile.getFile : TStringList;
begin
  Result := TStringList.Create;
  if not FileExists(FileName) then
  begin
    ForceDirectories(ExtractFilePath(FileName));
    Result.SaveToFile(FileName);
  end;
  Result.LoadFromFile(FileName);
end;

function TSaveFile.hasKey(Key : string) : Boolean;
begin
  Result := getIndex(Key) <> -1;
end;

function TSaveFile.getIndex(Key : String) : Integer;
var
Lines, Value  : TStringList;
i: Integer;
begin
  Lines := getFile;
  for i := 0 to Lines.Count-1 do
  begin
    Value := Explode(Lines[i], Splitter);
    if (Value.Count = 2) and (Key = Value[0]) then
      Exit(i)
    else if (Value.Count = 1) and (Key = Value[0]) then
      Exit(i);
  end;
  Exit(-1);
end;

function TSaveFile.getString(Key : String) : String;
var
Lines, Value  : TStringList;
i: Integer;
begin
  Lines := getFile;
  for i := 0 to Lines.Count-1 do
  begin
    Value := Explode(Lines[i], Splitter);
    if (Value.Count = 2) and (Key = Value[0]) then
      Exit(Value[1].Replace(SplitReplacement, Splitter));
  end;
  Exit('');
end;

function TSaveFile.getInteger(Key : String) : Integer;
var
Value : String;
begin
  Value := getString(Key);
  try
    Result := StrToInt(Value);
  except
    on e : Exception do
      Result := 0;
  end;
end;

function TSaveFile.getBoolean(Key : String) : Boolean;
var
Value : String;
begin
  Value := getString(Key);
  Result := Value = 'true';
end;

function TSaveFile.getStringList(Key : String) : TStringList;
var
Value : String;
begin
  if hasKey(Key) then
  begin
    Value := getString(Key);
    Result := Explode(Value, ';');
  end
  else
    Result := TStringList.Create;
  if (Result.Count = 1) and (Result[0] = '') then
    Result.Clear;
end;

function TSaveFile.getJson(Key : String) : ISuperObject;
var
Value : String;
begin
  Value := getString(Key);
  Result := TSuperObject.ParseString(PWideChar(Value), False);
end;

function TSaveFile.getArray(Key : String) : TSuperArray;
var
SuperObject : ISuperObject;
begin
  SuperObject := getJson(Key);
  Result := SuperObject.A['array'];
end;

procedure TSaveFile.setString(Key, Value : String);
var
Lines, Values  : TStringList;
i: Integer;
Saved : Boolean;
begin
  Lines := getFile;
  Saved := False;
  for i := 0 to Lines.Count-1 do
  begin
    Values := Explode(Lines[i], Splitter);
    if ((Values.Count = 2) or (Values.Count = 1)) and (Key = Values[0]) then
    begin
      Lines[i] := Key + Splitter + Value.Replace(Splitter, SplitReplacement);
      Saved := True;
    end;
  end;
  if not Saved then
    Lines.Add(Key + Splitter + Value.Replace(Splitter, SplitReplacement));
  Lines.SaveToFile(FileName);
end;

procedure TSaveFile.setInteger(Key : String; Value : Integer);
begin
  setString(Key, IntToStr(Value));
end;

procedure TSaveFile.setBoolean(Key : String; Value : Boolean);
begin
  if Value then
    setString(Key, 'true')
  else
    setString(Key, 'false');
end;

procedure TSaveFile.setStringList(Key : String; Value : TStringList);
begin
  setString(Key, Implode(Value, ';'));
end;

procedure TSaveFile.setJson(Key : String; Value : ISuperObject);
begin
  setString(Key, Value.AsJSon);
end;

procedure TSaveFile.setArray(Key : String; Value : TSuperArray);
var
superobject : ISuperObject;
i: Integer;
begin
  superobject := TSuperObject.Create;
  superobject.O['array'] := TSuperObject.Create(stArray);
  for i := 0 to Value.Length-1 do
    superobject.A['array'].O[i] := Value.O[i];
  setJson(Key, superobject);
end;

end.
