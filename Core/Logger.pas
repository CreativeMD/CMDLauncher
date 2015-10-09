unit Logger;

interface

uses System.Classes, System.Generics.Collections, StringUtils;

type
  TLog = class
    Content : TStringList;
    Listener : TList<TStrings>;
    procedure log(Value : String); overload;
    procedure log(Value : array of string); overload;
    procedure log(Value : TStrings); overload;
    procedure logLastLine(Value : string);
    function getLastLog : String;
    constructor Create;
  end;

var
MainLog : TLog;

implementation

procedure TLog.logLastLine(Value : string);
var
i : Integer;
begin
  Content[Content.Count-1] := Value;
  for i := 0 to Listener.Count-1 do
  begin
    Listener[i][Listener[i].Count-1] := Value;
    Listener[i].EndUpdate;
    //Listener[i].Delete(Listener[i].Count-1);
    //Listener[i].Add(Value);
  end;
end;

procedure TLog.log(Value : String);
var
  i: Integer;
begin
  Content.Add(Value);
  for i := 0 to Listener.Count-1 do
    Listener[i].Add(Value);
end;

procedure TLog.log(Value : array of string);
begin
  log(ArrayToList(Value));
end;

procedure TLog.log(Value : TStrings);
var
val : String;
i : Integer;
begin
  val := '[';
  for i := 0 to Value.Count-1 do
  begin
    val := val + Value[i];
    if i < Value.Count-1 then
      val := val + ';';
  end;
  val := val + ']';
end;

constructor TLog.Create;
begin
  Content := TStringList.Create;
  Listener := TList<TStrings>.Create;
end;

function TLog.getLastLog : String;
begin
  Result := '';
  if Content.Count > 0 then
    Result := Content[Content.Count-1];
end;

end.
