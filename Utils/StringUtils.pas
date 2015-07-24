unit StringUtils;

interface

uses System.Classes, System.SysUtils;


type
  TStringListHelper = class helper for TStringList
    function Contains(Search : String) : Boolean;
    function Remove(Item : String) : Boolean;
  end;

function ArrayToList(Value : array of String) : TStringList;
function Explode(Input, Splitter : String) : TStringList;
function Implode(Value : TStringList; Splitter : string) : string; overload;
function Implode(Value : array of string; Splitter : string) : string; overload;
function onlyContains(Input, Chars : String) : Boolean;
function isHigher(Lower, Higher : string) : Boolean;
function ExtractUrlFileName(Url : string) : string;
function ExtractLastFolder(Path : String) : string;
function RemoveLastFolderSpliter(Input : String) : string;
function isStringNumber(Input : String) : Boolean;

const
SplitReplacement : String = '§S';

implementation

function TStringListHelper.Contains(Search : String) : Boolean;
begin
  Result := Self.IndexOf(Search) <> -1;
end;

function TStringListHelper.Remove(Item : String) : Boolean;
var
index : Integer;
begin
  index := IndexOf(Item);
  if index <> -1 then
  begin
    Delete(index);
    Exit(True);
  end;
  Exit(False);
end;

function isStringNumber(Input : String) : Boolean;
var
P: PChar;
begin
  P := PChar(Input);
  Result := False;
  while P^ <> #0 do
  begin
    if not (P^ in ['0'..'9']) then Exit;
    Inc(P);
  end;
  Result := True;
end;

function RemoveLastFolderSpliter(Input : String) : string;
begin
  Result := Input;
  if (Input[Length(Input)] = '/') or (Input[Length(Input)] = '\') then
    Result := Input.Substring(0, Length(Input)-1);
end;

function isHigher(Lower, Higher : string) : Boolean;
var
LowerA, HigherA : TArray<String>;
i, lowI, higI : Integer;
found : Boolean;
begin
  LowerA := Lower.Split(['.']);
  HigherA := Higher.Split(['.']);
  Result := False;
  found := False;
  i := 0;
  while not found  do
  begin
    if (Length(LowerA) > i) and (Length(HigherA) > i) then
    begin
      lowI := StrToInt(LowerA[i]);
      higI := StrToInt(HigherA[i]);
      if lowI > higI then
        Exit(False)
      else if lowI < higI then
        Exit(True);
    end
    else if Length(LowerA) > i then
    begin
      Exit(False);
    end
    else if Length(HigherA) > i then
    begin
      Exit(True);
    end
    else
    begin
      Exit(False);
    end;
    i := i + 1;
  end;

end;

function ExtractLastFolder(Path : String) : string;
var
Folders : TStringList;
begin
  if Path.Contains('/') then
    Folders := Explode(Path, '/')
  else
    Folders := Explode(Path, '\');

  Result := '';
  if Folders.Count > 0 then
    Result := Folders[Folders.Count-1];
end;

function ExtractUrlFileName(Url : string) : string;
var
Split : TStringList;
begin
  Result := '';
  Split := Explode(Url, '/');
  if Split.Count > 0 then
    Result := Split[Split.Count-1];
end;

function ArrayToList(Value : array of String) : TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to Length(Value)-1 do
    Result.Add(Value[i]);
end;

function Explode(Input, Splitter : String) : TStringList;
var
Split : TArray<String>;
begin
  Split := Input.Split([Splitter], None);
  Result := ArrayToList(Split);
end;

function Implode(Value : TStringList; Splitter : string) : string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Value.Count-1 do
    Result := Result + Value[i].Replace(Splitter, SplitReplacement) + Splitter;
end;

function Implode(Value : array of string; Splitter : string) : string;
begin
  Result := Implode(ArrayToList(Value), Splitter);
end;

function onlyContains(Input, Chars : String) : Boolean;
var
  i: Integer;
begin
  if Chars = '' then
    Exit(True);
  for i := 1 to Length(Input) do
    if not Chars.ToLower.Contains(Input.ToLower[i]) then
      Exit(False);
  Exit(True);
end;

end.
