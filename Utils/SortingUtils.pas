unit SortingUtils;

interface

uses System.Generics.Collections, System.SysUtils, System.Classes;

type
  TWhiteBlackTyp = (ltWhite, ltBlack);
  TWhiteBlackList = class(TList<String>)
    ListType : TWhiteBlackTyp;
    function isValid(Input : String) : Boolean;
    constructor Create(ListType : TWhiteBlackTyp = ltBlack);
  end;


function SortVersions(List: TStringList; Index1, Index2: Integer): Integer;

implementation

uses StringUtils;

function SortVersions(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if isHigher(List[index1], List[index2]) then
    Result := 1
  else if isHigher(List[index2], List[index1]) then
    Result := -1
  else
    Result := 0;
end;

constructor TWhiteBlackList.Create(ListType : TWhiteBlackTyp = ltBlack);
begin
  Self.ListType := ListType;
end;

function TWhiteBlackList.isValid(Input : String) : Boolean;
var
i : Integer;
begin
  for i := 0 to Count-1 do
  begin
    if Items[i].EndsWith('/') or Items[i].EndsWith('\') then
    begin
      if Input.StartsWith(Items[i]) then
        if ListType = ltWhite then
          Exit(True)
        else
          Exit(False);
    end
    else
    begin
      if Input = Items[i] then
        if ListType = ltWhite then
          Exit(True)
        else
          Exit(False);
    end;
  end;
  if ListType = ltWhite then
    Exit(False)
  else
    Exit(True);
end;

end.
