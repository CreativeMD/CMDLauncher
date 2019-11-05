unit BuildUtils;

interface

uses Vcl.Graphics, System.SysUtils;

type
  TBuildType = (TExperimental, TDev, TStable, TRecommend);
  TBuildTypeObject = class;
  TBuildTypeHelper = record helper for TBuildType
    function getColor : TColor;
    function getObject : TBuildTypeObject;
  end;
  TBuildTypeObject = class(TObject)
    BuildType : TBuildType;
    constructor Create(BuildType : TBuildType);
  end;

function parseBuildTypeString(Input : String) : TBuildType;

implementation

function parseBuildTypeString(Input : String) : TBuildType;
begin
  Input := Input.toLower;
  if Input = 'experimental' then
    Exit(TExperimental);
  if (Input = 'dev') or (Input = 'beta') then
    Exit(TDev);
  if Input = 'stable' then
    Exit(TStable);
  if Input = 'recommend' then
    Exit(TRecommend);
  Exit(TStable);
end;

function TBuildTypeHelper.getColor : TColor;
begin
  case Self of
    TExperimental: Exit(clRed);
    TDev: Exit(clYellow);
    TStable: Exit(clSkyBlue);
    TRecommend: Exit(clGreen);
  end;
  Exit(clWhite);
end;

function TBuildTypeHelper.getObject : TBuildTypeObject;
begin
  Result := TBuildTypeObject.Create(Self);
end;

constructor TBuildTypeObject.Create(BuildType : TBuildType);
begin
  Self.BuildType := BuildType;
end;



end.
