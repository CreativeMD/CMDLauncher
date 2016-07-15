unit FontUtils;

interface
uses Winapi.Windows, System.Classes;

function LoadResourceFont( const ResourceName, FontName : string ) : boolean;

implementation

function LoadResourceFont( const ResourceName, FontName : string ) : boolean;
var
   //ResStream : TResourceStream;
   MemoryStream : TMemoryStream;
   FontsCount : integer;
   hFont : tHandle;
begin
  MemoryStream := TMemoryStream.Create;
  MemoryStream.LoadFromFile(ResourceName);
  //ResStream := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
  hFont := AddFontMemResourceEx(MemoryStream.Memory, MemoryStream.Size, nil, @FontsCount);
  result := (hFont <> 0);
  MemoryStream.Free();
end;

end.
