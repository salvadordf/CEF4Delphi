unit uCEFSharedMemoryRegion;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefSharedMemoryRegionRef = class(TCefBaseRefCountedRef, ICefSharedMemoryRegion)
    protected
      function IsValid: boolean;
      function Size: NativeUInt;
      function Memory: pointer;

    public
      class function UnWrap(data: Pointer): ICefSharedMemoryRegion;
  end;

implementation

function TCefSharedMemoryRegionRef.IsValid: Boolean;
begin
  Result := PCefSharedMemoryRegion(FData)^.is_valid(PCefSharedMemoryRegion(FData)) <> 0;
end;

function TCefSharedMemoryRegionRef.Size: NativeUInt;
begin
  Result := PCefSharedMemoryRegion(FData)^.Size(PCefSharedMemoryRegion(FData));
end;

function TCefSharedMemoryRegionRef.Memory: pointer;
begin
  Result := PCefSharedMemoryRegion(FData)^.Memory(PCefSharedMemoryRegion(FData));
end;

class function TCefSharedMemoryRegionRef.UnWrap(data: Pointer): ICefSharedMemoryRegion;
begin
  if (data <> nil) then
    Result := Create(data) as ICefSharedMemoryRegion
   else
    Result := nil;
end;

end.
