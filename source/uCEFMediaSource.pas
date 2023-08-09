unit uCEFMediaSource;

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
  TCefMediaSourceRef = class(TCefBaseRefCountedRef, ICefMediaSource)
  protected
    function GetId : ustring;
    function IsCastSource : boolean;
    function IsDialSource : boolean;
  public
    class function UnWrap(data: Pointer): ICefMediaSource;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefMediaSourceRef.GetId: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaSource(FData)^.get_id(PCefMediaSource(FData)));
end;

function TCefMediaSourceRef.IsCastSource: Boolean;
begin
  Result := PCefMediaSource(FData)^.is_cast_source(PCefMediaSource(FData)) <> 0;
end;

function TCefMediaSourceRef.IsDialSource: Boolean;
begin
  Result := PCefMediaSource(FData)^.is_dial_source(PCefMediaSource(FData)) <> 0;
end;

class function TCefMediaSourceRef.UnWrap(data: Pointer): ICefMediaSource;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaSource
   else
    Result := nil;
end;

end.
