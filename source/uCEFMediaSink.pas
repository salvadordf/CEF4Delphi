unit uCEFMediaSink;

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
  TCefMediaSinkRef = class(TCefBaseRefCountedRef, ICefMediaSink)
  protected
    function  GetId: ustring;
    function  GetName: ustring;
    function  GetIconType: TCefMediaSinkIconType;
    procedure GetDeviceInfo(const callback: ICefMediaSinkDeviceInfoCallback);
    function  IsCastSink: boolean;
    function  IsDialSink: boolean;
    function  IsCompatibleWith(const source: ICefMediaSource): boolean;
  public
    class function UnWrap(data: Pointer): ICefMediaSink;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function TCefMediaSinkRef.GetId: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaSink(FData)^.get_id(PCefMediaSink(FData)));
end;

function TCefMediaSinkRef.GetName: ustring;
begin
  Result := CefStringFreeAndGet(PCefMediaSink(FData)^.get_name(PCefMediaSink(FData)));
end;

function TCefMediaSinkRef.GetIconType: TCefMediaSinkIconType;
begin
  Result := PCefMediaSink(FData)^.get_icon_type(PCefMediaSink(FData));
end;

procedure TCefMediaSinkRef.GetDeviceInfo(const callback: ICefMediaSinkDeviceInfoCallback);
begin
  PCefMediaSink(FData)^.get_device_info(PCefMediaSink(FData), CefGetData(callback));
end;

function TCefMediaSinkRef.IsCastSink: Boolean;
begin
  Result := PCefMediaSink(FData)^.is_cast_sink(PCefMediaSink(FData)) <> 0;
end;

function TCefMediaSinkRef.IsDialSink: Boolean;
begin
  Result := PCefMediaSink(FData)^.is_dial_sink(PCefMediaSink(FData)) <> 0;
end;

function TCefMediaSinkRef.IsCompatibleWith(const source: ICefMediaSource): boolean;
begin
  Result := PCefMediaSink(FData)^.is_compatible_with(PCefMediaSink(FData), CefGetData(source)) <> 0;
end;

class function TCefMediaSinkRef.UnWrap(data: Pointer): ICefMediaSink;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaSink
   else
    Result := nil;
end;

end.
