unit uCEFv8ArrayBufferReleaseCallback;

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
  TCefv8ArrayBufferReleaseCallbackOwn = class(TCefBaseRefCountedOwn, ICefv8ArrayBufferReleaseCallback)
    protected
      procedure ReleaseBuffer(buffer : Pointer); virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastv8ArrayBufferReleaseCallback = class(TCefv8ArrayBufferReleaseCallbackOwn)
    protected
      FCallback: TCefv8ArrayBufferReleaseCallbackProc;

      procedure ReleaseBuffer(buffer : Pointer); override;

    public
      constructor Create(const callback: TCefv8ArrayBufferReleaseCallbackProc); reintroduce;
  end;

  TCefv8ArrayBufferReleaseCallbackRef = class(TCefBaseRefCountedRef, ICefv8ArrayBufferReleaseCallback)
    protected
      procedure ReleaseBuffer(buffer : Pointer); virtual;

    public
      class function UnWrap(data: Pointer): ICefv8ArrayBufferReleaseCallback;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_v8array_buffer_release_callback_release_buffer(self   : PCefv8ArrayBufferReleaseCallback;
                                                             buffer : Pointer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefv8ArrayBufferReleaseCallbackOwn) then
    TCefv8ArrayBufferReleaseCallbackOwn(TempObject).ReleaseBuffer(buffer);
end;


// TCefv8ArrayBufferReleaseCallbackOwn

constructor TCefv8ArrayBufferReleaseCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefv8ArrayBufferReleaseCallback));

  with PCefv8ArrayBufferReleaseCallback(FData)^ do
    release_buffer := {$IFDEF FPC}@{$ENDIF}cef_v8array_buffer_release_callback_release_buffer;
end;

procedure TCefv8ArrayBufferReleaseCallbackOwn.ReleaseBuffer(buffer: Pointer);
begin
  //
end;


// TCefFastv8ArrayBufferReleaseCallback

constructor TCefFastv8ArrayBufferReleaseCallback.Create(const callback: TCefv8ArrayBufferReleaseCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastv8ArrayBufferReleaseCallback.ReleaseBuffer(buffer: Pointer);
begin
  FCallback(buffer);
end;


// TCefv8ArrayBufferReleaseCallbackRef

procedure TCefv8ArrayBufferReleaseCallbackRef.ReleaseBuffer(buffer : Pointer);
begin
  PCefv8ArrayBufferReleaseCallback(FData)^.release_buffer(PCefv8ArrayBufferReleaseCallback(FData), buffer);
end;

class function TCefv8ArrayBufferReleaseCallbackRef.UnWrap(data: Pointer): ICefv8ArrayBufferReleaseCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefv8ArrayBufferReleaseCallback
   else
    Result := nil;
end;

end.
