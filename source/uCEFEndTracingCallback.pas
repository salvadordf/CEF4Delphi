unit uCEFEndTracingCallback;

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
  TCefEndTracingCallbackOwn = class(TCefBaseRefCountedOwn, ICefEndTracingCallback)
    protected
      procedure OnEndTracingComplete(const tracingFile: ustring); virtual;
    public
      constructor Create; virtual;
  end;

  TCefFastEndTracingCallback = class(TCefEndTracingCallbackOwn)
    protected
      FCallback: TCefEndTracingCallbackProc;
      procedure OnEndTracingComplete(const tracingFile: ustring); override;
    public
      constructor Create(const callback: TCefEndTracingCallbackProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

// TCefEndTracingCallbackOwn

procedure cef_end_tracing_callback_on_end_tracing_complete(      self         : PCefEndTracingCallback;
                                                           const tracing_file : PCefString); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefEndTracingCallbackOwn) then
    TCefEndTracingCallbackOwn(TempObject).OnEndTracingComplete(CefString(tracing_file));
end;

constructor TCefEndTracingCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefEndTracingCallback));

  with PCefEndTracingCallback(FData)^ do
    on_end_tracing_complete := {$IFDEF FPC}@{$ENDIF}cef_end_tracing_callback_on_end_tracing_complete;
end;

procedure TCefEndTracingCallbackOwn.OnEndTracingComplete(const tracingFile: ustring);
begin
  //
end;

// TCefFastEndTracingCallback

constructor TCefFastEndTracingCallback.Create(const callback: TCefEndTracingCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastEndTracingCallback.OnEndTracingComplete(const tracingFile: ustring);
begin
  FCallback(tracingFile);
end;

end.
