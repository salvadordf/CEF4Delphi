unit uCEFComponentUpdateCallback;

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
  TCefComponentUpdateCallbackOwn = class(TCefBaseRefCountedOwn, ICefComponentUpdateCallback)
    protected
      procedure OnComplete(const component_id: ustring; error: TCefComponentUpdateError); virtual;

    public
      constructor Create; virtual;
  end;

  TCefCustomComponentUpdateCallback = class(TCefComponentUpdateCallbackOwn)
    protected
      FEvents : Pointer;
      procedure OnComplete(const component_id: ustring; error: TCefComponentUpdateError); override;

    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

  TCefComponentUpdateCallbackRef = class(TCefBaseRefCountedRef, ICefComponentUpdateCallback)
    protected
      procedure OnComplete(const component_id: ustring; error: TCefComponentUpdateError);

    public
      class function UnWrap(data: Pointer): ICefComponentUpdateCallback;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions;

// TCefComponentUpdateCallbackOwn

procedure cef_component_update_callback_on_complete(self: PCefComponentUpdateCallback; const component_id: PCefString; error: TCefComponentUpdateError); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefComponentUpdateCallbackOwn) then
    TCefComponentUpdateCallbackOwn(TempObject).OnComplete(CefString(component_id), error);
end;

constructor TCefComponentUpdateCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefComponentUpdateCallback));

  with PCefComponentUpdateCallback(FData)^ do
    on_complete := {$IFDEF FPC}@{$ENDIF}cef_component_update_callback_on_complete;
end;

procedure TCefComponentUpdateCallbackOwn.OnComplete(const component_id: ustring; error: TCefComponentUpdateError);
begin
  //
end;


// TCefCustomComponentUpdateCallback

constructor TCefCustomComponentUpdateCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomComponentUpdateCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomComponentUpdateCallback.OnComplete(const component_id: ustring; error: TCefComponentUpdateError);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doOnComponentUpdateCompleted(component_id, error);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomComponentUpdateCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefComponentUpdateCallbackRef

procedure TCefComponentUpdateCallbackRef.OnComplete(const component_id: ustring; error: TCefComponentUpdateError);
var
  TempComponentId : TCefString;
begin
  TempComponentId := CefString(component_id);
  PCefComponentUpdateCallback(FData)^.on_complete(PCefComponentUpdateCallback(FData), @TempComponentId, error);
end;

class function TCefComponentUpdateCallbackRef.UnWrap(data: Pointer): ICefComponentUpdateCallback;
begin
  if (data <> nil) then
    Result := Create(data) as ICefComponentUpdateCallback
   else
    Result := nil;
end;

end.
