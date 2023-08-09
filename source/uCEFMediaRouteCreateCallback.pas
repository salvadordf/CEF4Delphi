unit uCEFMediaRouteCreateCallback;

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
  TCefMediaRouteCreateCallbackOwn = class(TCefBaseRefCountedOwn, ICefMediaRouteCreateCallback)
    protected
      procedure OnMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute); virtual; abstract;
    public
      constructor Create; virtual;
  end;

  TCefFastMediaRouteCreateCallback = class(TCefMediaRouteCreateCallbackOwn)
    protected
      FCallback: TCefMediaRouteCreateCallbackProc;

      procedure OnMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute); override;
    public
      constructor Create(const callback: TCefMediaRouteCreateCallbackProc); reintroduce;
      destructor  Destroy; override;
  end;

  TCefCustomMediaRouteCreateCallback = class(TCefMediaRouteCreateCallbackOwn)
    protected
      FEvents : Pointer;

      procedure OnMediaRouteCreateFinished(result: TCefMediaRouterCreateResult; const error: ustring; const route: ICefMediaRoute); override;
    public
      constructor Create(const aEvents : IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMediaRoute;

procedure cef_media_route_create_callback_on_media_route_create_finished(      self   : PCefMediaRouteCreateCallback;
                                                                               result : TCefMediaRouterCreateResult;
                                                                         const error  : PCefString;
                                                                               route  : PCefMediaRoute); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaRouteCreateCallbackOwn) then
    TCefMediaRouteCreateCallbackOwn(TempObject).OnMediaRouteCreateFinished(result,
                                                                           CefString(error),
                                                                           TCefMediaRouteRef.UnWrap(route));
end;


// *************************************************************
// ************** TCefMediaRouteCreateCallbackOwn **************
// *************************************************************

constructor TCefMediaRouteCreateCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefMediaRouteCreateCallback));

  PCefMediaRouteCreateCallback(FData)^.on_media_route_create_finished :=
    {$IFDEF FPC}@{$ENDIF}cef_media_route_create_callback_on_media_route_create_finished;
end;


// **************************************************************
// ************** TCefFastMediaRouteCreateCallback **************
// **************************************************************

constructor TCefFastMediaRouteCreateCallback.Create(const callback: TCefMediaRouteCreateCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastMediaRouteCreateCallback.OnMediaRouteCreateFinished(      result : TCefMediaRouterCreateResult;
                                                                      const error  : ustring;
                                                                      const route  : ICefMediaRoute);
begin
  if assigned(FCallback) then
    FCallback(result, error, route);
end;

destructor TCefFastMediaRouteCreateCallback.Destroy;
begin
  FCallback := nil;

  inherited Destroy;
end;


// ****************************************************************
// ************** TCefCustomMediaRouteCreateCallback **************
// ****************************************************************

constructor TCefCustomMediaRouteCreateCallback.Create(const aEvents : IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
end;

destructor TCefCustomMediaRouteCreateCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomMediaRouteCreateCallback.OnMediaRouteCreateFinished(      result : TCefMediaRouterCreateResult;
                                                                        const error  : ustring;
                                                                        const route  : ICefMediaRoute);
begin
  try
    try
      if (FEvents <> nil) then
        IChromiumEvents(FEvents).doMediaRouteCreateFinished(result, error, route);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomMediaRouteCreateCallback.OnMediaRouteCreateFinished', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
