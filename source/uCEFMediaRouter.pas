unit uCEFMediaRouter;

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
  TCefMediaRouterRef = class(TCefBaseRefCountedRef, ICefMediaRouter)
  protected
    function  AddObserver(const observer: ICefMediaObserver): ICefRegistration;
    function  GetSource(const urn: ustring): ICefMediaSource;
    procedure NotifyCurrentSinks;
    procedure CreateRoute(const source: ICefMediaSource; const sink: ICefMediaSink; const callback: ICefMediaRouteCreateCallback);
    procedure NotifyCurrentRoutes;
  public
    class function UnWrap(data: Pointer): ICefMediaRouter;
    class function Global: ICefMediaRouter;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFMediaSource, uCEFRegistration,
  uCEFMediaRouteCreateCallback;

function TCefMediaRouterRef.AddObserver(const observer: ICefMediaObserver): ICefRegistration;
begin
  Result := TCefRegistrationRef.UnWrap(PCefMediaRouter(FData)^.add_observer(PCefMediaRouter(FData),
                                                                            CefGetData(observer)));
end;

function TCefMediaRouterRef.GetSource(const urn: ustring): ICefMediaSource;
var
  TempURN : TCefString;
begin
  TempURN := CefString(urn);
  Result  := TCefMediaSourceRef.UnWrap(PCefMediaRouter(FData)^.get_source(PCefMediaRouter(FData),
                                                                          @TempURN));
end;

procedure TCefMediaRouterRef.NotifyCurrentSinks;
begin
  PCefMediaRouter(FData)^.notify_current_sinks(PCefMediaRouter(FData));
end;

procedure TCefMediaRouterRef.CreateRoute(const source   : ICefMediaSource;
                                         const sink     : ICefMediaSink;
                                         const callback : ICefMediaRouteCreateCallback);
begin
  PCefMediaRouter(FData)^.create_route(PCefMediaRouter(FData),
                                       CefGetData(source),
                                       CefGetData(sink),
                                       CefGetData(callback));
end;

procedure TCefMediaRouterRef.NotifyCurrentRoutes;
begin
  PCefMediaRouter(FData)^.notify_current_routes(PCefMediaRouter(FData));
end;

class function TCefMediaRouterRef.UnWrap(data: Pointer): ICefMediaRouter;
begin
  if (data <> nil) then
    Result := Create(data) as ICefMediaRouter
   else
    Result := nil;
end;

class function TCefMediaRouterRef.Global: ICefMediaRouter;
begin
  Result := UnWrap(cef_media_router_get_global(nil));
end;

end.
