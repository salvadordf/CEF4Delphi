// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFMediaRouter;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

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
  Result := UnWrap(cef_media_router_get_global());
end;

end.
