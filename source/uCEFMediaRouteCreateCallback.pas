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

unit uCEFMediaRouteCreateCallback;

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
