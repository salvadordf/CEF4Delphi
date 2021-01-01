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

unit uCEFSetCookieCallback;

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
  TCefSetCookieCallbackOwn = class(TCefBaseRefCountedOwn, ICefSetCookieCallback)
    protected
      procedure OnComplete(success: Boolean); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefFastSetCookieCallback = class(TCefSetCookieCallbackOwn)
    protected
      FCallback: TCefSetCookieCallbackProc;

      procedure OnComplete(success: Boolean); override;

    public
      constructor Create(const callback: TCefSetCookieCallbackProc); reintroduce;
  end;

  TCefCustomSetCookieCallback = class(TCefSetCookieCallbackOwn)
    protected
      FEvents : Pointer;
      FID     : integer;

      procedure OnComplete(success: Boolean); override;

    public
      constructor Create(const aEvents : IChromiumEvents; aID : integer); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions;

procedure cef_set_cookie_callback_on_complete(self    : PCefSetCookieCallback;
                                              success : Integer); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefSetCookieCallbackOwn) then
    TCefSetCookieCallbackOwn(TempObject).OnComplete(success <> 0);
end;

constructor TCefSetCookieCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefSetCookieCallback));

  with PCefSetCookieCallback(FData)^ do
    on_complete := {$IFDEF FPC}@{$ENDIF}cef_set_cookie_callback_on_complete;
end;

// TCefFastSetCookieCallback

constructor TCefFastSetCookieCallback.Create(const callback: TCefSetCookieCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastSetCookieCallback.OnComplete(success: Boolean);
begin
  FCallback(success);
end;


// TCefCustomSetCookieCallback

constructor TCefCustomSetCookieCallback.Create(const aEvents : IChromiumEvents; aID : integer);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FID     := aID;
end;

destructor TCefCustomSetCookieCallback.Destroy;
begin
  FEvents := nil;

  inherited Destroy;
end;

procedure TCefCustomSetCookieCallback.OnComplete(success: Boolean);
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doOnCookieSet(success, FID);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomSetCookieCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


end.
