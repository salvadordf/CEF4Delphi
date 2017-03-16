// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uCEFDeleteCookiesCallback;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefDeleteCookiesCallbackOwn = class(TCefBaseRefCountedOwn, ICefDeleteCookiesCallback)
    protected
      procedure OnComplete(numDeleted: Integer); virtual; abstract;

    public
      constructor Create; virtual;
  end;

  TCefFastDeleteCookiesCallback = class(TCefDeleteCookiesCallbackOwn)
    protected
      FCallback: TCefDeleteCookiesCallbackProc;

      procedure OnComplete(numDeleted: Integer); override;

    public
      constructor Create(const callback: TCefDeleteCookiesCallbackProc); reintroduce;
  end;

  TCefCustomDeleteCookiesCallback = class(TCefDeleteCookiesCallbackOwn)
    protected
      FChromiumBrowser : TObject;

      procedure OnComplete(numDeleted: Integer); override;

    public
      constructor Create(const aChromiumBrowser : TObject); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFChromium;

procedure cef_delete_cookie_callback_on_complete(self: PCefDeleteCookiesCallback; num_deleted: Integer); stdcall;
begin
  with TCefDeleteCookiesCallbackOwn(CefGetObject(self)) do OnComplete(num_deleted);
end;

// TCefDeleteCookiesCallbackOwn

constructor TCefDeleteCookiesCallbackOwn.Create;
begin
  inherited CreateData(SizeOf(TCefDeleteCookiesCallback));

  with PCefDeleteCookiesCallback(FData)^ do on_complete := cef_delete_cookie_callback_on_complete;
end;

// TCefFastDeleteCookiesCallback

constructor TCefFastDeleteCookiesCallback.Create(const callback: TCefDeleteCookiesCallbackProc);
begin
  inherited Create;

  FCallback := callback;
end;

procedure TCefFastDeleteCookiesCallback.OnComplete(numDeleted: Integer);
begin
  FCallback(numDeleted)
end;

// TCefCustomDeleteCookiesCallback

constructor TCefCustomDeleteCookiesCallback.Create(const aChromiumBrowser : TObject);
begin
  inherited Create;

  FChromiumBrowser := aChromiumBrowser;
end;

procedure TCefCustomDeleteCookiesCallback.OnComplete(numDeleted: Integer);
begin
  if (FChromiumBrowser <> nil) and (FChromiumBrowser is TChromium) then
    TChromium(FChromiumBrowser).Internal_CookiesDeleted(numDeleted);
end;

end.
