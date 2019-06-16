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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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

unit uCEFCookieManager;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefCookieManagerRef = class(TCefBaseRefCountedRef, ICefCookieManager)
    protected
      procedure SetSupportedSchemes(const schemes: TStrings; include_defaults: boolean; const callback: ICefCompletionCallback);
      procedure SetSupportedSchemesProc(const schemes: TStrings; include_defaults: boolean; const callback: TCefCompletionCallbackProc);
      function  VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
      function  VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
      function  VisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
      function  VisitUrlCookiesProc(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
      function  SetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; const callback: ICefSetCookieCallback): Boolean;
      function  SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; const callback: TCefSetCookieCallbackProc): Boolean;
      function  DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
      function  DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
      function  FlushStore(const handler: ICefCompletionCallback): Boolean;
      function  FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;

    public
      class function UnWrap(data: Pointer): ICefCookieManager;
      class function Global(const callback: ICefCompletionCallback): ICefCookieManager;
      class function GlobalProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCompletionCallback, uCEFDeleteCookiesCallback,
  uCEFSetCookieCallback, uCEFCookieVisitor, uCEFStringList;


function TCefCookieManagerRef.DeleteCookies(const url        : ustring;
                                            const cookieName : ustring;
                                            const callback   : ICefDeleteCookiesCallback): Boolean;
var
  TempURL, TempName : TCefString;
begin
  TempURL  := CefString(url);
  TempName := CefString(cookieName);
  Result   := PCefCookieManager(FData)^.delete_cookies(PCefCookieManager(FData), @TempURL, @TempName, CefGetData(callback)) <> 0;
end;

function TCefCookieManagerRef.DeleteCookiesProc(const url        : ustring;
                                                const cookieName : ustring;
                                                const callback   : TCefDeleteCookiesCallbackProc): Boolean;
begin
  Result := DeleteCookies(url, cookieName, TCefFastDeleteCookiesCallback.Create(callback));
end;

function TCefCookieManagerRef.FlushStore(const handler: ICefCompletionCallback): Boolean;
begin
  Result := PCefCookieManager(FData)^.flush_store(PCefCookieManager(FData), CefGetData(handler)) <> 0;
end;

function TCefCookieManagerRef.FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;
begin
  Result := FlushStore(TCefFastCompletionCallback.Create(proc))
end;

class function TCefCookieManagerRef.Global(const callback: ICefCompletionCallback): ICefCookieManager;
begin
  Result := UnWrap(cef_cookie_manager_get_global_manager(CefGetData(callback)));
end;

class function TCefCookieManagerRef.GlobalProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := Global(TCefFastCompletionCallback.Create(callback));
end;

function TCefCookieManagerRef.SetCookie(const url, name, value, domain, path: ustring;
                                              secure, httponly, hasExpires: Boolean;
                                        const creation, lastAccess, expires: TDateTime;
                                        const callback: ICefSetCookieCallback): Boolean;
var
  str  : TCefString;
  cook : TCefCookie;
begin
  str              := CefString(url);
  cook.name        := CefString(name);
  cook.value       := CefString(value);
  cook.domain      := CefString(domain);
  cook.path        := CefString(path);
  cook.secure      := Ord(secure);
  cook.httponly    := Ord(httponly);
  cook.creation    := DateTimeToCefTime(creation);
  cook.last_access := DateTimeToCefTime(lastAccess);
  cook.has_expires := Ord(hasExpires);

  if hasExpires then
    cook.expires := DateTimeToCefTime(expires)
   else
    FillChar(cook.expires, SizeOf(TCefTime), 0);

  Result := PCefCookieManager(FData)^.set_cookie(PCefCookieManager(FData), @str, @cook, CefGetData(callback)) <> 0;
end;

function TCefCookieManagerRef.SetCookieProc(const url, name, value, domain, path: ustring;
                                                  secure, httponly, hasExpires: Boolean;
                                            const creation, lastAccess, expires: TDateTime;
                                            const callback: TCefSetCookieCallbackProc): Boolean;
begin
  Result := SetCookie(url, name, value, domain, path,
                      secure, httponly, hasExpires,
                      creation, lastAccess, expires,
                      TCefFastSetCookieCallback.Create(callback));
end;

procedure TCefCookieManagerRef.SetSupportedSchemes(const schemes: TStrings; include_defaults: boolean; const callback: ICefCompletionCallback);
var
  TempSL : ICefStringList;
begin
  try
    TempSL := TCefStringListOwn.Create;
    TempSL.AddStrings(schemes);

    PCefCookieManager(FData)^.set_supported_schemes(PCefCookieManager(FData),
                                                    TempSL.Handle,
                                                    ord(include_defaults),
                                                    CefGetData(callback));
  finally
    TempSL := nil;
  end;
end;

procedure TCefCookieManagerRef.SetSupportedSchemesProc(const schemes: TStrings; include_defaults: boolean; const callback: TCefCompletionCallbackProc);
begin
  SetSupportedSchemes(schemes, include_defaults, TCefFastCompletionCallback.Create(callback));
end;

class function TCefCookieManagerRef.UnWrap(data: Pointer): ICefCookieManager;
begin
  if (data <> nil) then
    Result := Create(data) as ICefCookieManager
   else
    Result := nil;
end;

function TCefCookieManagerRef.VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
begin
  Result := PCefCookieManager(FData)^.visit_all_cookies(PCefCookieManager(FData), CefGetData(visitor)) <> 0;
end;

function TCefCookieManagerRef.VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
begin
  Result := VisitAllCookies(TCefFastCookieVisitor.Create(visitor) as ICefCookieVisitor);
end;

function TCefCookieManagerRef.VisitUrlCookies(const url             : ustring;
                                                    includeHttpOnly : Boolean;
                                              const visitor         : ICefCookieVisitor): Boolean;
var
  TempURL : TCefString;
begin
  TempURL := CefString(url);
  Result  := PCefCookieManager(FData)^.visit_url_cookies(PCefCookieManager(FData), @TempURL, Ord(includeHttpOnly), CefGetData(visitor)) <> 0;
end;

function TCefCookieManagerRef.VisitUrlCookiesProc(const url             : ustring;
                                                        includeHttpOnly : Boolean;
                                                  const visitor         : TCefCookieVisitorProc): Boolean;
begin
  Result := VisitUrlCookies(url, includeHttpOnly, TCefFastCookieVisitor.Create(visitor) as ICefCookieVisitor);
end;

end.
