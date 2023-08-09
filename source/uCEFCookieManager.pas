unit uCEFCookieManager;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFCompletionCallback;

type
  TCefCookieManagerRef = class(TCefBaseRefCountedRef, ICefCookieManager)
    protected
      function  VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
      function  VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
      function  VisitUrlCookies(const url: ustring; includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
      function  VisitUrlCookiesProc(const url: ustring; includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
      function  SetCookie(const url, name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; const callback: ICefSetCookieCallback): Boolean;
      function  SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; same_site : TCefCookieSameSite; priority : TCefCookiePriority; const callback: TCefSetCookieCallbackProc): Boolean;
      function  DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
      function  DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
      function  FlushStore(const callback: ICefCompletionCallback): Boolean;
      function  FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;

    public
      class function UnWrap(data: Pointer): ICefCookieManager;
      class function Global(const callback: ICefCompletionCallback): ICefCookieManager;
      class function GlobalProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
  end;

  TCefFlushStoreCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFDeleteCookiesCallback,
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

function TCefCookieManagerRef.FlushStore(const callback: ICefCompletionCallback): Boolean;
begin
  Result := PCefCookieManager(FData)^.flush_store(PCefCookieManager(FData), CefGetData(callback)) <> 0;
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
                                              same_site : TCefCookieSameSite;
                                              priority : TCefCookiePriority;
                                        const callback: ICefSetCookieCallback): Boolean;
var
  TempURL    : TCefString;
  TempCookie : TCefCookie;
begin
  TempURL                := CefString(url);
  TempCookie.name        := CefString(name);
  TempCookie.value       := CefString(value);
  TempCookie.domain      := CefString(domain);
  TempCookie.path        := CefString(path);
  TempCookie.secure      := Ord(secure);
  TempCookie.httponly    := Ord(httponly);
  TempCookie.creation    := DateTimeToCefBaseTime(creation);
  TempCookie.last_access := DateTimeToCefBaseTime(lastAccess);
  TempCookie.has_expires := Ord(hasExpires);
  TempCookie.same_site   := same_site;
  TempCookie.priority    := priority;

  if hasExpires then
    TempCookie.expires := DateTimeToCefBaseTime(expires)
   else
    TempCookie.expires := 0;

  Result := PCefCookieManager(FData)^.set_cookie(PCefCookieManager(FData), @TempURL, @TempCookie, CefGetData(callback)) <> 0;
end;

function TCefCookieManagerRef.SetCookieProc(const url, name, value, domain, path: ustring;
                                                  secure, httponly, hasExpires: Boolean;
                                            const creation, lastAccess, expires: TDateTime;
                                                  same_site : TCefCookieSameSite;
                                                  priority : TCefCookiePriority;
                                            const callback: TCefSetCookieCallbackProc): Boolean;
begin
  Result := SetCookie(url, name, value, domain, path,
                      secure, httponly, hasExpires,
                      creation, lastAccess, expires,
                      same_site, priority,
                      TCefFastSetCookieCallback.Create(callback));
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


// TCefFlushStoreCompletionCallback

procedure TCefFlushStoreCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doOnCookiesStoreFlushed;
    except
      on e : exception do
        if CustomExceptionHandler('TCefFlushStoreCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
