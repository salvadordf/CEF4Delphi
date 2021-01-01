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

unit uCEFCookieVisitor;

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
  TCefCookieVisitorOwn = class(TCefBaseRefCountedOwn, ICefCookieVisitor)
    protected
      function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; out deleteCookie: Boolean): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastCookieVisitor = class(TCefCookieVisitorOwn)
    protected
      FVisitor: TCefCookieVisitorProc;

      function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; out deleteCookie: Boolean): Boolean; override;

    public
      constructor Create(const visitor: TCefCookieVisitorProc); reintroduce;
  end;

  TCefCustomCookieVisitor = class(TCefCookieVisitorOwn)
    protected
      FEvents : Pointer;
      FID     : integer;

      function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; same_site : TCefCookieSameSite; priority : TCefCookiePriority; out deleteCookie: Boolean): Boolean; override;

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

function cef_cookie_visitor_visit(      self         : PCefCookieVisitor;
                                  const cookie       : PCefCookie;
                                        count        : Integer;
                                        total        : Integer;
                                        deleteCookie : PInteger): Integer; stdcall;
var
  delete     : Boolean;
  exp        : TDateTime;
  TempObject : TObject;
begin
  delete     := False;
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (cookie^.has_expires <> 0) then
    exp := CefTimeToDateTime(cookie^.expires)
   else
    exp := 0;

  if (TempObject <> nil) and (TempObject is TCefCookieVisitorOwn) then
    Result := Ord(TCefCookieVisitorOwn(TempObject).visit(CefString(@cookie^.name),
                                                         CefString(@cookie^.value),
                                                         CefString(@cookie^.domain),
                                                         CefString(@cookie^.path),
                                                         Boolean(cookie^.secure),
                                                         Boolean(cookie^.httponly),
                                                         Boolean(cookie^.has_expires),
                                                         CefTimeToDateTime(cookie^.creation),
                                                         CefTimeToDateTime(cookie^.last_access),
                                                         exp,
                                                         count,
                                                         total,
                                                         cookie^.same_site,
                                                         cookie^.priority,
                                                         delete));

  deleteCookie^ := Ord(delete);
end;

// TCefCookieVisitorOwn

constructor TCefCookieVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCookieVisitor));

  PCefCookieVisitor(FData)^.visit := {$IFDEF FPC}@{$ENDIF}cef_cookie_visitor_visit;
end;

function TCefCookieVisitorOwn.visit(const name, value, domain, path: ustring;
                                    secure, httponly, hasExpires: Boolean;
                                    const creation, lastAccess, expires: TDateTime;
                                    count, total: Integer;
                                    same_site : TCefCookieSameSite;
                                    priority : TCefCookiePriority;
                                    out deleteCookie: Boolean): Boolean;
begin
  Result := True;
end;

// TCefFastCookieVisitor

constructor TCefFastCookieVisitor.Create(const visitor: TCefCookieVisitorProc);
begin
  inherited Create;

  FVisitor := visitor;
end;

function TCefFastCookieVisitor.visit(const name, value, domain, path: ustring;
                                     secure, httponly, hasExpires: Boolean;
                                     const creation, lastAccess, expires: TDateTime;
                                     count, total: Integer;
                                     same_site : TCefCookieSameSite;
                                     priority : TCefCookiePriority;
                                     out deleteCookie: Boolean): Boolean;
begin
  Result := FVisitor(name, value, domain, path, secure, httponly, hasExpires,
                     creation, lastAccess, expires, count, total, same_site,
                     priority, deleteCookie);
end;


// TCefCustomCookieVisitor

constructor TCefCustomCookieVisitor.Create(const aEvents : IChromiumEvents; aID : integer);
begin
  inherited Create;

  FEvents := Pointer(aEvents);
  FID     := aID;
end;

destructor TCefCustomCookieVisitor.Destroy;
begin
  try
    try
      if (FEvents <> nil) then
        IChromiumEvents(FEvents).doOnCookieVisitorDestroyed(FID);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomCookieVisitor.Destroy', e) then raise;
    end;
  finally
    FEvents := nil;
    inherited Destroy;
  end;
end;

function TCefCustomCookieVisitor.visit(const name, value, domain, path: ustring;
                                       secure, httponly, hasExpires: Boolean;
                                       const creation, lastAccess, expires: TDateTime;
                                       count, total: Integer;
                                       same_site : TCefCookieSameSite;
                                       priority : TCefCookiePriority;
                                       out deleteCookie: Boolean): Boolean;
var
  TempDelete : boolean;
begin
  Result     := True;
  TempDelete := False;

  try
    try
      if (FEvents <> nil) then
        IChromiumEvents(FEvents).doOnCookiesVisited(name, value, domain, path, secure, httponly, hasExpires,
                                                    creation, lastAccess, expires, count, total, FID,
                                                    same_site, priority, TempDelete, Result);
    except
      on e : exception do
        if CustomExceptionHandler('TCefCustomCookieVisitor.visit', e) then raise;
    end;
  finally
    deleteCookie := TempDelete;
  end;
end;

end.
