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
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefCookieVisitorOwn = class(TCefBaseRefCountedOwn, ICefCookieVisitor)
    protected
      function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; out deleteCookie: Boolean): Boolean; virtual;

    public
      constructor Create; virtual;
  end;

  TCefFastCookieVisitor = class(TCefCookieVisitorOwn)
    protected
      FVisitor: TCefCookieVisitorProc;

      function visit(const name, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total: Integer; out deleteCookie: Boolean): Boolean; override;

    public
      constructor Create(const visitor: TCefCookieVisitorProc); reintroduce;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions;

function cef_cookie_visitor_visit(self: PCefCookieVisitor;
                                  const cookie: PCefCookie;
                                  count, total: Integer;
                                  deleteCookie: PInteger): Integer; stdcall;
var
  delete     : Boolean;
  exp        : TDateTime;
  TempObject : TObject;
begin
  delete     := False;
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (cookie.has_expires <> 0) then
    exp := CefTimeToDateTime(cookie.expires)
   else
    exp := 0;

  if (TempObject <> nil) and (TempObject is TCefCookieVisitorOwn) then
    Result := Ord(TCefCookieVisitorOwn(TempObject).visit(CefString(@cookie.name),
                                                         CefString(@cookie.value),
                                                         CefString(@cookie.domain),
                                                         CefString(@cookie.path),
                                                         Boolean(cookie.secure),
                                                         Boolean(cookie.httponly),
                                                         Boolean(cookie.has_expires),
                                                         CefTimeToDateTime(cookie.creation),
                                                         CefTimeToDateTime(cookie.last_access),
                                                         exp,
                                                         count,
                                                         total,
                                                         delete));

  deleteCookie^ := Ord(delete);
end;

// TCefCookieVisitorOwn

constructor TCefCookieVisitorOwn.Create;
begin
  inherited CreateData(SizeOf(TCefCookieVisitor));

  PCefCookieVisitor(FData)^.visit := cef_cookie_visitor_visit;
end;

function TCefCookieVisitorOwn.visit(const name, value, domain, path: ustring;
                                    secure, httponly, hasExpires: Boolean;
                                    const creation, lastAccess, expires: TDateTime;
                                    count, total: Integer;
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
                                     out deleteCookie: Boolean): Boolean;
begin
  Result := FVisitor(name, value, domain, path, secure, httponly, hasExpires,
                     creation, lastAccess, expires, count, total, deleteCookie);
end;



end.
