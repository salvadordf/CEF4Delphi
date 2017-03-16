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

unit uCEFResourceHandler;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefResourceHandlerOwn = class(TCefBaseRefCountedOwn, ICefResourceHandler)
    protected
      function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean; virtual;
      procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring); virtual;
      function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean; virtual;
      function  CanGetCookie(const cookie: PCefCookie): Boolean; virtual;
      function  CanSetCookie(const cookie: PCefCookie): Boolean; virtual;
      procedure Cancel; virtual;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest); virtual;
  end;

  TCefResourceHandlerClass = class of TCefResourceHandlerOwn;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCallback, uCEFRequest, uCEFResponse;

function cef_resource_handler_process_request(self: PCefResourceHandler; request: PCefRequest; callback: PCefCallback): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(CefGetObject(self)) do
    Result := Ord(ProcessRequest(TCefRequestRef.UnWrap(request), TCefCallbackRef.UnWrap(callback)));
end;

procedure cef_resource_handler_get_response_headers(self: PCefResourceHandler; response: PCefResponse; response_length: PInt64; redirectUrl: PCefString); stdcall;
var
  ru: ustring;
begin
  ru := '';

  with TCefResourceHandlerOwn(CefGetObject(self)) do
    GetResponseHeaders(TCefResponseRef.UnWrap(response), response_length^, ru);

  if ru <> '' then CefStringSet(redirectUrl, ru);
end;

function cef_resource_handler_read_response(self: PCefResourceHandler; data_out: Pointer; bytes_to_read: Integer; bytes_read: PInteger; callback: PCefCallback): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(CefGetObject(self)) do
    Result := Ord(ReadResponse(data_out, bytes_to_read, bytes_read^, TCefCallbackRef.UnWrap(callback)));
end;

function cef_resource_handler_can_get_cookie(self: PCefResourceHandler; const cookie: PCefCookie): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(CefGetObject(self)) do Result := Ord(CanGetCookie(cookie));
end;

function cef_resource_handler_can_set_cookie(self: PCefResourceHandler; const cookie: PCefCookie): Integer; stdcall;
begin
  with TCefResourceHandlerOwn(CefGetObject(self)) do Result := Ord(CanSetCookie(cookie));
end;

procedure cef_resource_handler_cancel(self: PCefResourceHandler); stdcall;
begin
  with TCefResourceHandlerOwn(CefGetObject(self)) do Cancel;
end;

procedure TCefResourceHandlerOwn.Cancel;
begin

end;

function TCefResourceHandlerOwn.CanGetCookie(const cookie: PCefCookie): Boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.CanSetCookie(const cookie: PCefCookie): Boolean;
begin
  Result := False;
end;

constructor TCefResourceHandlerOwn.Create(const browser    : ICefBrowser;
                                          const frame      : ICefFrame;
                                          const schemeName : ustring;
                                          const request    : ICefRequest);
begin
  inherited CreateData(SizeOf(TCefResourceHandler));

  with PCefResourceHandler(FData)^ do
    begin
      process_request      := cef_resource_handler_process_request;
      get_response_headers := cef_resource_handler_get_response_headers;
      read_response        := cef_resource_handler_read_response;
      can_get_cookie       := cef_resource_handler_can_get_cookie;
      can_set_cookie       := cef_resource_handler_can_set_cookie;
      cancel               := cef_resource_handler_cancel;
    end;
end;

procedure TCefResourceHandlerOwn.GetResponseHeaders(const response       : ICefResponse;
                                                    out   responseLength : Int64;
                                                    out   redirectUrl    : ustring);
begin

end;

function TCefResourceHandlerOwn.ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.ReadResponse(const dataOut     : Pointer;
                                                   bytesToRead : Integer;
                                             var   bytesRead   : Integer;
                                             const callback    : ICefCallback): Boolean;
begin
  Result := False;
end;

end.
