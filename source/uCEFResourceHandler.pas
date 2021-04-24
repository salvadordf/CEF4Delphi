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

unit uCEFResourceHandler;

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
  TCefResourceHandlerRef = class(TCefBaseRefCountedRef, ICefResourceHandler)
    protected
      function  open(const request: ICefRequest; var handle_request: boolean; const callback: ICefCallback): boolean;
      function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean;  // deprecated
      procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring);
      function  skip(bytes_to_skip: int64; var bytes_skipped: Int64; const callback: ICefResourceSkipCallback): boolean;
      function  read(const data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer; const callback: ICefResourceReadCallback): boolean;
      function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean;  // deprecated
      procedure Cancel;

    public
      class function UnWrap(data: Pointer): ICefResourceHandler;
  end;

  TCefResourceHandlerOwn = class(TCefBaseRefCountedOwn, ICefResourceHandler)
    protected
      function  open(const request: ICefRequest; var handle_request: boolean; const callback: ICefCallback): boolean; virtual;
      function  ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean; virtual; // deprecated
      procedure GetResponseHeaders(const response: ICefResponse; out responseLength: Int64; out redirectUrl: ustring); virtual;
      function  skip(bytes_to_skip: int64; var bytes_skipped: Int64; const callback: ICefResourceSkipCallback): boolean; virtual;
      function  read(const data_out: Pointer; bytes_to_read: Integer; var bytes_read: Integer; const callback: ICefResourceReadCallback): boolean; virtual;
      function  ReadResponse(const dataOut: Pointer; bytesToRead: Integer; var bytesRead: Integer; const callback: ICefCallback): Boolean; virtual; // deprecated
      procedure Cancel; virtual;

    public
      constructor Create(const browser: ICefBrowser; const frame: ICefFrame; const schemeName: ustring; const request: ICefRequest); virtual;
  end;

  TCefResourceHandlerClass = class of TCefResourceHandlerOwn;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFCallback, uCEFRequest, uCEFResponse,
  uCEFResourceSkipCallback, uCEFResourceReadCallback;


// TCefResourceHandlerOwn

function cef_resource_handler_open(self           : PCefResourceHandler;
                                   request        : PCefRequest;
                                   handle_request : PInteger;
                                   callback       : PCefCallback): Integer; stdcall;
var
  TempObject : TObject;
  TempHandleRequest : Boolean;
begin
  Result            := Ord(False);
  TempObject        := CefGetObject(self);
  TempHandleRequest := False;

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    Result := Ord(TCefResourceHandlerOwn(TempObject).Open(TCefRequestRef.UnWrap(request),
                                                          TempHandleRequest,
                                                          TCefCallbackRef.UnWrap(callback)));

  handle_request^ := Ord(TempHandleRequest);
end;

function cef_resource_handler_process_request(self     : PCefResourceHandler;
                                              request  : PCefRequest;
                                              callback : PCefCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    Result := Ord(TCefResourceHandlerOwn(TempObject).ProcessRequest(TCefRequestRef.UnWrap(request),
                                                                    TCefCallbackRef.UnWrap(callback)));
end;

procedure cef_resource_handler_get_response_headers(self            : PCefResourceHandler;
                                                    response        : PCefResponse;
                                                    response_length : PInt64;
                                                    redirectUrl     : PCefString); stdcall;
var
  TempRedirect : ustring;
  TempObject   : TObject;
begin
  TempRedirect := '';
  TempObject   := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    begin
      TCefResourceHandlerOwn(TempObject).GetResponseHeaders(TCefResponseRef.UnWrap(response),
                                                            response_length^,
                                                            TempRedirect);
      CefStringFree(redirectUrl);
      if (redirectUrl <> nil) then redirectUrl^ := CefStringAlloc(TempRedirect);
    end;
end;

function cef_resource_handler_skip(self          : PCefResourceHandler;
                                   bytes_to_skip : int64;
                                   bytes_skipped : PInt64;
                                   callback      : PCefResourceSkipCallback): Integer; stdcall;
var
  TempObject       : TObject;
  TempBytesSkipped : int64;
begin
  Result           := Ord(False);
  TempObject       := CefGetObject(self);
  TempBytesSkipped := bytes_skipped^;

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    Result := Ord(TCefResourceHandlerOwn(TempObject).Skip(bytes_to_skip,
                                                          TempBytesSkipped,
                                                          TCefResourceSkipCallbackRef.UnWrap(callback)));
  bytes_skipped^ := TempBytesSkipped;
end;

function cef_resource_handler_read(self          : PCefResourceHandler;
                                   data_out      : Pointer;
                                   bytes_to_read : Integer;
                                   bytes_read    : PInteger;
                                   callback      : PCefResourceReadCallback): Integer; stdcall;
var

  TempObject    : TObject;
  TempBytesRead : integer;
begin
  Result        := Ord(False);
  TempObject    := CefGetObject(self);
  TempBytesRead := bytes_read^;

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    Result := Ord(TCefResourceHandlerOwn(TempObject).Read(data_out,
                                                          bytes_to_read,
                                                          TempBytesRead,
                                                          TCefResourceReadCallbackRef.UnWrap(callback)));


  bytes_read^ := TempBytesRead;

end;

function cef_resource_handler_read_response(self          : PCefResourceHandler;
                                            data_out      : Pointer;
                                            bytes_to_read : Integer;
                                            bytes_read    : PInteger;
                                            callback      : PCefCallback): Integer; stdcall;
var
  TempObject    : TObject;
  TempBytesRead : integer;
begin
  Result        := Ord(False);
  TempObject    := CefGetObject(self);
  TempBytesRead := bytes_read^;

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    Result := Ord(TCefResourceHandlerOwn(TempObject).ReadResponse(data_out,
                                                                  bytes_to_read,
                                                                  TempBytesRead,
                                                                  TCefCallbackRef.UnWrap(callback)));

  bytes_read^ := TempBytesRead;
end;

procedure cef_resource_handler_cancel(self: PCefResourceHandler); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefResourceHandlerOwn) then
    TCefResourceHandlerOwn(TempObject).Cancel;
end;

procedure TCefResourceHandlerOwn.Cancel;
begin
  //
end;

constructor TCefResourceHandlerOwn.Create(const browser    : ICefBrowser;
                                          const frame      : ICefFrame;
                                          const schemeName : ustring;
                                          const request    : ICefRequest);
begin
  inherited CreateData(SizeOf(TCefResourceHandler));

  with PCefResourceHandler(FData)^ do
    begin
      open                 := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_open;
      process_request      := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_process_request;
      get_response_headers := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_get_response_headers;
      skip                 := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_skip;
      read                 := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_read;
      read_response        := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_read_response;
      cancel               := {$IFDEF FPC}@{$ENDIF}cef_resource_handler_cancel;
    end;
end;

procedure TCefResourceHandlerOwn.GetResponseHeaders(const response       : ICefResponse;
                                                    out   responseLength : Int64;
                                                    out   redirectUrl    : ustring);
begin
  //
end;

function TCefResourceHandlerOwn.open(const request        : ICefRequest;
                                     var   handle_request : boolean;
                                     const callback       : ICefCallback): boolean;
begin
  Result         := False;
  handle_request := False;
end;

function TCefResourceHandlerOwn.skip(      bytes_to_skip : int64;
                                     var   bytes_skipped : Int64;
                                     const callback      : ICefResourceSkipCallback): boolean;
begin
  Result := False;
end;

function TCefResourceHandlerOwn.read(const data_out      : Pointer;
                                           bytes_to_read : Integer;
                                     var   bytes_read    : Integer;
                                     const callback      : ICefResourceReadCallback): boolean;
begin
  bytes_read := -1;
  Result     := False;
end;

function TCefResourceHandlerOwn.ProcessRequest(const request  : ICefRequest;
                                               const callback : ICefCallback): Boolean;
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


// TCefResourceHandlerRef

class function TCefResourceHandlerRef.UnWrap(data: Pointer): ICefResourceHandler;
begin
  if (data <> nil) then
    Result := Create(data) as ICefResourceHandler
   else
    Result := nil;
end;

function TCefResourceHandlerRef.open(const request        : ICefRequest;
                                     var   handle_request : boolean;
                                     const callback       : ICefCallback): boolean;
var
  TempHandleRequest : integer;
begin
  TempHandleRequest := Ord(False);
  Result            := PCefResourceHandler(FData)^.open(PCefResourceHandler(FData),
                                                        CefGetData(request),
                                                        @TempHandleRequest,
                                                        CefGetData(callback)) <> 0;
  handle_request    := TempHandleRequest <> 0;
end;

function TCefResourceHandlerRef.ProcessRequest(const request  : ICefRequest;
                                               const callback : ICefCallback): boolean;
begin
  Result := PCefResourceHandler(FData)^.process_request(PCefResourceHandler(FData),
                                                        CefGetData(request),
                                                        CefGetData(callback)) <> 0;
end;

procedure TCefResourceHandlerRef.GetResponseHeaders(const response       : ICefResponse;
                                                    out   responseLength : Int64;
                                                    out   redirectUrl    : ustring);
var
  TempRedirectURL : TCefString;
begin
  TempRedirectURL := CefString(redirectUrl);
  PCefResourceHandler(FData)^.get_response_headers(PCefResourceHandler(FData),
                                                   CefGetData(response),
                                                   @responseLength,
                                                   @TempRedirectURL);
  redirectUrl := CefString(@TempRedirectURL);
end;

function TCefResourceHandlerRef.skip(      bytes_to_skip : int64;
                                     var   bytes_skipped : Int64;
                                     const callback      : ICefResourceSkipCallback): boolean;
begin
  Result := PCefResourceHandler(FData)^.skip(PCefResourceHandler(FData),
                                             bytes_to_skip,
                                             @bytes_skipped,
                                             CefGetData(callback)) <> 0;
end;

function TCefResourceHandlerRef.read(const data_out      : Pointer;
                                           bytes_to_read : Integer;
                                     var   bytes_read    : Integer;
                                     const callback      : ICefResourceReadCallback): boolean;
begin
  Result := PCefResourceHandler(FData)^.read(PCefResourceHandler(FData),
                                             data_out,
                                             bytes_to_read,
                                             @bytes_read,
                                             CefGetData(callback)) <> 0;
end;

function TCefResourceHandlerRef.ReadResponse(const dataOut     : Pointer;
                                                   bytesToRead : Integer;
                                             var   bytesRead   : Integer;
                                             const callback    : ICefCallback): boolean;
begin
  Result := PCefResourceHandler(FData)^.read_response(PCefResourceHandler(FData),
                                                      dataOut,
                                                      bytesToRead,
                                                      @bytesRead,
                                                      CefGetData(callback)) <> 0;
end;

procedure TCefResourceHandlerRef.Cancel;
begin
  PCefResourceHandler(FData)^.Cancel(PCefResourceHandler(FData));
end;


end.
