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
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

unit uCEFUrlrequestClient;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefUrlrequestClientOwn = class(TCefBaseRefCountedOwn, ICefUrlrequestClient)
  protected
    procedure OnRequestComplete(const request: ICefUrlRequest); virtual;
    procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64); virtual;
    procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64); virtual;
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt); virtual;
    function  OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean; virtual;
  public
    constructor Create; virtual;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFUrlRequest, uCEFAuthCallback;


procedure cef_url_request_client_on_request_complete(self    : PCefUrlRequestClient;
                                                     request : PCefUrlRequest); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnRequestComplete(TCefUrlRequestRef.UnWrap(request));
end;

procedure cef_url_request_client_on_upload_progress(self    : PCefUrlRequestClient;
                                                    request : PCefUrlRequest;
                                                    current : Int64;
                                                    total   : Int64); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnUploadProgress(TCefUrlRequestRef.UnWrap(request),
                                                         current,
                                                         total);
end;

procedure cef_url_request_client_on_download_progress(self    : PCefUrlRequestClient;
                                                      request : PCefUrlRequest;
                                                      current : Int64;
                                                      total   : Int64); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnDownloadProgress(TCefUrlRequestRef.UnWrap(request),
                                                           current,
                                                           total);
end;

procedure cef_url_request_client_on_download_data(      self        : PCefUrlRequestClient;
                                                        request     : PCefUrlRequest;
                                                  const data        : Pointer;
                                                        data_length : NativeUInt); stdcall;
var
  TempObject  : TObject;
begin
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    TCefUrlrequestClientOwn(TempObject).OnDownloadData(TCefUrlRequestRef.UnWrap(request),
                                                       data,
                                                       data_length);
end;

function cef_url_request_client_get_auth_credentials(      self     : PCefUrlRequestClient;
                                                           isProxy  : Integer;
                                                     const host     : PCefString;
                                                           port     : Integer;
                                                     const realm    : PCefString;
                                                     const scheme   : PCefString;
                                                           callback : PCefAuthCallback): Integer; stdcall;
var
  TempObject  : TObject;
begin
  Result      := Ord(False);
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefUrlrequestClientOwn) then
    Result := Ord(TCefUrlrequestClientOwn(TempObject).OnGetAuthCredentials(isProxy <> 0,
                                                                           CefString(host),
                                                                           port,
                                                                           CefString(realm),
                                                                           CefString(scheme),
                                                                           TCefAuthCallbackRef.UnWrap(callback)));
end;


constructor TCefUrlrequestClientOwn.Create;
begin
  inherited CreateData(SizeOf(TCefUrlrequestClient));

  with PCefUrlrequestClient(FData)^ do
    begin
      on_request_complete  := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_request_complete;
      on_upload_progress   := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_upload_progress;
      on_download_progress := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_download_progress;
      on_download_data     := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_on_download_data;
      get_auth_credentials := {$IFDEF FPC}@{$ENDIF}cef_url_request_client_get_auth_credentials;
    end;
end;

procedure TCefUrlrequestClientOwn.OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
begin
  //
end;

procedure TCefUrlrequestClientOwn.OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  //
end;

function TCefUrlrequestClientOwn.OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
begin
  Result := False;
end;

procedure TCefUrlrequestClientOwn.OnRequestComplete(const request: ICefUrlRequest);
begin
  //
end;

procedure TCefUrlrequestClientOwn.OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
begin
  //
end;

end.
