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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

 unit uCEFMediaAccessHandler;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefMediaAccessHandlerOwn = class(TCefBaseRefCountedOwn, ICefMediaAccessHandler)
    protected
      function  OnRequestMediaAccessPermission(const browser: ICefBrowser; const frame: ICefFrame; const requesting_url: ustring; requested_permissions: TCefMediaAccessPermissionTypes; const callback: ICefMediaAccessCallback): boolean;

      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFBrowser, uCEFFrame, uCEFMediaAccessCallback;

function cef_media_access_handler_on_request_media_access_permission(      self                  : PCefMediaAccessHandler;
                                                                           browser               : PCefBrowser;
                                                                           frame                 : PCefFrame;
                                                                     const requesting_url        : PCefString;
                                                                           requested_permissions : integer;
                                                                           callback              : PCefMediaAccessCallback): integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefMediaAccessHandlerOwn) then
    Result := Ord(TCefMediaAccessHandlerOwn(TempObject).OnRequestMediaAccessPermission(TCefBrowserRef.UnWrap(browser),
                                                                                       TCefFrameRef.UnWrap(frame),
                                                                                       CefString(requesting_url),
                                                                                       requested_permissions,
                                                                                       TCefMediaAccessCallbackRef.UnWrap(callback)));
end;

constructor TCefMediaAccessHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefMediaAccessHandler));

  with PCefMediaAccessHandler(FData)^ do
    on_request_media_access_permission := {$IFDEF FPC}@{$ENDIF}cef_media_access_handler_on_request_media_access_permission;
end;

function TCefMediaAccessHandlerOwn.OnRequestMediaAccessPermission(const browser               : ICefBrowser;
                                                                  const frame                 : ICefFrame;
                                                                  const requesting_url        : ustring;
                                                                        requested_permissions : TCefMediaAccessPermissionTypes;
                                                                  const callback              : ICefMediaAccessCallback): boolean;
begin
  Result := False;
end;

procedure TCefMediaAccessHandlerOwn.RemoveReferences;
begin
  //
end;

end.
