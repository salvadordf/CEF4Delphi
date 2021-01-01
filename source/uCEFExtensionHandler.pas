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

unit uCEFExtensionHandler;

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
  TCefExtensionHandlerRef = class(TCefBaseRefCountedRef, ICefExtensionHandler)
    protected
      procedure OnExtensionLoadFailed(result: TCefErrorcode);
      procedure OnExtensionLoaded(const extension: ICefExtension);
      procedure OnExtensionUnloaded(const extension: ICefExtension);
      function  OnBeforeBackgroundBrowser(const extension: ICefExtension; const url: ustring; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
      function  OnBeforeBrowser(const extension: ICefExtension; const browser, active_browser: ICefBrowser; index: Integer; const url: ustring; active: boolean; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings) : boolean;
      procedure GetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; var aRsltBrowser: ICefBrowser);
      function  CanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean;
      function  GetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean;
      procedure RemoveReferences;

    public
      class function UnWrap(data: Pointer): ICefExtensionHandler;
  end;

  TCefExtensionHandlerOwn = class(TCefBaseRefCountedOwn, ICefExtensionHandler)
    protected
      procedure OnExtensionLoadFailed(result: TCefErrorcode); virtual;
      procedure OnExtensionLoaded(const extension: ICefExtension); virtual;
      procedure OnExtensionUnloaded(const extension: ICefExtension); virtual;
      function  OnBeforeBackgroundBrowser(const extension: ICefExtension; const url: ustring; var client: ICefClient; var settings: TCefBrowserSettings) : boolean; virtual;
      function  OnBeforeBrowser(const extension: ICefExtension; const browser, active_browser: ICefBrowser; index: Integer; const url: ustring; active: boolean; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings) : boolean; virtual;
      procedure GetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; var aRsltBrowser: ICefBrowser); virtual;
      function  CanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean; virtual;
      function  GetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean; virtual;
      procedure RemoveReferences; virtual;

    public
      constructor Create; virtual;
  end;

  TCustomExtensionHandler = class(TCefExtensionHandlerOwn)
    protected
      FEvents : Pointer;

      procedure OnExtensionLoadFailed(result: TCefErrorcode); override;
      procedure OnExtensionLoaded(const extension: ICefExtension); override;
      procedure OnExtensionUnloaded(const extension: ICefExtension); override;
      function  OnBeforeBackgroundBrowser(const extension: ICefExtension; const url: ustring; var client: ICefClient; var settings: TCefBrowserSettings) : boolean; override;
      function  OnBeforeBrowser(const extension: ICefExtension; const browser, active_browser: ICefBrowser; index: Integer; const url: ustring; active: boolean; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings) : boolean; override;
      procedure GetActiveBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; var aRsltBrowser: ICefBrowser); override;
      function  CanAccessBrowser(const extension: ICefExtension; const browser: ICefBrowser; include_incognito: boolean; const target_browser: ICefBrowser): boolean; override;
      function  GetExtensionResource(const extension: ICefExtension; const browser: ICefBrowser; const file_: ustring; const callback: ICefGetExtensionResourceCallback): boolean; override;
      procedure RemoveReferences; override;

    public
      constructor Create(const events: IChromiumEvents); reintroduce;
      destructor  Destroy; override;
  end;

implementation

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFMiscFunctions, uCEFLibFunctions, uCEFGetExtensionResourceCallback,
  uCEFExtension, uCEFBrowser, uCEFClient;


// ***************************************************************************
// ************************ TCefExtensionHandlerRef **************************
// ***************************************************************************

procedure TCefExtensionHandlerRef.OnExtensionLoadFailed(result: TCefErrorcode);
begin
  //
end;

procedure TCefExtensionHandlerRef.OnExtensionLoaded(const extension: ICefExtension);
begin
  //
end;

procedure TCefExtensionHandlerRef.OnExtensionUnloaded(const extension: ICefExtension);
begin
  //
end;

function TCefExtensionHandlerRef.OnBeforeBackgroundBrowser(const extension : ICefExtension;
                                                           const url       : ustring;
                                                           var   client    : ICefClient;
                                                           var   settings  : TCefBrowserSettings) : boolean;
begin
  Result := False;
end;

function TCefExtensionHandlerRef.OnBeforeBrowser(const extension      : ICefExtension;
                                                 const browser        : ICefBrowser;
                                                 const active_browser : ICefBrowser;
                                                       index          : Integer;
                                                 const url            : ustring;
                                                       active         : boolean;
                                                 var   windowInfo     : TCefWindowInfo;
                                                 var   client         : ICefClient;
                                                 var   settings       : TCefBrowserSettings) : boolean;
begin
  Result := False;
end;

procedure TCefExtensionHandlerRef.GetActiveBrowser(const extension         : ICefExtension;
                                                   const browser           : ICefBrowser;
                                                         include_incognito : boolean;
                                                   var   aRsltBrowser      : ICefBrowser);
begin
  aRsltBrowser := nil;
end;

function TCefExtensionHandlerRef.CanAccessBrowser(const extension         : ICefExtension;
                                                  const browser           : ICefBrowser;
                                                        include_incognito : boolean;
                                                  const target_browser    : ICefBrowser): boolean;
begin
  Result := True;
end;

function TCefExtensionHandlerRef.GetExtensionResource(const extension : ICefExtension;
                                                      const browser   : ICefBrowser;
                                                      const file_     : ustring;
                                                      const callback  : ICefGetExtensionResourceCallback): boolean;
begin
  Result := False;
end;

procedure TCefExtensionHandlerRef.RemoveReferences;
begin
  //
end;

class function TCefExtensionHandlerRef.UnWrap(data: Pointer): ICefExtensionHandler;
begin
  if (data <> nil) then
    Result := Create(data) as ICefExtensionHandler
   else
    Result := nil;
end;


// ***************************************************************************
// ************************ TCefExtensionHandlerOwn **************************
// ***************************************************************************

procedure cef_extension_handler_on_extension_load_failed(self   : PCefExtensionHandler;
                                                         result : TCefErrorcode); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
    TCefExtensionHandlerOwn(TempObject).OnExtensionLoadFailed(result);
end;

procedure cef_extension_handler_on_extension_loaded(self      : PCefExtensionHandler;
                                                    extension : PCefExtension); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
    TCefExtensionHandlerOwn(TempObject).OnExtensionLoaded(TCefExtensionRef.UnWrap(extension));
end;

procedure cef_extension_handler_on_extension_unloaded(self      : PCefExtensionHandler;
                                                      extension : PCefExtension); stdcall;
var
  TempObject : TObject;
begin
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
    TCefExtensionHandlerOwn(TempObject).OnExtensionUnloaded(TCefExtensionRef.UnWrap(extension));
end;

function cef_extension_handler_on_before_background_browser(      self      : PCefExtensionHandler;
                                                                  extension : PCefExtension;
                                                            const url       : PCefString;
                                                            var   client    : PCefClient;
                                                                  settings  : PCefBrowserSettings) : Integer; stdcall;
var
  TempClient : ICefClient;
  TempObject : TObject;
begin
  try
    Result     := Ord(True);
    TempObject := CefGetObject(self);
    TempClient := TCefClientRef.UnWrap(client);


    if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
      Result := Ord(TCefExtensionHandlerOwn(TempObject).OnBeforeBackgroundBrowser(TCefExtensionRef.UnWrap(extension),
                                                                                  CefString(url),
                                                                                  TempClient,
                                                                                  settings^));

    if (TempClient = nil) then
      client := nil
     else
      if not(TempClient.SameAs(client)) then
        client := TempClient.Wrap;
  finally
    TempClient := nil;
  end;
end;

function cef_extension_handler_on_before_browser(      self           : PCefExtensionHandler;
                                                       extension      : PCefExtension;
                                                       browser        : PCefBrowser;
                                                       active_browser : PCefBrowser;
                                                       index          : Integer;
                                                 const url            : PCefString;
                                                       active         : Integer;
                                                       windowInfo     : PCefWindowInfo;
                                                 var   client         : PCefClient;
                                                       settings       : PCefBrowserSettings) : Integer; stdcall;
var
  TempClient : ICefClient;
  TempObject : TObject;
begin
  try
    Result     := Ord(True);
    TempObject := CefGetObject(self);
    TempClient := TCefClientRef.UnWrap(client);

    if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
      Result := Ord(TCefExtensionHandlerOwn(TempObject).OnBeforeBrowser(TCefExtensionRef.UnWrap(extension),
                                                                        TCefBrowserRef.UnWrap(browser),
                                                                        TCefBrowserRef.UnWrap(active_browser),
                                                                        index,
                                                                        CefString(url),
                                                                        active <> 0,
                                                                        windowInfo^,
                                                                        TempClient,
                                                                        settings^));

    if (TempClient = nil) then
      client := nil
     else
      if not(TempClient.SameAs(client)) then
        client := TempClient.Wrap;
  finally
    TempClient := nil;
  end;
end;

function cef_extension_handler_get_active_browser(self              : PCefExtensionHandler;
                                                  extension         : PCefExtension;
                                                  browser           : PCefBrowser;
                                                  include_incognito : Integer): PCefBrowser; stdcall;
var
  TempObject  : TObject;
  TempBrowser : ICefBrowser;
begin
  Result      := nil;
  TempBrowser := nil;
  TempObject  := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
    try
      TCefExtensionHandlerOwn(TempObject).GetActiveBrowser(TCefExtensionRef.UnWrap(extension),
                                                           TCefBrowserRef.UnWrap(browser),
                                                           include_incognito <> 0,
                                                           TempBrowser);

      if (TempBrowser <> nil) {$IFDEF DELPHI16_UP}and (TempBrowser is TCefBrowserRef){$ENDIF} then
        Result := TCefBrowserRef(TempBrowser).wrap;
    finally
      TempBrowser := nil;
    end;
end;

function cef_extension_handler_can_access_browser(self              : PCefExtensionHandler;
                                                  extension         : PCefExtension;
                                                  browser           : PCefBrowser;
                                                  include_incognito : Integer;
                                                  target_browser    : PCefBrowser): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(True);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
    Result := Ord(TCefExtensionHandlerOwn(TempObject).CanAccessBrowser(TCefExtensionRef.UnWrap(extension),
                                                                       TCefBrowserRef.UnWrap(browser),
                                                                       include_incognito <> 0,
                                                                       TCefBrowserRef.UnWrap(target_browser)));
end;

function cef_extension_handler_get_extension_resource(      self      : PCefExtensionHandler;
                                                            extension : PCefExtension;
                                                            browser   : PCefBrowser;
                                                      const file_     : PCefString;
                                                            callback  : PCefGetExtensionResourceCallback): Integer; stdcall;
var
  TempObject : TObject;
begin
  Result     := Ord(False);
  TempObject := CefGetObject(self);

  if (TempObject <> nil) and (TempObject is TCefExtensionHandlerOwn) then
    Result := Ord(TCefExtensionHandlerOwn(TempObject).GetExtensionResource(TCefExtensionRef.UnWrap(extension),
                                                                           TCefBrowserRef.UnWrap(browser),
                                                                           CefString(file_),
                                                                           TCefGetExtensionResourceCallbackRef.UnWrap(callback)));
end;

constructor TCefExtensionHandlerOwn.Create;
begin
  inherited CreateData(SizeOf(TCefExtensionHandler));

  with PCefExtensionHandler(FData)^ do
    begin
      on_extension_load_failed     := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_on_extension_load_failed;
      on_extension_loaded          := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_on_extension_loaded;
      on_extension_unloaded        := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_on_extension_unloaded;
      on_before_background_browser := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_on_before_background_browser;
      on_before_browser            := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_on_before_browser;
      get_active_browser           := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_get_active_browser;
      can_access_browser           := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_can_access_browser;
      get_extension_resource       := {$IFDEF FPC}@{$ENDIF}cef_extension_handler_get_extension_resource;
    end;
end;

procedure TCefExtensionHandlerOwn.OnExtensionLoadFailed(result: TCefErrorcode);
begin
  //
end;

procedure TCefExtensionHandlerOwn.OnExtensionLoaded(const extension: ICefExtension);
begin

  //
end;

procedure TCefExtensionHandlerOwn.OnExtensionUnloaded(const extension: ICefExtension);
begin

  //
end;

function TCefExtensionHandlerOwn.OnBeforeBackgroundBrowser(const extension : ICefExtension;
                                                           const url       : ustring;
                                                           var   client    : ICefClient;
                                                           var   settings  : TCefBrowserSettings) : boolean;
begin
  Result := False;
end;

function TCefExtensionHandlerOwn.OnBeforeBrowser(const extension      : ICefExtension;
                                                 const browser        : ICefBrowser;
                                                 const active_browser : ICefBrowser;
                                                       index          : Integer;
                                                 const url            : ustring;
                                                       active         : boolean;
                                                 var   windowInfo     : TCefWindowInfo;
                                                 var   client         : ICefClient;
                                                 var   settings       : TCefBrowserSettings) : boolean;
begin
  Result := False;
end;

procedure TCefExtensionHandlerOwn.GetActiveBrowser(const extension         : ICefExtension;
                                                   const browser           : ICefBrowser;
                                                         include_incognito : boolean;
                                                   var   aRsltBrowser      : ICefBrowser);
begin
  //
end;

function TCefExtensionHandlerOwn.CanAccessBrowser(const extension         : ICefExtension;
                                                  const browser           : ICefBrowser;
                                                        include_incognito : boolean;
                                                  const target_browser    : ICefBrowser): boolean;
begin
  Result := True;
end;

function TCefExtensionHandlerOwn.GetExtensionResource(const extension : ICefExtension;
                                                      const browser   : ICefBrowser;
                                                      const file_     : ustring;
                                                      const callback  : ICefGetExtensionResourceCallback): boolean;
begin
  Result := False;
end;

procedure TCefExtensionHandlerOwn.RemoveReferences;
begin
  //
end;

// ***************************************************************************
// ************************ TCustomExtensionHandler **************************
// ***************************************************************************

constructor TCustomExtensionHandler.Create(const events: IChromiumEvents);
begin
  inherited Create;

  FEvents := Pointer(events);
end;

destructor TCustomExtensionHandler.Destroy;
begin
  RemoveReferences;

  inherited Destroy;
end;

procedure TCustomExtensionHandler.RemoveReferences;
begin
  FEvents := nil;
end;

procedure TCustomExtensionHandler.OnExtensionLoadFailed(result: TCefErrorcode);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnExtensionLoadFailed(result);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.OnExtensionLoadFailed', e) then raise;
  end;
end;

procedure TCustomExtensionHandler.OnExtensionLoaded(const extension: ICefExtension);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnExtensionLoaded(extension);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.OnExtensionLoaded', e) then raise;
  end;
end;

procedure TCustomExtensionHandler.OnExtensionUnloaded(const extension: ICefExtension);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnExtensionUnloaded(extension);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.OnExtensionUnloaded', e) then raise;
  end;
end;

function TCustomExtensionHandler.OnBeforeBackgroundBrowser(const extension : ICefExtension;
                                                           const url       : ustring;
                                                           var   client    : ICefClient;
                                                           var   settings  : TCefBrowserSettings) : boolean;
begin
  Result := False;

  try
    if (FEvents <> nil) then
      Result := IChromiumEvents(FEvents).doOnExtensionBeforeBackgroundBrowser(extension,
                                                                              url,
                                                                              client,
                                                                              settings);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.OnBeforeBackgroundBrowser', e) then raise;
  end;
end;

function TCustomExtensionHandler.OnBeforeBrowser(const extension      : ICefExtension;
                                                 const browser        : ICefBrowser;
                                                 const active_browser : ICefBrowser;
                                                       index          : Integer;
                                                 const url            : ustring;
                                                       active         : boolean;
                                                 var   windowInfo     : TCefWindowInfo;
                                                 var   client         : ICefClient;
                                                 var   settings       : TCefBrowserSettings) : boolean;
begin
  Result := False;

  try
    if (FEvents <> nil) then
      Result := IChromiumEvents(FEvents).doOnExtensionBeforeBrowser(extension,
                                                                    browser,
                                                                    active_browser,
                                                                    index,
                                                                    url,
                                                                    active,
                                                                    windowInfo,
                                                                    client,
                                                                    settings);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.OnBeforeBrowser', e) then raise;
  end;
end;

procedure TCustomExtensionHandler.GetActiveBrowser(const extension         : ICefExtension;
                                                   const browser           : ICefBrowser;
                                                         include_incognito : boolean;
                                                   var   aRsltBrowser      : ICefBrowser);
begin
  try
    if (FEvents <> nil) then
      IChromiumEvents(FEvents).doOnExtensionGetActiveBrowser(extension,
                                                             browser,
                                                             include_incognito,
                                                             aRsltBrowser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.GetActiveBrowser', e) then raise;
  end;
end;

function TCustomExtensionHandler.CanAccessBrowser(const extension         : ICefExtension;
                                                  const browser           : ICefBrowser;
                                                        include_incognito : boolean;
                                                  const target_browser    : ICefBrowser): boolean;
begin
  Result := True;

  try
    if (FEvents <> nil) then
      Result := IChromiumEvents(FEvents).doOnExtensionCanAccessBrowser(extension,
                                                                       browser,
                                                                       include_incognito,
                                                                       target_browser);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.CanAccessBrowser', e) then raise;
  end;
end;

function TCustomExtensionHandler.GetExtensionResource(const extension : ICefExtension;
                                                      const browser   : ICefBrowser;
                                                      const file_     : ustring;
                                                      const callback  : ICefGetExtensionResourceCallback): boolean;
begin
  Result := False;

  try
    if (FEvents <> nil) then
      Result := IChromiumEvents(FEvents).doOnExtensionGetExtensionResource(extension,
                                                                           browser,
                                                                           file_,
                                                                           callback);
  except
    on e : exception do
      if CustomExceptionHandler('TCustomExtensionHandler.GetExtensionResource', e) then raise;
  end;
end;

end.
