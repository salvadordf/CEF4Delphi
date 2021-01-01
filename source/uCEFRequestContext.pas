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

unit uCEFRequestContext;

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
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes, uCEFCompletionCallback;

type
  TCefRequestContextRef = class(TCefBaseRefCountedRef, ICefRequestContext)
    protected
      function  IsSame(const other: ICefRequestContext): Boolean;
      function  IsSharingWith(const other: ICefRequestContext): Boolean;
      function  IsGlobal: Boolean;
      function  GetHandler: ICefRequestContextHandler;
      function  GetCachePath: ustring;
      function  GetCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
      function  GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
      function  RegisterSchemeHandlerFactory(const schemeName, domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
      function  ClearSchemeHandlerFactories: Boolean;
      procedure PurgePluginListCache(reloadPages: Boolean);
      function  HasPreference(const name: ustring): Boolean;
      function  GetPreference(const name: ustring): ICefValue;
      function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
      function  CanSetPreference(const name: ustring): Boolean;
      function  SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
      procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
      procedure ClearHttpAuthCredentials(const callback: ICefCompletionCallback);
      procedure CloseAllConnections(const callback: ICefCompletionCallback);
      procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
      procedure LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
      function  DidLoadExtension(const extension_id: ustring): boolean;
      function  HasExtension(const extension_id: ustring): boolean;
      function  GetExtensions(const extension_ids: TStringList): boolean;
      function  GetExtension(const extension_id: ustring): ICefExtension;
      function  GetMediaRouter: ICefMediaRouter;

    public
      class function UnWrap(data: Pointer): ICefRequestContext;
      class function Global: ICefRequestContext;
      class function New(const settings: PCefRequestContextSettings; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      class function New(const aCache, aAcceptLanguageList : ustring; aPersistSessionCookies, aPersistUserPreferences, aIgnoreCertificateErrors : boolean; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      class function Shared(const other: ICefRequestContext; const handler: ICefRequestContextHandler): ICefRequestContext;
  end;

  TCefClearCertificateExceptionsCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

  TCefClearHttpAuthCredentialsCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

  TCefCloseAllConnectionsCompletionCallback = class(TCefCustomCompletionCallback)
    protected
      procedure OnComplete; override;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFValue, uCEFDictionaryValue,
  uCEFCookieManager, uCEFRequestContextHandler, uCEFExtension, uCEFStringList,
  uCEFMediaRouter;

function TCefRequestContextRef.ClearSchemeHandlerFactories: Boolean;
begin
  Result := PCefRequestContext(FData)^.clear_scheme_handler_factories(PCefRequestContext(FData)) <> 0;
end;

function TCefRequestContextRef.GetCachePath: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequestContext(FData)^.get_cache_path(PCefRequestContext(FData)));
end;

function TCefRequestContextRef.GetCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
begin
  Result := TCefCookieManagerRef.UnWrap(PCefRequestContext(FData)^.get_cookie_manager(PCefRequestContext(FData), CefGetData(callback)));
end;

function TCefRequestContextRef.GetCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := GetCookieManager(TCefFastCompletionCallback.Create(callback));
end;

function TCefRequestContextRef.GetHandler: ICefRequestContextHandler;
var
  TempHandler : PCefRequestContextHandler;
begin
  TempHandler := PCefRequestContext(FData)^.get_handler(PCefRequestContext(FData));

  if (TempHandler <> nil) then
    Result := TCefRequestContextHandlerRef.UnWrap(TempHandler)
   else
    Result := nil;
end;

class function TCefRequestContextRef.Global: ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_get_global_context());
end;

function TCefRequestContextRef.IsGlobal: Boolean;
begin
  Result := PCefRequestContext(FData)^.is_global(PCefRequestContext(FData)) <> 0;
end;

function TCefRequestContextRef.IsSame(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData)^.is_same(PCefRequestContext(FData), CefGetData(other)) <> 0;
end;

function TCefRequestContextRef.IsSharingWith(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData)^.is_sharing_with(PCefRequestContext(FData), CefGetData(other)) <> 0;
end;

class function TCefRequestContextRef.New(const settings : PCefRequestContextSettings;
                                         const handler  : ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_create_context(settings, CefGetData(handler)));
end;

class function TCefRequestContextRef.New(const aCache                       : ustring;
                                         const aAcceptLanguageList          : ustring;
                                               aPersistSessionCookies       : boolean;
                                               aPersistUserPreferences      : boolean;
                                               aIgnoreCertificateErrors     : boolean;
                                         const handler                      : ICefRequestContextHandler): ICefRequestContext;
var
  TempSettings : TCefRequestContextSettings;
begin
  TempSettings.size                           := SizeOf(TCefRequestContextSettings);
  TempSettings.cache_path                     := CefString(aCache);
  TempSettings.persist_session_cookies        := Ord(aPersistSessionCookies);
  TempSettings.persist_user_preferences       := Ord(aPersistUserPreferences);
  TempSettings.ignore_certificate_errors      := Ord(aIgnoreCertificateErrors);
  TempSettings.accept_language_list           := CefString(aAcceptLanguageList);

  Result := UnWrap(cef_request_context_create_context(@TempSettings, CefGetData(handler)));
end;

procedure TCefRequestContextRef.PurgePluginListCache(reloadPages: Boolean);
begin
  PCefRequestContext(FData)^.purge_plugin_list_cache(PCefRequestContext(FData), Ord(reloadPages));
end;

function TCefRequestContextRef.HasPreference(const name: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefRequestContext(FData)^.has_preference(PCefRequestContext(FData), @TempName) <> 0;
end;

function TCefRequestContextRef.GetPreference(const name: ustring): ICefValue;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   :=  TCefValueRef.UnWrap(PCefRequestContext(FData)^.get_preference(PCefRequestContext(FData), @TempName));
end;

function TCefRequestContextRef.GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefRequestContext(FData)^.get_all_preferences(PCefRequestContext(FData), Ord(includeDefaults)));
end;

function TCefRequestContextRef.CanSetPreference(const name: ustring): Boolean;
var
  TempName : TCefString;
begin
  TempName := CefString(name);
  Result   := PCefRequestContext(FData)^.can_set_preference(PCefRequestContext(FData), @TempName) <> 0;
end;

function TCefRequestContextRef.SetPreference(const name  : ustring;
                                             const value : ICefValue;
                                             out   error : ustring): Boolean;
var
  TempName, TempError : TCefString;
begin
  CefStringInitialize(@TempError);

  TempName := CefString(name);
  Result   := PCefRequestContext(FData)^.set_preference(PCefRequestContext(FData), @TempName, CefGetData(value), @TempError) <> 0;
  error    := CefStringClearAndGet(@TempError);
end;

procedure TCefRequestContextRef.ClearCertificateExceptions(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData)^.clear_certificate_exceptions(PCefRequestContext(FData), CefGetData(callback));
end;

procedure TCefRequestContextRef.ClearHttpAuthCredentials(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData)^.clear_http_auth_credentials(PCefRequestContext(FData), CefGetData(callback));
end;

procedure TCefRequestContextRef.CloseAllConnections(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData)^.close_all_connections(PCefRequestContext(FData), CefGetData(callback));
end;

procedure TCefRequestContextRef.ResolveHost(const origin   : ustring;
                                            const callback : ICefResolveCallback);
var
  TempOrigin : TCefString;
begin
  TempOrigin := CefString(origin);
  PCefRequestContext(FData)^.resolve_host(PCefRequestContext(FData), @TempOrigin, CefGetData(callback));
end;

procedure TCefRequestContextRef.LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
var
  TempDir : TCefString;
begin
  TempDir := CefString(root_directory);
  PCefRequestContext(FData)^.load_extension(PCefRequestContext(FData), @TempDir, CefGetData(manifest), CefGetData(handler));
end;

function TCefRequestContextRef.DidLoadExtension(const extension_id: ustring): boolean;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := PCefRequestContext(FData)^.did_load_extension(PCefRequestContext(FData), @TempID) <> 0;
end;

function TCefRequestContextRef.HasExtension(const extension_id: ustring): boolean;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := PCefRequestContext(FData)^.has_extension(PCefRequestContext(FData), @TempID) <> 0;
end;

function TCefRequestContextRef.GetExtensions(const extension_ids: TStringList): boolean;
var
  TempSL : ICefStringList;
begin
  Result := False;
  TempSL := TCefStringListOwn.Create;

  if (PCefRequestContext(FData)^.get_extensions(PCefRequestContext(FData), TempSL.Handle) <> 0) then
    begin
      TempSL.CopyToStrings(extension_ids);
      Result := True;
    end;
end;

function TCefRequestContextRef.GetExtension(const extension_id: ustring): ICefExtension;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := TCefExtensionRef.UnWrap(PCefRequestContext(FData)^.get_extension(PCefRequestContext(FData), @TempID));
end;

function TCefRequestContextRef.GetMediaRouter: ICefMediaRouter;
begin
  Result := TCefMediaRouterRef.UnWrap(PCefRequestContext(FData)^.get_media_router(PCefRequestContext(FData)));
end;

function TCefRequestContextRef.RegisterSchemeHandlerFactory(const schemeName : ustring;
                                                            const domainName : ustring;
                                                            const factory    : ICefSchemeHandlerFactory): Boolean;
var
  TempScheme, TempDomain : TCefString;
begin
  TempScheme := CefString(schemeName);
  TempDomain := CefString(domainName);
  Result     := PCefRequestContext(FData)^.register_scheme_handler_factory(PCefRequestContext(FData),
                                                                           @TempScheme,
                                                                           @TempDomain,
                                                                           CefGetData(factory)) <> 0;
end;

class function TCefRequestContextRef.Shared(const other   : ICefRequestContext;
                                            const handler : ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(cef_create_context_shared(CefGetData(other), CefGetData(handler)));
end;

class function TCefRequestContextRef.UnWrap(data: Pointer): ICefRequestContext;
begin
  if (data <> nil) then
    Result := Create(data) as ICefRequestContext
   else
    Result := nil;
end;


// TCefClearCertificateExceptionsCompletionCallback

procedure TCefClearCertificateExceptionsCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doCertificateExceptionsCleared;
    except
      on e : exception do
        if CustomExceptionHandler('TCefClearCertificateExceptionsCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefClearHttpAuthCredentialsCompletionCallback

procedure TCefClearHttpAuthCredentialsCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doHttpAuthCredentialsCleared;
    except
      on e : exception do
        if CustomExceptionHandler('TCefClearHttpAuthCredentialsCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;


// TCefCloseAllConnectionsCompletionCallback

procedure TCefCloseAllConnectionsCompletionCallback.OnComplete;
begin
  try
    try
      if (FEvents <> nil) then IChromiumEvents(FEvents).doAllConnectionsClosed;
    except
      on e : exception do
        if CustomExceptionHandler('TCefCloseAllConnectionsCompletionCallback.OnComplete', e) then raise;
    end;
  finally
    FEvents := nil;
  end;
end;

end.
