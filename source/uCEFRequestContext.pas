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

unit uCEFRequestContext;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFBaseRefCounted, uCEFInterfaces, uCEFTypes;

type
  TCefRequestContextRef = class(TCefBaseRefCountedRef, ICefRequestContext)
    protected
      function  IsSame(const other: ICefRequestContext): Boolean;
      function  IsSharingWith(const other: ICefRequestContext): Boolean;
      function  IsGlobal: Boolean;
      function  GetHandler: ICefRequestContextHandler;
      function  GetCachePath: ustring;
      function  GetDefaultCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
      function  GetDefaultCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
      function  RegisterSchemeHandlerFactory(const schemeName, domainName: ustring; const factory: ICefSchemeHandlerFactory): Boolean;
      function  ClearSchemeHandlerFactories: Boolean;
      procedure PurgePluginListCache(reloadPages: Boolean);
      function  HasPreference(const name: ustring): Boolean;
      function  GetPreference(const name: ustring): ICefValue;
      function  GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
      function  CanSetPreference(const name: ustring): Boolean;
      function  SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
      procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
      procedure CloseAllConnections(const callback: ICefCompletionCallback);
      procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
      function  ResolveHostCached(const origin: ustring; const resolvedIps: TStrings): TCefErrorCode;
      procedure LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
      function  DidLoadExtension(const extension_id: ustring): boolean;
      function  HasExtension(const extension_id: ustring): boolean;
      function  GetExtensions(const extension_ids: TStringList): boolean;
      function  GetExtension(const extension_id: ustring): ICefExtension;

    public
      class function UnWrap(data: Pointer): ICefRequestContext;
      class function Global: ICefRequestContext;
      class function New(const settings: PCefRequestContextSettings; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      class function New(const aCache, aAcceptLanguageList : ustring; aPersistSessionCookies, aPersistUserPreferences, aIgnoreCertificateErrors, aEnableNetSecurityExpiration : boolean; const handler: ICefRequestContextHandler = nil): ICefRequestContext; overload;
      class function Shared(const other: ICefRequestContext; const handler: ICefRequestContextHandler): ICefRequestContext;
  end;

implementation

uses
  uCEFMiscFunctions, uCEFLibFunctions, uCEFValue, uCEFDictionaryValue, uCEFCookieManager,
  uCEFCompletionCallback, uCEFRequestContextHandler, uCEFExtension;

function TCefRequestContextRef.ClearSchemeHandlerFactories: Boolean;
begin
  Result := PCefRequestContext(FData).clear_scheme_handler_factories(FData) <> 0;
end;

function TCefRequestContextRef.GetCachePath: ustring;
begin
  Result := CefStringFreeAndGet(PCefRequestContext(FData).get_cache_path(FData));
end;

function TCefRequestContextRef.GetDefaultCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
begin
  Result := TCefCookieManagerRef.UnWrap(PCefRequestContext(FData).get_default_cookie_manager(FData, CefGetData(callback)));
end;

function TCefRequestContextRef.GetDefaultCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
begin
  Result := GetDefaultCookieManager(TCefFastCompletionCallback.Create(callback));
end;

function TCefRequestContextRef.GetHandler: ICefRequestContextHandler;
begin
  Result := TCefRequestContextHandlerRef.UnWrap(PCefRequestContext(FData).get_handler(FData));
end;

class function TCefRequestContextRef.Global: ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_get_global_context());
end;

function TCefRequestContextRef.IsGlobal: Boolean;
begin
  Result := PCefRequestContext(FData).is_global(FData) <> 0;
end;

function TCefRequestContextRef.IsSame(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData).is_same(FData, CefGetData(other)) <> 0;
end;

function TCefRequestContextRef.IsSharingWith(const other: ICefRequestContext): Boolean;
begin
  Result := PCefRequestContext(FData).is_sharing_with(FData, CefGetData(other)) <> 0;
end;

class function TCefRequestContextRef.New(const settings: PCefRequestContextSettings;
                                         const handler: ICefRequestContextHandler): ICefRequestContext;
begin
  Result := UnWrap(cef_request_context_create_context(settings, CefGetData(handler)));
end;

class function TCefRequestContextRef.New(const aCache                       : ustring;
                                         const aAcceptLanguageList          : ustring;
                                               aPersistSessionCookies       : boolean;
                                               aPersistUserPreferences      : boolean;
                                               aIgnoreCertificateErrors     : boolean;
                                               aEnableNetSecurityExpiration : boolean;
                                         const handler                      : ICefRequestContextHandler): ICefRequestContext;
var
  TempSettings : TCefRequestContextSettings;
begin
  TempSettings.size                           := SizeOf(TCefRequestContextSettings);
  TempSettings.cache_path                     := CefString(aCache);
  TempSettings.persist_session_cookies        := Ord(aPersistSessionCookies);
  TempSettings.persist_user_preferences       := Ord(aPersistUserPreferences);
  TempSettings.ignore_certificate_errors      := Ord(aIgnoreCertificateErrors);
  TempSettings.enable_net_security_expiration := Ord(aEnableNetSecurityExpiration);
  TempSettings.accept_language_list           := CefString(aAcceptLanguageList);

  Result := UnWrap(cef_request_context_create_context(@TempSettings, CefGetData(handler)));
end;

procedure TCefRequestContextRef.PurgePluginListCache(reloadPages: Boolean);
begin
  PCefRequestContext(FData).purge_plugin_list_cache(FData, Ord(reloadPages));
end;

function TCefRequestContextRef.HasPreference(const name: ustring): Boolean;
var
  n : TCefString;
begin
  n      := CefString(name);
  Result := PCefRequestContext(FData).has_preference(FData, @n) <> 0;
end;

function TCefRequestContextRef.GetPreference(const name: ustring): ICefValue;
var
  n : TCefString;
begin
  n      := CefString(name);
  Result :=  TCefValueRef.UnWrap(PCefRequestContext(FData).get_preference(FData, @n));
end;

function TCefRequestContextRef.GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
begin
  Result := TCefDictionaryValueRef.UnWrap(PCefRequestContext(FData).get_all_preferences(FData, Ord(includeDefaults)));
end;

function TCefRequestContextRef.CanSetPreference(const name: ustring): Boolean;
var
  n: TCefString;
begin
  n      := CefString(name);
  Result := PCefRequestContext(FData).can_set_preference(FData, @n) <> 0;
end;

function TCefRequestContextRef.SetPreference(const name  : ustring;
                                             const value : ICefValue;
                                             out   error : ustring): Boolean;
var
  n, e: TCefString;
begin
  n := CefString(name);
  FillChar(e, SizeOf(e), 0);
  Result := PCefRequestContext(FData).set_preference(FData, @n, CefGetData(value), @e) <> 0;
  error  := CefString(@e);
end;

procedure TCefRequestContextRef.ClearCertificateExceptions(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData).clear_certificate_exceptions(FData, CefGetData(callback));
end;

procedure TCefRequestContextRef.CloseAllConnections(const callback: ICefCompletionCallback);
begin
  PCefRequestContext(FData).close_all_connections(FData, CefGetData(callback));
end;

procedure TCefRequestContextRef.ResolveHost(const origin   : ustring;
                                            const callback : ICefResolveCallback);
var
  o: TCefString;
begin
  o := CefString(origin);
  PCefRequestContext(FData).resolve_host(FData, @o, CefGetData(callback));
end;

function TCefRequestContextRef.ResolveHostCached(const origin      : ustring;
                                                 const resolvedIps : TStrings): TCefErrorCode;
var
  ips : TCefStringList;
  o, str : TCefString;
  i : Integer;
begin
  ips := cef_string_list_alloc;

  try
    o      := CefString(origin);
    Result := PCefRequestContext(FData).resolve_host_cached(FData, @o, ips);

    if Assigned(ips) then
      for i := 0 to cef_string_list_size(ips) - 1 do
        begin
          FillChar(str, SizeOf(str), 0);
          cef_string_list_value(ips, i, @str);
          resolvedIps.Add(CefStringClearAndGet(str));
        end;
  finally
    cef_string_list_free(ips);
  end;
end;

procedure TCefRequestContextRef.LoadExtension(const root_directory: ustring; const manifest: ICefDictionaryValue; const handler: ICefExtensionHandler);
var
  TempDir : TCefString;
begin
  TempDir := CefString(root_directory);
  PCefRequestContext(FData).load_extension(FData, @TempDir, CefGetData(manifest), CefGetData(handler));
end;

function TCefRequestContextRef.DidLoadExtension(const extension_id: ustring): boolean;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := PCefRequestContext(FData).did_load_extension(FData, @TempID) <> 0;
end;

function TCefRequestContextRef.HasExtension(const extension_id: ustring): boolean;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := PCefRequestContext(FData).has_extension(FData, @TempID) <> 0;
end;

function TCefRequestContextRef.GetExtensions(const extension_ids: TStringList): boolean;
var
  TempIDs    : TCefStringList;
  i, j       : integer;
  TempString : TCefString;
begin
  TempIDs := cef_string_list_alloc;

  try
    Result := PCefRequestContext(FData).get_extensions(FData, TempIDs) <> 0;

    if Assigned(TempIDs) then
      begin
        i := 0;
        j := cef_string_list_size(TempIDs);

        while (i < j) do
          begin
            FillChar(TempString, SizeOf(TempString), 0);
            cef_string_list_value(TempIDs, i, @TempString);
            extension_ids.Add(CefStringClearAndGet(TempString));
            inc(i);
          end;
      end;
  finally
    cef_string_list_free(TempIDs);
  end;
end;

function TCefRequestContextRef.GetExtension(const extension_id: ustring): ICefExtension;
var
  TempID : TCefString;
begin
  TempID := CefString(extension_id);
  Result := TCefExtensionRef.UnWrap(PCefRequestContext(FData).get_extension(FData, @TempID));
end;

function TCefRequestContextRef.RegisterSchemeHandlerFactory(const schemeName : ustring;
                                                            const domainName : ustring;
                                                            const factory    : ICefSchemeHandlerFactory): Boolean;
var
  s, d: TCefString;
begin
  s      := CefString(schemeName);
  d      := CefString(domainName);
  Result := PCefRequestContext(FData).register_scheme_handler_factory(FData, @s, @d, CefGetData(factory)) <> 0;
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

end.
