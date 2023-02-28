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
//        Copyright © 2023 Salvador Diaz Fau. All rights reserved.
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

unit uCEFApplicationEvents;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uCEFTypes, uCEFInterfaces, uCEFSchemeRegistrar, uCEFPreferenceRegistrar;

type
  // ICefApp
  TOnRegisterCustomSchemesEvent      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const registrar: TCefSchemeRegistrarRef) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};

  // ICefBrowserProcessHandler
  TOnRegisterCustomPreferencesEvent  = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(type_: TCefPreferencesType; const registrar: TCefPreferenceRegistrarRef) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnContextInitializedEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}{$IFNDEF FPC} of object{$ENDIF}{$ENDIF};
  TOnBeforeChildProcessLaunchEvent   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const commandLine: ICefCommandLine) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnScheduleMessagePumpWorkEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const delayMs: Int64) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC} of object{$ENDIF}{$ENDIF};
  TOnGetDefaultClientEvent           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(var aClient : ICefClient) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC} of object{$ENDIF}{$ENDIF};

  // ICefResourceBundleHandler
  TOnGetLocalizedStringEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(stringId: Integer; out stringVal: ustring; var aResult : Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnGetDataResourceEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(resourceId: Integer; out data: Pointer; out dataSize: NativeUInt; var aResult : Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnGetDataResourceForScaleEvent    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(resourceId: Integer; scaleFactor: TCefScaleFactor; out data: Pointer; out dataSize: NativeUInt; var aResult : Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};

  // ICefRenderProcessHandler
  TOnWebKitInitializedEvent          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnBrowserCreatedEvent             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const extra_info: ICefDictionaryValue) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnBrowserDestroyedEvent           = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnContextCreatedEvent             = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnContextReleasedEvent            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnUncaughtExceptionEvent          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnFocusedNodeChangedEvent         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnProcessMessageReceivedEvent     = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; var aHandled : boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF LCL} of object {$ENDIF} {$ENDIF};

  // ICefLoadHandler
  TOnRenderLoadingStateChange        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadStart                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadEnd                   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};
  TOnRenderLoadError                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring) {$IFNDEF DELPHI12_UP}{$IFNDEF FPC}of object{$ENDIF}{$ENDIF};

implementation

end.
