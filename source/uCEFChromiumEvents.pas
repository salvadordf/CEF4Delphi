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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

unit uCEFChromiumEvents;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$IFNDEF CPUX64}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes, {$IFDEF MSWINDOWS}WinApi.Messages,{$ENDIF}
  {$ELSE}
  Classes, {$IFDEF MSWINDOWS}Messages,{$ENDIF}
  {$ENDIF}
  uCEFTypes, uCEFInterfaces;

type
  // ICefClient
  TOnProcessMessageReceived       = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean) of object;

  // ICefLoadHandler
  TOnLoadStart                    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType) of object;
  TOnLoadEnd                      = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer) of object;
  TOnLoadError                    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring) of object;
  TOnLoadingStateChange           = procedure(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean) of object;

  // ICefFocusHandler
  TOnTakeFocus                    = procedure(Sender: TObject; const browser: ICefBrowser; next: Boolean) of object;
  TOnSetFocus                     = procedure(Sender: TObject; const browser: ICefBrowser; source: TCefFocusSource; out Result: Boolean) of object;
  TOnGotFocus                     = procedure(Sender: TObject; const browser: ICefBrowser) of object;

  // ICefContextMenuHandler
  TOnBeforeContextMenu            = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel) of object;
  TOnRunContextMenu               = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel; const callback: ICefRunContextMenuCallback; var aResult : Boolean) of object;
  TOnContextMenuCommand           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean) of object;
  TOnContextMenuDismissed         = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame) of object;

  // ICefKeyboardHandler
  TOnPreKeyEvent                  = procedure(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean) of object;
  TOnKeyEvent                     = procedure(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean) of object;

  // ICefDisplayHandler
  TOnAddressChange                = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring) of object;
  TOnTitleChange                  = procedure(Sender: TObject; const browser: ICefBrowser; const title: ustring) of object;
  TOnFavIconUrlChange             = procedure(Sender: TObject; const browser: ICefBrowser; const iconUrls: TStrings) of object;
  TOnFullScreenModeChange         = procedure(Sender: TObject; const browser: ICefBrowser; fullscreen: Boolean) of object;
  TOnTooltip                      = procedure(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean) of object;
  TOnStatusMessage                = procedure(Sender: TObject; const browser: ICefBrowser; const value: ustring) of object;
  TOnConsoleMessage               = procedure(Sender: TObject; const browser: ICefBrowser; level: TCefLogSeverity; const message, source: ustring; line: Integer; out Result: Boolean) of object;
  TOnAutoResize                   = procedure(Sender: TObject; const browser: ICefBrowser; const new_size: PCefSize; out Result: Boolean) of object;
  TOnLoadingProgressChange        = procedure(Sender: TObject; const browser: ICefBrowser; const progress: double) of object;

  // ICefDownloadHandler
  TOnBeforeDownload               = procedure(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback) of object;
  TOnDownloadUpdated              = procedure(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback) of object;

  // ICefJsDialogHandler
  TOnJsdialog                     = procedure(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean) of object;
  TOnBeforeUnloadDialog           = procedure(Sender: TObject; const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback; out Result: Boolean) of object;
  TOnResetDialogState             = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnDialogClosed                 = procedure(Sender: TObject; const browser: ICefBrowser) of object;

  // ICefLifeSpanHandler
  TOnBeforePopup                  = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean) of object;
  TOnAfterCreated                 = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnBeforeClose                  = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnClose                        = procedure(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction) of object;

  // ICefRequestHandler
  TOnBeforeBrowse                 = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; user_gesture, isRedirect: Boolean; out Result: Boolean) of object;
  TOnOpenUrlFromTab               = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean) of Object;
  TOnGetAuthCredentials           = procedure(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback; out Result: Boolean) of object;
  TOnQuotaRequest                 = procedure(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback; out Result: Boolean) of object;
  TOnCertificateError             = procedure(Sender: TObject; const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback; out Result: Boolean) of Object;
  TOnSelectClientCertificate      = procedure(Sender: TObject; const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback; var aResult : boolean) of object;
  TOnPluginCrashed                = procedure(Sender: TObject; const browser: ICefBrowser; const pluginPath: ustring) of object;
  TOnRenderViewReady              = procedure(Sender: Tobject; const browser: ICefBrowser) of Object;
  TOnRenderProcessTerminated      = procedure(Sender: TObject; const browser: ICefBrowser; status: TCefTerminationStatus) of object;
  TOnGetResourceRequestHandler    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; is_navigation, is_download: boolean; const request_initiator: ustring; var disable_default_handling: boolean; var aExternalResourceRequestHandler : ICefResourceRequestHandler) of object;

  // ICefResourceRequestHandler
  TOnBeforeResourceLoad           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback; out Result: TCefReturnValue) of object;
  TOnGetResourceHandler           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var aResourceHandler : ICefResourceHandler) of object;
  TOnResourceRedirect             = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring) of object;
  TOnResourceResponse             = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: Boolean) of Object;
  TOnGetResourceResponseFilter    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: ICefResponseFilter) of object;
  TOnResourceLoadComplete         = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64) of object;
  TOnProtocolExecution            = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; var allowOsExecution: Boolean) of object;

  // ICefCookieAccessFilter
  TOnCanSendCookie                = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const cookie: PCefCookie; var aResult: boolean) of object;
  TOnCanSaveCookie                = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; const cookie: PCefCookie; var aResult : boolean) of object;

  // ICefDialogHandler
  TOnFileDialog                   = procedure(Sender: TObject; const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback; out Result: Boolean) of Object;

  // ICefRenderHandler
  TOnGetAccessibilityHandler      = procedure(Sender: TObject; var aAccessibilityHandler : ICefAccessibilityHandler) of Object;
  TOnGetRootScreenRect            = procedure(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect; out Result: Boolean) of Object;
  TOnGetViewRect                  = procedure(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect) of Object;
  TOnGetScreenPoint               = procedure(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean) of Object;
  TOnGetScreenInfo                = procedure(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean) of Object;
  TOnPopupShow                    = procedure(Sender: TObject; const browser: ICefBrowser; show: Boolean) of Object;
  TOnPopupSize                    = procedure(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect) of Object;
  TOnPaint                        = procedure(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer) of Object;
  TOnAcceleratedPaint             = procedure(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; shared_handle: Pointer) of Object;
  TOnCursorChange                 = procedure(Sender: TObject; const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo) of Object;
  TOnStartDragging                = procedure(Sender: TObject; const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean) of Object;
  TOnUpdateDragCursor             = procedure(Sender: TObject; const browser: ICefBrowser; operation: TCefDragOperation) of Object;
  TOnScrollOffsetChanged          = procedure(Sender: TObject; const browser: ICefBrowser; x, y: Double) of Object;
  TOnIMECompositionRangeChanged   = procedure(Sender: TObject; const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect) of Object;
  TOnTextSelectionChanged         = procedure(Sender: TObject; const browser: ICefBrowser; const selected_text: ustring; const selected_range: PCefRange) of Object;
  TOnVirtualKeyboardRequested     = procedure(Sender: TObject; const browser: ICefBrowser; input_mode: TCefTextInpuMode) of Object;

  // ICefDragHandler
  TOnDragEnter                    = procedure(Sender: TObject; const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean) of Object;
  TOnDraggableRegionsChanged      = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; regionsCount: NativeUInt; const regions: PCefDraggableRegionArray) of Object;

  // ICefFindHandler
  TOnFindResult                   = procedure(Sender: TObject; const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean) of Object;

  // ICefRequestContextHandler
  TOnRequestContextInitialized    = procedure(Sender: TObject; const request_context: ICefRequestContext) of Object;
  TOnBeforePluginLoad             = procedure(Sender: TObject; const mimeType, pluginUrl:ustring; isMainFrame : boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy; var aResult : boolean) of Object;
  // ICefRequestContextHandler uses the same TOnGetResourceRequestHandler event type defined for ICefRequestHandler

  // Custom
  TOnTextResultAvailableEvent              = procedure(Sender: TObject; const aText : ustring) of object;
  TOnPdfPrintFinishedEvent                 = procedure(Sender: TObject; aResultOK : boolean) of object;
  TOnPrefsAvailableEvent                   = procedure(Sender: TObject; aResultOK : boolean) of object;
  TOnCookiesDeletedEvent                   = procedure(Sender: TObject; numDeleted : integer) of object;
  TOnResolvedIPsAvailableEvent             = procedure(Sender: TObject; result: TCefErrorCode; const resolvedIps: TStrings) of object;
  TOnNavigationVisitorResultAvailableEvent = procedure(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer; var aResult : boolean) of object;
  TOnDownloadImageFinishedEvent            = procedure(Sender: TObject; const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage) of object;
  TOnExecuteTaskOnCefThread                = procedure(Sender: TObject; aTaskID : cardinal) of object;
  TOnCookiesVisited                        = procedure(Sender: TObject; const name_, value, domain, path: ustring; secure, httponly, hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count, total, aID : Integer; var aDeleteCookie, aResult : Boolean) of object;
  TOnCookieVisitorDestroyed                = procedure(Sender: TObject; aID : integer) of object;
  TOnCookieSet                             = procedure(Sender: TObject; aSuccess : boolean; aID : integer) of object;
  TOnZoomPctAvailable                      = procedure(Sender: TObject; const aZoomPct : double) of object;
  {$IFDEF MSWINDOWS}
  TOnCompMsgEvent                          = procedure(var aMessage: TMessage; var aHandled: Boolean) of object;
  {$ENDIF}

implementation

end.
