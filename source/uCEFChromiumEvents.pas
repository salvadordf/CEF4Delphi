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

unit uCEFChromiumEvents;

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
  uCEFTypes, uCEFInterfaces;

type
  TOnAccessibilityEvent           = procedure(Sender: TObject; const value: ICefValue) of object;
  TOnTextResultAvailableEvent     = procedure(Sender: TObject; const aText : string) of object;
  TOnPdfPrintFinishedEvent        = procedure(Sender: TObject; aResultOK : boolean) of object;
  TOnCookiesDeletedEvent          = procedure(Sender: TObject; numDeleted : integer) of object;
  TOnProcessMessageReceived       = procedure(Sender: TObject; const browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean) of object;
  TOnLoadingStateChange           = procedure(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean) of object;
  TOnLoadStart                    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType) of object;
  TOnLoadEnd                      = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer) of object;
  TOnLoadError                    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring) of object;
  TOnTakeFocus                    = procedure(Sender: TObject; const browser: ICefBrowser; next: Boolean) of object;
  TOnSetFocus                     = procedure(Sender: TObject; const browser: ICefBrowser; source: TCefFocusSource; out Result: Boolean) of object;
  TOnGotFocus                     = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnBeforeContextMenu            = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel) of object;
  TOnContextMenuCommand           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean) of object;
  TOnContextMenuDismissed         = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame) of object;
  TOnPreKeyEvent                  = procedure(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean; out Result: Boolean) of object;
  TOnKeyEvent                     = procedure(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean) of object;
  TOnAddressChange                = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring) of object;
  TOnTitleChange                  = procedure(Sender: TObject; const browser: ICefBrowser; const title: ustring) of object;
  TOnFavIconUrlChange             = procedure(Sender: TObject; const browser: ICefBrowser; const iconUrls: TStrings) of object;
  TOnFullScreenModeChange         = procedure(Sender: TObject; const browser: ICefBrowser; fullscreen: Boolean) of object;
  TOnTooltip                      = procedure(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean) of object;
  TOnStatusMessage                = procedure(Sender: TObject; const browser: ICefBrowser; const value: ustring) of object;
  TOnConsoleMessage               = procedure(Sender: TObject; const browser: ICefBrowser; const message, source: ustring; line: Integer; out Result: Boolean) of object;
  TOnBeforeDownload               = procedure(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback) of object;
  TOnDownloadUpdated              = procedure(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback) of object;
  TOnRequestGeolocationPermission = procedure(Sender: TObject; const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback; out Result: Boolean) of object;
  TOnCancelGeolocationPermission  = procedure(Sender: TObject; const browser: ICefBrowser; requestId: Integer) of object;
  TOnJsdialog                     = procedure(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean) of object;
  TOnBeforeUnloadDialog           = procedure(Sender: TObject; const browser: ICefBrowser; const messageText: ustring; isReload: Boolean; const callback: ICefJsDialogCallback; out Result: Boolean) of object;
  TOnResetDialogState             = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnDialogClosed                 = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnBeforePopup                  = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean; out Result: Boolean) of object;
  TOnAfterCreated                 = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnBeforeClose                  = procedure(Sender: TObject; const browser: ICefBrowser) of object;
  TOnClose                        = procedure(Sender: TObject; const browser: ICefBrowser; out Result: Boolean) of object;
  TOnBeforeBrowse                 = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean; out Result: Boolean) of object;
  TOnOpenUrlFromTab               = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean) of Object;
  TOnBeforeResourceLoad           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback; out Result: TCefReturnValue) of object;
  TOnGetResourceHandler           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; out Result: ICefResourceHandler) of object;
  TOnResourceRedirect             = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring) of object;
  TOnResourceResponse             = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: Boolean) of Object;
  TOnGetResourceResponseFilter    = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: ICefResponseFilter) of object;
  TOnResourceLoadComplete         = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64) of object;
  TOnGetAuthCredentials           = procedure(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback; out Result: Boolean) of object;
  TOnQuotaRequest                 = procedure(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback; out Result: Boolean) of object;
  TOnProtocolExecution            = procedure(Sender: TObject; const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean) of object;
  TOnCertificateError             = procedure(Sender: TObject; const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback; out Result: Boolean) of Object;
  TOnSelectClientCertificate      = procedure(Sender: TObject; const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback; var aResult : boolean) of object;
  TOnPluginCrashed                = procedure(Sender: TObject; const browser: ICefBrowser; const pluginPath: ustring) of object;
  TOnRenderViewReady              = procedure(Sender: Tobject; const browser: ICefBrowser) of Object;
  TOnRenderProcessTerminated      = procedure(Sender: TObject; const browser: ICefBrowser; status: TCefTerminationStatus) of object;
  TOnFileDialog                   = procedure(Sender: TObject; const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback; out Result: Boolean) of Object;
  TOnGetAccessibilityHandler      = procedure(Sender: TObject; var aAccessibilityHandler : ICefAccessibilityHandler) of Object;
  TOnGetRootScreenRect            = procedure(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect; out Result: Boolean) of Object;
  TOnGetViewRect                  = procedure(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect; out Result: Boolean) of Object;
  TOnGetScreenPoint               = procedure(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean) of Object;
  TOnGetScreenInfo                = procedure(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean) of Object;
  TOnPopupShow                    = procedure(Sender: TObject; const browser: ICefBrowser; show: Boolean) of Object;
  TOnPopupSize                    = procedure(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect) of Object;
  TOnPaint                        = procedure(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer) of Object;
  TOnCursorChange                 = procedure(Sender: TObject; const browser: ICefBrowser; cursor: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo) of Object;
  TOnStartDragging                = procedure(Sender: TObject; const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer; out Result: Boolean) of Object;
  TOnUpdateDragCursor             = procedure(Sender: TObject; const browser: ICefBrowser; operation: TCefDragOperation) of Object;
  TOnScrollOffsetChanged          = procedure(Sender: TObject; const browser: ICefBrowser; x, y: Double) of Object;
  TOnIMECompositionRangeChanged   = procedure(Sender: TObject; const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect) of Object;
  TOnDragEnter                    = procedure(Sender: TObject; const browser: ICefBrowser; const dragData: ICefDragData; mask: TCefDragOperations; out Result: Boolean) of Object;
  TOnDraggableRegionsChanged      = procedure(Sender: TObject; const browser: ICefBrowser; regionsCount: NativeUInt; regions: PCefDraggableRegionArray)of Object;
  TOnFindResult                   = procedure(Sender: TObject; const browser: ICefBrowser; identifier, count: Integer; const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean) of Object;


implementation

end.
