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

unit uCEFInterfaces;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes,
  {$ELSE}
  Windows, Classes,
  {$ENDIF}
  uCEFTypes, uCEFSchemeRegistrar;

type
  ICefBrowser = interface;
  ICefFrame = interface;
  ICefRequest = interface;
  ICefv8Value = interface;
  ICefDomVisitor = interface;
  ICefDomDocument = interface;
  ICefDomNode = interface;
  ICefv8Context = interface;
  ICefListValue = interface;
  ICefBinaryValue = interface;
  ICefDictionaryValue = interface;
  ICefClient = interface;
  ICefUrlrequestClient = interface;
  ICefBrowserHost = interface;
  ICefTask = interface;
  ICefTaskRunner = interface;
  ICefFileDialogCallback = interface;
  ICefPrintHandler = interface;
  ICefPrintDialogCallback = interface;
  ICefPrintJobCallback = interface;
  ICefRequestContext = interface;
  ICefAccessibilityHandler = interface;
  ICefDragData = interface;
  ICefNavigationEntry = interface;
  ICefSslInfo = interface;
  ICefSSLStatus = interface;
  ICefImage = interface;
  IChromiumEvents = interface;
  ICefThread = interface;
  ICefWaitableEvent = interface;
  ICefX509CertPrincipal = interface;
  ICefX509Certificate = interface;
  ICefSelectClientCertificateCallback = interface;
  ICefCommandLine = interface;
  ICefRequestHandler = interface;
  ICefResourceBundleHandler = interface;
  ICefBrowserProcessHandler = interface;
  ICefRenderProcessHandler = interface;
  ICefProcessMessage = interface;

  TCefv8ValueArray         = array of ICefv8Value;
  TCefX509CertificateArray = array of ICefX509Certificate;
  TCefBinaryValueArray     = array of ICefBinaryValue;

  TOnPdfPrintFinishedProc          = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const path: ustring; ok: Boolean);
  TCefDomVisitorProc               = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const document: ICefDomDocument);
  TCefDomVisitorProc2              = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser : ICefBrowser; const document: ICefDomDocument);
  TCefStringVisitorProc            = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const str: ustring);

  TOnRegisterCustomSchemes         = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const registrar: TCefSchemeRegistrarRef) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnGetResourceBundleHandler      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(var aCefResourceBundleHandler : ICefResourceBundleHandler) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnGetBrowserProcessHandler      = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(var aCefBrowserProcessHandler : ICefBrowserProcessHandler) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnGetRenderProcessHandler       = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(var aCefRenderProcessHandler : ICefRenderProcessHandler) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnBeforeCommandLineProcessing   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const processType: ustring; const commandLine: ICefCommandLine) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnCustomMessage                 = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(const browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage) {$IFNDEF DELPHI12_UP}of object{$ENDIF};
  TOnWebKitReady                   = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure() {$IFNDEF DELPHI12_UP}of object{$ENDIF};

  TCefCompletionCallbackProc       = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure;
  TCefSetCookieCallbackProc        = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(success: Boolean);
  TCefDeleteCookiesCallbackProc    = {$IFDEF DELPHI12_UP}reference to{$ENDIF} procedure(numDeleted: Integer);



  ICefBaseRefCounted = interface
    ['{1F9A7B44-DCDC-4477-9180-3ADD44BDEB7B}']
    function Wrap: Pointer;
  end;

  ICefRunFileDialogCallback = interface(ICefBaseRefCounted)
  ['{59FCECC6-E897-45BA-873B-F09586C4BE47}']
    procedure OnFileDialogDismissed(selectedAcceptFilter: Integer; filePaths: TStrings);
  end;

  TCefRunFileDialogCallbackProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF}
    procedure(selectedAcceptFilter: Integer; filePaths: TStrings);

  ICefNavigationEntryVisitor = interface(ICefBaseRefCounted)
  ['{CC4D6BC9-0168-4C2C-98BA-45E9AA9CD619}']
    function Visit(const entry: ICefNavigationEntry;
      current: Boolean; index, total: Integer): Boolean;
  end;

  TCefNavigationEntryVisitorProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF}
    function(const entry: ICefNavigationEntry; current: Boolean; index, total: Integer): Boolean;

  ICefPdfPrintCallback = interface(ICefBaseRefCounted)
  ['{F1CC58E9-2C30-4932-91AE-467C8D8EFB8E}']
    procedure OnPdfPrintFinished(const path: ustring; ok: Boolean);
  end;

  TOnDownloadImageFinishedProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF}
    procedure(const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);

  ICefDownloadImageCallback = interface(ICefBaseRefCounted)
  ['{0C6E9032-27DF-4584-95C6-DC3C7CB63727}']
    procedure OnDownloadImageFinished(const imageUrl: ustring;
      httpStatusCode: Integer; const image: ICefImage);
  end;

  ICefBrowserHost = interface(ICefBaseRefCounted)
    ['{53AE02FF-EF5D-48C3-A43E-069DA9535424}']
    function  GetBrowser: ICefBrowser;
    procedure CloseBrowser(forceClose: Boolean);
    function  TryCloseBrowser: Boolean;
    procedure SetFocus(focus: Boolean);
    function  GetWindowHandle: TCefWindowHandle;
    function  GetOpenerWindowHandle: TCefWindowHandle;
    function  HasView: Boolean;
    function  GetRequestContext: ICefRequestContext;
    function  GetZoomLevel: Double;
    procedure SetZoomLevel(zoomLevel: Double);
    procedure RunFileDialog(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: ICefRunFileDialogCallback);
    procedure RunFileDialogProc(mode: TCefFileDialogMode; const title, defaultFilePath: ustring; acceptFilters: TStrings; selectedAcceptFilter: Integer; const callback: TCefRunFileDialogCallbackProc);
    procedure StartDownload(const url: ustring);
    procedure DownloadImage(const imageUrl: ustring; isFavicon: Boolean; maxImageSize: Cardinal; bypassCache: Boolean; const callback: ICefDownloadImageCallback);
    procedure Print;
    procedure PrintToPdf(const path: ustring; settings: PCefPdfPrintSettings; const callback: ICefPdfPrintCallback);
    procedure PrintToPdfProc(const path: ustring; settings: PCefPdfPrintSettings; const callback: TOnPdfPrintFinishedProc);
    procedure Find(identifier: Integer; const searchText: ustring; forward, matchCase, findNext: Boolean);
    procedure StopFinding(clearSelection: Boolean);
    procedure ShowDevTools(const windowInfo: PCefWindowInfo; const client: ICefClient; const settings: PCefBrowserSettings; inspectElementAt: PCefPoint);
    procedure CloseDevTools;
    function  HasDevTools: Boolean;
    procedure GetNavigationEntries(const visitor: ICefNavigationEntryVisitor; currentOnly: Boolean);
    procedure GetNavigationEntriesProc(const proc: TCefNavigationEntryVisitorProc; currentOnly: Boolean);
    procedure SetMouseCursorChangeDisabled(disabled: Boolean);
    function  IsMouseCursorChangeDisabled: Boolean;
    procedure ReplaceMisspelling(const word: ustring);
    procedure AddWordToDictionary(const word: ustring);
    function  IsWindowRenderingDisabled: Boolean;
    procedure WasResized;
    procedure WasHidden(hidden: Boolean);
    procedure NotifyScreenInfoChanged;
    procedure Invalidate(kind: TCefPaintElementType);
    procedure SendKeyEvent(const event: PCefKeyEvent);
    procedure SendMouseClickEvent(const event: PCefMouseEvent; kind: TCefMouseButtonType; mouseUp: Boolean; clickCount: Integer);
    procedure SendMouseMoveEvent(const event: PCefMouseEvent; mouseLeave: Boolean);
    procedure SendMouseWheelEvent(const event: PCefMouseEvent; deltaX, deltaY: Integer);
    procedure SendFocusEvent(setFocus: Boolean);
    procedure SendCaptureLostEvent;
    procedure NotifyMoveOrResizeStarted;
    function  GetWindowlessFrameRate(): Integer;
    procedure SetWindowlessFrameRate(frameRate: Integer);
    procedure IMESetComposition(const text: ustring; underlinesCount : NativeUInt; const underlines : PCefCompositionUnderline; const replacement_range, selection_range : PCefRange);
    procedure IMECommitText(const text: ustring; const replacement_range : PCefRange; relative_cursor_pos : integer);
    procedure IMEFinishComposingText(keep_selection : boolean);
    procedure IMECancelComposition;
    procedure DragTargetDragEnter(const dragData: ICefDragData; const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragOver(const event: PCefMouseEvent; allowedOps: TCefDragOperations);
    procedure DragTargetDragLeave;
    procedure DragTargetDrop(event: PCefMouseEvent);
    procedure DragSourceEndedAt(x, y: Integer; op: TCefDragOperation);
    procedure DragSourceSystemDragEnded;
    function  GetVisibleNavigationEntry : ICefNavigationEntry;
    procedure SetAccessibilityState(accessibilityState: TCefState);

    property Browser: ICefBrowser read GetBrowser;
    property WindowHandle: TCefWindowHandle read GetWindowHandle;
    property OpenerWindowHandle: TCefWindowHandle read GetOpenerWindowHandle;
    property ZoomLevel: Double read GetZoomLevel write SetZoomLevel;
    property RequestContext: ICefRequestContext read GetRequestContext;
    property VisibleNavigationEntry : ICefNavigationEntry read GetVisibleNavigationEntry;
  end;

  ICefProcessMessage = interface(ICefBaseRefCounted)
    ['{E0B1001A-8777-425A-869B-29D40B8B93B1}']
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefProcessMessage;
    function GetName: ustring;
    function GetArgumentList: ICefListValue;
    property Name: ustring read GetName;
    property ArgumentList: ICefListValue read GetArgumentList;
  end;

  ICefBrowser = interface(ICefBaseRefCounted)
  ['{BA003C2E-CF15-458F-9D4A-FE3CEFCF3EEF}']
    function GetHost: ICefBrowserHost;
    function CanGoBack: Boolean;
    procedure GoBack;
    function CanGoForward: Boolean;
    procedure GoForward;
    function IsLoading: Boolean;
    procedure Reload;
    procedure ReloadIgnoreCache;
    procedure StopLoad;
    function GetIdentifier: Integer;
    function IsSame(const that: ICefBrowser): Boolean;
    function IsPopup: Boolean;
    function HasDocument: Boolean;
    function GetMainFrame: ICefFrame;
    function GetFocusedFrame: ICefFrame;
    function GetFrameByident(identifier: Int64): ICefFrame;
    function GetFrame(const name: ustring): ICefFrame;
    function GetFrameCount: NativeUInt;
    procedure GetFrameIdentifiers(count: PNativeUInt; identifiers: PInt64);
    procedure GetFrameNames(names: TStrings);
    function SendProcessMessage(targetProcess: TCefProcessId; const ProcMessage: ICefProcessMessage): Boolean;
    property MainFrame: ICefFrame read GetMainFrame;
    property FocusedFrame: ICefFrame read GetFocusedFrame;
    property FrameCount: NativeUInt read GetFrameCount;
    property Host: ICefBrowserHost read GetHost;
    property Identifier: Integer read GetIdentifier;
  end;

  ICefPostDataElement = interface(ICefBaseRefCounted)
    ['{3353D1B8-0300-4ADC-8D74-4FF31C77D13C}']
    function IsReadOnly: Boolean;
    procedure SetToEmpty;
    procedure SetToFile(const fileName: ustring);
    procedure SetToBytes(size: NativeUInt; bytes: Pointer);
    function GetType: TCefPostDataElementType;
    function GetFile: ustring;
    function GetBytesCount: NativeUInt;
    function GetBytes(size: NativeUInt; bytes: Pointer): NativeUInt;
  end;

  ICefPostData = interface(ICefBaseRefCounted)
    ['{1E677630-9339-4732-BB99-D6FE4DE4AEC0}']
    function IsReadOnly: Boolean;
    function HasExcludedElements: Boolean;
    function GetCount: NativeUInt;
    function GetElements(Count: NativeUInt): IInterfaceList; // ICefPostDataElement
    function RemoveElement(const element: ICefPostDataElement): Integer;
    function AddElement(const element: ICefPostDataElement): Integer;
    procedure RemoveElements;
  end;

  ICefStringMap = interface
  ['{A33EBC01-B23A-4918-86A4-E24A243B342F}']
    function GetHandle: TCefStringMap;
    function GetSize: Integer;
    function Find(const Key: ustring): ustring;
    function GetKey(Index: Integer): ustring;
    function GetValue(Index: Integer): ustring;
    procedure Append(const Key, Value: ustring);
    procedure Clear;

    property Handle: TCefStringMap read GetHandle;
    property Size: Integer read GetSize;
    property Key[index: Integer]: ustring read GetKey;
    property Value[index: Integer]: ustring read GetValue;
  end;

  ICefStringMultimap = interface
    ['{583ED0C2-A9D6-4034-A7C9-20EC7E47F0C7}']
    function GetHandle: TCefStringMultimap;
    function GetSize: Integer;
    function FindCount(const Key: ustring): Integer;
    function GetEnumerate(const Key: ustring; ValueIndex: Integer): ustring;
    function GetKey(Index: Integer): ustring;
    function GetValue(Index: Integer): ustring;
    procedure Append(const Key, Value: ustring);
    procedure Clear;

    property Handle: TCefStringMap read GetHandle;
    property Size: Integer read GetSize;
    property Key[index: Integer]: ustring read GetKey;
    property Value[index: Integer]: ustring read GetValue;
    property Enumerate[const Key: ustring; ValueIndex: Integer]: ustring read GetEnumerate;
  end;

  ICefRequest = interface(ICefBaseRefCounted)
    ['{FB4718D3-7D13-4979-9F4C-D7F6C0EC592A}']
    function IsReadOnly: Boolean;
    function GetUrl: ustring;
    function GetMethod: ustring;
    function GetPostData: ICefPostData;
    procedure GetHeaderMap(const HeaderMap: ICefStringMultimap);
    procedure SetUrl(const value: ustring);
    procedure SetMethod(const value: ustring);
    procedure SetReferrer(const referrerUrl: ustring; policy: TCefReferrerPolicy);
    function GetReferrerUrl: ustring;
    function GetReferrerPolicy: TCefReferrerPolicy;
    procedure SetPostData(const value: ICefPostData);
    procedure SetHeaderMap(const HeaderMap: ICefStringMultimap);
    function GetFlags: TCefUrlRequestFlags;
    procedure SetFlags(flags: TCefUrlRequestFlags);
    function GetFirstPartyForCookies: ustring;
    procedure SetFirstPartyForCookies(const url: ustring);
    procedure Assign(const url, method: ustring;
      const postData: ICefPostData; const headerMap: ICefStringMultimap);
    function GetResourceType: TCefResourceType;
    function GetTransitionType: TCefTransitionType;

    function GetIdentifier: UInt64;

    property Url: ustring read GetUrl write SetUrl;
    property Method: ustring read GetMethod write SetMethod;
    property ReferrerUrl: ustring read GetReferrerUrl;
    property ReferrerPolicy: TCefReferrerPolicy read GetReferrerPolicy;
    property PostData: ICefPostData read GetPostData write SetPostData;
    property Flags: TCefUrlRequestFlags read GetFlags write SetFlags;
    property FirstPartyForCookies: ustring read GetFirstPartyForCookies write SetFirstPartyForCookies;
    property ResourceType: TCefResourceType read GetResourceType;
    property TransitionType: TCefTransitionType read GetTransitionType;
    property Identifier: UInt64 read GetIdentifier;
  end;

  ICefStringVisitor = interface(ICefBaseRefCounted)
    ['{63ED4D6C-2FC8-4537-964B-B84C008F6158}']
    procedure Visit(const str: ustring);
  end;

  ICefFrame = interface(ICefBaseRefCounted)
    ['{8FD3D3A6-EA3A-4A72-8501-0276BD5C3D1D}']
    function IsValid: Boolean;
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Del;
    procedure SelectAll;
    procedure ViewSource;
    procedure GetSource(const visitor: ICefStringVisitor);
    procedure GetSourceProc(const proc: TCefStringVisitorProc);
    procedure GetText(const visitor: ICefStringVisitor);
    procedure GetTextProc(const proc: TCefStringVisitorProc);
    procedure LoadRequest(const request: ICefRequest);
    procedure LoadUrl(const url: ustring);
    procedure LoadString(const str, url: ustring);
    procedure ExecuteJavaScript(const code, scriptUrl: ustring; startLine: Integer);
    function IsMain: Boolean;
    function IsFocused: Boolean;
    function GetName: ustring;
    function GetIdentifier: Int64;
    function GetParent: ICefFrame;
    function GetUrl: ustring;
    function GetBrowser: ICefBrowser;
    function GetV8Context: ICefv8Context;
    procedure VisitDom(const visitor: ICefDomVisitor);
    procedure VisitDomProc(const proc: TCefDomVisitorProc);
    property Name: ustring read GetName;
    property Url: ustring read GetUrl;
    property Browser: ICefBrowser read GetBrowser;
    property Parent: ICefFrame read GetParent;
  end;


  ICefCustomStreamReader = interface(ICefBaseRefCounted)
    ['{BBCFF23A-6FE7-4C28-B13E-6D2ACA5C83B7}']
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  end;

  ICefStreamReader = interface(ICefBaseRefCounted)
    ['{DD5361CB-E558-49C5-A4BD-D1CE84ADB277}']
    function Read(ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Eof: Boolean;
    function MayBlock: Boolean;
  end;

  ICefWriteHandler = interface(ICefBaseRefCounted)
    ['{F2431888-4EAB-421E-9EC3-320BE695AF30}']
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Flush: Integer;
    function MayBlock: Boolean;
  end;

  ICefStreamWriter = interface(ICefBaseRefCounted)
    ['{4AA6C477-7D8A-4D5A-A704-67F900A827E7}']
    function Write(const ptr: Pointer; size, n: NativeUInt): NativeUInt;
    function Seek(offset: Int64; whence: Integer): Integer;
    function Tell: Int64;
    function Flush: Integer;
    function MayBlock: Boolean;
  end;

  ICefResponse = interface(ICefBaseRefCounted)
  ['{E9C896E4-59A8-4B96-AB5E-6EA3A498B7F1}']
    function IsReadOnly: Boolean;
    function GetError: TCefErrorCode;
    procedure SetError(error: TCefErrorCode);
    function GetStatus: Integer;
    procedure SetStatus(status: Integer);
    function GetStatusText: ustring;
    procedure SetStatusText(const StatusText: ustring);
    function GetMimeType: ustring;
    procedure SetMimeType(const mimetype: ustring);
    function GetHeader(const name: ustring): ustring;
    procedure GetHeaderMap(const headerMap: ICefStringMultimap);
    procedure SetHeaderMap(const headerMap: ICefStringMultimap);
    property Status: Integer read GetStatus write SetStatus;
    property StatusText: ustring read GetStatusText write SetStatusText;
    property MimeType: ustring read GetMimeType write SetMimeType;
    property Error: TCefErrorCode read GetError write SetError;
  end;

  ICefDownloadItem = interface(ICefBaseRefCounted)
  ['{B34BD320-A82E-4185-8E84-B98E5EEC803F}']
    function IsValid: Boolean;
    function IsInProgress: Boolean;
    function IsComplete: Boolean;
    function IsCanceled: Boolean;
    function GetCurrentSpeed: Int64;
    function GetPercentComplete: Integer;
    function GetTotalBytes: Int64;
    function GetReceivedBytes: Int64;
    function GetStartTime: TDateTime;
    function GetEndTime: TDateTime;
    function GetFullPath: ustring;
    function GetId: Cardinal;
    function GetUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetSuggestedFileName: ustring;
    function GetContentDisposition: ustring;
    function GetMimeType: ustring;

    property CurrentSpeed: Int64 read GetCurrentSpeed;
    property PercentComplete: Integer read GetPercentComplete;
    property TotalBytes: Int64 read GetTotalBytes;
    property ReceivedBytes: Int64 read GetReceivedBytes;
    property StartTime: TDateTime read GetStartTime;
    property EndTime: TDateTime read GetEndTime;
    property FullPath: ustring read GetFullPath;
    property Id: Cardinal read GetId;
    property Url: ustring read GetUrl;
    property OriginalUrl: ustring read GetOriginalUrl;
    property SuggestedFileName: ustring read GetSuggestedFileName;
    property ContentDisposition: ustring read GetContentDisposition;
    property MimeType: ustring read GetMimeType;
  end;

  ICefBeforeDownloadCallback = interface(ICefBaseRefCounted)
  ['{5A81AF75-CBA2-444D-AD8E-522160F36433}']
    procedure Cont(const downloadPath: ustring; showDialog: Boolean);
  end;

  ICefDownloadItemCallback = interface(ICefBaseRefCounted)
  ['{498F103F-BE64-4D5F-86B7-B37EC69E1735}']
    procedure Cancel;
    procedure Pause;
    procedure Resume;
  end;

  ICefDownloadHandler = interface(ICefBaseRefCounted)
  ['{3137F90A-5DC5-43C1-858D-A269F28EF4F1}']
    procedure OnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure OnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const callback: ICefDownloadItemCallback);
  end;

  ICefV8Exception = interface(ICefBaseRefCounted)
    ['{7E422CF0-05AC-4A60-A029-F45105DCE6A4}']
    function GetMessage: ustring;
    function GetSourceLine: ustring;
    function GetScriptResourceName: ustring;
    function GetLineNumber: Integer;
    function GetStartPosition: Integer;
    function GetEndPosition: Integer;
    function GetStartColumn: Integer;
    function GetEndColumn: Integer;

    property Message: ustring read GetMessage;
    property SourceLine: ustring read GetSourceLine;
    property ScriptResourceName: ustring read GetScriptResourceName;
    property LineNumber: Integer read GetLineNumber;
    property StartPosition: Integer read GetStartPosition;
    property EndPosition: Integer read GetEndPosition;
    property StartColumn: Integer read GetStartColumn;
    property EndColumn: Integer read GetEndColumn;
  end;

  ICefv8Context = interface(ICefBaseRefCounted)
    ['{2295A11A-8773-41F2-AD42-308C215062D9}']
    function GetTaskRunner: ICefTaskRunner;
    function IsValid: Boolean;
    function GetBrowser: ICefBrowser;
    function GetFrame: ICefFrame;
    function GetGlobal: ICefv8Value;
    function Enter: Boolean;
    function Exit: Boolean;
    function IsSame(const that: ICefv8Context): Boolean;
    function Eval(const code: ustring; const script_url: ustring; start_line: integer; var retval: ICefv8Value; var exception: ICefV8Exception): Boolean;
    property Browser: ICefBrowser read GetBrowser;
    property Frame: ICefFrame read GetFrame;
    property Global: ICefv8Value read GetGlobal;
  end;

  ICefv8Handler = interface(ICefBaseRefCounted)
    ['{F94CDC60-FDCB-422D-96D5-D2A775BD5D73}']
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
  end;

  ICefV8Interceptor = interface(ICefBaseRefCounted)
    ['{B3B8FD7C-A916-4B25-93A2-2892AC324F21}']
    function GetByName(const name: ustring; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
    function GetByIndex(index: integer; const obj: ICefv8Value; out retval: ICefv8Value; const exception: ustring): boolean;
    function SetByName(const name: ustring; const obj: ICefv8Value; const value: ICefv8Value; const exception: ustring): boolean;
    function SetByIndex(index: integer; const obj: ICefv8Value; const value: ICefv8Value; const exception: ustring): boolean;
  end;

  ICefV8Accessor = interface(ICefBaseRefCounted)
    ['{DCA6D4A2-726A-4E24-AA64-5E8C731D868A}']
    function Get(const name: ustring; const obj: ICefv8Value; out value: ICefv8Value; const exception: ustring): Boolean;
    function Put(const name: ustring; const obj: ICefv8Value; const value: ICefv8Value; const exception: ustring): Boolean;
  end;

  ICefTask = interface(ICefBaseRefCounted)
    ['{0D965470-4A86-47CE-BD39-A8770021AD7E}']
    procedure Execute;
  end;

  ICefTaskRunner = interface(ICefBaseRefCounted)
  ['{6A500FA3-77B7-4418-8EA8-6337EED1337B}']
    function IsSame(const that: ICefTaskRunner): Boolean;
    function BelongsToCurrentThread: Boolean;
    function BelongsToThread(threadId: TCefThreadId): Boolean;
    function PostTask(const task: ICefTask): Boolean; stdcall;
    function PostDelayedTask(const task: ICefTask; delayMs: Int64): Boolean;
  end;

  ICefThread = interface(ICefBaseRefCounted)
    ['{26B30EA5-F44A-4C40-97DF-67FD9E73A4FF}']
    function  GetTaskRunner : ICefTaskRunner;
    function  GetPlatformThreadID : TCefPlatformThreadId;
    procedure Stop;
    function  IsRunning : boolean;
  end;

  ICefWaitableEvent = interface(ICefBaseRefCounted)
    ['{965C90C9-3DAE-457F-AA64-E04FF508094A}']
    procedure Reset;
    procedure Signal;
    function  IsSignaled : boolean;
    procedure Wait;
    function  TimedWait(max_ms: int64): boolean;
  end;

  ICefv8Value = interface(ICefBaseRefCounted)
  ['{52319B8D-75A8-422C-BD4B-16FA08CC7F42}']
    function IsValid: Boolean;
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsBool: Boolean;
    function IsInt: Boolean;
    function IsUInt: Boolean;
    function IsDouble: Boolean;
    function IsDate: Boolean;
    function IsString: Boolean;
    function IsObject: Boolean;
    function IsArray: Boolean;
    function IsFunction: Boolean;
    function IsSame(const that: ICefv8Value): Boolean;
    function GetBoolValue: Boolean;
    function GetIntValue: Integer;
    function GetUIntValue: Cardinal;
    function GetDoubleValue: Double;
    function GetDateValue: TDateTime;
    function GetStringValue: ustring;
    function IsUserCreated: Boolean;
    function HasException: Boolean;
    function GetException: ICefV8Exception;
    function ClearException: Boolean;
    function WillRethrowExceptions: Boolean;
    function SetRethrowExceptions(rethrow: Boolean): Boolean;
    function HasValueByKey(const key: ustring): Boolean;
    function HasValueByIndex(index: Integer): Boolean;
    function DeleteValueByKey(const key: ustring): Boolean;
    function DeleteValueByIndex(index: Integer): Boolean;
    function GetValueByKey(const key: ustring): ICefv8Value;
    function GetValueByIndex(index: Integer): ICefv8Value;
    function SetValueByKey(const key: ustring; const value: ICefv8Value;
      attribute: TCefV8PropertyAttributes): Boolean;
    function SetValueByIndex(index: Integer; const value: ICefv8Value): Boolean;
    function SetValueByAccessor(const key: ustring; settings: TCefV8AccessControls;
      attribute: TCefV8PropertyAttributes): Boolean;
    function GetKeys(const keys: TStrings): Integer;
    function SetUserData(const data: ICefv8Value): Boolean;
    function GetUserData: ICefv8Value;
    function GetExternallyAllocatedMemory: Integer;
    function AdjustExternallyAllocatedMemory(changeInBytes: Integer): Integer;
    function GetArrayLength: Integer;
    function GetFunctionName: ustring;
    function GetFunctionHandler: ICefv8Handler;
    function ExecuteFunction(const obj: ICefv8Value;
      const arguments: TCefv8ValueArray): ICefv8Value;
    function ExecuteFunctionWithContext(const context: ICefv8Context;
      const obj: ICefv8Value; const arguments: TCefv8ValueArray): ICefv8Value;
  end;

  ICefV8StackFrame = interface(ICefBaseRefCounted)
  ['{BA1FFBF4-E9F2-4842-A827-DC220F324286}']
    function IsValid: Boolean;
    function GetScriptName: ustring;
    function GetScriptNameOrSourceUrl: ustring;
    function GetFunctionName: ustring;
    function GetLineNumber: Integer;
    function GetColumn: Integer;
    function IsEval: Boolean;
    function IsConstructor: Boolean;

    property ScriptName: ustring read GetScriptName;
    property ScriptNameOrSourceUrl: ustring read GetScriptNameOrSourceUrl;
    property FunctionName: ustring read GetFunctionName;
    property LineNumber: Integer read GetLineNumber;
    property Column: Integer read GetColumn;
  end;

  ICefV8StackTrace = interface(ICefBaseRefCounted)
  ['{32111C84-B7F7-4E3A-92B9-7CA1D0ADB613}']
    function IsValid: Boolean;
    function GetFrameCount: Integer;
    function GetFrame(index: Integer): ICefV8StackFrame;
    property FrameCount: Integer read GetFrameCount;
    property Frame[index: Integer]: ICefV8StackFrame read GetFrame;
  end;

  ICefXmlReader = interface(ICefBaseRefCounted)
  ['{0DE686C3-A8D7-45D2-82FD-92F7F4E62A90}']
    function MoveToNextNode: Boolean;
    function Close: Boolean;
    function HasError: Boolean;
    function GetError: ustring;
    function GetType: TCefXmlNodeType;
    function GetDepth: Integer;
    function GetLocalName: ustring;
    function GetPrefix: ustring;
    function GetQualifiedName: ustring;
    function GetNamespaceUri: ustring;
    function GetBaseUri: ustring;
    function GetXmlLang: ustring;
    function IsEmptyElement: Boolean;
    function HasValue: Boolean;
    function GetValue: ustring;
    function HasAttributes: Boolean;
    function GetAttributeCount: NativeUInt;
    function GetAttributeByIndex(index: Integer): ustring;
    function GetAttributeByQName(const qualifiedName: ustring): ustring;
    function GetAttributeByLName(const localName, namespaceURI: ustring): ustring;
    function GetInnerXml: ustring;
    function GetOuterXml: ustring;
    function GetLineNumber: Integer;
    function MoveToAttributeByIndex(index: Integer): Boolean;
    function MoveToAttributeByQName(const qualifiedName: ustring): Boolean;
    function MoveToAttributeByLName(const localName, namespaceURI: ustring): Boolean;
    function MoveToFirstAttribute: Boolean;
    function MoveToNextAttribute: Boolean;
    function MoveToCarryingElement: Boolean;
  end;

  ICefZipReader = interface(ICefBaseRefCounted)
  ['{3B6C591F-9877-42B3-8892-AA7B27DA34A8}']
    function MoveToFirstFile: Boolean;
    function MoveToNextFile: Boolean;
    function MoveToFile(const fileName: ustring; caseSensitive: Boolean): Boolean;
    function Close: Boolean;
    function GetFileName: ustring;
    function GetFileSize: Int64;
    function GetFileLastModified: TCefTime;
    function OpenFile(const password: ustring): Boolean;
    function CloseFile: Boolean;
    function ReadFile(buffer: Pointer; bufferSize: NativeUInt): Integer;
    function Tell: Int64;
    function Eof: Boolean;
  end;

  ICefDomNode = interface(ICefBaseRefCounted)
  ['{96C03C9E-9C98-491A-8DAD-1947332232D6}']
    function GetType: TCefDomNodeType;
    function IsText: Boolean;
    function IsElement: Boolean;
    function IsEditable: Boolean;
    function IsFormControlElement: Boolean;
    function GetFormControlElementType: ustring;
    function IsSame(const that: ICefDomNode): Boolean;
    function GetName: ustring;
    function GetValue: ustring;
    function SetValue(const value: ustring): Boolean;
    function GetAsMarkup: ustring;
    function GetDocument: ICefDomDocument;
    function GetParent: ICefDomNode;
    function GetPreviousSibling: ICefDomNode;
    function GetNextSibling: ICefDomNode;
    function HasChildren: Boolean;
    function GetFirstChild: ICefDomNode;
    function GetLastChild: ICefDomNode;
    function GetElementTagName: ustring;
    function HasElementAttributes: Boolean;
    function HasElementAttribute(const attrName: ustring): Boolean;
    function GetElementAttribute(const attrName: ustring): ustring;
    procedure GetElementAttributes(const attrMap: ICefStringMap);
    function SetElementAttribute(const attrName, value: ustring): Boolean;
    function GetElementInnerText: ustring;
    function GetElementBounds: TCefRect;

    property NodeType: TCefDomNodeType read GetType;
    property Name: ustring read GetName;
    property AsMarkup: ustring read GetAsMarkup;
    property Document: ICefDomDocument read GetDocument;
    property Parent: ICefDomNode read GetParent;
    property PreviousSibling: ICefDomNode read GetPreviousSibling;
    property NextSibling: ICefDomNode read GetNextSibling;
    property FirstChild: ICefDomNode read GetFirstChild;
    property LastChild: ICefDomNode read GetLastChild;
    property ElementTagName: ustring read GetElementTagName;
    property ElementInnerText: ustring read GetElementInnerText;
    property ElementBounds: TCefRect read GetElementBounds;
  end;

  ICefDomDocument = interface(ICefBaseRefCounted)
  ['{08E74052-45AF-4F69-A578-98A5C3959426}']
    function GetType: TCefDomDocumentType;
    function GetDocument: ICefDomNode;
    function GetBody: ICefDomNode;
    function GetHead: ICefDomNode;
    function GetTitle: ustring;
    function GetElementById(const id: ustring): ICefDomNode;
    function GetFocusedNode: ICefDomNode;
    function HasSelection: Boolean;
    function GetSelectionStartOffset: Integer;
    function GetSelectionEndOffset: Integer;
    function GetSelectionAsMarkup: ustring;
    function GetSelectionAsText: ustring;
    function GetBaseUrl: ustring;
    function GetCompleteUrl(const partialURL: ustring): ustring;
    property DocType: TCefDomDocumentType read GetType;
    property Document: ICefDomNode read GetDocument;
    property Body: ICefDomNode read GetBody;
    property Head: ICefDomNode read GetHead;
    property Title: ustring read GetTitle;
    property FocusedNode: ICefDomNode read GetFocusedNode;
    property SelectionStartOffset: Integer read GetSelectionStartOffset;
    property SelectionEndOffset: Integer read GetSelectionEndOffset;
    property SelectionAsMarkup: ustring read GetSelectionAsMarkup;
    property SelectionAsText: ustring read GetSelectionAsText;
    property BaseUrl: ustring read GetBaseUrl;
  end;

  ICefDomVisitor = interface(ICefBaseRefCounted)
  ['{30398428-3196-4531-B968-2DDBED36F6B0}']
    procedure visit(const document: ICefDomDocument);
  end;

  ICefCookieVisitor = interface(ICefBaseRefCounted)
  ['{8378CF1B-84AB-4FDB-9B86-34DDABCCC402}']
    function visit(const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      count, total: Integer; out deleteCookie: Boolean): Boolean;
  end;

  ICefResourceBundleHandler = interface(ICefBaseRefCounted)
    ['{09C264FD-7E03-41E3-87B3-4234E82B5EA2}']
    function GetLocalizedString(stringId: Integer; out stringVal: ustring): Boolean;
    function GetDataResource(resourceId: Integer; out data: Pointer; out dataSize: NativeUInt): Boolean;
  end;

  ICefCommandLine = interface(ICefBaseRefCounted)
  ['{6B43D21B-0F2C-4B94-B4E6-4AF0D7669D8E}']
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefCommandLine;
    procedure InitFromArgv(argc: Integer; const argv: PPAnsiChar);
    procedure InitFromString(const commandLine: ustring);
    procedure Reset;
    function GetCommandLineString: ustring;
    procedure GetArgv(args: TStrings);
    function GetProgram: ustring;
    procedure SetProgram(const prog: ustring);
    function HasSwitches: Boolean;
    function HasSwitch(const name: ustring): Boolean;
    function GetSwitchValue(const name: ustring): ustring;
    procedure GetSwitches(switches: TStrings);
    procedure AppendSwitch(const name: ustring);
    procedure AppendSwitchWithValue(const name, value: ustring);
    function HasArguments: Boolean;
    procedure GetArguments(arguments: TStrings);
    procedure AppendArgument(const argument: ustring);
    procedure PrependWrapper(const wrapper: ustring);
    property CommandLineString: ustring read GetCommandLineString;
  end;

  ICefBrowserProcessHandler = interface(ICefBaseRefCounted)
  ['{27291B7A-C0AE-4EE0-9115-15C810E22F6C}']
    procedure OnContextInitialized;
    procedure OnBeforeChildProcessLaunch(const commandLine: ICefCommandLine);
    procedure OnRenderProcessThreadCreated(const extraInfo: ICefListValue);
    procedure OnScheduleMessagePumpWork(delayMs: Int64);
  end;

  ICefRenderProcessHandler = interface(ICefBaseRefCounted)
  ['{FADEE3BC-BF66-430A-BA5D-1EE3782ECC58}']
    procedure OnRenderThreadCreated(const extraInfo: ICefListValue) ;
    procedure OnWebKitInitialized;
    procedure OnBrowserCreated(const browser: ICefBrowser);
    procedure OnBrowserDestroyed(const browser: ICefBrowser);
    procedure OnContextCreated(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context);
    procedure OnContextReleased(const browser: ICefBrowser;
      const frame: ICefFrame; const context: ICefv8Context);
    procedure OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame;
      const context: ICefv8Context; const exception: ICefV8Exception;
      const stackTrace: ICefV8StackTrace);
    procedure OnFocusedNodeChanged(const browser: ICefBrowser;
      const frame: ICefFrame; const node: ICefDomNode);
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
  end;

  ICefApp = interface(ICefBaseRefCounted)
    ['{970CA670-9070-4642-B188-7D8A22DAEED4}']
    procedure OnBeforeCommandLineProcessing(const processType: ustring; const commandLine: ICefCommandLine);
    procedure OnRegisterCustomSchemes(const registrar: TCefSchemeRegistrarRef);
    function  GetResourceBundleHandler: ICefResourceBundleHandler;
    function  GetBrowserProcessHandler: ICefBrowserProcessHandler;
    function  GetRenderProcessHandler: ICefRenderProcessHandler;
  end;

  TCefCookieVisitorProc = {$IFDEF DELPHI12_UP}reference to{$ENDIF} function(
    const name, value, domain, path: ustring; secure, httponly,
    hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
    count, total: Integer; out deleteCookie: Boolean): Boolean;

  ICefCompletionCallback = interface(ICefBaseRefCounted)
    ['{A8ECCFBB-FEE0-446F-AB32-AD69A7478D57}']
    procedure OnComplete;
  end;

  ICefSetCookieCallback = interface(ICefBaseRefCounted)
    ['{16E14B6F-CB0A-4F9D-A008-239E0BC7B892}']
    procedure OnComplete(success: Boolean);
  end;

  ICefDeleteCookiesCallback = interface(ICefBaseRefCounted)
    ['{758B79A1-B9E8-4F0D-94A0-DCE5AFADE33D}']
    procedure OnComplete(numDeleted: Integer);
  end;

  ICefCookieManager = Interface(ICefBaseRefCounted)
    ['{CC1749E6-9AD3-4283-8430-AF6CBF3E8785}']
    procedure SetSupportedSchemes(schemes: TStrings; const callback: ICefCompletionCallback);
    procedure SetSupportedSchemesProc(schemes: TStrings; const callback: TCefCompletionCallbackProc);
    function VisitAllCookies(const visitor: ICefCookieVisitor): Boolean;
    function VisitAllCookiesProc(const visitor: TCefCookieVisitorProc): Boolean;
    function VisitUrlCookies(const url: ustring;
      includeHttpOnly: Boolean; const visitor: ICefCookieVisitor): Boolean;
    function VisitUrlCookiesProc(const url: ustring;
      includeHttpOnly: Boolean; const visitor: TCefCookieVisitorProc): Boolean;
    function SetCookie(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      const callback: ICefSetCookieCallback): Boolean;
    function SetCookieProc(const url: ustring; const name, value, domain, path: ustring; secure, httponly,
      hasExpires: Boolean; const creation, lastAccess, expires: TDateTime;
      const callback: TCefSetCookieCallbackProc): Boolean;
    function DeleteCookies(const url, cookieName: ustring; const callback: ICefDeleteCookiesCallback): Boolean;
    function DeleteCookiesProc(const url, cookieName: ustring; const callback: TCefDeleteCookiesCallbackProc): Boolean;
    function SetStoragePath(const path: ustring; persistSessionCookies: Boolean; const callback: ICefCompletionCallback): Boolean;
    function SetStoragePathProc(const path: ustring; persistSessionCookies: Boolean; const callback: TCefCompletionCallbackProc): Boolean;
    function FlushStore(const handler: ICefCompletionCallback): Boolean;
    function FlushStoreProc(const proc: TCefCompletionCallbackProc): Boolean;
  end;

  ICefWebPluginInfo = interface(ICefBaseRefCounted)
    ['{AA879E58-F649-44B1-AF9C-655FF5B79A02}']
    function GetName: ustring;
    function GetPath: ustring;
    function GetVersion: ustring;
    function GetDescription: ustring;

    property Name: ustring read GetName;
    property Path: ustring read GetPath;
    property Version: ustring read GetVersion;
    property Description: ustring read GetDescription;
  end;

  ICefCallback = interface(ICefBaseRefCounted)
  ['{1B8C449F-E2D6-4B78-9BBA-6F47E8BCDF37}']
    procedure Cont;
    procedure Cancel;
  end;

  ICefResourceHandler = interface(ICefBaseRefCounted)
  ['{BD3EA208-AAAD-488C-BFF2-76993022F2B5}']
    function ProcessRequest(const request: ICefRequest; const callback: ICefCallback): Boolean;
    procedure GetResponseHeaders(const response: ICefResponse;
      out responseLength: Int64; out redirectUrl: ustring);
    function ReadResponse(const dataOut: Pointer; bytesToRead: Integer;
      var bytesRead: Integer; const callback: ICefCallback): Boolean;
    function CanGetCookie(const cookie: PCefCookie): Boolean;
    function CanSetCookie(const cookie: PCefCookie): Boolean;
    procedure Cancel;
  end;

  ICefSchemeHandlerFactory = interface(ICefBaseRefCounted)
    ['{4D9B7960-B73B-4EBD-9ABE-6C1C43C245EB}']
    function New(const browser: ICefBrowser; const frame: ICefFrame;
      const schemeName: ustring; const request: ICefRequest): ICefResourceHandler;
  end;

  ICefAuthCallback = interface(ICefBaseRefCounted)
  ['{500C2023-BF4D-4FF7-9C04-165E5C389131}']
    procedure Cont(const username, password: ustring);
    procedure Cancel;
  end;

  ICefJsDialogCallback = interface(ICefBaseRefCounted)
  ['{187B2156-9947-4108-87AB-32E559E1B026}']
    procedure Cont(success: Boolean; const userInput: ustring);
  end;

  ICefContextMenuParams = interface(ICefBaseRefCounted)
  ['{E31BFA9E-D4E2-49B7-A05D-20018C8794EB}']
    function GetXCoord: Integer;
    function GetYCoord: Integer;
    function GetTypeFlags: TCefContextMenuTypeFlags;
    function GetLinkUrl: ustring;
    function GetUnfilteredLinkUrl: ustring;
    function GetSourceUrl: ustring;
    function HasImageContents: Boolean;
    function GetTitleText: ustring;
    function GetPageUrl: ustring;
    function GetFrameUrl: ustring;
    function GetFrameCharset: ustring;
    function GetMediaType: TCefContextMenuMediaType;
    function GetMediaStateFlags: TCefContextMenuMediaStateFlags;
    function GetSelectionText: ustring;
    function GetMisspelledWord: ustring;
    function GetDictionarySuggestions(const suggestions: TStringList): Boolean;
    function IsEditable: Boolean;
    function IsSpellCheckEnabled: Boolean;
    function GetEditStateFlags: TCefContextMenuEditStateFlags;
    function IsCustomMenu: Boolean;
    function IsPepperMenu: Boolean;

    property XCoord: Integer read GetXCoord;
    property YCoord: Integer read GetYCoord;
    property TypeFlags: TCefContextMenuTypeFlags read GetTypeFlags;
    property LinkUrl: ustring read GetLinkUrl;
    property UnfilteredLinkUrl: ustring read GetUnfilteredLinkUrl;
    property SourceUrl: ustring read GetSourceUrl;
    property TitleText: ustring read GetTitleText;
    property PageUrl: ustring read GetPageUrl;
    property FrameUrl: ustring read GetFrameUrl;
    property FrameCharset: ustring read GetFrameCharset;
    property MediaType: TCefContextMenuMediaType read GetMediaType;
    property MediaStateFlags: TCefContextMenuMediaStateFlags read GetMediaStateFlags;
    property SelectionText: ustring read GetSelectionText;
    property EditStateFlags: TCefContextMenuEditStateFlags read GetEditStateFlags;
  end;

  ICefMenuModel = interface(ICefBaseRefCounted)
  ['{40AF19D3-8B4E-44B8-8F89-DEB5907FC495}']
    function IsSubMenu: Boolean;
    function Clear: Boolean;
    function GetCount: Integer;
    function AddSeparator: Boolean;
    function AddItem(commandId: Integer; const text: ustring): Boolean;
    function AddCheckItem(commandId: Integer; const text: ustring): Boolean;
    function AddRadioItem(commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function AddSubMenu(commandId: Integer; const text: ustring): ICefMenuModel;
    function InsertSeparatorAt(index: Integer): Boolean;
    function InsertItemAt(index, commandId: Integer; const text: ustring): Boolean;
    function InsertCheckItemAt(index, commandId: Integer; const text: ustring): Boolean;
    function InsertRadioItemAt(index, commandId: Integer; const text: ustring; groupId: Integer): Boolean;
    function InsertSubMenuAt(index, commandId: Integer; const text: ustring): ICefMenuModel;
    function Remove(commandId: Integer): Boolean;
    function RemoveAt(index: Integer): Boolean;
    function GetIndexOf(commandId: Integer): Integer;
    function GetCommandIdAt(index: Integer): Integer;
    function SetCommandIdAt(index, commandId: Integer): Boolean;
    function GetLabel(commandId: Integer): ustring;
    function GetLabelAt(index: Integer): ustring;
    function SetLabel(commandId: Integer; const text: ustring): Boolean;
    function SetLabelAt(index: Integer; const text: ustring): Boolean;
    function GetType(commandId: Integer): TCefMenuItemType;
    function GetTypeAt(index: Integer): TCefMenuItemType;
    function GetGroupId(commandId: Integer): Integer;
    function GetGroupIdAt(index: Integer): Integer;
    function SetGroupId(commandId, groupId: Integer): Boolean;
    function SetGroupIdAt(index, groupId: Integer): Boolean;
    function GetSubMenu(commandId: Integer): ICefMenuModel;
    function GetSubMenuAt(index: Integer): ICefMenuModel;
    function IsVisible(commandId: Integer): Boolean;
    function isVisibleAt(index: Integer): Boolean;
    function SetVisible(commandId: Integer; visible: Boolean): Boolean;
    function SetVisibleAt(index: Integer; visible: Boolean): Boolean;
    function IsEnabled(commandId: Integer): Boolean;
    function IsEnabledAt(index: Integer): Boolean;
    function SetEnabled(commandId: Integer; enabled: Boolean): Boolean;
    function SetEnabledAt(index: Integer; enabled: Boolean): Boolean;
    function IsChecked(commandId: Integer): Boolean;
    function IsCheckedAt(index: Integer): Boolean;
    function setChecked(commandId: Integer; checked: Boolean): Boolean;
    function setCheckedAt(index: Integer; checked: Boolean): Boolean;
    function HasAccelerator(commandId: Integer): Boolean;
    function HasAcceleratorAt(index: Integer): Boolean;
    function SetAccelerator(commandId, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetAcceleratorAt(index, keyCode: Integer; shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function RemoveAccelerator(commandId: Integer): Boolean;
    function RemoveAcceleratorAt(index: Integer): Boolean;
    function GetAccelerator(commandId: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function GetAcceleratorAt(index: Integer; out keyCode: Integer; out shiftPressed, ctrlPressed, altPressed: Boolean): Boolean;
    function SetColor(commandId: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function SetColorAt(index: Integer; colorType: TCefMenuColorType; color: TCefColor): Boolean;
    function GetColor(commandId: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function GetColorAt(index: Integer; colorType: TCefMenuColorType; out color: TCefColor): Boolean;
    function SetFontList(commandId: Integer; const fontList: ustring): Boolean;
    function SetFontListAt(index: Integer; const fontList: ustring): Boolean;
  end;

  ICefValue = interface(ICefBaseRefCounted)
  ['{66F9F439-B12B-4EC3-A945-91AE4EF4D4BA}']
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefValue): Boolean;
    function IsEqual(const that: ICefValue): Boolean;

    function Copy: ICefValue;

    function GetType: TCefValueType;
    function GetBool: Boolean;
    function GetInt: Integer;
    function GetDouble: Double;
    function GetString: ustring;
    function GetBinary: ICefBinaryValue;
    function GetDictionary: ICefDictionaryValue;
    function GetList: ICefListValue;

    function SetNull: Boolean;
    function SetBool(value: Integer): Boolean;
    function SetInt(value: Integer): Boolean;
    function SetDouble(value: Double): Boolean;
    function SetString(const value: ustring): Boolean;
    function SetBinary(const value: ICefBinaryValue): Boolean;
    function SetDictionary(const value: ICefDictionaryValue): Boolean;
    function SetList(const value: ICefListValue): Boolean;
  end;

  ICefBinaryValue = interface(ICefBaseRefCounted)
  ['{974AA40A-9C5C-4726-81F0-9F0D46D7C5B3}']
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsSame(const that: ICefBinaryValue): Boolean;
    function IsEqual(const that: ICefBinaryValue): Boolean;
    function Copy: ICefBinaryValue;
    function GetSize: NativeUInt;
    function GetData(buffer: Pointer; bufferSize, dataOffset: NativeUInt): NativeUInt;
  end;

  ICefDictionaryValue = interface(ICefBaseRefCounted)
  ['{B9638559-54DC-498C-8185-233EEF12BC69}']
    function IsValid: Boolean;
    function isOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefDictionaryValue): Boolean;
    function IsEqual(const that: ICefDictionaryValue): Boolean;
    function Copy(excludeEmptyChildren: Boolean): ICefDictionaryValue;
    function GetSize: NativeUInt;
    function Clear: Boolean;
    function HasKey(const key: ustring): Boolean;
    function GetKeys(const keys: TStrings): Boolean;
    function Remove(const key: ustring): Boolean;
    function GetType(const key: ustring): TCefValueType;
    function GetValue(const key: ustring): ICefValue;
    function GetBool(const key: ustring): Boolean;
    function GetInt(const key: ustring): Integer;
    function GetDouble(const key: ustring): Double;
    function GetString(const key: ustring): ustring;
    function GetBinary(const key: ustring): ICefBinaryValue;
    function GetDictionary(const key: ustring): ICefDictionaryValue;
    function GetList(const key: ustring): ICefListValue;
    function SetValue(const key: ustring; const value: ICefValue): Boolean;
    function SetNull(const key: ustring): Boolean;
    function SetBool(const key: ustring; value: Boolean): Boolean;
    function SetInt(const key: ustring; value: Integer): Boolean;
    function SetDouble(const key: ustring; value: Double): Boolean;
    function SetString(const key, value: ustring): Boolean;
    function SetBinary(const key: ustring; const value: ICefBinaryValue): Boolean;
    function SetDictionary(const key: ustring; const value: ICefDictionaryValue): Boolean;
    function SetList(const key: ustring; const value: ICefListValue): Boolean;
  end;


  ICefListValue = interface(ICefBaseRefCounted)
  ['{09174B9D-0CC6-4360-BBB0-3CC0117F70F6}']
    function IsValid: Boolean;
    function IsOwned: Boolean;
    function IsReadOnly: Boolean;
    function IsSame(const that: ICefListValue): Boolean;
    function IsEqual(const that: ICefListValue): Boolean;
    function Copy: ICefListValue;
    function SetSize(size: NativeUInt): Boolean;
    function GetSize: NativeUInt;
    function Clear: Boolean;
    function Remove(index: NativeUInt): Boolean;
    function GetType(index: NativeUInt): TCefValueType;
    function GetValue(index: NativeUInt): ICefValue;
    function GetBool(index: NativeUInt): Boolean;
    function GetInt(index: NativeUInt): Integer;
    function GetDouble(index: NativeUInt): Double;
    function GetString(index: NativeUInt): ustring;
    function GetBinary(index: NativeUInt): ICefBinaryValue;
    function GetDictionary(index: NativeUInt): ICefDictionaryValue;
    function GetList(index: NativeUInt): ICefListValue;
    function SetValue(index: NativeUInt; const value: ICefValue): Boolean;
    function SetNull(index: NativeUInt): Boolean;
    function SetBool(index: NativeUInt; value: Boolean): Boolean;
    function SetInt(index: NativeUInt; value: Integer): Boolean;
    function SetDouble(index: NativeUInt; value: Double): Boolean;
    function SetString(index: NativeUInt; const value: ustring): Boolean;
    function SetBinary(index: NativeUInt; const value: ICefBinaryValue): Boolean;
    function SetDictionary(index: NativeUInt; const value: ICefDictionaryValue): Boolean;
    function SetList(index: NativeUInt; const value: ICefListValue): Boolean;
  end;


  ICefLifeSpanHandler = interface(ICefBaseRefCounted)
  ['{0A3EB782-A319-4C35-9B46-09B2834D7169}']
    function OnBeforePopup(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean;
    procedure OnAfterCreated(const browser: ICefBrowser);
    procedure OnBeforeClose(const browser: ICefBrowser);
    function DoClose(const browser: ICefBrowser): Boolean;
  end;

  ICefLoadHandler = interface(ICefBaseRefCounted)
  ['{2C63FB82-345D-4A5B-9858-5AE7A85C9F49}']
    procedure OnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure OnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure OnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure OnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring);
  end;

  ICefRequestCallback = interface(ICefBaseRefCounted)
  ['{A35B8FD5-226B-41A8-A763-1940787D321C}']
    procedure Cont(allow: Boolean);
    procedure Cancel;
  end;

  ICefResponseFilter = interface(ICefBaseRefCounted)
  ['{5013BC3C-F1AE-407A-A571-A4C6B1D6831E}']
    function InitFilter: Boolean;
    function Filter(dataIn: Pointer; dataInSize : NativeUInt; dataInRead: PNativeUInt; dataOut: Pointer; dataOutSize : NativeUInt; dataOutWritten: PNativeUInt): TCefResponseFilterStatus;
  end;

  ICefRequestHandler = interface(ICefBaseRefCounted)
  ['{050877A9-D1F8-4EB3-B58E-50DC3E3D39FD}']
    function  OnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; isRedirect: Boolean): Boolean;
    function  OnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean): Boolean;
    function  OnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
    function  GetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest): ICefResourceHandler;
    procedure OnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    function  OnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): Boolean;
    function  GetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
    procedure OnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus; receivedContentLength: Int64);
    function  GetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
    function  OnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring; newSize: Int64; const callback: ICefRequestCallback): Boolean;
    procedure OnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
    function  OnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
    function  OnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure OnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
    procedure OnRenderViewReady(const browser: ICefBrowser);
    procedure OnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);
  end;

  ICefDisplayHandler = interface(ICefBaseRefCounted)
  ['{1EC7C76D-6969-41D1-B26D-079BCFF054C4}']
    procedure OnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure OnTitleChange(const browser: ICefBrowser; const title: ustring);
    procedure OnFaviconUrlChange(const browser: ICefBrowser; icon_urls: TStrings);
    procedure OnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
    function OnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
    procedure OnStatusMessage(const browser: ICefBrowser; const value: ustring);
    function OnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: Integer): Boolean;
  end;

  ICefFocusHandler = interface(ICefBaseRefCounted)
  ['{BB7FA3FA-7B1A-4ADC-8E50-12A24018DD90}']
    procedure OnTakeFocus(const browser: ICefBrowser; next: Boolean);
    function OnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
    procedure OnGotFocus(const browser: ICefBrowser);
  end;

  ICefKeyboardHandler = interface(ICefBaseRefCounted)
  ['{0512F4EC-ED88-44C9-90D3-5C6D03D3B146}']
    function OnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function OnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean;
  end;

  ICefJsDialogHandler = interface(ICefBaseRefCounted)
  ['{64E18F86-DAC5-4ED1-8589-44DE45B9DB56}']
    function OnJsdialog(const browser: ICefBrowser; const originUrl: ustring;
      dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
      const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function OnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean;
    procedure OnResetDialogState(const browser: ICefBrowser);
    procedure OnDialogClosed(const browser: ICefBrowser);
  end;

  ICefRunContextMenuCallback = interface(ICefBaseRefCounted)
  ['{44C3C6E3-B64D-4F6E-A318-4A0F3A72EB00}']
    procedure Cont(commandId: Integer; eventFlags: TCefEventFlags);
    procedure Cancel;
  end;

  ICefContextMenuHandler = interface(ICefBaseRefCounted)
  ['{C2951895-4087-49D5-BA18-4D9BA4F5EDD7}']
    procedure OnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    function RunContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel;
      const callback: ICefRunContextMenuCallback): Boolean;
    function OnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags): Boolean;
    procedure OnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);
  end;

  ICefAccessibilityHandler = interface(ICefBaseRefCounted)
  ['{1878C3C7-7692-44AB-BFE0-6C387106816B}']
    procedure OnAccessibilityTreeChange(const value: ICefValue);
    procedure OnAccessibilityLocationChange(const value: ICefValue);
  end;

  ICefDialogHandler = interface(ICefBaseRefCounted)
  ['{7763F4B2-8BE1-4E80-AC43-8B825850DC67}']
    function OnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFilePath: ustring; acceptFilters: TStrings;
      selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean;
  end;

  ICefGeolocationCallback = interface(ICefBaseRefCounted)
  ['{272B8E4F-4AE4-4F14-BC4E-5924FA0C149D}']
    procedure Cont(allow: Boolean);
  end;

  ICefGeolocationHandler = interface(ICefBaseRefCounted)
  ['{1178EE62-BAE7-4E44-932B-EAAC7A18191C}']
    function OnRequestGeolocationPermission(const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback): Boolean;
    procedure OnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer);
  end;

  ICefRenderHandler = interface(ICefBaseRefCounted)
  ['{1FC1C22B-085A-4741-9366-5249B88EC410}']
    procedure GetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    function  GetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function  GetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function  GetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer): Boolean;
    function  GetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure OnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure OnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure OnPaint(const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure OnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle; CursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    function  OnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData; allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    procedure OnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    procedure OnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure OnIMECompositionRangeChanged(const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);
  end;

  ICefClient = interface(ICefBaseRefCounted)
    ['{1D502075-2FF0-4E13-A112-9E541CD811F4}']
    function GetContextMenuHandler: ICefContextMenuHandler;
    function GetDisplayHandler: ICefDisplayHandler;
    function GetDownloadHandler: ICefDownloadHandler;
    function GetFocusHandler: ICefFocusHandler;
    function GetGeolocationHandler: ICefGeolocationHandler;
    function GetJsdialogHandler: ICefJsdialogHandler;
    function GetKeyboardHandler: ICefKeyboardHandler;
    function GetLifeSpanHandler: ICefLifeSpanHandler;
    function GetLoadHandler: ICefLoadHandler;
    function GetRenderHandler: ICefRenderHandler;
    function GetRequestHandler: ICefRequestHandler;
    function OnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;
  end;

  ICefUrlRequest = interface(ICefBaseRefCounted)
    ['{59226AC1-A0FA-4D59-9DF4-A65C42391A67}']
    function GetRequest: ICefRequest;
    function GetRequestStatus: TCefUrlRequestStatus;
    function GetRequestError: Integer;
    function GetResponse: ICefResponse;
    procedure Cancel;
  end;

  ICefUrlrequestClient = interface(ICefBaseRefCounted)
    ['{114155BD-C248-4651-9A4F-26F3F9A4F737}']
    procedure OnRequestComplete(const request: ICefUrlRequest);
    procedure OnUploadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure OnDownloadProgress(const request: ICefUrlRequest; current, total: Int64);
    procedure OnDownloadData(const request: ICefUrlRequest; data: Pointer; dataLength: NativeUInt);
    function OnGetAuthCredentials(isProxy: Boolean; const host: ustring; port: Integer;
      const realm, scheme: ustring; const callback: ICefAuthCallback): Boolean;
  end;

  ICefWebPluginInfoVisitor = interface(ICefBaseRefCounted)
  ['{7523D432-4424-4804-ACAD-E67D2313436E}']
    function Visit(const info: ICefWebPluginInfo; count, total: Integer): Boolean;
  end;

  ICefWebPluginUnstableCallback = interface(ICefBaseRefCounted)
  ['{67459829-EB47-4B7E-9D69-2EE77DF0E71E}']
    procedure IsUnstable(const path: ustring; unstable: Boolean);
  end;

  ICefRegisterCDMCallback = interface(ICefBaseRefCounted)
  ['{6C39AB3B-F724-483F-ABA0-37F6E0AECF35}']
    procedure OnCDMRegistrationComplete(result: TCefCDMRegistrationError; const error_message: ustring);
  end;

  ICefEndTracingCallback = interface(ICefBaseRefCounted)
  ['{79020EBE-9D1D-49A6-9714-8778FE8929F2}']
    procedure OnEndTracingComplete(const tracingFile: ustring);
  end;

  ICefGetGeolocationCallback = interface(ICefBaseRefCounted)
  ['{ACB82FD9-3FFD-43F9-BF1A-A4849BF5B814}']
    procedure OnLocationUpdate(const position: PCefGeoposition);
  end;

  ICefFileDialogCallback = interface(ICefBaseRefCounted)
  ['{1AF659AB-4522-4E39-9C52-184000D8E3C7}']
    procedure Cont(selectedAcceptFilter: Integer; filePaths: TStrings);
    procedure Cancel;
  end;

  ICefDragData = interface(ICefBaseRefCounted)
  ['{FBB6A487-F633-4055-AB3E-6619EDE75683}']
    function  Clone: ICefDragData;
    function  IsReadOnly: Boolean;
    function  IsLink: Boolean;
    function  IsFragment: Boolean;
    function  IsFile: Boolean;
    function  GetLinkUrl: ustring;
    function  GetLinkTitle: ustring;
    function  GetLinkMetadata: ustring;
    function  GetFragmentText: ustring;
    function  GetFragmentHtml: ustring;
    function  GetFragmentBaseUrl: ustring;
    function  GetFileName: ustring;
    function  GetFileContents(const writer: ICefStreamWriter): NativeUInt;
    function  GetFileNames(names: TStrings): Integer;
    procedure SetLinkUrl(const url: ustring);
    procedure SetLinkTitle(const title: ustring);
    procedure SetLinkMetadata(const data: ustring);
    procedure SetFragmentText(const text: ustring);
    procedure SetFragmentHtml(const html: ustring);
    procedure SetFragmentBaseUrl(const baseUrl: ustring);
    procedure ResetFileContents;
    procedure AddFile(const path, displayName: ustring);
    function  GetImage : ICefImage;
    function  GetImageHotspot : TCefPoint;
    function  HasImage : boolean;
  end;

  ICefDragHandler = interface(ICefBaseRefCounted)
    ['{59A89579-5B18-489F-A25C-5CC25FF831FC}']
    function OnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations): Boolean;
    procedure OnDraggableRegionsChanged(const browser: ICefBrowser;
      regionsCount: NativeUInt; regions: PCefDraggableRegionArray);
  end;

  ICefFindHandler = interface(ICefBaseRefCounted)
    ['{F20DF234-BD43-42B3-A80B-D354A9E5B787}']
    procedure OnFindResult(const browser: ICefBrowser;
      identifier, count: Integer; const selectionRect: PCefRect;
      activeMatchOrdinal: Integer; finalUpdate: Boolean);
  end;

  ICefRequestContextHandler = interface(ICefBaseRefCounted)
    ['{76EB1FA7-78DF-4FD5-ABB3-1CDD3E73A140}']
    function GetCookieManager: ICefCookieManager;
    function OnBeforePluginLoad(const mimeType, pluginUrl:ustring; isMainFrame : boolean; const topOriginUrl: ustring;
      const pluginInfo: ICefWebPluginInfo; pluginPolicy: PCefPluginPolicy): Boolean;
  end;

  ICefResolveCallback = interface(ICefBaseRefCounted)
    ['{0C0EA252-7968-4163-A1BE-A1453576DD06}']
    procedure OnResolveCompleted(result: TCefErrorCode; resolvedIps: TStrings);
  end;

  ICefRequestContext = interface(ICefBaseRefCounted)
    ['{5830847A-2971-4BD5-ABE6-21451F8923F7}']
    function IsSame(const other: ICefRequestContext): Boolean;
    function IsSharingWith(const other: ICefRequestContext): Boolean;
    function IsGlobal: Boolean;
    function GetHandler: ICefRequestContextHandler;
    function GetCachePath: ustring;
    function GetDefaultCookieManager(const callback: ICefCompletionCallback): ICefCookieManager;
    function GetDefaultCookieManagerProc(const callback: TCefCompletionCallbackProc): ICefCookieManager;
    function RegisterSchemeHandlerFactory(const schemeName, domainName: ustring;
        const factory: ICefSchemeHandlerFactory): Boolean;
    function ClearSchemeHandlerFactories: Boolean;
    procedure PurgePluginListCache(reloadPages: Boolean);
    function HasPreference(const name: ustring): Boolean;
    function GetPreference(const name: ustring): ICefValue;
    function GetAllPreferences(includeDefaults: Boolean): ICefDictionaryValue;
    function CanSetPreference(const name: ustring): Boolean;
    function SetPreference(const name: ustring; const value: ICefValue; out error: ustring): Boolean;
    procedure ClearCertificateExceptions(const callback: ICefCompletionCallback);
    procedure CloseAllConnections(const callback: ICefCompletionCallback);
    procedure ResolveHost(const origin: ustring; const callback: ICefResolveCallback);
    function ResolveHostCached(const origin: ustring; resolvedIps: TStrings): TCefErrorCode;
  end;

  ICefPrintSettings = Interface(ICefBaseRefCounted)
    ['{ACBD2395-E9C1-49E5-B7F3-344DAA4A0F12}']
    function IsValid: Boolean;
    function IsReadOnly: Boolean;
    function Copy: ICefPrintSettings;
    procedure SetOrientation(landscape: Boolean);
    function IsLandscape: Boolean;
    procedure SetPrinterPrintableArea(
      const physicalSizeDeviceUnits: PCefSize;
      const printableAreaDeviceUnits: PCefRect;
      landscapeNeedsFlip: Boolean); stdcall;
    procedure SetDeviceName(const name: ustring);
    function GetDeviceName: ustring;
    procedure SetDpi(dpi: Integer);
    function GetDpi: Integer;
    procedure SetPageRanges(const ranges: TCefRangeArray);
    function GetPageRangesCount: NativeUInt;
    procedure GetPageRanges(out ranges: TCefRangeArray);
    procedure SetSelectionOnly(selectionOnly: Boolean);
    function IsSelectionOnly: Boolean;
    procedure SetCollate(collate: Boolean);
    function WillCollate: Boolean;
    procedure SetColorModel(model: TCefColorModel);
    function GetColorModel: TCefColorModel;
    procedure SetCopies(copies: Integer);
    function GetCopies: Integer;
    procedure SetDuplexMode(mode: TCefDuplexMode);
    function GetDuplexMode: TCefDuplexMode;

    property Landscape: Boolean read IsLandscape write SetOrientation;
    property DeviceName: ustring read GetDeviceName write SetDeviceName;
    property Dpi: Integer read GetDpi write SetDpi;
    property SelectionOnly: Boolean read IsSelectionOnly write SetSelectionOnly;
    property Collate: Boolean read WillCollate write SetCollate;
    property ColorModel: TCefColorModel read GetColorModel write SetColorModel;
    property Copies: Integer read GetCopies write SetCopies;
    property DuplexMode: TCefDuplexMode read GetDuplexMode write SetDuplexMode;
  end;

  ICefPrintDialogCallback = interface(ICefBaseRefCounted)
    ['{1D7FB71E-0019-4A80-95ED-91DDD019253B}']
    procedure cont(const settings: ICefPrintSettings);
    procedure cancel;
  end;

  ICefPrintJobCallback = interface(ICefBaseRefCounted)
    ['{5554852A-052C-464B-A868-B618C7E7E2FD}']
    procedure cont;
  end;

  ICefPrintHandler = interface(ICefBaseRefCounted)
    ['{2831D5C9-6E2B-4A30-A65A-0F4435371EFC}']
    procedure OnPrintStart(const browser: ICefBrowser);
    procedure OnPrintSettings(const browser: ICefBrowser; const settings: ICefPrintSettings; getDefaults: boolean);
    function  OnPrintDialog(const browser: ICefBrowser; hasSelection: boolean; const callback: ICefPrintDialogCallback): boolean;
    function  OnPrintJob(const browser: ICefBrowser; const documentName, PDFFilePath: ustring; const callback: ICefPrintJobCallback): boolean;
    procedure OnPrintReset(const browser: ICefBrowser);
    function  GetPDFPaperSize(deviceUnitsPerInch: Integer): TCefSize;
  end;

  ICefNavigationEntry = interface(ICefBaseRefCounted)
  ['{D17B4B37-AA45-42D9-B4E4-AAB6FE2AB297}']
    function IsValid: Boolean;
    function GetUrl: ustring;
    function GetDisplayUrl: ustring;
    function GetOriginalUrl: ustring;
    function GetTitle: ustring;
    function GetTransitionType: TCefTransitionType;
    function HasPostData: Boolean;
    function GetCompletionTime: TDateTime;
    function GetHttpStatusCode: Integer;
    function GetSSLStatus: ICefSSLStatus;

    property Url: ustring read GetUrl;
    property DisplayUrl: ustring read GetDisplayUrl;
    property OriginalUrl: ustring read GetOriginalUrl;
    property Title: ustring read GetTitle;
    property TransitionType: TCefTransitionType read GetTransitionType;
    property CompletionTime: TDateTime read GetCompletionTime;
    property HttpStatusCode: Integer read GetHttpStatusCode;
    property SSLStatus: ICefSSLStatus read GetSSLStatus;
  end;

  ICefX509CertPrincipal = interface(ICefBaseRefCounted)
  ['{CD3621ED-7D68-4A1F-95B5-190C7001B65F}']
    function GetDisplayName: ustring;
    function GetCommonName: ustring;
    function GetLocalityName: ustring;
    function GetStateOrProvinceName: ustring;
    function GetCountryName: ustring;
    procedure GetStreetAddresses(addresses: TStrings);
    procedure GetOrganizationNames(names: TStrings);
    procedure GetOrganizationUnitNames(names: TStrings);
    procedure GetDomainComponents(components: TStrings);
  end;

  ICefX509Certificate = interface(ICefBaseRefCounted)
  ['{C897979D-F068-4428-82DF-4221612FF7E0}']
    function GetSubject: ICefX509CertPrincipal;
    function GetIssuer: ICefX509CertPrincipal;
    function GetSerialNumber: ICefBinaryValue;
    function GetValidStart: TCefTime;
    function GetValidExpiry: TCefTime;
    function GetDerEncoded: ICefBinaryValue;
    function GetPemEncoded: ICefBinaryValue;
    function GetIssuerChainSize: NativeUInt;
    procedure GetDEREncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
    procedure GetPEMEncodedIssuerChain(chainCount: NativeUInt; var chain : TCefBinaryValueArray);
  end;

  ICefSslInfo = interface(ICefBaseRefCounted)
  ['{67EC86BD-DE7D-453D-908F-AD15626C514F}']
    function GetCertStatus: TCefCertStatus;
    function GetX509Certificate: ICefX509Certificate;
  end;

  ICefSSLStatus = interface(ICefBaseRefCounted)
    ['{E3F004F2-03D5-46A2-91D0-510C50F3B225}']
    function IsSecureConnection: boolean;
    function GetCertStatus: TCefCertStatus;
    function GetSSLVersion: TCefSSLVersion;
    function GetContentStatus: TCefSSLContentStatus;
    function GetX509Certificate: ICefX509Certificate;
  end;

  ICefSelectClientCertificateCallback = interface(ICefBaseRefCounted)
    ['{003E3D09-ADE8-4C6E-A174-079D3D616608}']
    procedure Select(const cert: ICefX509Certificate);
  end;

  ICefResourceBundle = interface(ICefBaseRefCounted)
  ['{3213CF97-C854-452B-B615-39192F8D07DC}']
    function GetLocalizedString(stringId: Integer): ustring;
    function GetDataResource(resourceId: Integer;
      out data: Pointer; out dataSize: NativeUInt): Boolean;
    function GetDataResourceForScale(resourceId: Integer; scaleFactor: TCefScaleFactor;
      out data: Pointer; out dataSize: NativeUInt): Boolean;
  end;

  ICefImage = interface(ICefBaseRefCounted)
    ['{E2C2F424-26A2-4498-BB45-DA23219831BE}']
    function IsEmpty: Boolean;
    function IsSame(const that: ICefImage): Boolean;
    function AddBitmap(scaleFactor: Single; pixelWidth, pixelHeight: Integer;
      colorType: TCefColorType; alphaType: TCefAlphaType; pixelData: Pointer;
      pixelDataSize: NativeUInt): Boolean;
    function AddPng(scaleFactor: Single; const pngData: Pointer; pngDataSize: NativeUInt): Boolean;
    function AddJpeg(scaleFactor: Single; const jpegData: Pointer; jpegDataSize: NativeUInt): Boolean;
    function GetWidth: NativeUInt;
    function GetHeight: NativeUInt;
    function HasRepresentation(scaleFactor: Single): Boolean;
    function RemoveRepresentation(scaleFactor: Single): Boolean;
    function GetRepresentationInfo(scaleFactor: Single; actualScaleFactor: PSingle;
      pixelWidth, pixelHeight: PInteger): Boolean;
    function GetAsBitmap(scaleFactor: Single; colorType: TCefColorType;
      alphaType: TCefAlphaType; pixelWidth, pixelHeight: PInteger): ICefBinaryValue;
    function GetAsPng(scaleFactor: Single; withTransparency: Boolean;
      pixelWidth, pixelHeight: PInteger): ICefBinaryValue;
    function GetAsJpeg(scaleFactor: Single; quality: Integer;
      pixelWidth, pixelHeight: PInteger): ICefBinaryValue;

    property Width: NativeUInt read GetWidth;
    property Height: NativeUInt read GetHeight;
  end;

  ICefMenuModelDelegate = interface(ICefBaseRefCounted)
    ['{1430D202-2795-433E-9A35-C79A0996F316}']
    procedure ExecuteCommand(const menuModel: ICefMenuModel; commandId: Integer; eventFlags: TCefEventFlags);
    procedure MouseOutsideMenu(const menuModel: ICefMenuModel; const screenPoint: PCefPoint);
    procedure UnhandledOpenSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    procedure UnhandledCloseSubmenu(const menuModel: ICefMenuModel; isRTL: boolean);
    procedure MenuWillShow(const menuModel: ICefMenuModel);
    procedure MenuClosed(const menuModel: ICefMenuModel);
    function  FormatLabel(const menuModel: ICefMenuModel; const label_ : ustring) : boolean;
  end;

  IChromiumEvents = interface
  ['{0C139DB1-0349-4D7F-8155-76FEA6A0126D}']
    procedure GetSettings(var settings: TCefBrowserSettings);
    function doOnProcessMessageReceived(const browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage): Boolean;

    procedure doOnLoadingStateChange(const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure doOnLoadStart(const browser: ICefBrowser; const frame: ICefFrame; transitionType: TCefTransitionType);
    procedure doOnLoadEnd(const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure doOnLoadError(const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
      const errorText, failedUrl: ustring);

    procedure doOnTakeFocus(const browser: ICefBrowser; next: Boolean);
    function doOnSetFocus(const browser: ICefBrowser; source: TCefFocusSource): Boolean;
    procedure doOnGotFocus(const browser: ICefBrowser);

    procedure doOnBeforeContextMenu(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    function doOnContextMenuCommand(const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: TCefEventFlags): Boolean;
    procedure doOnContextMenuDismissed(const browser: ICefBrowser; const frame: ICefFrame);

    function doOnPreKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle; out isKeyboardShortcut: Boolean): Boolean;
    function doOnKeyEvent(const browser: ICefBrowser; const event: PCefKeyEvent;
      osEvent: TCefEventHandle): Boolean;

    procedure doOnAddressChange(const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure doOnTitleChange(const browser: ICefBrowser; const title: ustring);
    procedure doOnFaviconUrlChange(const browser: ICefBrowser; iconUrls: TStrings);
    procedure doOnFullScreenModeChange(const browser: ICefBrowser; fullscreen: Boolean);
    function doOnTooltip(const browser: ICefBrowser; var text: ustring): Boolean;
    procedure doOnStatusMessage(const browser: ICefBrowser; const value: ustring);
    function doOnConsoleMessage(const browser: ICefBrowser; const message, source: ustring; line: Integer): Boolean;

    function doOnRequestGeolocationPermission(const browser: ICefBrowser; const requestingUrl: ustring; requestId: Integer; const callback: ICefGeolocationCallback): Boolean;
    procedure doOnCancelGeolocationPermission(const browser: ICefBrowser; requestId: Integer);

    procedure doOnBeforeDownload(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
      const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure doOnDownloadUpdated(const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
        const callback: ICefDownloadItemCallback);

    function doOnJsdialog(const browser: ICefBrowser; const originUrl: ustring;
      dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
      const callback: ICefJsDialogCallback; out suppressMessage: Boolean): Boolean;
    function doOnBeforeUnloadDialog(const browser: ICefBrowser;
      const messageText: ustring; isReload: Boolean;
      const callback: ICefJsDialogCallback): Boolean;
    procedure doOnResetDialogState(const browser: ICefBrowser);
    procedure doOnDialogClosed(const browser: ICefBrowser);

    function doOnBeforePopup(const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      var popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean): Boolean;
    procedure doOnAfterCreated(const browser: ICefBrowser);
    procedure doOnBeforeClose(const browser: ICefBrowser);
    function doOnClose(const browser: ICefBrowser): Boolean;

    function doOnBeforeBrowse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; isRedirect: Boolean): Boolean;
    function doOnOpenUrlFromTab(const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean): Boolean;
    function doOnBeforeResourceLoad(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const callback: ICefRequestCallback): TCefReturnValue;
    function doOnGetResourceHandler(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest): ICefResourceHandler;
    procedure doOnResourceRedirect(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; var newUrl: ustring);
    function doOnResourceResponse(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse): Boolean;
    function doOnGetResourceResponseFilter(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse): ICefResponseFilter;
    procedure doOnResourceLoadComplete(const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse; status: TCefUrlRequestStatus;
      receivedContentLength: Int64);
    function doOnGetAuthCredentials(const browser: ICefBrowser; const frame: ICefFrame;
      isProxy: Boolean; const host: ustring; port: Integer; const realm, scheme: ustring;
      const callback: ICefAuthCallback): Boolean;
    function doOnQuotaRequest(const browser: ICefBrowser; const originUrl: ustring;
      newSize: Int64; const callback: ICefRequestCallback): Boolean;
    procedure doOnProtocolExecution(const browser: ICefBrowser; const url: ustring; out allowOsExecution: Boolean);
    function doOnCertificateError(const browser: ICefBrowser; certError: TCefErrorcode;
      const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback): Boolean;
    function  doOnSelectClientCertificate(const browser: ICefBrowser; isProxy: boolean; const host: ustring; port: integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback): boolean;
    procedure doOnPluginCrashed(const browser: ICefBrowser; const pluginPath: ustring);
    procedure doOnRenderViewReady(const browser: ICefBrowser);
    procedure doOnRenderProcessTerminated(const browser: ICefBrowser; status: TCefTerminationStatus);


    function doOnFileDialog(const browser: ICefBrowser; mode: TCefFileDialogMode;
      const title, defaultFilePath: ustring; acceptFilters: TStrings;
      selectedAcceptFilter: Integer; const callback: ICefFileDialogCallback): Boolean;

    procedure doOnGetAccessibilityHandler(var aAccessibilityHandler : ICefAccessibilityHandler);
    function doOnGetRootScreenRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function doOnGetViewRect(const browser: ICefBrowser; var rect: TCefRect): Boolean;
    function doOnGetScreenPoint(const browser: ICefBrowser; viewX, viewY: Integer;
      var screenX, screenY: Integer): Boolean;
    function doOnGetScreenInfo(const browser: ICefBrowser; var screenInfo: TCefScreenInfo): Boolean;
    procedure doOnPopupShow(const browser: ICefBrowser; show: Boolean);
    procedure doOnPopupSize(const browser: ICefBrowser; const rect: PCefRect);
    procedure doOnPaint(const browser: ICefBrowser; kind: TCefPaintElementType;
      dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
      const buffer: Pointer; width, height: Integer);
    procedure doOnCursorChange(const browser: ICefBrowser; cursor: TCefCursorHandle;
      cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    function doOnStartDragging(const browser: ICefBrowser; const dragData: ICefDragData;
      allowedOps: TCefDragOperations; x, y: Integer): Boolean;
    procedure doOnUpdateDragCursor(const browser: ICefBrowser; operation: TCefDragOperation);
    procedure doOnScrollOffsetChanged(const browser: ICefBrowser; x, y: Double);
    procedure doOnIMECompositionRangeChanged(const browser: ICefBrowser;
                                             const selected_range: PCefRange;
                                                   character_boundsCount: NativeUInt;
                                             const character_bounds: PCefRect);

    function doOnDragEnter(const browser: ICefBrowser; const dragData: ICefDragData;
      mask: TCefDragOperations): Boolean;
    procedure doOnDraggableRegionsChanged(const browser: ICefBrowser;
      regionsCount: NativeUInt; regions: PCefDraggableRegionArray);

    procedure doOnFindResult(const browser: ICefBrowser; identifier, count: Integer;
      const selectionRect: PCefRect; activeMatchOrdinal: Integer; finalUpdate: Boolean);
  end;

implementation

end.
