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

unit uMiniBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Menus, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls, Vcl.ClipBrd,
  System.UITypes, Vcl.AppEvnts, Winapi.ActiveX, Winapi.ShlObj,
  System.NetEncoding,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  AppEvnts, ActiveX, ShlObj, NetEncoding,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes,
  uCEFConstants, uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MINIBROWSER_SHOWDEVTOOLS     = WM_APP + $101;
  MINIBROWSER_HIDEDEVTOOLS     = WM_APP + $102;
  MINIBROWSER_COPYHTML         = WM_APP + $103;
  MINIBROWSER_SHOWRESPONSE     = WM_APP + $104;
  MINIBROWSER_COPYFRAMEIDS     = WM_APP + $105;
  MINIBROWSER_COPYFRAMENAMES   = WM_APP + $106;
  MINIBROWSER_SAVEPREFERENCES  = WM_APP + $107;
  MINIBROWSER_COPYALLTEXT      = WM_APP + $108;
  MINIBROWSER_TAKESNAPSHOT     = WM_APP + $109;
  MINIBROWSER_SHOWNAVIGATION   = WM_APP + $10A;
  MINIBROWSER_COOKIESFLUSHED   = WM_APP + $10B;
  MINIBROWSER_PDFPRINT_END     = WM_APP + $10C;
  MINIBROWSER_PREFS_AVLBL      = WM_APP + $10D;
  MINIBROWSER_DTDATA_AVLBL     = WM_APP + $10E;

  MINIBROWSER_HOMEPAGE = 'https://www.google.com';

  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS    = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS    = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_COPYHTML        = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_JSWRITEDOC      = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_JSPRINTDOC      = MENU_ID_USER_FIRST + 5;
  MINIBROWSER_CONTEXTMENU_SHOWRESPONSE    = MENU_ID_USER_FIRST + 6;
  MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS    = MENU_ID_USER_FIRST + 7;
  MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES  = MENU_ID_USER_FIRST + 8;
  MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES = MENU_ID_USER_FIRST + 9;
  MINIBROWSER_CONTEXTMENU_COPYALLTEXT     = MENU_ID_USER_FIRST + 10;
  MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT    = MENU_ID_USER_FIRST + 11;
  MINIBROWSER_CONTEXTMENU_GETNAVIGATION   = MENU_ID_USER_FIRST + 12;
  MINIBROWSER_CONTEXTMENU_MUTEAUDIO       = MENU_ID_USER_FIRST + 13;
  MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO     = MENU_ID_USER_FIRST + 14;

type
  TMiniBrowserFrm = class(TForm)
    NavControlPnl: TPanel;
    NavButtonPnl: TPanel;
    URLEditPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    StopBtn: TButton;
    DevTools: TCEFWindowParent;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    URLCbx: TComboBox;
    ConfigPnl: TPanel;
    ConfigBtn: TButton;
    PopupMenu1: TPopupMenu;
    DevTools1: TMenuItem;
    N1: TMenuItem;
    Preferences1: TMenuItem;
    GoBtn: TButton;
    N2: TMenuItem;
    PrintinPDF1: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    Zoom1: TMenuItem;
    Inczoom1: TMenuItem;
    Deczoom1: TMenuItem;
    Resetzoom1: TMenuItem;
    SaveDialog1: TSaveDialog;
    ApplicationEvents1: TApplicationEvents;
    OpenDialog1: TOpenDialog;
    N4: TMenuItem;
    Openfile1: TMenuItem;
    Resolvehost1: TMenuItem;
    Timer1: TTimer;
    OpenfilewithaDAT1: TMenuItem;
    N5: TMenuItem;
    Memoryinfo1: TMenuItem;
    Downloadimage1: TMenuItem;
    Simulatekeyboardpresses1: TMenuItem;
    Flushcookies1: TMenuItem;
    Acceptlanguage1: TMenuItem;
    FindText1: TMenuItem;
    Clearcache1: TMenuItem;
    akescreenshot1: TMenuItem;
    Useragent1: TMenuItem;
    ClearallstorageforcurrentURL1: TMenuItem;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1StatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);
    procedure Chromium1TextResultAvailable(Sender: TObject; const aText: ustring);
    procedure Chromium1FullScreenModeChange(Sender: TObject; const browser: ICefBrowser; fullscreen: Boolean);
    procedure Chromium1PreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg; out isKeyboardShortcut, Result: Boolean);
    procedure Chromium1KeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg; out Result: Boolean);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1PdfPrintFinished(Sender: TObject; aResultOK: Boolean);
    procedure Chromium1ResourceResponse(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: Boolean);
    procedure Chromium1ResolvedHostAvailable(Sender: TObject; result: Integer; const resolvedIps: TStrings);
    procedure Chromium1PrefsAvailable(Sender: TObject; aResultOK: Boolean);
    procedure Chromium1BeforeDownload(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback);
    procedure Chromium1DownloadUpdated(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);
    procedure Chromium1BeforeResourceLoad(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefRequestCallback; out Result: TCefReturnValue);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1RenderCompMsg(Sender: TObject; var aMessage : TMessage; var aHandled: Boolean);
    procedure Chromium1LoadingProgressChange(Sender: TObject; const browser: ICefBrowser; const progress: Double);
    procedure Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
    procedure Chromium1CertificateError(Sender: TObject; const browser: ICefBrowser; certError: Integer; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefRequestCallback; out Result: Boolean);
    procedure Chromium1NavigationVisitorResultAvailable(Sender: TObject; const entry: ICefNavigationEntry; current: Boolean; index, total: Integer; var aResult: Boolean);
    procedure Chromium1DownloadImageFinished(Sender: TObject; const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
    procedure Chromium1CookiesFlushed(Sender: TObject);
    procedure Chromium1BeforePluginLoad(Sender: TObject; const mimeType, pluginUrl: ustring; isMainFrame: Boolean; const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo; var pluginPolicy: TCefPluginPolicy; var aResult: Boolean);
    procedure Chromium1ZoomPctAvailable(Sender: TObject; const aZoomPct: Double);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);

    procedure BackBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure DevTools1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure ConfigBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure PrintinPDF1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Inczoom1Click(Sender: TObject);
    procedure Deczoom1Click(Sender: TObject);
    procedure Resetzoom1Click(Sender: TObject);
    procedure Openfile1Click(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure Resolvehost1Click(Sender: TObject);
    procedure OpenfilewithaDAT1Click(Sender: TObject);
    procedure Memoryinfo1Click(Sender: TObject);
    procedure Downloadimage1Click(Sender: TObject);
    procedure Simulatekeyboardpresses1Click(Sender: TObject);
    procedure Flushcookies1Click(Sender: TObject);
    procedure Acceptlanguage1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FindText1Click(Sender: TObject);
    procedure Clearcache1Click(Sender: TObject);
    procedure akescreenshot1Click(Sender: TObject);
    procedure Useragent1Click(Sender: TObject);
    procedure ClearallstorageforcurrentURL1Click(Sender: TObject);

  protected
    FDevToolsMsgID    : integer;
    FScreenshotMsgID  : integer;
    FDevToolsMsgValue : ustring;

    FResponse   : TStringList;
    FRequest    : TStringList;
    FNavigation : TStringList;
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure AddURL(const aURL : string);

    procedure ShowDevTools(aPoint : TPoint); overload;
    procedure ShowDevTools; overload;
    procedure HideDevTools;

    procedure HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
    procedure HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);

    procedure InspectRequest(const aRequest : ICefRequest);
    procedure InspectResponse(const aResponse : ICefResponse);

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ShowDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_SHOWDEVTOOLS;
    procedure HideDevToolsMsg(var aMessage : TMessage); message MINIBROWSER_HIDEDEVTOOLS;
    procedure CopyAllTextMsg(var aMessage : TMessage); message MINIBROWSER_COPYALLTEXT;
    procedure CopyHTMLMsg(var aMessage : TMessage); message MINIBROWSER_COPYHTML;
    procedure CopyFramesIDsMsg(var aMessage : TMessage); message MINIBROWSER_COPYFRAMEIDS;
    procedure CopyFramesNamesMsg(var aMessage : TMessage); message MINIBROWSER_COPYFRAMENAMES;
    procedure ShowResponseMsg(var aMessage : TMessage); message MINIBROWSER_SHOWRESPONSE;
    procedure ShowNavigationMsg(var aMessage : TMessage); message MINIBROWSER_SHOWNAVIGATION;
    procedure SavePreferencesMsg(var aMessage : TMessage); message MINIBROWSER_SAVEPREFERENCES;
    procedure TakeSnapshotMsg(var aMessage : TMessage); message MINIBROWSER_TAKESNAPSHOT;
    procedure CookiesFlushedMsg(var aMessage : TMessage); message MINIBROWSER_COOKIESFLUSHED;
    procedure PrintPDFEndMsg(var aMessage : TMessage); message MINIBROWSER_PDFPRINT_END;
    procedure PreferencesAvailableMsg(var aMessage : TMessage); message MINIBROWSER_PREFS_AVLBL;
    procedure DevToolsDataAvailableMsg(var aMessage : TMessage); message MINIBROWSER_DTDATA_AVLBL;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

  public
    procedure ShowStatusText(const aText : string);

  end;

var
  MiniBrowserFrm : TMiniBrowserFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uPreferences, uCefStringMultimap, uCEFMiscFunctions, uSimpleTextViewer,
  uCEFClient, uFindFrm, uCEFDictionaryValue;

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                     := TCefApplication.Create;
  GlobalCEFApp.LogFile             := 'debug.log';
  GlobalCEFApp.LogSeverity         := LOGSEVERITY_INFO;
  GlobalCEFApp.cache               := 'cache';
  GlobalCEFApp.EnablePrintPreview  := True;

  // This is a workaround for the CEF4Delphi issue #324 :
  // https://github.com/salvadordf/CEF4Delphi/issues/324
  GlobalCEFApp.DisableFeatures := 'WinUseBrowserSpellChecker';
end;

procedure TMiniBrowserFrm.BackBtnClick(Sender: TObject);
begin
  Chromium1.GoBack;
end;

procedure TMiniBrowserFrm.ForwardBtnClick(Sender: TObject);
begin
  Chromium1.GoForward;
end;

procedure TMiniBrowserFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(URLCbx.Text);
end;

procedure TMiniBrowserFrm.ReloadBtnClick(Sender: TObject);
begin
  Chromium1.Reload;
end;

procedure TMiniBrowserFrm.Resetzoom1Click(Sender: TObject);
begin
  Chromium1.ResetZoomStep;
end;

procedure TMiniBrowserFrm.Resolvehost1Click(Sender: TObject);
var
  TempURL : string;
begin
  TempURL := inputbox('Resolve host', 'URL :', 'http://google.com');
  if (length(TempURL) > 0) then Chromium1.ResolveHost(TempURL);
end;

procedure TMiniBrowserFrm.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then AddURL(url);
end;

procedure TMiniBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if Chromium1.IsSameBrowser(browser) then
    PostMessage(Handle, CEF_AFTERCREATED, 0, 0)
   else
    SendMessage(browser.Host.WindowHandle, WM_SETICON, 1, application.Icon.Handle); // Use the same icon in the popup window
end;

procedure TMiniBrowserFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // The main browser is being destroyed
  if (Chromium1.BrowserId = 0) then
    begin
      FCanClose := True;
      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TMiniBrowserFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if Chromium1.IsSameBrowser(browser) then
    begin
      model.AddSeparator;
      model.AddItem(MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT,    'Take snapshot...');
      model.AddItem(MINIBROWSER_CONTEXTMENU_GETNAVIGATION,   'Get navigation entries');
      model.AddSeparator;
      model.AddItem(MINIBROWSER_CONTEXTMENU_COPYALLTEXT,     'Copy displayed text to clipboard');
      model.AddItem(MINIBROWSER_CONTEXTMENU_COPYHTML,        'Copy HTML to clipboard');
      model.AddItem(MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS,    'Copy HTML frame identifiers to clipboard');
      model.AddItem(MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES,  'Copy HTML frame names to clipboard');

      model.AddSeparator;
      model.AddItem(MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES, 'Save preferences as...');
      model.AddSeparator;
      model.AddItem(MINIBROWSER_CONTEXTMENU_JSWRITEDOC,      'Modify HTML document');
      model.AddItem(MINIBROWSER_CONTEXTMENU_JSPRINTDOC,      'Print using Javascript');
      model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWRESPONSE,    'Show server headers');

      if DevTools.Visible then
        model.AddItem(MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS, 'Hide DevTools')
       else
        model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');

      if Chromium1.AudioMuted then
        model.AddItem(MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO, 'Unmute audio')
       else
        model.AddItem(MINIBROWSER_CONTEXTMENU_MUTEAUDIO,   'Mute audio');
    end
   else
    model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

function PathToMyDocuments : string;
var
  Allocator : IMalloc;
  Path      : pchar;
  idList    : PItemIDList;
begin
  Result   := '';
  Path     := nil;
  idList   := nil;

  try
    if (SHGetMalloc(Allocator) = S_OK) then
      begin
        GetMem(Path, MAX_PATH);
        if (SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, idList) = S_OK) and
           SHGetPathFromIDList(idList, Path) then
          Result := string(Path);
      end;
  finally
    if (Path   <> nil) then FreeMem(Path);
    if (idList <> nil) then Allocator.Free(idList);
  end;
end;

procedure TMiniBrowserFrm.Chromium1BeforeDownload(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const suggestedName: ustring;
  const callback: ICefBeforeDownloadCallback);
var
  TempMyDocuments, TempFullPath, TempName : string;
begin
  if not(Chromium1.IsSameBrowser(browser)) or
     (downloadItem = nil) or
     not(downloadItem.IsValid) then
    exit;

  TempMyDocuments := PathToMyDocuments;

  if (length(suggestedName) > 0) then
    TempName := suggestedName
   else
    TempName := 'DownloadedFile';

  if (length(TempMyDocuments) > 0) then
    TempFullPath := IncludeTrailingPathDelimiter(TempMyDocuments) + TempName
   else
    TempFullPath := TempName;

  callback.cont(TempFullPath, False);
end;

procedure TMiniBrowserFrm.Chromium1BeforePluginLoad(Sender: TObject;
  const mimeType, pluginUrl: ustring; isMainFrame: Boolean;
  const topOriginUrl: ustring; const pluginInfo: ICefWebPluginInfo;
  var pluginPolicy: TCefPluginPolicy; var aResult: Boolean);
begin
  // Always allow the PDF plugin to load.
  if (pluginPolicy <> PLUGIN_POLICY_ALLOW) and
     (CompareText(mimeType, 'application/pdf') = 0) then
    begin
      pluginPolicy := PLUGIN_POLICY_ALLOW;
      aResult      := True;
    end
   else
    aResult := False;
end;

procedure TMiniBrowserFrm.Chromium1BeforeResourceLoad(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const callback: ICefRequestCallback;
  out Result: TCefReturnValue);
begin
  Result := RV_CONTINUE;

  if Chromium1.IsSameBrowser(browser) and
     (frame <> nil) and
     frame.IsMain and
     frame.IsValid then
    InspectRequest(request);
end;

procedure TMiniBrowserFrm.Chromium1CertificateError(Sender: TObject;
  const browser: ICefBrowser; certError: Integer;
  const requestUrl: ustring; const sslInfo: ICefSslInfo;
  const callback: ICefRequestCallback; out Result: Boolean);
begin
  CefDebugLog('Certificate error code:' + inttostr(certError) +
              ' - URL:' + requestUrl, CEF_LOG_SEVERITY_ERROR);
  Result := False;
end;

procedure TMiniBrowserFrm.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  if (browser <> nil) and
     (Chromium1.BrowserId = browser.Identifier) and
     (CEFWindowParent1 <> nil) then
    begin
      PostMessage(Handle, CEF_DESTROY, 0, 0);
      aAction := cbaDelay;
    end;
end;

procedure TMiniBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempParam : WParam;
  TempInfo : TCefWindowInfo;
  TempClient : ICefClient;
  TempSettings : TCefBrowserSettings;
begin
  Result := False;

  if Chromium1.IsSameBrowser(browser) then
    case commandId of
      MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS :
        PostMessage(Handle, MINIBROWSER_HIDEDEVTOOLS, 0, 0);

      MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
        begin
          TempParam := ((params.XCoord and $FFFF) shl 16) or (params.YCoord and $FFFF);
          PostMessage(Handle, MINIBROWSER_SHOWDEVTOOLS, TempParam, 0);
        end;

      MINIBROWSER_CONTEXTMENU_COPYALLTEXT :
        PostMessage(Handle, MINIBROWSER_COPYALLTEXT, 0, 0);

      MINIBROWSER_CONTEXTMENU_COPYHTML :
        PostMessage(Handle, MINIBROWSER_COPYHTML, 0, 0);

      MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS :
        PostMessage(Handle, MINIBROWSER_COPYFRAMEIDS, 0, 0);

      MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES :
        PostMessage(Handle, MINIBROWSER_COPYFRAMENAMES, 0, 0);

      MINIBROWSER_CONTEXTMENU_SHOWRESPONSE :
        PostMessage(Handle, MINIBROWSER_SHOWRESPONSE, 0, 0);

      MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES :
        PostMessage(Handle, MINIBROWSER_SAVEPREFERENCES, 0, 0);

      MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT :
        PostMessage(Handle, MINIBROWSER_TAKESNAPSHOT, 0, 0);

      MINIBROWSER_CONTEXTMENU_GETNAVIGATION :
        begin
          FNavigation.Clear;
          Chromium1.GetNavigationEntries(False);
        end;

      MINIBROWSER_CONTEXTMENU_JSWRITEDOC :
        if (frame <> nil) and frame.IsValid then
          frame.ExecuteJavaScript(
            'var css = ' + chr(39) + '@page {size: A4; margin: 0;} @media print {html, body {width: 210mm; height: 297mm;}}' + chr(39) + '; ' +
            'var style = document.createElement(' + chr(39) + 'style' + chr(39) + '); ' +
            'style.type = ' + chr(39) + 'text/css' + chr(39) + '; ' +
            'style.appendChild(document.createTextNode(css)); ' +
            'document.head.appendChild(style);',
            'about:blank', 0);

      MINIBROWSER_CONTEXTMENU_JSPRINTDOC :
        if (frame <> nil) and frame.IsValid then
          frame.ExecuteJavaScript('window.print();', 'about:blank', 0);

      MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO :
        Chromium1.AudioMuted := False;

      MINIBROWSER_CONTEXTMENU_MUTEAUDIO :
        Chromium1.AudioMuted := True;
    end
   else
    case commandId of
      MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
        try
          WindowInfoAsPopUp(TempInfo, browser.Host.WindowHandle, 'DevTools');
          TempClient := TCustomClientHandler.Create(Chromium1, True);
          FillChar(TempSettings, SizeOf(TCefBrowserSettings), 0);
          browser.Host.ShowDevTools(@TempInfo, TempClient, @TempSettings, nil);
        finally
          TempClient := nil
        end;
    end;
end;

procedure TMiniBrowserFrm.Chromium1CookiesFlushed(Sender: TObject);
begin
  PostMessage(Handle, MINIBROWSER_COOKIESFLUSHED, 0, 0);
end;

procedure TMiniBrowserFrm.CookiesFlushedMsg(var aMessage : TMessage);
begin
  showmessage('The cookies were flushed successfully');
end;

procedure TMiniBrowserFrm.PrintPDFEndMsg(var aMessage : TMessage);
begin
  if (aMessage.lParam <> 0) then
    showmessage('The PDF file was generated successfully')
   else
    showmessage('There was a problem generating the PDF file.');
end;

procedure TMiniBrowserFrm.PreferencesAvailableMsg(var aMessage : TMessage);
begin
  if (aMessage.lParam <> 0) then
    showmessage('The preferences file was generated successfully')
   else
    showmessage('There was a problem generating the preferences file.');
end;

procedure TMiniBrowserFrm.Chromium1DownloadUpdated(Sender: TObject;
  const browser: ICefBrowser; const downloadItem: ICefDownloadItem;
  const callback: ICefDownloadItemCallback);
var
  TempString : string;
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  if downloadItem.IsComplete then
    ShowStatusText(downloadItem.FullPath + ' completed')
   else
    if downloadItem.IsCanceled then
      ShowStatusText(downloadItem.FullPath + ' canceled')
     else
      if downloadItem.IsInProgress then
        begin
          if (downloadItem.PercentComplete >= 0) then
            TempString := downloadItem.FullPath + ' : ' + inttostr(downloadItem.PercentComplete) + '%'
           else
            TempString := downloadItem.FullPath + ' : ' + inttostr(downloadItem.ReceivedBytes) + ' bytes received';

          ShowStatusText(TempString);
        end;
end;

procedure TMiniBrowserFrm.Chromium1FullScreenModeChange(Sender: TObject;
  const browser: ICefBrowser; fullscreen: Boolean);
begin                    
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  // This event is executed in a CEF thread and this can cause problems when
  // you change the 'Enabled' and 'Visible' properties from VCL components.
  // It's recommended to change the 'Enabled' and 'Visible' properties
  // in the main application thread and not in a CEF thread.
  // It's much safer to use PostMessage to send a message to the main form with
  // all this information and update those properties in the procedure handling
  // that message.

  if fullscreen then
    begin
      NavControlPnl.Visible := False;
      StatusBar1.Visible    := False;

      if (WindowState = wsMaximized) then WindowState := wsNormal;

      BorderIcons := [];
      BorderStyle := bsNone;
      WindowState := wsMaximized;
    end
   else
    begin
      BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      BorderStyle := bsSizeable;
      WindowState := wsNormal;

      NavControlPnl.Visible := True;
      StatusBar1.Visible    := True;
    end;
end;

procedure TMiniBrowserFrm.Chromium1KeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
  out Result: Boolean);
var
  TempMsg : TMsg;
begin
  Result := False;

  if (event <> nil) and (osEvent <> nil) then
    case osEvent.Message of
      WM_KEYUP :
        begin
          TempMsg := osEvent^;

          HandleKeyUp(TempMsg, Result);
        end;

      WM_KEYDOWN :
        begin
          TempMsg := osEvent^;

          HandleKeyDown(TempMsg, Result);
        end;
    end;
end;

procedure TMiniBrowserFrm.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  case Msg.message of
    WM_KEYUP   : HandleKeyUp(Msg, Handled);
    WM_KEYDOWN : HandleKeyDown(Msg, Handled);
  end;
end;

procedure TMiniBrowserFrm.HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
var
  TempMessage : TMessage;
  TempKeyMsg  : TWMKey;
begin
  TempMessage.Msg     := aMsg.message;
  TempMessage.wParam  := aMsg.wParam;
  TempMessage.lParam  := aMsg.lParam;
  TempKeyMsg          := TWMKey(TempMessage);

  if (TempKeyMsg.CharCode = VK_F12) then
    begin
      aHandled := True;

      if DevTools.Visible then
        PostMessage(Handle, MINIBROWSER_HIDEDEVTOOLS, 0, 0)
       else
        PostMessage(Handle, MINIBROWSER_SHOWDEVTOOLS, 0, 0);
    end;
end;

procedure TMiniBrowserFrm.HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);
var
  TempMessage : TMessage;
  TempKeyMsg  : TWMKey;
begin
  TempMessage.Msg     := aMsg.message;
  TempMessage.wParam  := aMsg.wParam;
  TempMessage.lParam  := aMsg.lParam;
  TempKeyMsg          := TWMKey(TempMessage);

  if (TempKeyMsg.CharCode = VK_F12) then aHandled := True;
end;

procedure TMiniBrowserFrm.Chromium1LoadEnd(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  httpStatusCode: Integer);
begin
  if (frame = nil) or not(frame.IsValid) then exit;

  if frame.IsMain then
    StatusBar1.Panels[1].Text := 'main frame loaded : ' + quotedstr(frame.name)
   else
    StatusBar1.Panels[1].Text := 'frame loaded : ' + quotedstr(frame.name);
end;

procedure TMiniBrowserFrm.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
var
  TempString : string;
begin
  if (errorCode = ERR_ABORTED) then exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  Chromium1.LoadString(TempString, frame);
end;

procedure TMiniBrowserFrm.Chromium1LoadingProgressChange(Sender: TObject;
  const browser: ICefBrowser; const progress: Double);
begin
  StatusBar1.Panels[0].Text := 'Loading... ' + FloatToStrF(progress * 100, ffFixed, 3, 0) + '%';
end;

procedure TMiniBrowserFrm.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if not(Chromium1.IsSameBrowser(browser)) or FClosing then exit;

  // This event is executed in a CEF thread and this can cause problems when
  // you change the 'Enabled' and 'Visible' properties from VCL components.
  // It's recommended to change the 'Enabled' and 'Visible' properties
  // in the main application thread and not in a CEF thread.
  // It's much safer to use PostMessage to send a message to the main form with
  // all this information and update those properties in the procedure handling
  // that message.

  BackBtn.Enabled    := canGoBack;
  ForwardBtn.Enabled := canGoForward;

  if isLoading then
    begin
      StatusBar1.Panels[0].Text := 'Loading...';
      ReloadBtn.Enabled         := False;
      StopBtn.Enabled           := True;
    end
   else
    begin
      StatusBar1.Panels[0].Text := 'Finished';
      ReloadBtn.Enabled         := True;
      StopBtn.Enabled           := False;
    end;
end;

procedure TMiniBrowserFrm.Chromium1NavigationVisitorResultAvailable(Sender: TObject;
  const entry: ICefNavigationEntry; current: Boolean; index, total: Integer;
  var aResult: Boolean);
begin
  if (entry <> nil) and entry.IsValid then FNavigation.Add(entry.Url);

  if (index < pred(total)) then
    aResult := True
   else
    begin
      aResult := False;
      PostMessage(Handle, MINIBROWSER_SHOWNAVIGATION, 0, 0);
    end;
end;

procedure TMiniBrowserFrm.Chromium1PdfPrintFinished(Sender: TObject; aResultOK: Boolean);
begin
  PostMessage(Handle, MINIBROWSER_PDFPRINT_END, 0, LPARAM(aResultOK));
end;

procedure TMiniBrowserFrm.Chromium1PrefsAvailable(Sender: TObject; aResultOK: Boolean);
begin
  PostMessage(Handle, MINIBROWSER_PREFS_AVLBL, 0, LPARAM(aResultOK));
end;

procedure TMiniBrowserFrm.Chromium1PreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: PMsg;
  out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;

  if (event <> nil) and
     (event.kind in [KEYEVENT_KEYDOWN, KEYEVENT_KEYUP]) and
     (event.windows_key_code = VK_F12) then
    isKeyboardShortcut := True;
end;

procedure TMiniBrowserFrm.Chromium1RenderCompMsg(Sender: TObject; var aMessage : TMessage; var aHandled: Boolean);
begin
  if not(FClosing) and (aMessage.Msg = WM_MOUSEMOVE) then
    begin
      StatusBar1.Panels[2].Text := 'x : ' + inttostr(aMessage.lParam and $FFFF);
      StatusBar1.Panels[3].Text := 'y : ' + inttostr((aMessage.lParam and $FFFF0000) shr 16);
    end;
end;

procedure TMiniBrowserFrm.Chromium1ResolvedHostAvailable(Sender: TObject;
  result: Integer; const resolvedIps: TStrings);
begin
  if (result = ERR_NONE) then
    showmessage('Resolved IPs : ' + resolvedIps.CommaText)
   else
    showmessage('There was a problem resolving the host.' + CRLF +
                'Error code : ' + inttostr(result));
end;

procedure TMiniBrowserFrm.InspectRequest(const aRequest : ICefRequest);
var
  TempHeaderMap : ICefStringMultimap;
  i, j : integer;
begin
  if (aRequest <> nil) then
    begin
      FRequest.Clear;

      TempHeaderMap := TCefStringMultimapOwn.Create;
      aRequest.GetHeaderMap(TempHeaderMap);

      i := 0;
      j := TempHeaderMap.Size;

      while (i < j) do
        begin
          FRequest.Add(TempHeaderMap.Key[i] + '=' + TempHeaderMap.Value[i]);
          inc(i);
        end;
    end;
end;

procedure TMiniBrowserFrm.InspectResponse(const aResponse : ICefResponse);
var
  TempHeaderMap : ICefStringMultimap;
  i, j : integer;
begin
  if (aResponse <> nil) then
    begin
      FResponse.Clear;

      TempHeaderMap := TCefStringMultimapOwn.Create;
      aResponse.GetHeaderMap(TempHeaderMap);

      i := 0;
      j := TempHeaderMap.Size;

      while (i < j) do
        begin
          FResponse.Add(TempHeaderMap.Key[i] + '=' + TempHeaderMap.Value[i]);
          inc(i);
        end;
    end;
end;

procedure TMiniBrowserFrm.Memoryinfo1Click(Sender: TObject);
const
  BYTES_PER_MEGABYTE = 1024 * 1024;
var
  TempMessage : string;
begin
  TempMessage := 'Total memory used by this application : ' + inttostr(GlobalCEFApp.UsedMemory div BYTES_PER_MEGABYTE) + ' Mb' + CRLF +
                 'Total system memory : ' +  inttostr(GlobalCEFApp.TotalSystemMemory div BYTES_PER_MEGABYTE) + ' Mb' + CRLF +
                 'Available physical memory : ' + inttostr(GlobalCEFApp.AvailableSystemMemory div BYTES_PER_MEGABYTE) + ' Mb' + CRLF +
                 'Memory load : ' + inttostr(GlobalCEFApp.SystemMemoryLoad) + ' %';

  MessageDlg(TempMessage, mtInformation, [mbOK], 0);
end;

procedure TMiniBrowserFrm.Chromium1ResourceResponse(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  out Result: Boolean);
begin
  Result := False;

  if Chromium1.IsSameBrowser(browser) and
     (frame <> nil) and
     frame.IsMain and
     frame.IsValid then
    InspectResponse(response);
end;

procedure TMiniBrowserFrm.ShowStatusText(const aText : string);
begin
  if not(FClosing) then StatusBar1.Panels[1].Text := aText;
end;

procedure TMiniBrowserFrm.Simulatekeyboardpresses1Click(Sender: TObject);
const
  SIMULATED_KEY_PRESSES = 'QWERTY';
var
  i : integer;
  TempKeyEvent : TCefKeyEvent;
begin
  // This procedure is extremely simplified.
  // Use the SimpleOSRBrowser demo to log the real TCefKeyEvent values
  // if you use anything different than uppercase letters.

  for i := 1 to length(SIMULATED_KEY_PRESSES) do
    begin
      // WM_KEYDOWN
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := 0;
      TempKeyEvent.windows_key_code        := ord(SIMULATED_KEY_PRESSES[i]);
      TempKeyEvent.native_key_code         := 0;
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);
      Chromium1.SendKeyEvent(@TempKeyEvent);

      // WM_CHAR
      TempKeyEvent.kind := KEYEVENT_CHAR;
      Chromium1.SendKeyEvent(@TempKeyEvent);

      // WM_KEYUP
      TempKeyEvent.kind := KEYEVENT_KEYUP;
      Chromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TMiniBrowserFrm.StopBtnClick(Sender: TObject);
begin
  Chromium1.StopLoad;
end;

procedure TMiniBrowserFrm.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  if Chromium1.IsSameBrowser(browser) then ShowStatusText(value);
end;

procedure TMiniBrowserFrm.Chromium1TextResultAvailable(Sender: TObject; const aText: ustring);
begin
  clipboard.AsText := aText;
end;

procedure TMiniBrowserFrm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  if (title <> '') then
    caption := 'MiniBrowser - ' + title
   else
    caption := 'MiniBrowser';
end;

procedure TMiniBrowserFrm.Chromium1ZoomPctAvailable(Sender: TObject;
  const aZoomPct: Double);
begin
  ShowStatusText('Zoom : ' + floattostr(aZoomPct) + '%');
end;

procedure TMiniBrowserFrm.ClearallstorageforcurrentURL1Click(Sender: TObject);
begin
  Chromium1.ClearDataForOrigin(URLCbx.Text);
end;

procedure TMiniBrowserFrm.Clearcache1Click(Sender: TObject);
begin
  Chromium1.ClearCache;
end;

procedure TMiniBrowserFrm.FindText1Click(Sender: TObject);
begin
  FindFrm.Show;
end;

procedure TMiniBrowserFrm.Flushcookies1Click(Sender: TObject);
begin
  if not(Chromium1.FlushCookieStore(False)) then
    showmessage('There was a problem flushing the cookies.');
end;

procedure TMiniBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;

      // if TChromium.MultiBrowserMode is enabled then we have to close all
      // stored browsers and not only the main browser.
      Chromium1.CloseAllBrowsers;
    end;
end;

procedure TMiniBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose            := False;
  FClosing             := False;
  FResponse            := TStringList.Create;
  FRequest             := TStringList.Create;
  FNavigation          := TStringList.Create;

  FDevToolsMsgID       := 0;

  // The MultiBrowserMode store all the browser references in TChromium.
  // The first browser reference is the browser in the main form.
  // When MiniBrowser allows CEF to create child popup browsers it will also
  // store their reference inside TChromium and you can use all the TChromium's
  // methods and properties to manipulate those browsers.
  // To do that call TChromium.SelectBrowser with the browser ID that will be
  // used when you call any method or property in TChromium.
  Chromium1.MultiBrowserMode := True;
  Chromium1.DefaultURL       := MINIBROWSER_HOMEPAGE;
end;

procedure TMiniBrowserFrm.FormDestroy(Sender: TObject);
begin
  FResponse.Free;
  FRequest.Free;
  FNavigation.Free;
end;

procedure TMiniBrowserFrm.FormShow(Sender: TObject);
begin
  ShowStatusText('Initializing browser. Please wait...');

  // WebRTC's IP leaking can lowered/avoided by setting these preferences
  // To test this go to https://www.browserleaks.com/webrtc
  Chromium1.WebRTCIPHandlingPolicy := hpDisableNonProxiedUDP;
  Chromium1.WebRTCMultipleRoutes   := STATE_DISABLED;
  Chromium1.WebRTCNonproxiedUDP    := STATE_DISABLED;

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TMiniBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMiniBrowserFrm.Useragent1Click(Sender: TObject);
var
  TempUA : string;
begin
  TempUA := inputbox('MiniBrowser demo', 'Set new user agent string', '');

  if (length(TempUA) > 0) then
    Chromium1.SetUserAgentOverride(TempUA);
end;

procedure TMiniBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
end;

procedure TMiniBrowserFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  FreeAndNil(CEFWindowParent1);
end;

procedure TMiniBrowserFrm.Acceptlanguage1Click(Sender: TObject);
var
  TempLanguageList : ustring;
begin
  TempLanguageList := Chromium1.AcceptLanguageList;
  if (length(TempLanguageList) = 0) then TempLanguageList := Chromium1.Options.AcceptLanguageList;
  if (length(TempLanguageList) = 0) then TempLanguageList := GlobalCEFApp.AcceptLanguageList;

  Chromium1.AcceptLanguageList := InputBox('Language', 'Accept language list', TempLanguageList);
end;

procedure TMiniBrowserFrm.AddURL(const aURL : string);
begin
  if (URLCbx.Items.IndexOf(aURL) < 0) then URLCbx.Items.Add(aURL);

  URLCbx.Text := aURL;
end;

procedure TMiniBrowserFrm.akescreenshot1Click(Sender: TObject);
begin
  inc(FDevToolsMsgID);
  FScreenshotMsgID := FDevToolsMsgID;
  Chromium1.ExecuteDevToolsMethod(FScreenshotMsgID, 'Page.captureScreenshot', nil);
end;

procedure TMiniBrowserFrm.Chromium1DevToolsMethodResult(      Sender     : TObject;
                                                        const browser    : ICefBrowser;
                                                              message_id : Integer;
                                                              success    : Boolean;
                                                        const result     : ICefValue);
var
  TempDict    : ICefDictionaryValue;
  TempValue   : ICefValue;
  TempResult  : WPARAM;
  TempCode    : integer;
  TempMessage : string;
begin
  FDevToolsMsgValue := '';
  TempResult        := 0;

  if success then
    begin
      TempResult        := 1;
      FDevToolsMsgValue := '';

      if (result <> nil) then
        begin
          TempDict := result.GetDictionary;

          if (TempDict <> nil) and (TempDict.GetSize > 0) then
            begin
              TempValue := TempDict.GetValue('data');

              if (TempValue <> nil) and (TempValue.GetType = VTYPE_STRING) then
                FDevToolsMsgValue := TempValue.GetString;
            end;
        end;
    end
   else
    if (result <> nil) then
      begin
        TempDict := result.GetDictionary;

        if (TempDict <> nil) then
          begin
            TempCode    := 0;
            TempMessage := '';
            TempValue   := TempDict.GetValue('code');

            if (TempValue <> nil) and (TempValue.GetType = VTYPE_INT) then
              TempCode := TempValue.GetInt;

            TempValue := TempDict.GetValue('message');

            if (TempValue <> nil) and (TempValue.GetType = VTYPE_STRING) then
              TempMessage := TempValue.GetString;

            if (length(TempMessage) > 0) then
              FDevToolsMsgValue := 'DevTools Error (' + inttostr(TempCode) + ') : ' + quotedstr(TempMessage);
          end;
      end;

  PostMessage(Handle, MINIBROWSER_DTDATA_AVLBL, TempResult, message_id);
end;

procedure TMiniBrowserFrm.DevToolsDataAvailableMsg(var aMessage : TMessage);
var
  TempData : TBytes;
  TempFile : TFileStream;
  TempLen  : integer;
begin
  if (aMessage.WParam <> 0) then
    begin
      if (length(FDevToolsMsgValue) > 0) then
        begin
          TempData := TNetEncoding.Base64.DecodeStringToBytes(FDevToolsMsgValue);
          TempLen  := length(TempData);

          if (TempLen > 0) then
            begin
              TempFile := nil;

              if (aMessage.LParam = FScreenshotMsgID) then
                begin
                  SaveDialog1.DefaultExt := 'png';
                  SaveDialog1.Filter     := 'PNG files (*.png)|*.PNG';
                end
               else
                begin
                  SaveDialog1.DefaultExt := '';
                  SaveDialog1.Filter     := 'All files (*.*)|*.*';
                end;

              if SaveDialog1.Execute then
                try
                  try
                    TempFile := TFileStream.Create(SaveDialog1.FileName, fmCreate);
                    TempFile.WriteBuffer(TempData[0], TempLen);
                    showmessage('File saved successfully');
                  except
                    showmessage('There was an error saving the file');
                  end;
                finally
                  if (TempFile <> nil) then TempFile.Free;
                end;
            end
           else
            showmessage('There was an error decoding the data');
        end
       else
        showmessage('DevTools method executed successfully!');
    end
   else
    if (length(FDevToolsMsgValue) > 0) then
      showmessage(FDevToolsMsgValue)
     else
      showmessage('There was an error in the DevTools method');
end;

procedure TMiniBrowserFrm.ShowDevToolsMsg(var aMessage : TMessage);
var
  TempPoint : TPoint;
begin
  TempPoint.x := (aMessage.wParam shr 16) and $FFFF;
  TempPoint.y := aMessage.wParam and $FFFF;
  ShowDevTools(TempPoint);
end;

procedure TMiniBrowserFrm.HideDevToolsMsg(var aMessage : TMessage);
begin
  HideDevTools;
  Chromium1.SetFocus(True);
end;

procedure TMiniBrowserFrm.Inczoom1Click(Sender: TObject);
begin
  Chromium1.IncZoomStep;
end;

procedure TMiniBrowserFrm.Openfile1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Any file (*.*)|*.*';

  if OpenDialog1.Execute then
    begin
      // This is a quick solution to load files. The file URL should be properly encoded.
      Chromium1.LoadURL('file:///' + OpenDialog1.FileName);
    end;
end;

procedure TMiniBrowserFrm.OpenfilewithaDAT1Click(Sender: TObject);
var
  TempFile : TMemoryStream;
begin
  TempFile := nil;

  try
    try
      OpenDialog1.Filter := 'HTML files (*.html)|*.HTML;*.HTM|PDF files (*.pdf)|*.PDF';

      if OpenDialog1.Execute then
        begin
          // Use TByteStream instead of TMemoryStream if your Delphi version supports it.
          TempFile := TMemoryStream.Create;
          TempFile.LoadFromFile(OpenDialog1.FileName);

          if (OpenDialog1.FilterIndex = 1) then
            Chromium1.LoadResource(TempFile, 'text/html', 'utf-8')
           else
            Chromium1.LoadResource(TempFile, 'application/pdf', 'utf-8');
        end;
    except
      on e : exception do
        if CustomExceptionHandler('TMiniBrowserFrm.OpenfilewithaDAT1Click', e) then raise;
    end;
  finally
    if (TempFile <> nil) then FreeAndNil(TempFile);
  end;
end;

procedure TMiniBrowserFrm.PopupMenu1Popup(Sender: TObject);
begin
  if DevTools.Visible then
    DevTools1.Caption := 'Hide DevTools'
   else
    DevTools1.Caption := 'Show DevTools';
end;

procedure TMiniBrowserFrm.Preferences1Click(Sender: TObject);
begin
  case Chromium1.ProxyScheme of
    psSOCKS4 : PreferencesFrm.ProxySchemeCb.ItemIndex := 1;
    psSOCKS5 : PreferencesFrm.ProxySchemeCb.ItemIndex := 2;
    else       PreferencesFrm.ProxySchemeCb.ItemIndex := 0;
  end;

  PreferencesFrm.ProxyTypeCbx.ItemIndex           := Chromium1.ProxyType;
  PreferencesFrm.ProxyServerEdt.Text              := Chromium1.ProxyServer;
  PreferencesFrm.ProxyPortEdt.Text                := inttostr(Chromium1.ProxyPort);
  PreferencesFrm.ProxyUsernameEdt.Text            := Chromium1.ProxyUsername;
  PreferencesFrm.ProxyPasswordEdt.Text            := Chromium1.ProxyPassword;
  PreferencesFrm.ProxyScriptURLEdt.Text           := Chromium1.ProxyScriptURL;
  PreferencesFrm.ProxyByPassListEdt.Text          := Chromium1.ProxyByPassList;
  PreferencesFrm.HeaderNameEdt.Text               := Chromium1.CustomHeaderName;
  PreferencesFrm.HeaderValueEdt.Text              := Chromium1.CustomHeaderValue;
  PreferencesFrm.MaxConnectionsPerProxyEdt.Value  := Chromium1.MaxConnectionsPerProxy;

  if (PreferencesFrm.ShowModal = mrOk) then
    begin
      Chromium1.ProxyType              := PreferencesFrm.ProxyTypeCbx.ItemIndex;
      Chromium1.ProxyServer            := PreferencesFrm.ProxyServerEdt.Text;
      Chromium1.ProxyPort              := strtoint(PreferencesFrm.ProxyPortEdt.Text);
      Chromium1.ProxyUsername          := PreferencesFrm.ProxyUsernameEdt.Text;
      Chromium1.ProxyPassword          := PreferencesFrm.ProxyPasswordEdt.Text;
      Chromium1.ProxyScriptURL         := PreferencesFrm.ProxyScriptURLEdt.Text;
      Chromium1.ProxyByPassList        := PreferencesFrm.ProxyByPassListEdt.Text;
      Chromium1.CustomHeaderName       := PreferencesFrm.HeaderNameEdt.Text;
      Chromium1.CustomHeaderValue      := PreferencesFrm.HeaderValueEdt.Text;
      Chromium1.MaxConnectionsPerProxy := PreferencesFrm.MaxConnectionsPerProxyEdt.Value;

      case PreferencesFrm.ProxySchemeCb.ItemIndex of
        1  : Chromium1.ProxyScheme := psSOCKS4;
        2  : Chromium1.ProxyScheme := psSOCKS5;
        else Chromium1.ProxyScheme := psHTTP;
      end;

      Chromium1.UpdatePreferences;
    end;
end;

procedure TMiniBrowserFrm.Print1Click(Sender: TObject);
begin
  Chromium1.Print;
end;

procedure TMiniBrowserFrm.PrintinPDF1Click(Sender: TObject);
begin
  SaveDialog1.DefaultExt := 'pdf';
  SaveDialog1.Filter     := 'PDF files (*.pdf)|*.PDF';

  if SaveDialog1.Execute and (length(SaveDialog1.FileName) > 0) then
    Chromium1.PrintToPDF(SaveDialog1.FileName, Chromium1.DocumentURL, Chromium1.DocumentURL);
end;

procedure TMiniBrowserFrm.ConfigBtnClick(Sender: TObject);
var
  TempPoint : TPoint;
begin
  TempPoint.x := ConfigBtn.left;
  TempPoint.y := ConfigBtn.top + ConfigBtn.Height;
  TempPoint   := ConfigPnl.ClientToScreen(TempPoint);

  PopupMenu1.Popup(TempPoint.x, TempPoint.y);
end;

procedure TMiniBrowserFrm.CopyHTMLMsg(var aMessage : TMessage);
begin
  Chromium1.RetrieveHTML;
end;

procedure TMiniBrowserFrm.CopyAllTextMsg(var aMessage : TMessage);
var
  TempName : string;
begin
  TempName := InputBox('Frame name', 'Type the fame name or leave it blank to select the main frame :', '');
  Chromium1.RetrieveText(TempName);
end;

procedure TMiniBrowserFrm.CopyFramesIDsMsg(var aMessage : TMessage);
var
  i          : NativeUInt;
  TempCount  : NativeUInt;
  TempArray  : TCefFrameIdentifierArray;
  TempString : string;
begin
  TempCount := Chromium1.FrameCount;

  if Chromium1.GetFrameIdentifiers(TempCount, TempArray) then
    begin
      TempString := '';
      i          := 0;

      while (i < TempCount) do
        begin
          TempString := TempString + inttostr(TempArray[i]) + CRLF;
          inc(i);
        end;

      clipboard.AsText := TempString;
    end;
end;

procedure TMiniBrowserFrm.CopyFramesNamesMsg(var aMessage : TMessage);
var
  TempSL : TStringList;
begin
  try
    TempSL := TStringList.Create;

    if Chromium1.GetFrameNames(TStrings(TempSL)) then clipboard.AsText := TempSL.Text;
  finally
    FreeAndNil(TempSL);
  end;
end;

procedure TMiniBrowserFrm.ShowResponseMsg(var aMessage : TMessage);
begin
  SimpleTextViewerFrm.Memo1.Lines.Clear;

  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  SimpleTextViewerFrm.Memo1.Lines.Add('Request headers : ');
  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  if (FRequest <> nil) then SimpleTextViewerFrm.Memo1.Lines.AddStrings(FRequest);

  SimpleTextViewerFrm.Memo1.Lines.Add('');

  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  SimpleTextViewerFrm.Memo1.Lines.Add('Response headers : ');
  SimpleTextViewerFrm.Memo1.Lines.Add('--------------------------');
  if (FResponse <> nil) then SimpleTextViewerFrm.Memo1.Lines.AddStrings(FResponse);

  SimpleTextViewerFrm.ShowModal;
end;

procedure TMiniBrowserFrm.ShowNavigationMsg(var aMessage : TMessage);
begin
  SimpleTextViewerFrm.Memo1.Lines.Clear;
  SimpleTextViewerFrm.Memo1.Lines.AddStrings(FNavigation);
  SimpleTextViewerFrm.ShowModal;
end;

procedure TMiniBrowserFrm.SavePreferencesMsg(var aMessage : TMessage);
begin
  SaveDialog1.DefaultExt := 'txt';
  SaveDialog1.Filter     := 'Text files (*.txt)|*.TXT';

  if SaveDialog1.Execute and (length(SaveDialog1.FileName) > 0) then
    Chromium1.SavePreferences(SaveDialog1.FileName);
end;

procedure TMiniBrowserFrm.TakeSnapshotMsg(var aMessage : TMessage);
var
  TempBitmap : TBitmap;
begin
  TempBitmap := nil;

  try
    SaveDialog1.DefaultExt := 'bmp';
    SaveDialog1.Filter     := 'Bitmap files (*.bmp)|*.BMP';

    if SaveDialog1.Execute and
       (length(SaveDialog1.FileName) > 0) and
       Chromium1.TakeSnapshot(TempBitmap) then
      TempBitmap.SaveToFile(SaveDialog1.FileName);
  finally
    if (TempBitmap <> nil) then FreeAndNil(TempBitmap);
  end;
end;

procedure TMiniBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMiniBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TMiniBrowserFrm.Deczoom1Click(Sender: TObject);
begin
  Chromium1.DecZoomStep;
end;

procedure TMiniBrowserFrm.DevTools1Click(Sender: TObject);
begin
  if DevTools.Visible then
    HideDevTools
   else
    ShowDevTools;
end;

procedure TMiniBrowserFrm.Downloadimage1Click(Sender: TObject);
var
  TempURL : string;
begin
  TempURL := InputBox('Download Image', 'URL:', 'https://www.briskbard.com/images/logo.png');

  if (length(TempURL) > 0) then
    Chromium1.DownloadImage(TempURL, False, 0, False);
end;

procedure TMiniBrowserFrm.Chromium1DownloadImageFinished(      Sender         : TObject;
                                                         const imageUrl       : ustring;
                                                               httpStatusCode : Integer;
                                                         const image          : ICefImage);
var
  TempBinValue : ICefBinaryValue;
  TempWidth    : integer;
  TempHeight   : integer;
  TempBuffer   : TBytes;
  TempPointer  : pointer;
  TempSize     : NativeUInt;
  TempStream   : TFileStream;
  TempParts    : TUrlParts;
  i            : integer;
begin
  TempStream := nil;

  try
    try
      if (httpStatusCode = 200) and (image <> nil) and not(image.IsEmpty) then
        begin
          TempBinValue := image.GetAsPng(1, True, TempWidth, TempHeight);

          if (TempBinValue <> nil) and
             TempBinValue.IsValid  then
            begin
              TempSize := TempBinValue.Size;

              SaveDialog1.DefaultExt := 'png';
              SaveDialog1.Filter     := 'PNG files (*.png)|*.PNG';

              CefParseUrl(imageUrl, TempParts);
              i := LastDelimiter('/', TempParts.path);

              // TODO : The file name should be sanitized.
              if (i > 0) then
                SaveDialog1.FileName := copy(TempParts.path, succ(i), length(TempParts.path))
               else
                SaveDialog1.FileName := TempParts.path;

              if (TempSize > 0) and
                 SaveDialog1.Execute and
                 (length(SaveDialog1.FileName) > 0) then
                begin
                  SetLength(TempBuffer, TempSize);
                  TempPointer := @TempBuffer[0];
                  TempSize    := TempBinValue.GetData(TempPointer, TempSize, 0);

                  if (TempSize > 0) then
                    begin
                      TempStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
                      TempStream.Write(TempBuffer, TempSize);
                    end;
                end;
            end;
        end;
    except
      on e : exception do
        if CustomExceptionHandler('Chromium1DownloadImageFinishedEvent', e) then raise;
    end;
  finally
    if (TempStream <> nil) then FreeAndNil(TempStream);
    SetLength(TempBuffer, 0);
  end;
end;

procedure TMiniBrowserFrm.ShowDevTools(aPoint : TPoint);
begin
  Splitter1.Visible := True;
  DevTools.Visible  := True;
  DevTools.Width    := Width div 4;
  Chromium1.ShowDevTools(aPoint, DevTools);
end;

procedure TMiniBrowserFrm.ShowDevTools;
var
  TempPoint : TPoint;
begin
  TempPoint.x := low(integer);
  TempPoint.y := low(integer);
  ShowDevTools(TempPoint);
end;

procedure TMiniBrowserFrm.HideDevTools;
begin
  Chromium1.CloseDevTools(DevTools);
  Splitter1.Visible := False;
  DevTools.Visible  := False;
  DevTools.Width    := 0;
end;

end.
