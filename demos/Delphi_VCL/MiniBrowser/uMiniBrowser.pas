unit uMiniBrowser;

{$I ..\..\..\source\cef.inc}

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
  uCEFConstants, uCEFWinControl, uCEFChromiumCore, uCEFFileDialogInfo;

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
  MINIBROWSER_SHOWFILEDLG      = WM_APP + $10F;
  MINIBROWSER_SELECTCERT       = WM_APP + $110;
  MINIBROWSER_MEDIAACCESSRQST  = WM_APP + $111;
  MINIBROWSER_SHOWBROWSERINFO  = WM_APP + $112;

  MINIBROWSER_HOMEPAGE = 'https://www.google.com';

  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS      = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS      = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_COPYHTML          = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_JSWRITEDOC        = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_JSPRINTDOC        = MENU_ID_USER_FIRST + 5;
  MINIBROWSER_CONTEXTMENU_SHOWRESPONSE      = MENU_ID_USER_FIRST + 6;
  MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS      = MENU_ID_USER_FIRST + 7;
  MINIBROWSER_CONTEXTMENU_COPYFRAMENAMES    = MENU_ID_USER_FIRST + 8;
  MINIBROWSER_CONTEXTMENU_SAVEPREFERENCES   = MENU_ID_USER_FIRST + 9;
  MINIBROWSER_CONTEXTMENU_COPYALLTEXT       = MENU_ID_USER_FIRST + 10;
  MINIBROWSER_CONTEXTMENU_TAKESNAPSHOT      = MENU_ID_USER_FIRST + 11;
  MINIBROWSER_CONTEXTMENU_GETNAVIGATION     = MENU_ID_USER_FIRST + 12;
  MINIBROWSER_CONTEXTMENU_MUTEAUDIO         = MENU_ID_USER_FIRST + 13;
  MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO       = MENU_ID_USER_FIRST + 14;
  MINIBROWSER_CONTEXTMENU_INCZOOM           = MENU_ID_USER_FIRST + 15;
  MINIBROWSER_CONTEXTMENU_DECZOOM           = MENU_ID_USER_FIRST + 16;
  MINIBROWSER_CONTEXTMENU_RESETZOOM         = MENU_ID_USER_FIRST + 17;
  MINIBROWSER_CONTEXTMENU_EXITFULLSCREEN    = MENU_ID_USER_FIRST + 18;
  MINIBROWSER_CONTEXTMENU_BROWSERINFO       = MENU_ID_USER_FIRST + 19;
  MINIBROWSER_CONTEXTMENU_COLORSCHEME       = MENU_ID_USER_FIRST + 20;
  MINIBROWSER_CONTEXTMENU_COLORSCHEMESYSTEM = MENU_ID_USER_FIRST + 21;
  MINIBROWSER_CONTEXTMENU_COLORSCHEMELIGHT  = MENU_ID_USER_FIRST + 22;
  MINIBROWSER_CONTEXTMENU_COLORSCHEMEDARK   = MENU_ID_USER_FIRST + 23;
  MINIBROWSER_CONTEXTMENU_TASKMANAGER       = MENU_ID_USER_FIRST + 24;

  DEVTOOLS_SCREENSHOT_MSGID       = 1001;
  DEVTOOLS_MHTML_MSGID            = 1002;
  DEVTOOLS_BROWSERINFO_MSGID      = 1003;

type
  TBrowserInfo = record
    protocolVersion : string;
    product         : string;
    revision        : string;
    userAgent       : string;
    jsVersion       : string;
    RuntimeStyle    : string;
  end;

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
    CEFinfo1: TMenuItem;
    SaveasMHTML1: TMenuItem;
    Allowdownloads1: TMenuItem;
    Toggleaudio1: TMenuItem;

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
    procedure Chromium1PreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
    procedure Chromium1KeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
    procedure Chromium1PdfPrintFinished(Sender: TObject; aResultOK: Boolean);
    procedure Chromium1ResourceResponse(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const response: ICefResponse; out Result: Boolean);
    procedure Chromium1PrefsAvailable(Sender: TObject; aResultOK: Boolean);
    procedure Chromium1BeforeDownload(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const suggestedName: ustring; const callback: ICefBeforeDownloadCallback; var aResult : boolean);
    procedure Chromium1DownloadUpdated(Sender: TObject; const browser: ICefBrowser; const downloadItem: ICefDownloadItem; const callback: ICefDownloadItemCallback);
    procedure Chromium1BeforeResourceLoad(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; const callback: ICefCallback; out Result: TCefReturnValue);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1RenderCompMsg(Sender: TObject; var aMessage : TMessage; var aHandled: Boolean);
    procedure Chromium1LoadingProgressChange(Sender: TObject; const browser: ICefBrowser; const progress: Double);
    procedure Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1NavigationVisitorResultAvailable(Sender: TObject; const entry: ICefNavigationEntry; current: Boolean; index, total: Integer; var aResult: Boolean);
    procedure Chromium1DownloadImageFinished(Sender: TObject; const imageUrl: ustring; httpStatusCode: Integer; const image: ICefImage);
    procedure Chromium1CookiesFlushed(Sender: TObject);
    procedure Chromium1ZoomPctAvailable(Sender: TObject; const aZoomPct: Double);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);
    procedure Chromium1SelectClientCertificate(Sender: TObject; const browser: ICefBrowser; isProxy: Boolean; const host: ustring; port: Integer; certificatesCount: NativeUInt; const certificates: TCefX509CertificateArray; const callback: ICefSelectClientCertificateCallback; var aResult: Boolean);
    procedure Chromium1CanDownload(Sender: TObject; const browser: ICefBrowser; const url, request_method: ustring; var aResult: Boolean);
    procedure Chromium1MediaAccessChange(Sender: TObject; const browser: ICefBrowser; has_video_access, has_audio_access: Boolean);
    procedure Chromium1RequestMediaAccessPermission(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const requesting_origin: ustring; requested_permissions: Cardinal; const callback: ICefMediaAccessCallback; var aResult: Boolean);
    procedure Chromium1CertificateError(Sender: TObject; const browser: ICefBrowser; certError: TCefErrorCode; const requestUrl: ustring; const sslInfo: ICefSslInfo; const callback: ICefCallback; out Result: Boolean);
    procedure Chromium1ConsoleMessage(Sender: TObject; const browser: ICefBrowser; level: TCefLogSeverity; const message_, source: ustring; line: Integer; out Result: Boolean);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean);
    procedure Chromium1FileDialog(Sender: TObject; const browser: ICefBrowser; mode: TCefFileDialogMode; const title, defaultFilePath: ustring; const acceptFilters, accept_extensions, accept_descriptions: TStrings; const callback: ICefFileDialogCallback; var Result: Boolean);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
    procedure Chromium1ResolvedHostAvailable(Sender: TObject; result: TCefErrorCode; const resolvedIps: TStrings);
    procedure Chromium1ChromeCommand(Sender: TObject; const browser: ICefBrowser; command_id: Integer; disposition: TCefWindowOpenDisposition; var aResult: Boolean);

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
    procedure CEFinfo1Click(Sender: TObject);
    procedure SaveasMHTML1Click(Sender: TObject);
    procedure Allowdownloads1Click(Sender: TObject);
    procedure Toggleaudio1Click(Sender: TObject);

  protected
    FPendingMsgID        : integer;
    FDevToolsMsgValue    : ustring;
    FShutdownReason      : string;
    FHasShutdownReason   : boolean;
    FSelectCertCallback  : ICefSelectClientCertificateCallback;
    FCertificates        : TCefX509CertificateArray;
    FAllowDownloads      : boolean;
    FJSException         : integer;
    FBrowserInfo         : TBrowserInfo;

    FMediaAccessCallback  : ICefMediaAccessCallback;
    FRequestingOrigin     : string;
    FRequestedPermissions : cardinal;

    FResponse   : TStringList;
    FRequest    : TStringList;
    FNavigation : TStringList;

    FFileDialogInfo : TCEFFileDialogInfo;

    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure AddURL(const aURL : string);
    procedure DestroyCertificates;

    procedure ShowDevTools(aPoint : TPoint); overload;
    procedure ShowDevTools; overload;
    procedure HideDevTools;

    procedure ShowTaskManager;

    procedure HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
    procedure HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);
    procedure HandleBrowserInfo(const aResult : ICefValue);

    procedure ReplaceAcceptEncoding(const aRequest : ICefRequest);
    procedure InspectRequest(const aRequest : ICefRequest);
    procedure InspectResponse(const aResponse : ICefResponse);

    function  ShowOpenFileDialog(var aFilePaths : TStringList; aMultiple : boolean) : boolean;
    function  ShowOpenFolderDialog(var aFilePaths : TStringList) : boolean;
    function  ShowSaveFileDialog(var aFilePaths : TStringList) : boolean;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
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
    procedure ShowFileDialogMsg(var aMessage : TMessage); message MINIBROWSER_SHOWFILEDLG;
    procedure SelectCertificateMsg(var aMessage : TMessage); message MINIBROWSER_SELECTCERT;
    procedure MediaAccessRequestMsg(var aMessage : TMessage); message MINIBROWSER_MEDIAACCESSRQST;
    procedure ShowBrowserInfoMsg(var aMessage : TMessage); message MINIBROWSER_SHOWBROWSERINFO;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure WMQueryEndSession(var aMessage: TWMQueryEndSession); message WM_QUERYENDSESSION;

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
  uCEFClient, uFindFrm, uCEFDictionaryValue, uDirectorySelector, uSelectCertForm,
  uCEFWindowInfoWrapper, uCEFTaskManager;

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure GlobalCEFApp_OnUncaughtException(const browser: ICefBrowser; const frame: ICefFrame; const context: ICefv8Context; const exception: ICefV8Exception; const stackTrace: ICefV8StackTrace);
begin
  // This code runs in the render process and we can't set a breakpoint to debug it from the main application process.
  // Read this for more information about debugging CEF subprocesses :
  // https://www.briskbard.com/index.php?lang=en&pageid=cef#debugging
  // In this example we only use the "console trick" explained in the DOMVisitor demo to send some custom text to the
  // main process that will be received in TChromiumCore.OnConsoleMessage
  // Load the following page and delete the try..catch lines to test this event :
  // https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_state_throw_error
  if assigned(frame) and frame.IsValid then
    frame.ExecuteJavaScript('console.log("GlobalCEFApp_OnUncaughtException");', '', 0);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.cache                      := 'cache';
  GlobalCEFApp.EnablePrintPreview         := True;
  GlobalCEFApp.EnableGPU                  := True;
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  GlobalCEFApp.UncaughtExceptionStackSize := 50;
  GlobalCEFApp.OnUncaughtException        := GlobalCEFApp_OnUncaughtException;
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

procedure TMiniBrowserFrm.CEFinfo1Click(Sender: TObject);
var
  TempInfo : string;
begin
  TempInfo := 'libcef.dll : '         + CRLF + GlobalCEFApp.LibCefVersion    + CRLF + CRLF +
              'chrome_elf.dll : '     + CRLF + GlobalCEFApp.ChromeVersion    + CRLF + CRLF +
              'Universal API hash : ' + CRLF + GlobalCEFApp.ApiHashUniversal + CRLF + CRLF +
              'Platform API hash : '  + CRLF + GlobalCEFApp.ApiHashPlatform  + CRLF + CRLF +
              'Commit API hash : '    + CRLF + GlobalCEFApp.ApiHashCommit;

  showmessage(TempInfo);
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
var
  TempModel: ICefMenuModel;
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
      model.AddItem(MINIBROWSER_CONTEXTMENU_BROWSERINFO,     'Browser information...');
      model.AddItem(MINIBROWSER_CONTEXTMENU_TASKMANAGER,     'Task Manager...');

      if DevTools.Visible then
        model.AddItem(MINIBROWSER_CONTEXTMENU_HIDEDEVTOOLS, 'Hide DevTools')
       else
        model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');

      if Chromium1.AudioMuted then
        model.AddItem(MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO, 'Unmute audio')
       else
        model.AddItem(MINIBROWSER_CONTEXTMENU_MUTEAUDIO,   'Mute audio');

      model.AddSeparator;
      if Chromium1.CanIncZoom then
        model.AddItem(MINIBROWSER_CONTEXTMENU_INCZOOM,   'Increment zoom');

      if Chromium1.CanDecZoom then
        model.AddItem(MINIBROWSER_CONTEXTMENU_DECZOOM,   'Decrement zoom');

      if Chromium1.CanResetZoom then
        model.AddItem(MINIBROWSER_CONTEXTMENU_RESETZOOM, 'Reset zoom');

      if Chromium1.Fullscreen then
        begin
          model.AddSeparator;
          model.AddItem(MINIBROWSER_CONTEXTMENU_EXITFULLSCREEN, 'Exit fullscreen');
        end;

      model.AddSeparator;
      TempModel := model.AddSubMenu(MINIBROWSER_CONTEXTMENU_COLORSCHEME, 'Color scheme');
      TempModel.AddCheckItem(MINIBROWSER_CONTEXTMENU_COLORSCHEMESYSTEM, 'System');
      TempModel.AddCheckItem(MINIBROWSER_CONTEXTMENU_COLORSCHEMELIGHT, 'Light');
      TempModel.AddCheckItem(MINIBROWSER_CONTEXTMENU_COLORSCHEMEDARK, 'Dark');

      case Chromium1.ChromeColorSchemeMode of
        CEF_COLOR_VARIANT_LIGHT : TempModel.setChecked(MINIBROWSER_CONTEXTMENU_COLORSCHEMELIGHT,  True);
        CEF_COLOR_VARIANT_DARK  : TempModel.setChecked(MINIBROWSER_CONTEXTMENU_COLORSCHEMEDARK,   True);
        else                      TempModel.setChecked(MINIBROWSER_CONTEXTMENU_COLORSCHEMESYSTEM, True);
      end;
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
  const callback: ICefBeforeDownloadCallback; var aResult : boolean);
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

  aResult := True;
  callback.cont(TempFullPath, True);
end;

procedure TMiniBrowserFrm.Chromium1BeforeResourceLoad(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const callback: ICefCallback;
  out Result: TCefReturnValue);
begin
  Result := RV_CONTINUE;

  if Chromium1.IsSameBrowser(browser) and
     (frame <> nil) and
     frame.IsMain and
     frame.IsValid then
    begin
      ReplaceAcceptEncoding(request);
      InspectRequest(request);
    end;
end;

procedure TMiniBrowserFrm.Chromium1CanDownload(Sender: TObject;
  const browser: ICefBrowser; const url, request_method: ustring;
  var aResult: Boolean);
begin
  aResult := FAllowDownloads;
end;

procedure TMiniBrowserFrm.Chromium1CertificateError(Sender: TObject;
  const browser: ICefBrowser; certError: TCefErrorCode;
  const requestUrl: ustring; const sslInfo: ICefSslInfo;
  const callback: ICefCallback; out Result: Boolean);
begin
  CefDebugLog('Certificate error code:' + inttostr(certError) +
              ' - URL:' + requestUrl, CEF_LOG_SEVERITY_ERROR);
  Result := False;
end;

procedure TMiniBrowserFrm.Chromium1ChromeCommand(Sender: TObject;
  const browser: ICefBrowser; command_id: Integer;
  disposition: TCefWindowOpenDisposition; var aResult: Boolean);
begin
  aResult := (command_id = IDC_HELP_PAGE_VIA_KEYBOARD) or // Block the new Chromium window created when the user presses F1 for help.
             (command_id = IDC_FULLSCREEN);               // Block the "switch to full screen" command when the user presses F11.
end;

procedure TMiniBrowserFrm.Chromium1ConsoleMessage(Sender: TObject;
  const browser: ICefBrowser; level: TCefLogSeverity;
  const message_, source: ustring; line: Integer; out Result: Boolean);
begin
  if (message_ = 'GlobalCEFApp_OnUncaughtException') then
    begin
      inc(FJSException);
      StatusBar1.Panels[4].Text := 'JS exception: ' + inttostr(FJSException);
    end;
end;

procedure TMiniBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; out Result: Boolean);
var
  TempParam : WParam;
  TempPoint : TPoint;
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

      MINIBROWSER_CONTEXTMENU_INCZOOM :
        Chromium1.IncZoomCommand;

      MINIBROWSER_CONTEXTMENU_DECZOOM :
        Chromium1.DecZoomCommand;

      MINIBROWSER_CONTEXTMENU_RESETZOOM :
        Chromium1.ResetZoomCommand;

      MINIBROWSER_CONTEXTMENU_EXITFULLSCREEN :
        Chromium1.ExitFullscreen(True);

      MINIBROWSER_CONTEXTMENU_BROWSERINFO :
        begin
          FPendingMsgID := DEVTOOLS_BROWSERINFO_MSGID;
          Chromium1.ExecuteDevToolsMethod(0, 'Browser.getVersion', nil);
        end;

      MINIBROWSER_CONTEXTMENU_COLORSCHEMELIGHT :
        Chromium1.SetChromeColorScheme(CEF_COLOR_VARIANT_LIGHT, 0);

      MINIBROWSER_CONTEXTMENU_COLORSCHEMEDARK :
        Chromium1.SetChromeColorScheme(CEF_COLOR_VARIANT_DARK, 0);

      MINIBROWSER_CONTEXTMENU_COLORSCHEMESYSTEM :
        Chromium1.SetChromeColorScheme(CEF_COLOR_VARIANT_SYSTEM, 0);

      MINIBROWSER_CONTEXTMENU_TASKMANAGER :
        ShowTaskManager;
    end
   else
    case commandId of
      MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
        begin
          TempPoint.x := params.XCoord;
          TempPoint.y := params.YCoord;
          ShowDevTools(TempPoint);
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
      if downloadItem.IsInterrupted then
        ShowStatusText(downloadItem.FullPath + ' interrupted')
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

procedure TMiniBrowserFrm.Chromium1FileDialog(Sender: TObject;
  const browser: ICefBrowser; mode: TCefFileDialogMode;
  const title, defaultFilePath: ustring; const acceptFilters, accept_extensions, accept_descriptions: TStrings;
  const callback: ICefFileDialogCallback; var Result: Boolean);
begin
  Result := True;

  FFileDialogInfo.Mode                   := mode;
  FFileDialogInfo.Title                  := title;
  FFileDialogInfo.DefaultFilePath        := defaultFilePath;
  FFileDialogInfo.Callback               := callback;
  FFileDialogInfo.AcceptFilters          := acceptFilters;
  FFileDialogInfo.AcceptExtensions       := accept_extensions;
  FFileDialogInfo.AcceptDescriptions     := accept_descriptions;

  PostMessage(Handle, MINIBROWSER_SHOWFILEDLG, 0, 0);
end;


function TMiniBrowserFrm.ShowOpenFileDialog(var aFilePaths : TStringList; aMultiple : boolean) : boolean;
var
  TempDialog  : TOpenDialog;
  TempOptions : TOpenOptions;
begin
  Result := False;

  TempDialog             := TOpenDialog.Create(Application.MainForm);
  TempDialog.Title       := FFileDialogInfo.Title;
  TempDialog.InitialDir  := FFileDialogInfo.DefaultFilePath;
  TempDialog.Filter      := FFileDialogInfo.DialogFilter;
  TempOptions            := TempDialog.Options;

  if aMultiple then include(TempOptions, ofAllowMultiSelect);

  TempDialog.Options := TempOptions;

  if TempDialog.Execute(Handle) then
    begin
      if aMultiple then
        aFilePaths.AddStrings(TempDialog.Files)
       else
        aFilePaths.Add(TempDialog.FileName);

      Result := True;
    end;

  FreeAndNil(TempDialog);
end;

function TMiniBrowserFrm.ShowOpenFolderDialog(var aFilePaths : TStringList) : boolean;
var
  TempDirectorySelector : TDirectorySelectorFrm;
begin
  Result := False;

  TempDirectorySelector             := TDirectorySelectorFrm.Create(Application.MainForm);
  TempDirectorySelector.SelectedDir := FFileDialogInfo.DefaultFilePath;

  if (TempDirectorySelector.ShowModal = mrOk) then
    begin
      {$WARN SYMBOL_PLATFORM OFF}
      aFilePaths.Add(IncludeTrailingBackslash(TempDirectorySelector.SelectedDir));
      {$WARN SYMBOL_PLATFORM ON}
      Result := True;
    end;

  FreeAndNil(TempDirectorySelector);
end;

function TMiniBrowserFrm.ShowSaveFileDialog(var aFilePaths : TStringList) : boolean;
var
  TempDialog  : TSaveDialog;
  TempOptions : TOpenOptions;
begin
  Result := False;

  TempDialog             := TSaveDialog.Create(Application.MainForm);
  TempDialog.Title       := FFileDialogInfo.Title;
  TempDialog.Filter      := FFileDialogInfo.DialogFilter;
  TempDialog.FileName    := ExtractFileName(FFileDialogInfo.DefaultFilePath);
  TempDialog.InitialDir  := ExtractFileDir(FFileDialogInfo.DefaultFilePath);
  TempOptions            := TempDialog.Options;

  TempDialog.Options := TempOptions;

  if TempDialog.Execute(Handle) and
     (length(TempDialog.FileName) > 0) then
    begin
      aFilePaths.Add(TempDialog.FileName);
      Result := True;
    end;

  FreeAndNil(TempDialog);
end;

procedure TMiniBrowserFrm.ShowFileDialogMsg(var aMessage : TMessage);
var
  TempResult    : boolean;
  TempFilePaths : TStringList;
begin
  TempFilePaths := TStringList.Create;

  case FFileDialogInfo.DialogType of
    dtOpen          : TempResult := ShowOpenFileDialog(TempFilePaths, False);
    dtOpenMultiple  : TempResult := ShowOpenFileDialog(TempFilePaths, True);
    dtOpenFolder    : TempResult := ShowOpenFolderDialog(TempFilePaths);
    dtSave          : TempResult := ShowSaveFileDialog(TempFilePaths);
    else              TempResult := False;
  end;

  if TempResult then
    FFileDialogInfo.Callback.Cont(TempFilePaths)
   else
    FFileDialogInfo.Callback.Cancel;

  FFileDialogInfo.Clear;
  TempFilePaths.Free;
end;

procedure TMiniBrowserFrm.SelectCertificateMsg(var aMessage : TMessage);
var
  TempSelector : TSelectCertForm;
  TempInfo : string;
  i : integer;
begin
  if assigned(FCertificates) and assigned(FSelectCertCallback) then
    try
      TempSelector := TSelectCertForm.Create(self);

      i := 0;
      while (i < length(FCertificates)) do
        begin
          TempInfo := FCertificates[i].GetSubject.GetDisplayName + ' - ' +
                      'Valid until : ' + DateToStr(CefBaseTimeToDateTime(FCertificates[i].GetValidExpiry));

          TempSelector.Certificates.Add(TempInfo);
          inc(i);
        end;

      TempSelector.ShowModal;

      if TempSelector.Selected >= 0 then
        FSelectCertCallback.Select(FCertificates[TempSelector.Selected])
       else
        FSelectCertCallback.Select(nil);
    finally
      FSelectCertCallback := nil;
      DestroyCertificates;
      FreeAndNil(TempSelector);
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
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle;
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

procedure TMiniBrowserFrm.Allowdownloads1Click(Sender: TObject);
begin
  FAllowDownloads := not(FAllowDownloads);
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
var
  TempHandle : THandle;
begin
  if FClosing or (frame = nil) or not(frame.IsValid) or (browser = nil) then exit;

  if Chromium1.IsSameBrowser(browser) then
    begin
      if frame.IsMain then
        StatusBar1.Panels[1].Text := 'main frame loaded : ' + quotedstr(frame.name)
       else
        StatusBar1.Panels[1].Text := 'frame loaded : ' + quotedstr(frame.name);
    end
   else
    begin
      // This is a workaround for a focus issue in popup windows handled by CEF
      TempHandle := WinApi.Windows.GetWindow(Browser.Host.WindowHandle, GW_OWNER);
      if (TempHandle <> Handle) then
        WinApi.Windows.SetFocus(TempHandle);
    end;
end;

procedure TMiniBrowserFrm.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
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

procedure TMiniBrowserFrm.Chromium1MediaAccessChange(Sender: TObject;
  const browser: ICefBrowser; has_video_access, has_audio_access: Boolean);
var
  TempText : string;
begin
  // This event is executed in a CEF thread and this can cause problems when
  // you change the 'Enabled' and 'Visible' properties from VCL components.
  // It's recommended to change the 'Enabled' and 'Visible' properties
  // in the main application thread and not in a CEF thread.
  // It's much safer to use PostMessage to send a message to the main form with
  // all this information and update those properties in the procedure handling
  // that message.
  TempText := 'Video access : ' + BooltoStr(has_video_access, True) + ' - ' +
              'Audio access : ' + BooltoStr(has_audio_access, True);
  StatusBar1.Panels[1].Text := TempText;
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
  const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle;
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
  if FClosing then exit;

  case aMessage.Msg of
    WM_MOUSEMOVE :
      begin
        StatusBar1.Panels[2].Text := 'x : ' + inttostr(aMessage.lParam and $FFFF);
        StatusBar1.Panels[3].Text := 'y : ' + inttostr((aMessage.lParam and $FFFF0000) shr 16);
      end;
  end;
end;

procedure TMiniBrowserFrm.Chromium1RequestMediaAccessPermission(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const requesting_origin: ustring; requested_permissions: Cardinal;
  const callback: ICefMediaAccessCallback; var aResult: Boolean);
begin
  aResult                 := True;
  FMediaAccessCallback    := callback;
  FRequestingOrigin       := requesting_origin;
  FRequestedPermissions   := requested_permissions;

  PostMessage(Handle, MINIBROWSER_MEDIAACCESSRQST, 0, 0);
end;

procedure TMiniBrowserFrm.MediaAccessRequestMsg(var aMessage : TMessage);
var
  TempMessage : string;
  TempPermissions : TStringList;
begin
  TempPermissions := TStringList.Create;
  try
    if ((FRequestedPermissions and CEF_MEDIA_PERMISSION_DEVICE_AUDIO_CAPTURE) <> 0) then
      TempPermissions.Add('Device audio');

    if ((FRequestedPermissions and CEF_MEDIA_PERMISSION_DEVICE_VIDEO_CAPTURE) <> 0) then
      TempPermissions.Add('Device video');

    if ((FRequestedPermissions and CEF_MEDIA_PERMISSION_DESKTOP_AUDIO_CAPTURE) <> 0) then
      TempPermissions.Add('Desktop audio');

    if ((FRequestedPermissions and CEF_MEDIA_PERMISSION_DESKTOP_VIDEO_CAPTURE) <> 0) then
      TempPermissions.Add('Desktop video');

    if assigned(FMediaAccessCallback) and (TempPermissions.Count > 0) then
      begin
        TempMessage := FRequestingOrigin +
                       ' is asking for permission to access : ' + CRLF +
                       TempPermissions.Text + CRLF +
                       'Do you want allow it?';

       if (MessageDlg(TempMessage, mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes) then
         FMediaAccessCallback.cont(FRequestedPermissions)
        else
         FMediaAccessCallback.Cancel;
      end;
  finally
    TempPermissions.Free;
    FMediaAccessCallback := nil;
  end;
end;

procedure TMiniBrowserFrm.ShowBrowserInfoMsg(var aMessage : TMessage);
var
  TempInfo : string;
begin
  TempInfo := 'protocolVersion: ' + FBrowserInfo.protocolVersion + CRLF +
              'product: '         + FBrowserInfo.product         + CRLF +
              'revision: '        + FBrowserInfo.revision        + CRLF +
              'userAgent: '       + FBrowserInfo.userAgent       + CRLF +
              'jsVersion: '       + FBrowserInfo.jsVersion       + CRLF +
              'RuntimeStyle: '    + FBrowserInfo.RuntimeStyle    + CRLF + CRLF +
              'GetDefaultCEFUserAgent: ' + GetDefaultCEFUserAgent;

  showmessage(TempInfo);
end;

procedure TMiniBrowserFrm.Chromium1ResolvedHostAvailable(Sender: TObject;
  result: TCefErrorCode; const resolvedIps: TStrings);
begin
  if (result = ERR_NONE) then
    showmessage('Resolved IPs : ' + resolvedIps.CommaText)
   else
    showmessage('There was a problem resolving the host.' + CRLF +
                'Error code : ' + inttostr(result));
end;

// This is just an example of HTTP header replacement.
procedure TMiniBrowserFrm.ReplaceAcceptEncoding(const aRequest : ICefRequest);
const
  ACCEPT_ENCODING_HEADER = 'Accept-Encoding';
var
  TempOldMap, TempNewMap : ICefStringMultimap;
  i : NativeUInt;
begin
  try
    TempNewMap := TCefStringMultimapOwn.Create;
    TempOldMap := TCefStringMultimapOwn.Create;

    // We get all the old request headers
    aRequest.GetHeaderMap(TempOldMap);

    i := 0;
    while (i < TempOldMap.Size) do
      begin
        // Copy all headers except the "Accept-Encoding" header
        if (CompareText(TempOldMap.Key[i], ACCEPT_ENCODING_HEADER) <> 0) then
          TempNewMap.Append(TempOldMap.Key[i], TempOldMap.Value[i]);

        inc(i);
      end;

    // We append the new "Accept-Encoding" header with a custom value
    TempNewMap.Append(ACCEPT_ENCODING_HEADER, 'gzip');

    // And then we set all the headers in the request
    aRequest.SetHeaderMap(TempNewMap);
  finally
    TempNewMap := nil;
    TempOldMap := nil;
  end;
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

procedure TMiniBrowserFrm.Toggleaudio1Click(Sender: TObject);
begin
  Chromium1.ToggleAudioMuted;
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

procedure TMiniBrowserFrm.Chromium1SelectClientCertificate(Sender: TObject;
  const browser: ICefBrowser; isProxy: Boolean; const host: ustring;
  port: Integer; certificatesCount: NativeUInt;
  const certificates: TCefX509CertificateArray;
  const callback: ICefSelectClientCertificateCallback; var aResult: Boolean);
var
  i : integer;
begin
  if assigned(callback) and assigned(certificates) and (length(certificates) > 0) then
    begin
      aResult             := True;
      FSelectCertCallback := callback;

      SetLength(FCertificates, length(certificates));

      for i := 0 to pred(length(certificates)) do
        FCertificates[i] := certificates[i];

      PostMessage(Handle, MINIBROWSER_SELECTCERT, 0, 0);
    end
   else
    aResult := False;
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
  Chromium1.ExecuteChromeCommand(IDC_FIND, CEF_WOD_CURRENT_TAB);
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

      if DevTools.Visible then HideDevTools;

      // if TChromium.MultiBrowserMode is enabled then we have to close all
      // stored browsers and not only the main browser.
      Chromium1.CloseAllBrowsers;

      CEFWindowParent1.Free;
    end;
end;

procedure TMiniBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose            := False;
  FClosing             := False;
  FResponse            := TStringList.Create;
  FRequest             := TStringList.Create;
  FNavigation          := TStringList.Create;
  FSelectCertCallback  := nil;
  FMediaAccessCallback := nil;
  FCertificates        := nil;
  FPendingMsgID        := 0;
  FAllowDownloads      := True;

  // Windows may show this text message while shutting down the operating system
  FShutdownReason      := 'MiniBrowser closing...';
  FHasShutdownReason   := ShutdownBlockReasonCreate(Application.Handle, @FShutdownReason[1]);

  FFileDialogInfo := TCEFFileDialogInfo.Create;

  // The MultiBrowserMode store all the browser references in TChromium.
  // The first browser reference is the browser in the main form.
  // When MiniBrowser allows CEF to create child popup browsers it will also
  // store their reference inside TChromium and you can use all the TChromium's
  // methods and properties to manipulate those browsers.
  // To do that call TChromium.SelectBrowser with the browser ID that will be
  // used when you call any method or property in TChromium.
  Chromium1.MultiBrowserMode := True;
  Chromium1.DefaultURL       := MINIBROWSER_HOMEPAGE;
  //Chromium1.RuntimeStyle     := CEF_RUNTIME_STYLE_ALLOY;
  //Chromium1.RuntimeStyle     := CEF_RUNTIME_STYLE_CHROME;
end;

procedure TMiniBrowserFrm.FormDestroy(Sender: TObject);
begin
  if FHasShutdownReason then
    ShutdownBlockReasonDestroy(Application.Handle);

  DestroyCertificates;

  FSelectCertCallback  := nil;
  FMediaAccessCallback := nil;
  FResponse.Free;
  FRequest.Free;
  FNavigation.Free;
  FFileDialogInfo.Free;
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
  TempOldUA, TempNewUA : string;
begin
  TempOldUA := GetDefaultCEFUserAgent;
  TempNewUA := inputbox('MiniBrowser demo', 'Set new user agent string', TempOldUA);

  if (length(TempNewUA) > 0) and (TempOldUA <> TempNewUA) then
    Chromium1.SetUserAgentOverride(TempNewUA);
end;

procedure TMiniBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
end;

procedure TMiniBrowserFrm.Acceptlanguage1Click(Sender: TObject);
var
  TempLanguageList : ustring;
begin
  TempLanguageList := Chromium1.AcceptLanguageList;
  if (length(TempLanguageList) = 0) then TempLanguageList := GlobalCEFApp.AcceptLanguageList;

  Chromium1.AcceptLanguageList := InputBox('Language', 'Accept language list', TempLanguageList);
end;

procedure TMiniBrowserFrm.AddURL(const aURL : string);
begin
  if (URLCbx.Items.IndexOf(aURL) < 0) then URLCbx.Items.Add(aURL);

  URLCbx.Text := aURL;
end;

procedure TMiniBrowserFrm.DestroyCertificates;
var
  i : integer;
begin
  if assigned(FCertificates) then
    begin
      i := 0;
      while (i < length(FCertificates)) do
        begin
          FCertificates[i] := nil;
          inc(i);
        end;

      Finalize(FCertificates);
      FCertificates := nil;
    end;
end;

procedure TMiniBrowserFrm.akescreenshot1Click(Sender: TObject);
begin
  FPendingMsgID := DEVTOOLS_SCREENSHOT_MSGID;
  Chromium1.ExecuteDevToolsMethod(0, 'Page.captureScreenshot', nil);
end;

procedure TMiniBrowserFrm.HandleBrowserInfo(const aResult : ICefValue);
var
  TempDict : ICefDictionaryValue;
begin
  if (aResult = nil) then exit;

  TempDict := aResult.GetDictionary;

  if (TempDict <> nil) and (TempDict.GetSize > 0) then
    begin
      FBrowserInfo.protocolVersion := TempDict.GetValue('protocolVersion').GetString;
      FBrowserInfo.product         := TempDict.GetValue('product').GetString;
      FBrowserInfo.revision        := TempDict.GetValue('revision').GetString;
      FBrowserInfo.userAgent       := TempDict.GetValue('userAgent').GetString;
      FBrowserInfo.jsVersion       := TempDict.GetValue('jsVersion').GetString;

      case Chromium1.RuntimeStyle of
        CEF_RUNTIME_STYLE_CHROME : FBrowserInfo.RuntimeStyle := 'Chrome';
        CEF_RUNTIME_STYLE_ALLOY  : FBrowserInfo.RuntimeStyle := 'Alloy';
        else                       FBrowserInfo.RuntimeStyle := 'Default';
      end;

      PostMessage(Handle, MINIBROWSER_SHOWBROWSERINFO, 0, 0);
    end;
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
      if (FPendingMsgID = DEVTOOLS_BROWSERINFO_MSGID) then
        begin
          HandleBrowserInfo(result);
          FPendingMsgID := 0;
          exit;
        end
       else
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

  PostMessage(Handle, MINIBROWSER_DTDATA_AVLBL, TempResult, 0);
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
          TempData := nil;

          case FPendingMsgID of
            DEVTOOLS_SCREENSHOT_MSGID :
              begin
                SaveDialog1.DefaultExt := 'png';
                SaveDialog1.Filter     := 'PNG files (*.png)|*.PNG';
                {$IFDEF DELPHI21_UP}
                // TO-DO: TNetEncoding was a new feature in Delphi XE7. Replace
                // TNetEncoding.Base64.DecodeStringToBytes with Soap.EncdDecd.DecodeBase64 for older Delphi versions
                TempData               := TNetEncoding.Base64.DecodeStringToBytes(FDevToolsMsgValue);
                {$ENDIF}
              end;

            DEVTOOLS_MHTML_MSGID :
              begin
                SaveDialog1.DefaultExt := 'mhtml';
                SaveDialog1.Filter     := 'MHTML files (*.mhtml)|*.MHTML';
                TempData               := BytesOf(FDevToolsMsgValue);
              end;

            else
              begin
                SaveDialog1.DefaultExt := '';
                SaveDialog1.Filter     := 'All files (*.*)|*.*';
                {$IFDEF DELPHI21_UP}
                // TO-DO: TNetEncoding was a new feature in Delphi XE7. Replace
                // TNetEncoding.Base64.DecodeStringToBytes with Soap.EncdDecd.DecodeBase64 for older Delphi versions
                TempData               := TNetEncoding.Base64.DecodeStringToBytes(FDevToolsMsgValue);
                {$ENDIF}
              end;
          end;

          FPendingMsgID := 0;
          TempLen       := length(TempData);

          if (TempLen > 0) then
            begin
              TempFile := nil;

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

  Allowdownloads1.Checked := FAllowDownloads;
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
    Chromium1.PrintToPDF(SaveDialog1.FileName);
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
  TempSL : TStringList;
begin
  TempSL := TStringList.Create;

  if Chromium1.GetFrameIdentifiers(TStrings(TempSL)) then
    clipboard.AsText := TempSL.Text;

  TempSL.Free;
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

procedure TMiniBrowserFrm.SaveasMHTML1Click(Sender: TObject);
var
  TempParams : ICefDictionaryValue;
begin
  try
    TempParams  := TCefDictionaryValueRef.New;
    TempParams.SetString('format', 'mhtml');
    FPendingMsgID := DEVTOOLS_MHTML_MSGID;
    Chromium1.ExecuteDevToolsMethod(0, 'Page.captureSnapshot', TempParams);
  finally
    TempParams := nil;
  end;
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

procedure TMiniBrowserFrm.ShowTaskManager;
var
  TempTaskManager : ICefTaskManager;
  i : integer;
  TempIDs: TCefCustomInt64Array;
  TempInfo : TCustomTaskInfo;
  TempResult : string;
begin
  TempTaskManager := TCefTaskManagerRef.New;

  if assigned(TempTaskManager) and
     TempTaskManager.GetTaskIdsList(TempIDs) then
    begin
      TempResult := 'Task Manager Information :' + CRLF + CRLF;
      i := 0;

      while (i < length(TempIDs)) do
        begin
          if TempTaskManager.GetTaskInfo(TempIDs[i], TempInfo) then
            begin
              TempResult := TempResult + 'id: ' + inttostr(TempInfo.id) + ', type: ';

              case TempInfo.type_ of
                CEF_TASK_TYPE_BROWSER          : TempResult := TempResult + 'browser';
                CEF_TASK_TYPE_GPU              : TempResult := TempResult + 'GPU';
                CEF_TASK_TYPE_ZYGOTE           : TempResult := TempResult + 'zygote';
                CEF_TASK_TYPE_UTILITY          : TempResult := TempResult + 'utility';
                CEF_TASK_TYPE_RENDERER         : TempResult := TempResult + 'renderer';
                CEF_TASK_TYPE_EXTENSION        : TempResult := TempResult + 'extension';
                CEF_TASK_TYPE_GUEST            : TempResult := TempResult + 'guest';
                CEF_TASK_TYPE_PLUGIN           : TempResult := TempResult + 'plugin';
                CEF_TASK_TYPE_SANDBOX_HELPER   : TempResult := TempResult + 'sandbox helper';
                CEF_TASK_TYPE_DEDICATED_WORKER : TempResult := TempResult + 'dedicated worker';
                CEF_TASK_TYPE_SHARED_WORKER    : TempResult := TempResult + 'shared worker';
                CEF_TASK_TYPE_SERVICE_WORKER   : TempResult := TempResult + 'service worker';
                else                             TempResult := TempResult + 'unknown';
              end;

              TempResult := TempResult + ', title: ' + TempInfo.title + CRLF;
            end;

          inc(i);
        end;

      showmessage(TempResult);
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

procedure TMiniBrowserFrm.WMQueryEndSession(var aMessage: TWMQueryEndSession);
begin
  // We return False (0) to close the browser correctly.
  // Windows may show the FShutdownReason message that we created in
  // TForm.OnCreate if the shutdown takes too much time.
  // CEF4Delphi sets the subprocesses to receive the WM_QUERYENDSESSION
  // message after the main browser process with a
  // SetProcessShutdownParameters call
  aMessage.Result := 0;
  PostMessage(Handle, WM_CLOSE, 0, 0);
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
