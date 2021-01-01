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

unit uCookieVisitor;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.StrUtils,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, StrUtils,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFCookieManager, uCEFCookieVisitor, uCEFWinControl, uCEFSentinel,
  uCEFChromiumCore;

const
  MINIBROWSER_SHOWCOOKIES    = WM_APP + $101;
  MINIBROWSER_COOKIESDELETED = WM_APP + $102;
  MINIBROWSER_COOKIESET      = WM_APP + $103;

  MINIBROWSER_CONTEXTMENU_DELETECOOKIES     = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_GETCOOKIES        = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_SETCOOKIE         = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_GETGOOGLECOOKIES  = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_DELETECACHE       = MENU_ID_USER_FIRST + 5;

  BLOCKED_COOKIE_DOMAIN = 'briskbard.com';

type
  TCookieVisitorFrm = class(TForm)
    AddressBarPnl: TPanel;
    Edit1: TEdit;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    Timer1: TTimer;
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure FormShow(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1CookiesDeleted(Sender: TObject;
      numDeleted: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1BeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1CookieSet(Sender: TObject; aSuccess: Boolean;
      aID: Integer);
    procedure Chromium1CookieVisitorDestroyed(Sender: TObject;
      aID: Integer);
    procedure Chromium1CanSaveCookie(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const request: ICefRequest; const response: ICefResponse;
      const cookie: PCefCookie; var aResult: Boolean);
    procedure Chromium1CookiesVisited(Sender: TObject; const name_, value,
      domain, path: ustring; secure, httponly, hasExpires: Boolean;
      const creation, lastAccess, expires: TDateTime; count, total,
      aID: Integer; same_site: TCefCookieSameSite; priority: Integer;
      var aDeleteCookie, aResult: Boolean);

  private
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ShowCookiesMsg(var aMessage : TMessage); message MINIBROWSER_SHOWCOOKIES;
    procedure CookiesDeletedMsg(var aMessage : TMessage); message MINIBROWSER_COOKIESDELETED;
    procedure CookieSetMsg(var aMessage : TMessage); message MINIBROWSER_COOKIESET;

  protected
    FText     : string;
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

  public
    procedure AddCookieInfo(const aCookie : TCookie);

  end;

var
  CookieVisitorFrm: TCookieVisitorFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uSimpleTextViewer, uCEFTask, uCEFMiscFunctions, uCEFDictionaryValue;

// This demo has a context menu to test several TChromium functions related to cookies like TChromium.VisitAllCookies,
// TChromium.SetCookie, TChromium.DeleteCookies, etc.

// TChromium.VisitAllCookies and TChromium.VisitURLCookies trigger the TChromium.OnCookiesVisited event for each
// cookie and it'll save the information using the AddCookieInfo function.
// When the last cookie arrives we show the information in a SimpleTextViewer form.

// TChromium.SetCookie triggers TChromium.OnCookieSet when it has set the cookie.

// TChromium.DeleteCookies triggers TChromium.OnCookiesDeleted when the cookies have been deleted.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                  := TCefApplication.Create;
  GlobalCEFApp.cache            := 'cache';
  //GlobalCEFApp.LogFile          := 'cef.log';
  //GlobalCEFApp.LogSeverity      := LOGSEVERITY_VERBOSE;
end;

procedure TCookieVisitorFrm.AddCookieInfo(const aCookie : TCookie);
begin
  // This should be protected by a mutex.
  FText := FText + aCookie.name + ' : ' + aCookie.value + ' (' + aCookie.domain + ')' + #13 + #10;
end;

procedure TCookieVisitorFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  AddressBarPnl.Enabled := True;
end;

procedure TCookieVisitorFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TCookieVisitorFrm.ShowCookiesMsg(var aMessage : TMessage);
begin
  SimpleTextViewerFrm.Memo1.Lines.Text := FText; // This should be protected by a mutex.
  SimpleTextViewerFrm.ShowModal;
end;

procedure TCookieVisitorFrm.CookiesDeletedMsg(var aMessage : TMessage);
begin
  showmessage('Deleted cookies : ' + inttostr(aMessage.lParam));
end;

procedure TCookieVisitorFrm.CookieSetMsg(var aMessage : TMessage);
begin
  if (aMessage.wParam <> 0) then
    showmessage('Cookie set successfully !')
   else
    showmessage('There was a problem setting the cookie');
end;

procedure TCookieVisitorFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TCookieVisitorFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(Edit1.Text);
end;

procedure TCookieVisitorFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TCookieVisitorFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TCookieVisitorFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_DELETECOOKIES,    'Delete cookies');
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_GETCOOKIES,       'Visit all cookies');
  model.AddItem(MINIBROWSER_CONTEXTMENU_GETGOOGLECOOKIES, 'Visit cookies from Google');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETCOOKIE,        'Set cookie');
  model.AddItem(MINIBROWSER_CONTEXTMENU_DELETECACHE,      'Delete cache');
end;

procedure TCookieVisitorFrm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TCookieVisitorFrm.Chromium1CanSaveCookie(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const request: ICefRequest; const response: ICefResponse;
  const cookie: PCefCookie; var aResult: Boolean);
var
  TempDomain : string;
begin
  aResult := True;

  // This event can't block cookies set in JavaScript

  if (cookie               <> nil) and
     (cookie.domain.str    <> nil) and
     (cookie.domain.length  > 0)   then
    begin
      SetString(TempDomain, cookie.domain.str, cookie.domain.length);

      // TO-DO: See the "Domain Matching" section in the RFC for "HTTP State Management Mechanism".
      if AnsiEndsStr(BLOCKED_COOKIE_DOMAIN, TempDomain) then
        aResult := False;
    end;
end;

procedure TCookieVisitorFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TCookieVisitorFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_DELETECOOKIES : Chromium1.DeleteCookies;

    MINIBROWSER_CONTEXTMENU_GETCOOKIES :
      begin
        // This should be protected by a mutex
        FText := '';
        Chromium1.VisitAllCookies;
      end;

    MINIBROWSER_CONTEXTMENU_GETGOOGLECOOKIES :
      begin
        // This should be protected by a mutex
        FText := '';
        Chromium1.VisitURLCookies('https://www.google.com');
      end;

    MINIBROWSER_CONTEXTMENU_SETCOOKIE :
      Chromium1.SetCookie('https://www.example.com',
                          'example_cookie_name',
                          '1234',
                          '',
                          '/',
                          True,
                          True,
                          True,
                          now,
                          now,
                          now + 1,
                          CEF_COOKIE_SAME_SITE_UNSPECIFIED,
                          CEF_COOKIE_PRIORITY_MEDIUM,
                          False);

    MINIBROWSER_CONTEXTMENU_DELETECACHE :
      Chromium1.ExecuteDevToolsMethod(0, 'Network.clearBrowserCache', nil);
  end;
end;

procedure TCookieVisitorFrm.Chromium1CookiesDeleted(Sender: TObject; numDeleted: Integer);
begin
  PostMessage(Handle, MINIBROWSER_COOKIESDELETED, 0, numDeleted);
end;

procedure TCookieVisitorFrm.Chromium1CookieSet(Sender: TObject;
  aSuccess: Boolean; aID: Integer);
begin
  PostMessage(Handle, MINIBROWSER_COOKIESET, WPARAM(aSuccess), aID);
end;

procedure TCookieVisitorFrm.Chromium1CookiesVisited(Sender: TObject;
  const name_, value, domain, path: ustring; secure, httponly,
  hasExpires: Boolean; const creation, lastAccess, expires: TDateTime; count,
  total, aID: Integer; same_site: TCefCookieSameSite; priority: Integer;
  var aDeleteCookie, aResult: Boolean);
var
  TempCookie : TCookie;
begin
  aDeleteCookie := False;

  TempCookie.name        := name_;
  TempCookie.value       := value;
  TempCookie.domain      := domain;
  TempCookie.path        := path;
  TempCookie.secure      := secure;
  TempCookie.httponly    := httponly;
  TempCookie.creation    := creation;
  TempCookie.last_access := lastAccess;
  TempCookie.has_expires := hasExpires;
  TempCookie.expires     := expires;
  TempCookie.same_site   := same_site;
  TempCookie.priority    := priority;

  AddCookieInfo(TempCookie);

  aResult := (count <> pred(total));
end;

procedure TCookieVisitorFrm.Chromium1CookieVisitorDestroyed(Sender: TObject; aID: Integer);
begin
  PostMessage(Handle, MINIBROWSER_SHOWCOOKIES, 0, 0);
end;

procedure TCookieVisitorFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TCookieVisitorFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;

  Chromium1.DefaultURL := Edit1.Text;
end;

procedure TCookieVisitorFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TCookieVisitorFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TCookieVisitorFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TCookieVisitorFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TCookieVisitorFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
