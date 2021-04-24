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

unit uSchemeRegistrationBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Menus,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types, Vcl.ComCtrls, Vcl.ClipBrd,
  System.UITypes,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFSchemeRegistrar,
  uCEFTypes, uCEFConstants, uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MINIBROWSER_CONTEXTMENU_REGSCHEME    = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_CLEARFACT    = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS = MENU_ID_USER_FIRST + 3;

  CUSTOM_SCHEME_NAME = 'hello';

type
  TSchemeRegistrationBrowserFrm = class(TForm)
    AddressBarPnl: TPanel;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressCbx: TComboBox;
    Timer1: TTimer;
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  private
    { Private declarations }
  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  public
    { Public declarations }
  end;

var
  SchemeRegistrationBrowserFrm: TSchemeRegistrationBrowserFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFSchemeHandlerFactory, uCEFMiscFunctions, uHelloScheme;

// This demo shows how to register a custom 'hello' scheme.

// To register the scheme we use a resource handler in uHelloScheme.pas that inherits from TCefResourceHandlerOwn
// You can register the scheme in TForm.OnCreate or later.

// IMPORTANT : If you notice that some functionality is missing in your application and the debug.log file shows
// weird warnings then register the 'http' or 'https' schemes instead of creating a custom scheme.

// For more information about the resource handler read this page :
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefResourceHandler.html

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure GlobalCEFApp_OnRegCustomSchemes(const registrar: TCefSchemeRegistrarRef);
begin
  registrar.AddCustomScheme(CUSTOM_SCHEME_NAME, CEF_SCHEME_OPTION_STANDARD or CEF_SCHEME_OPTION_LOCAL);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                    := TCefApplication.Create;
  GlobalCEFApp.OnRegCustomSchemes := GlobalCEFApp_OnRegCustomSchemes;
  GlobalCEFApp.DisableWebSecurity := True;

  // GlobalCEFApp.LogFile              := 'debug.log';
  // GlobalCEFApp.LogSeverity          := LOGSEVERITY_VERBOSE;
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1BeforeClose(
  Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1BeforeContextMenu(
  Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_REGSCHEME,   'Register scheme');
  model.AddItem(MINIBROWSER_CONTEXTMENU_CLEARFACT,   'Clear schemes');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1BeforePopup(
  Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TSchemeRegistrationBrowserFrm.Chromium1ContextMenuCommand(
  Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempPoint : TPoint;
  TempFactory: ICefSchemeHandlerFactory;
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_REGSCHEME :
      if (browser <> nil) and
         (browser.host <> nil) and
         (browser.host.RequestContext <> nil) then
        begin
          // You can register the Scheme Handler Factory in the DPR file or later, for example in a context menu command.
          TempFactory := TCefSchemeHandlerFactoryOwn.Create(THelloScheme);
          if not(browser.host.RequestContext.RegisterSchemeHandlerFactory(CUSTOM_SCHEME_NAME, '', TempFactory)) then
            MessageDlg('RegisterSchemeHandlerFactory error !', mtError, [mbOk], 0);
        end;

    MINIBROWSER_CONTEXTMENU_CLEARFACT :
      if (browser <> nil) and
         (browser.host <> nil) and
         (browser.host.RequestContext <> nil) then
        begin
          if not(browser.host.RequestContext.ClearSchemeHandlerFactories) then
            MessageDlg('ClearSchemeHandlerFactories error !', mtError, [mbOk], 0);
        end;

    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
      begin
        TempPoint.x := params.XCoord;
        TempPoint.y := params.YCoord;
        Chromium1.ShowDevTools(TempPoint, nil);
      end;
  end;
end;

procedure TSchemeRegistrationBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TSchemeRegistrationBrowserFrm.FormCreate(Sender: TObject);
begin
  // You can register the Scheme Handler Factory here or later, for example in a context menu command.
  CefRegisterSchemeHandlerFactory(CUSTOM_SCHEME_NAME, '', THelloScheme);
end;

procedure TSchemeRegistrationBrowserFrm.FormShow(Sender: TObject);
begin
  Chromium1.DefaultURL := AddressCbx.Text;
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TSchemeRegistrationBrowserFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressCbx.Text);
end;

procedure TSchemeRegistrationBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TSchemeRegistrationBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  AddressBarPnl.Enabled := True;
end;

procedure TSchemeRegistrationBrowserFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TSchemeRegistrationBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TSchemeRegistrationBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TSchemeRegistrationBrowserFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TSchemeRegistrationBrowserFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
