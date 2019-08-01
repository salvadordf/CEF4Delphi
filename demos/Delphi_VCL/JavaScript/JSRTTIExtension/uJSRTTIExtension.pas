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
//        Copyright © 2017 Salvador Diaz Fau. All rights reserved.
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

unit uJSRTTIExtension;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFWinControl;

const
  MINIBROWSER_SHOWTEXTVIEWER = WM_APP + $100;

  MINIBROWSER_CONTEXTMENU_SETJSEVENT        = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_JSVISITDOM        = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_MUTATIONOBSERVER  = MENU_ID_USER_FIRST + 3;

  MOUSEOVER_MESSAGE_NAME        = 'mouseover';
  CUSTOMNAME_MESSAGE_NAME       = 'customname';
  MUTATIONOBSERVER_MESSAGE_NAME = 'mutationobservermsgname';

type
  TJSRTTIExtensionFrm = class(TForm)
    NavControlPnl: TPanel;
    Edit1: TEdit;
    GoBtn: TButton;
    StatusBar1: TStatusBar;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure Chromium1BeforeContextMenu(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer;
      eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1BeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);
  protected
    FText : string;
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ShowTextViewerMsg(var aMessage : TMessage); message MINIBROWSER_SHOWTEXTVIEWER;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
  public
    { Public declarations }
  end;

var
  JSRTTIExtensionFrm: TJSRTTIExtensionFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uSimpleTextViewer, uCEFv8Handler, uTestExtension, uCEFMiscFunctions;

// The CEF3 document describing extensions is here :
// https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

// This demo has a TTestExtension class that is registered in the
// GlobalCEFApp.OnWebKitInitialized event when the application is initializing.

// TTestExtension can send information back to the browser with a process message.
// The TTestExtension.mouseover function do this by calling
// TCefv8ContextRef.Current.Browser.SendProcessMessage(PID_BROWSER, msg);

// TCefv8ContextRef.Current returns the v8 context for the frame that is currently executing JS,
// TCefv8ContextRef.Current.Browser.SendProcessMessage should send a message to the right browser even
// if you have created several browsers in one app.

// That message is received in the TChromium.OnProcessMessageReceived event.
// Even if you create several TChromium objects you should have no problem because each of them will have its own
// TChromium.OnProcessMessageReceived event to receive the messages from the extension.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure GlobalCEFApp_OnWebKitInitialized;
begin
{$IFDEF DELPHI14_UP}
  // Registering the extension. Read this document for more details :
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md
  TCefRTTIExtension.Register('myextension', TTestExtension);
{$ENDIF}
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                     := TCefApplication.Create;
  GlobalCEFApp.OnWebKitInitialized := GlobalCEFApp_OnWebKitInitialized;
  GlobalCEFApp.DisableFeatures     := 'NetworkService,OutOfBlinkCors';
end;

procedure TJSRTTIExtensionFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(Edit1.Text);
end;

procedure TJSRTTIExtensionFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TJSRTTIExtensionFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  // Adding some custom context menu entries
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETJSEVENT,       'Set mouseover event');
  model.AddItem(MINIBROWSER_CONTEXTMENU_JSVISITDOM,       'Visit DOM in JavaScript');
  model.AddItem(MINIBROWSER_CONTEXTMENU_MUTATIONOBSERVER, 'Add mutation observer');
end;

procedure TJSRTTIExtensionFrm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TJSRTTIExtensionFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
begin
  Result := False;

  // Here is the code executed for each custom context menu entry

  case commandId of
    MINIBROWSER_CONTEXTMENU_SETJSEVENT :
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript(
          'document.body.addEventListener("mouseover", function(evt){'+
            'function getpath(n){'+
              'var ret = "<" + n.nodeName + ">";'+
              'if (n.parentNode){return getpath(n.parentNode) + ret} else '+
              'return ret'+
            '};'+
            'myextension.mouseover(getpath(evt.target))}'+   // This is the call from JavaScript to the extension with DELPHI code in uTestExtension.pas
          ')', 'about:blank', 0);

    MINIBROWSER_CONTEXTMENU_JSVISITDOM :
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript(
          'var testhtml = document.body.innerHTML;' +
          'myextension.sendresulttobrowser(testhtml, ' + quotedstr(CUSTOMNAME_MESSAGE_NAME) + ');',  // This is the call from JavaScript to the extension with DELPHI code in uTestExtension.pas
          'about:blank', 0);

    MINIBROWSER_CONTEXTMENU_MUTATIONOBSERVER :
      // This MutatioObserver is based on this example https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver
      // This observer is configured to execute the callback when the attributes in the search box at google.com
      // changes its value. The callback calls a JavaScript extension called "myextension.sendresulttobrowser" to send
      // the "value" attribute to Delphi.
      // Delphi receives the the information in the Chromium1ProcessMessageReceived procedure and shows it in the status bar.
      if (browser <> nil) and (browser.MainFrame <> nil) then
        browser.MainFrame.ExecuteJavaScript(
          'var targetNode = document.getElementById(' + quotedstr('lst-ib') + ');' +  // 'lst-ib' is the ID attribute in the search box at google.com
          'var config = { attributes: true, childList: false, subtree: false };'+
          'var callback = function(mutationsList, observer) {' +
          '    for(var mutation of mutationsList) {' +
          '         if (mutation.type == ' + quotedstr('attributes') + ') {' +
          '            myextension.sendresulttobrowser(document.getElementById(' + quotedstr('lst-ib') + ').value, ' + quotedstr(MUTATIONOBSERVER_MESSAGE_NAME) + ');' +
          '        }' +
          '    }' +
          '};' +
          'var observer = new MutationObserver(callback);' +
          'observer.observe(targetNode, config);',
          'about:blank', 0);
  end;
end;

procedure TJSRTTIExtensionFrm.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  Result := False;

  if (message = nil) or (message.ArgumentList = nil) then exit;

  // This function receives the messages with the JavaScript results

  // Many of these events are received in different threads and the VCL
  // doesn't like to create and destroy components in different threads.

  // It's safer to store the results and send a message to the main thread to show them.

  // The message names are defined in the extension or in JS code.

  if (message.Name = MOUSEOVER_MESSAGE_NAME) then
    begin
      StatusBar1.Panels[0].Text := message.ArgumentList.GetString(0); // this doesn't create/destroy components
      Result := True;
    end
   else
    if (message.Name = CUSTOMNAME_MESSAGE_NAME) then
      begin
        FText := message.ArgumentList.GetString(0);
        PostMessage(Handle, MINIBROWSER_SHOWTEXTVIEWER, 0, 0);
        Result := True;
      end
     else
      if (message.Name = MUTATIONOBSERVER_MESSAGE_NAME) then
        begin
          StatusBar1.Panels[0].Text := message.ArgumentList.GetString(0);
          Result := True;
        end;
end;

procedure TJSRTTIExtensionFrm.FormShow(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Initializing browser. Please wait...';

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TJSRTTIExtensionFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSRTTIExtensionFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSRTTIExtensionFrm.ShowTextViewerMsg(var aMessage : TMessage);
begin
  // This form will show the HTML received from JavaScript
  SimpleTextViewerFrm.Memo1.Lines.Text := FText;
  SimpleTextViewerFrm.ShowModal;
end;

procedure TJSRTTIExtensionFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TJSRTTIExtensionFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  StatusBar1.Panels[0].Text := '';
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TJSRTTIExtensionFrm.Chromium1BeforeClose(
  Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TJSRTTIExtensionFrm.Chromium1Close(
  Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TJSRTTIExtensionFrm.FormCloseQuery(
  Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TJSRTTIExtensionFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
end;

procedure TJSRTTIExtensionFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

end.
