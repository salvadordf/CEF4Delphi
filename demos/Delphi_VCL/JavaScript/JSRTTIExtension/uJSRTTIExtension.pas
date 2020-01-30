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
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MINIBROWSER_SHOWTEXTVIEWER = WM_APP + $100;

  MINIBROWSER_CONTEXTMENU_SETJSEVENT        = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_JSVISITDOM        = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_MUTATIONOBSERVER  = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS      = MENU_ID_USER_FIRST + 4;

  MOUSEOVER_MESSAGE_NAME        = 'mouseover';
  CUSTOMNAME_MESSAGE_NAME       = 'customname';

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


// Please, read the code comments in the JSExtension demo (uJSExtension.pas) before using this demo!

// This demo is almost identical to JSExtension but it uses a slightly easier
// way to register JavaScript extensions inherited from the DCEF3 project.

// Instead of creating a custom class inherited from TCefv8HandlerOwn and calling the
// CefRegisterExtension function, this demo uses the TCefRTTIExtension.Register
// class procedure to register the TTestExtension class, which is a custom Delphi
// class with 2 class procedures.

// TCefRTTIExtension uses the RTTI from the TTestExtension class to generate the
// JS code and the ICefv8Handler parameters needed by CefRegisterExtension.

// You still need to call TCefRTTIExtension.Register in the GlobalCEFApp.OnWebKitInitialized event
// and use process messages to send information between processes.

// TTestExtension can send information back to the browser with a process message.
// The TTestExtension.mouseover function do this by calling
// TCefv8ContextRef.Current.Browser.MainFrame.SendProcessMessage(PID_BROWSER, msg);

// That message is received in the TChromium.OnProcessMessageReceived event.


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
  if TCefRTTIExtension.Register('myextension', TTestExtension) then
    {$IFDEF DEBUG}CefDebugLog('JavaScript extension registered successfully!'){$ENDIF}
   else
    {$IFDEF DEBUG}CefDebugLog('There was an error registering the JavaScript extension!'){$ENDIF};
{$ENDIF}
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                     := TCefApplication.Create;
  GlobalCEFApp.OnWebKitInitialized := GlobalCEFApp_OnWebKitInitialized;
  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile             := 'debug.log';
  GlobalCEFApp.LogSeverity         := LOGSEVERITY_INFO;
  {$ENDIF}
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
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS,     'Show DevTools');
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
const
  ELEMENT_ID = 'keywords'; // ID attribute in the search box at https://www.briskbard.com/forum/
var
  TempPoint : TPoint;
  TempJSCode : string;
begin
  Result := False;

  // Here is the code executed for each custom context menu entry
  case commandId of
    MINIBROWSER_CONTEXTMENU_SETJSEVENT :
      if (frame <> nil) and frame.IsValid then
        begin
          TempJSCode := 'document.body.addEventListener("mouseover", function(evt){'+
                          'function getpath(n){'+
                            'var ret = "<" + n.nodeName + ">"; '+
                            'if (n.parentNode){return getpath(n.parentNode) + ret} else '+
                            'return ret'+
                          '}; '+
                          'myextension.mouseover(getpath(evt.target))}'+   // This is the call from JavaScript to the extension with DELPHI code in uTestExtension.pas
                        ')';

          frame.ExecuteJavaScript(TempJSCode, 'about:blank', 0);
        end;

    MINIBROWSER_CONTEXTMENU_JSVISITDOM :
      if (frame <> nil) and frame.IsValid then
        begin
          // This is the call from JavaScript to the extension with DELPHI code in uTestExtension.pas
          TempJSCode := 'var testhtml = document.body.innerHTML; ' +
                        'myextension.sendresulttobrowser(testhtml, ' + quotedstr(CUSTOMNAME_MESSAGE_NAME) + ');';

          frame.ExecuteJavaScript(TempJSCode, 'about:blank', 0);
        end;

    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
      begin
        TempPoint.x := params.XCoord;
        TempPoint.y := params.YCoord;

        Chromium1.ShowDevTools(TempPoint, nil);
      end;
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
      end;
end;

procedure TJSRTTIExtensionFrm.FormShow(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Initializing browser. Please wait...';

  Chromium1.DefaultURL := Edit1.Text;

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
