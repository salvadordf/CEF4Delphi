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

unit uJSExtension;

{$MODE Delphi}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$ELSE}
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFWinControl, uCEFSentinel;

const
  MINIBROWSER_SHOWTEXTVIEWER = WM_APP + $100;

  MINIBROWSER_CONTEXTMENU_SETJSEVENT   = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_JSVISITDOM   = MENU_ID_USER_FIRST + 2;    
  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS = MENU_ID_USER_FIRST + 3;   
  MINIBROWSER_CONTEXTMENU_OFFLINE      = MENU_ID_USER_FIRST + 4;

  MOUSEOVER_MESSAGE_NAME  = 'mouseover';
  CUSTOMNAME_MESSAGE_NAME = 'customname';

type

  { TJSExtensionFrm }

  TJSExtensionFrm = class(TForm)
    NavControlPnl: TPanel;
    Edit1: TEdit;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    StatusPnl: TPanel;
    Timer1: TTimer;
    procedure CEFSentinel1Close(Sender: TObject);
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
                                                    
    FOffline : boolean;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure ShowTextViewerMsg(var aMessage : TMessage); message MINIBROWSER_SHOWTEXTVIEWER;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    function SwitchOfflineMode : integer;
  public
    { Public declarations }
  end;

var
  JSExtensionFrm: TJSExtensionFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uSimpleTextViewer, uCEFMiscFunctions, uTestExtensionHandler, uCEFDictionaryValue;

// BASIC CONCEPTS
// ==============

// Chromium uses several processes to carry out all the tasks needed to handle a web page :
// 1. The main application process is called "BROWSER PROCESS" and it runs the UI.
// 2. The layout and interpretation of HTML is done in the "RENDERER PROCESS".

// Read this for more details about Chromium's architecture :
// http://www.chromium.org/developers/design-documents/multi-process-architecture

// Each process is isolated from the rest and you need to use some kind of inter-process
// communication (IPC) to send information between them. This isolation and protection is
// guaranteed by the operating system and it's the main reason Chromium uses several processes.

// In many cases, you need to use JavaScript or visit the DOM to return some results to Pascal.
// The DOM and JavaScript live in the RENDERER PROCESS, while the Pascal code of your application
// lives in the BROWSER PROCESS.

// As commented before, the operating system isolates each process and this means that you
// can't access anything declared in one process like variables, fields, classes, controls, etc.
// from a different process.

// However, CEF has several ways to send information between processes and you can also use your
// own inter-process communication methods.

// If you need to execute some JavaScript code all you need is to call TChromium.ExecuteJavaScript
// from your application's code in the BROWSER PROCESS and CEF will take care of executing that
// code in the RENDERER PROCESS.

// If you need to send a message to the RENDERER PROCESS from the BROWSER PROCESS you can use
// TChromium.SendProcessMessage.

// To send messages to the BROWSER PROCESS from the RENDERER PROCESS you can use
// ICefFrame.SendProcessMessage


//     --------------   TChromium.SendProcessMessage  --------------
//     |            | ------------------------------> |            |
//     |  BROWSER   |                                 |  RENDERER  |
//     |            |                                 |            |
//     |  PROCESS   |   ICefFrame.SendProcessMessage  |  PROCESS   |
//     |            | <------------------------------ |            |
//     --------------                                 --------------


// To receive the messages sent from the RENDERER PROCESS you need to use the
// TChromium.OnProcessMessageReceived event. This event is executed in a CEF thread that belongs
// to the BROWSER PROCESS.

// To receive the messages sent from the BROWSER PROCESS you need to use the
// TCefApplication.OnProcessMessageReceived event (GlobalCEFApp.OnProcessMessageReceived).
// This event is executed in a CEF thread that belongs to the RENDERER PROCESS.


// JAVASCRIPT EXTENSIONS
// =====================

// CEF exposes a large number of JS features for integration in client applications.
// You can use JS types, arrays, functions, extensions, objects, etc.

// All of those features are described in detail here :
// https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

// One of the most interesting JS features available in CEF are the JavaScript extensions because they
// can be used to execute custom Pascal code from JavaScript.

// If you put all you know so far together you can get any result or information in Pascal from
// JavaScript following these steps :
// 1. Use TChromium.ExecuteJavaScript to execute your custom JavaScript code.
// 2. That custom JavaScript code is executed in the RENDERER PROCESS and it can call functions in your
//    custom JavaScript extension, which executes Pascal code. This Pascal code is also executed in
//    the RENDERER PROCESS.
// 3. The Pascal code in the JavaScript extension can use ICefFrame.SendProcessMessage to send
//    information back to the BROWSER PROCESS.
// 4. The BROWSER PROCESS receives the information in the TChromium.OnProcessMessageReceived event.

// To create a JavaScript extension in CEF you have to create a new class that inherits from
// TCefv8HandlerOwn and it has to override the "execute" function. Open uTestExtensionHandler.pas
// to see an example and read this for more details about the "execute" parameters :
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefV8Handler.html

// In order to use that extension, you must register it in the GlobalCEFApp.OnWebKitInitialized event
// as you can see in the GlobalCEFApp_OnWebKitInitialized procedure on this PAS unit.

// You have to call the CefRegisterExtension function with 3 parameters :
// 1. name : The extension name.
// 2. code : Any valid JS code but in this case it includes 2 "native function" forward declarations.
// 3. Handler : An instance of your TCefv8HandlerOwn subclass.

// Notice that the code used with the CefRegisterExtension function in this demo is declaring
// "myextension.mouseover" as a function that calls the "mouseover" native function, and the
// "myextension.sendresulttobrowser" function that calls the "sendresulttobrowser" native function.

// The "execute" function in the custom TCefv8HandlerOwn subclass will compare the "name" parameter
// with the name of the of the native function used in the code that registered this extension.
// As you can see in this demo, TTestExtensionHandler.Execute compares the "name" parameter with
// "mouseover" and "sendresulttobrowser" to execute the code you want for each of those custom functions.

// TTestExtensionHandler.Execute is executed in the RENDERER PROCESS and it uses a process message
// to send some results to he BROWSER PROCESS.
// It uses TCefv8ContextRef.Current.Browser.MainFrame to call the SendProcessMessage procedure for
// the main frame.

// The message is a TCefProcessMessageRef instance and you can set the information you want to send using
// its ArgumentList property.

// You can add several items to ArgumentList using different indexes in the SetString, SetInt, SetBool,
// SetBinary, etc. functions.
// There is a size limit in the binary parameters of only a few kilobytes. Compress the binary data, use
// alternative IPC methods or use a database protected by a mutex if necessary.

// For more information about this, read the following pages :
// https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md#markdown-header-functions-and-extensions
// https://bitbucket.org/chromiumembedded/cef/src/b6b033a52bb3f7059d169a4c8389966c8fe2531b/include/capi/cef_v8_capi.h#lines-924


// TESTING THIS DEMO :
// ===================
// 1. Run the demo and wait until google.com is loaded
// 2. Right-click and select the "Set the mouseover event" menu option.
// 3. Move the mouse pointer over the web page and see the HTML elements in the status bar.

// When you select the "Set the mouseover event" menu option, the
// TChromium.OnContextMenuCommand event is triggered and it adds an event listener to the
// document's body. That listener calls one of the functions available in the registered
// extension called "myextension.mouseover".

// The TTestExtensionHandler.Execute function in the extension is executed and it
// uses TCefv8ContextRef.Current.Browser.MainFrame.SendProcessMessage(PID_BROWSER, msg)
// to send a process message with the results to the browser process.

// That message is received in the TChromium.OnProcessMessageReceived event and it shows
// the information in the status bar.

// If you have to debug the code executed by the extension you will need to use the
// debugging methods described in
// https://www.briskbard.com/index.php?lang=en&pageid=cef


// DESTRUCTION STEPS
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers
//    the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1
//    in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure GlobalCEFApp_OnWebKitInitialized;
var
  TempExtensionCode : string;
  TempHandler       : ICefv8Handler;
begin
  // This is a JS extension example with 2 functions and several parameters.
  // Please, read the "JavaScript Integration" wiki page at
  // https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md

  TempExtensionCode := 'var myextension;' +
                       'if (!myextension)' +
                       '  myextension = {};' +
                       '(function() {' +
                       '  myextension.mouseover = function(a) {' +
                       '    native function mouseover();' +
                       '    mouseover(a);' +
                       '  };' +
                       '  myextension.sendresulttobrowser = function(b,c) {' +
                       '    native function sendresulttobrowser();' +
                       '    sendresulttobrowser(b,c);' +
                       '  };' +
                       '})();';

  try
    TempHandler := TTestExtensionHandler.Create;

    if CefRegisterExtension('myextension', TempExtensionCode, TempHandler) then
      {$IFDEF DEBUG}CefDebugLog('JavaScript extension registered successfully!'){$ENDIF}
     else
      {$IFDEF DEBUG}CefDebugLog('There was an error registering the JavaScript extension!'){$ENDIF};
  finally
    TempHandler := nil;
  end;
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

procedure TJSExtensionFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(Edit1.Text);
end;

procedure TJSExtensionFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TJSExtensionFrm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TJSExtensionFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  // Adding some custom context menu entries
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETJSEVENT,   'Set mouseover event');
  model.AddItem(MINIBROWSER_CONTEXTMENU_JSVISITDOM,   'Visit DOM in JavaScript');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');           
  model.AddCheckItem(MINIBROWSER_CONTEXTMENU_OFFLINE, 'Offline');
  model.SetChecked(MINIBROWSER_CONTEXTMENU_OFFLINE,   FOffline);
end;

procedure TJSExtensionFrm.Chromium1BeforePopup(Sender: TObject;
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

procedure TJSExtensionFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TJSExtensionFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);   
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
                            'var ret = "<" + n.nodeName + ">";'+
                            'if (n.parentNode){return getpath(n.parentNode) + ret} else '+
                            'return ret'+
                          '};'+
                          'myextension.mouseover(getpath(evt.target))}'+
                        ')';

          frame.ExecuteJavaScript(TempJSCode, 'about:blank', 0);
        end;

    MINIBROWSER_CONTEXTMENU_JSVISITDOM :
      if (frame <> nil) and frame.IsValid then
        begin
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

    MINIBROWSER_CONTEXTMENU_OFFLINE :
      SwitchOfflineMode;
  end;
end;         

// This is a simple example to set the "offline" mode in the DevTools using the TChromium methods directly.
function TJSExtensionFrm.SwitchOfflineMode : integer;
var
  TempParams : ICefDictionaryValue;
begin
  try
    FOffline := not(FOffline);

    TempParams := TCefDictionaryValueRef.New;
    TempParams.SetBool('offline', FOffline);
    TempParams.SetDouble('latency', 0);
    TempParams.SetDouble('downloadThroughput', 0);
    TempParams.SetDouble('uploadThroughput', 0);

    Result := Chromium1.ExecuteDevToolsMethod(0, 'Network.emulateNetworkConditions', TempParams);
  finally
    TempParams := nil;
  end;
end;

procedure TJSExtensionFrm.Chromium1ProcessMessageReceived(Sender: TObject;
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
      StatusPnl.Caption := message.ArgumentList.GetString(0); // this doesn't create/destroy components
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

procedure TJSExtensionFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TJSExtensionFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
end;

procedure TJSExtensionFrm.FormShow(Sender: TObject);
begin
  StatusPnl.Caption := 'Initializing browser. Please wait...';

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TJSExtensionFrm.CEFSentinel1Close(Sender: TObject);
begin

end;

procedure TJSExtensionFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSExtensionFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TJSExtensionFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TJSExtensionFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TJSExtensionFrm.ShowTextViewerMsg(var aMessage : TMessage);
begin
  // This form will show the HTML received from JavaScript
  SimpleTextViewerFrm.Memo1.Lines.Text := FText;
  SimpleTextViewerFrm.ShowModal;
end;

procedure TJSExtensionFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TJSExtensionFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  StatusPnl.Caption := '';
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TJSExtensionFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

end.
