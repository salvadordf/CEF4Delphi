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

unit uDOMVisitor;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.SyncObjs, System.Classes, Vcl.Graphics, Vcl.Menus, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Types,
  Vcl.ComCtrls, Vcl.ClipBrd, System.UITypes,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus, SyncObjs,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes,
  uCEFConstants, uCEFWinControl, uCEFChromiumCore, uCEFChromiumEvents;

const
  MINIBROWSER_VISITDOM_PARTIAL            = WM_APP + $101;
  MINIBROWSER_VISITDOM_FULL               = WM_APP + $102;
  MINIBROWSER_COPYFRAMEIDS_1              = WM_APP + $103;
  MINIBROWSER_COPYFRAMEIDS_2              = WM_APP + $104;
  MINIBROWSER_SHOWMESSAGE                 = WM_APP + $105;
  MINIBROWSER_SHOWSTATUSTEXT              = WM_APP + $106;
  MINIBROWSER_VISITDOM_JS                 = WM_APP + $107;
  MINIBROWSER_SHOWERROR                   = WM_APP + $108;

  MINIBROWSER_CONTEXTMENU_VISITDOM_PARTIAL = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_VISITDOM_FULL    = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS_1   = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS_2   = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_VISITDOM_JS      = MENU_ID_USER_FIRST + 5;
  MINIBROWSER_CONTEXTMENU_SETINPUTVALUE_JS = MENU_ID_USER_FIRST + 6;
  MINIBROWSER_CONTEXTMENU_SETINPUTVALUE_DT = MENU_ID_USER_FIRST + 7;
  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS     = MENU_ID_USER_FIRST + 8;

  DOMVISITOR_MSGNAME_PARTIAL  = 'domvisitorpartial';
  DOMVISITOR_MSGNAME_FULL     = 'domvisitorfull';
  RETRIEVEDOM_MSGNAME_PARTIAL = 'retrievedompartial';
  RETRIEVEDOM_MSGNAME_FULL    = 'retrievedomfull';
  FRAMEIDS_MSGNAME            = 'getframeids';
  CONSOLE_MSG_PREAMBLE        = 'DOMVISITOR';

  NODE_ID = 'keywords';

type
  TDTVisitStatus = (dvsIdle, dvsGettingDocNodeID, dvsQueryingSelector, dvsSettingAttributeValue);

  TDOMVisitorFrm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressBarPnl: TPanel;
    AddressEdt: TEdit;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Panel1: TPanel;
    GoBtn: TButton;
    VisitDOMBtn: TButton;

    procedure Timer1Timer(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure VisitDOMBtnClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal; out Result: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1ConsoleMessage(Sender: TObject; const browser: ICefBrowser; level: Cardinal; const message, source: ustring; line: Integer; out Result: Boolean);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    // Critical section and fields to show information received in CEF events safely.
    FCritSection : TCriticalSection;
    FMsgContents : string;
    FStatusText  : string;

    FStatus      : TDTVisitStatus;
    FErrorText   : string;

    function  GetMsgContents : string;
    function  GetStatusText : string;
    function  GetErrorText : string;

    procedure SetMsgContents(const aValue : string);
    procedure SetStatusText(const aValue : string);
    procedure SetErrorText(const aValue : string);

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
    procedure VisitDOMMsg(var aMessage : TMessage); message MINIBROWSER_VISITDOM_PARTIAL;
    procedure VisitDOM2Msg(var aMessage : TMessage); message MINIBROWSER_VISITDOM_FULL;
    procedure VisitDOM3Msg(var aMessage : TMessage); message MINIBROWSER_VISITDOM_JS;
    procedure CopyFrameIDs1(var aMessage : TMessage); message MINIBROWSER_COPYFRAMEIDS_1;
    procedure CopyFrameIDs2(var aMessage : TMessage); message MINIBROWSER_COPYFRAMEIDS_2;
    procedure ShowMessageMsg(var aMessage : TMessage); message MINIBROWSER_SHOWMESSAGE;
    procedure ShowStatusTextMsg(var aMessage : TMessage); message MINIBROWSER_SHOWSTATUSTEXT;
    procedure ShowErrorMsg(var aMessage : TMessage); message MINIBROWSER_SHOWERROR;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

    procedure ShowStatusText(const aText : string);
    function  QuerySelector(aNodeID : integer; const aSelector : string) : integer;
    function  SetAttributeValue(aNodeID : integer; const aName, aValue : string) : integer;

    function  HandleGetDocumentRslt(aSuccess : boolean; const aResult: ICefValue) : boolean;
    function  HandleQuerySelectorRslt(aSuccess : boolean; const aResult: ICefValue) : boolean;
    function  HandleSetAttributeValueRslt(aSuccess : boolean; const aResult: ICefValue) : boolean;
    function  HandleErrorRslt(const aResult: ICefValue) : boolean;

    property  MsgContents : string   read GetMsgContents  write SetMsgContents;
    property  StatusText  : string   read GetStatusText   write SetStatusText;
    property  ErrorText   : string   read GetErrorText    write SetErrorText;
  end;

var
  DOMVisitorFrm: TDOMVisitorFrm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFProcessMessage, uCEFMiscFunctions, uCEFSchemeRegistrar,
  uCEFRenderProcessHandler, uCEFv8Handler, uCEFDomVisitor, uCEFDomNode,
  uCEFTask, uCEFDictionaryValue, uCEFJson;

// This demo sends messages from the browser process to the render process,
// and from the render process to the browser process.

// To send a message from the browser process you must use the
// TChromium.SendProcessMessage procedure with a PID_RENDERER parameter. The
// render process receives those messages in the
// GlobalCEFApp.OnProcessMessageReceived event.

// To send messages from the render process you must use the
// frame.SendProcessMessage procedure with a PID_BROWSER parameter. The browser
// process receives those messages in the TChromium.OnProcessMessageReceived
// event.

// message.name is used to identify different messages sent with
// SendProcessMessage.

// The OnProcessMessageReceived event can recognize any number of messages
// identifying them by message.name

// The CEF API is not as powerful as JavaScript to visit the DOM. Consider using
// TChromium.ExecuteJavaScript to execute custom JS code in case you need more
// powerful features.

// Read the code comments in the JSExtension demo for more information about the
// Chromium processes and how to send messages between them :
// https://github.com/salvadordf/CEF4Delphi/blob/master/demos/Delphi_VCL/JavaScript/JSExtension/uJSExtension.pas

// This demo also uses de "console trick" to send information from the render
// process to the browser process.
// This method for sending text messages is limited to around 10000 characters
// but it's much easier to implement than using a JavaScript extension.
// It cosist of using the JavaScript command "console.log" with a known text
// preamble. The browser process receives the console message in the
// TChromium.OnConsoleMessage event and we identify the right message thanks to
// the preamble in the message.

// Other alternative ways to send information between processes :
// 1. Use the JavaScript functions "alert" or "prompt" : They're also limited to
//    10000 characters in the text message but "prompt" doesn't have limits for the
//    value (defaultPromptText). https://www.briskbard.com/forum/viewtopic.php?t=1251#p5324
// 2. Use websockets or any other custom IPC message.
// 3. Use a common database protected by a named mutex. A proces would only have to
//    send commands to the other process when the information is ready to be read
//    in the database.

// This demo also uses DevTool methods to change the "value" attribute of an
// INPUT HTML element. Each method is called using the
// TChromium.ExecuteDevToolsMethod function and the results are received in the
// TChromium.OnDevToolsMethodResult event.

// To test this feature right click on the web page and select the "Set INPUT
// value using DevTools methods" option.

// That menu option will execute the "DOM.getDocument" method to get the NodeId
// of the document node and it will trigger the TChromium.OnDevToolsMethodResult event.
// In that event we use the NodeId of the document to call the "DOM.querySelector" method
// with the "#keywords" selector, which is the ID atttribute of the INPUT element we need.
// The TChromium.OnDevToolsMethodResult event is triggered once again and now we have the
// NodeId of the INPUT element. Now we can call the "DOM.setAttributeValue" method to
// update the "value" attribute in the INPUT element.

// Read these documents for more details about the DevTools methods :
// General information -> https://chromedevtools.github.io/devtools-protocol/
// "DOM.getDocument" method -> https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-getDocument
// "DOM.querySelector" method -> https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelector
// "DOM.setAttributeValue" method -> https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-setAttributeValue

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which
//    triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy
//    CEFWindowParent1 in the main thread, which triggers the
//    TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the
//    form.

procedure SimpleDOMIteration(const aDocument: ICefDomDocument);
var
  TempHead, TempChild : ICefDomNode;
begin
  try
    if (aDocument <> nil) then
      begin
        TempHead := aDocument.Head;

        if (TempHead <> nil) then
          begin
            TempChild := TempHead.FirstChild;

            while (TempChild <> nil) do
              begin
                CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Head child element : ' + TempChild.Name);
                TempChild := TempChild.NextSibling;
              end;
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleDOMIteration', e) then raise;
  end;
end;

procedure SimpleNodeSearch(const aDocument: ICefDomDocument; const aFrame : ICefFrame);
var
  TempNode : ICefDomNode;
  TempJSCode, TempMessage : ustring;
begin
  try
    if (aDocument <> nil) then
      begin
        TempNode := aDocument.GetElementById(NODE_ID);

        if (TempNode <> nil) then
          begin
            // Here we send the name and value of the element with the "console trick".
            // The name and value contents are included in TempMessage and the we
            // execute "console.log" in JavaScript to send TempMessage with a
            // known preamble that will be used to identify the message in the
            // TChromium.OnConsoleMessage event.

            // NOTE : In case you try to read or write node values using the CEF API
            // you should know that ICefDomNode.GetValue and ICefDomNode.SetValue
            // only work in text nodes. ICefDomNode.GetElementAttribute returns
            // the attribute value specified in the HTML and not the current value.

            // It's recommended that you use JavaScript or DevTools methods if
            // you need to get or set the value of HTML elements.
            // For example, if you want to use the "console trick" and you want
            // to get the value of the search box in our forum you would have to
            // execute this JavaScript code :
            // console.log("DOMVISITOR" + document.getElementById("keywords").value);

            TempMessage := 'name:' + TempNode.Name;
            TempJSCode  := 'console.log("' + CONSOLE_MSG_PREAMBLE + TempMessage + '");';
            aFrame.ExecuteJavaScript(TempJSCode, 'about:blank', 0);
          end;

        TempNode := aDocument.GetFocusedNode;

        if (TempNode <> nil) then
          begin
            CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Focused element name : ' + TempNode.Name);
            CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'Focused element inner text : ' + TempNode.ElementInnerText);
          end;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('SimpleNodeSearch', e) then raise;
  end;
end;

procedure DOMVisitor_OnDocAvailable(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  TempMessage : ICefProcessMessage;
begin
  // This function is called from a different process.
  // document is only valid inside this function.
  // As an example, this function only writes the document title to the 'debug.log' file.
  CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.Title : ' + document.Title);

  if document.HasSelection then
    CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.SelectionAsText : ' + quotedstr(document.SelectionAsText))
   else
    CefLog('CEF4Delphi', 1, CEF_LOG_SEVERITY_ERROR, 'document.HasSelection : False');

  // Simple DOM iteration example
  SimpleDOMIteration(document);

  // Simple DOM searches
  SimpleNodeSearch(document, frame);

  // Sending back some custom results to the browser process
  // Notice that the DOMVISITOR_MSGNAME_PARTIAL message name needs to be recognized in
  // Chromium1ProcessMessageReceived
  try
    TempMessage := TCefProcessMessageRef.New(DOMVISITOR_MSGNAME_PARTIAL);
    TempMessage.ArgumentList.SetString(0, 'document.Title : ' + document.Title);

    if (frame <> nil) and frame.IsValid then
      frame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;

procedure DOMVisitor_OnDocAvailableFullMarkup(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  TempMessage : ICefProcessMessage;
begin
  // Sending back some custom results to the browser process
  // Notice that the DOMVISITOR_MSGNAME_FULL message name needs to be recognized in
  // Chromium1ProcessMessageReceived
  try
    TempMessage := TCefProcessMessageRef.New(DOMVISITOR_MSGNAME_FULL);
    TempMessage.ArgumentList.SetString(0, document.Body.AsMarkup);

    if (frame <> nil) and frame.IsValid then
      frame.SendProcessMessage(PID_BROWSER, TempMessage);
  finally
    TempMessage := nil;
  end;
end;

procedure DOMVisitor_GetFrameIDs(const browser: ICefBrowser; const frame : ICefFrame);
var
  i          : NativeUInt;
  TempCount  : NativeUInt;
  TempArray  : TCefFrameIdentifierArray;
  TempString : ustring;
  TempMsg    : ICefProcessMessage;
begin
  TempCount := browser.FrameCount;

  if browser.GetFrameIdentifiers(TempCount, TempArray) then
    begin
      TempString := '';
      i          := 0;

      while (i < TempCount) do
        begin
          TempString := TempString + inttostr(TempArray[i]) + CRLF;
          inc(i);
        end;

      try
        TempMsg := TCefProcessMessageRef.New(FRAMEIDS_MSGNAME);
        TempMsg.ArgumentList.SetString(0, TempString);

        if (frame <> nil) and frame.IsValid then
          frame.SendProcessMessage(PID_BROWSER, TempMsg);
      finally
        TempMsg := nil;
      end;
    end;
end;

procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                const frame         : ICefFrame;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);
var
  TempVisitor : TCefFastDomVisitor2;
begin
  aHandled := False;

  if (browser <> nil) then
    begin
      if (message.name = RETRIEVEDOM_MSGNAME_PARTIAL) then
        begin
          if (frame <> nil) and frame.IsValid then
            begin
              TempVisitor := TCefFastDomVisitor2.Create(browser, frame, DOMVisitor_OnDocAvailable);
              frame.VisitDom(TempVisitor);
            end;

          aHandled := True;
        end
       else
        if (message.name = RETRIEVEDOM_MSGNAME_FULL) then
          begin
            if (frame <> nil) and frame.IsValid then
              begin
                TempVisitor := TCefFastDomVisitor2.Create(browser, frame, DOMVisitor_OnDocAvailableFullMarkup);
                frame.VisitDom(TempVisitor);
              end;

            aHandled := True;
          end
         else
          if (message.name = FRAMEIDS_MSGNAME) then
            begin
              DOMVisitor_GetFrameIDs(browser, frame);
              aHandled := True;
            end;
    end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                          := TCefApplication.Create;
  GlobalCEFApp.RemoteDebuggingPort      := 9000;
  GlobalCEFApp.OnProcessMessageReceived := GlobalCEFApp_OnProcessMessageReceived;

  // Enabling the debug log file for then DOM visitor demo.
  // This adds lots of warnings to the console, specially if you run this inside VirtualBox.
  // Remove it if you don't want to use the DOM visitor
  GlobalCEFApp.LogFile              := 'debug.log';
  GlobalCEFApp.LogSeverity          := LOGSEVERITY_INFO;

  // Delphi can only debug one process and it debugs the browser process by
  // default. If you need to debug code executed in the render process you will
  // need to use any of the methods described here :
  // https://www.briskbard.com/index.php?lang=en&pageid=cef#debugging

  // Using the "Single process" mode is one of the ways to debug all the code
  // because everything is executed in the browser process and Delphi won't have
  // any problems. However, The "Single process" mode is unsupported by CEF and
  // it causes unexpected issues. You should *ONLY* use it for debugging
  // purposses.
  //GlobalCEFApp.SingleProcess := True;
end;

procedure TDOMVisitorFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TDOMVisitorFrm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TDOMVisitorFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddItem(MINIBROWSER_CONTEXTMENU_VISITDOM_PARTIAL,  'Visit DOM in CEF (only Title)');
  model.AddItem(MINIBROWSER_CONTEXTMENU_VISITDOM_FULL,     'Visit DOM in CEF (BODY HTML)');
  model.AddItem(MINIBROWSER_CONTEXTMENU_VISITDOM_JS,       'Visit DOM using JavaScript');
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS_1,    'Copy frame IDs in the browser process');
  model.AddItem(MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS_2,    'Copy frame IDs in the render process');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETINPUTVALUE_JS,  'Set INPUT value using JavaScript');
  model.AddItem(MINIBROWSER_CONTEXTMENU_SETINPUTVALUE_DT,  'Set INPUT value using DevTools methods');
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS,      'Show DevTools');
end;

procedure TDOMVisitorFrm.Chromium1BeforePopup(Sender: TObject;
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

procedure TDOMVisitorFrm.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TDOMVisitorFrm.Chromium1ConsoleMessage(Sender: TObject;
  const browser: ICefBrowser; level: Cardinal; const message, source: ustring;
  line: Integer; out Result: Boolean);
begin
  // In this event we receive the message with the name and value of a DOM node
  // from the render process.
  // This event may receive many other messages but we identify our message
  // thanks to the preamble.
  // The we set MsgContents with the rest of the message and send a
  // MINIBROWSER_SHOWMESSAGE message to show MsgContents in the main thread safely.
  // This and many other TChromium events are executed in a CEF thread. The VCL
  // should be used only in the main thread and we use a message and a field
  // protected by a synchronization object to call showmessage safely.
  if (length(message) > 0) and
     (copy(message, 1, length(CONSOLE_MSG_PREAMBLE)) = CONSOLE_MSG_PREAMBLE) then
    begin
      MsgContents := copy(message, succ(length(CONSOLE_MSG_PREAMBLE)), length(message));

      if (length(MsgContents) = 0) then
        MsgContents := 'There was an error reading the search box information'
       else
        MsgContents := 'Search box information: ' + quotedstr(MsgContents);

      PostMessage(Handle, MINIBROWSER_SHOWMESSAGE, 0, 0);
    end;
end;

procedure TDOMVisitorFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: Cardinal; out Result: Boolean);
var
  TempPoint : TPoint;
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_VISITDOM_PARTIAL :
      PostMessage(Handle, MINIBROWSER_VISITDOM_PARTIAL, 0, 0);

    MINIBROWSER_CONTEXTMENU_VISITDOM_FULL :
      PostMessage(Handle, MINIBROWSER_VISITDOM_FULL, 0, 0);

    MINIBROWSER_CONTEXTMENU_VISITDOM_JS :
      PostMessage(Handle, MINIBROWSER_VISITDOM_JS, 0, 0);

    MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS_1 :
      PostMessage(Handle, MINIBROWSER_COPYFRAMEIDS_1, 0, 0);

    MINIBROWSER_CONTEXTMENU_COPYFRAMEIDS_2 :
      PostMessage(Handle, MINIBROWSER_COPYFRAMEIDS_2, 0, 0);

    MINIBROWSER_CONTEXTMENU_SETINPUTVALUE_JS :
      frame.ExecuteJavaScript('document.getElementById("' + NODE_ID + '").value = "qwerty";', 'about:blank', 0);

    MINIBROWSER_CONTEXTMENU_SETINPUTVALUE_DT :
      // https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-getDocument
      if (Chromium1.ExecuteDevToolsMethod(0, 'DOM.getDocument', nil) <> 0) then
        FStatus := dvsGettingDocNodeID
       else
        FStatus := dvsIdle;

    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS :
      begin
        TempPoint.x := params.XCoord;
        TempPoint.y := params.YCoord;
        Chromium1.ShowDevTools(TempPoint, nil);
      end;
  end;
end;

function TDOMVisitorFrm.HandleGetDocumentRslt(aSuccess : boolean; const aResult: ICefValue) : boolean;
var
  TempRsltDict, TempRootNode : ICefDictionaryValue;
  TempDocNodeID : integer;
begin
  Result := False;

  if aSuccess and (aResult <> nil) then
    begin
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadDictionary(TempRsltDict, 'root', TempRootNode) and
         TCEFJson.ReadInteger(TempRootNode, 'nodeId', TempDocNodeID) and
         (QuerySelector(TempDocNodeID, '#' + NODE_ID) <> 0) then
        Result := True;
    end
   else
    if not(HandleErrorRslt(aResult)) then
      ErrorText := 'GetDocument was not successful!';

  if not(Result) then
    PostMessage(Handle, MINIBROWSER_SHOWERROR, 0, 0);
end;

function TDOMVisitorFrm.HandleQuerySelectorRslt(aSuccess : boolean; const aResult: ICefValue) : boolean;
var
  TempRsltDict : ICefDictionaryValue;
  TempNodeID : integer;
begin
  Result := False;

  if aSuccess and (aResult <> nil) then
    begin
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadInteger(TempRsltDict, 'nodeId', TempNodeID) and
         (SetAttributeValue(TempNodeID, 'value', 'qwerty') <> 0) then
        Result := True;
    end
   else
    if not(HandleErrorRslt(aResult)) then
      ErrorText := 'QuerySelector was not successful!';

  if not(Result) then
    PostMessage(Handle, MINIBROWSER_SHOWERROR, 0, 0);
end;

function TDOMVisitorFrm.HandleSetAttributeValueRslt(aSuccess : boolean; const aResult: ICefValue) : boolean;
begin
  Result := False;

  if aSuccess then
    Result := True
   else
    if not(HandleErrorRslt(aResult)) then
      ErrorText := 'SetAttributeValue was not successful!';

  if not(Result) then
    PostMessage(Handle, MINIBROWSER_SHOWERROR, 0, 0);
end;

function TDOMVisitorFrm.HandleErrorRslt(const aResult: ICefValue) : boolean;
var
  TempRsltDict : ICefDictionaryValue;
  TempCode : integer;
  TempMessage : ustring;
begin
  Result := False;

  if (aResult <> nil) then
    begin
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadInteger(TempRsltDict, 'code', TempCode) and
         TCEFJson.ReadString(TempRsltDict, 'message', TempMessage) then
        begin
          ErrorText := 'Error (' + inttostr(TempCode) + ') : ' + quotedstr(TempMessage);
          Result := True;
        end;
    end;
end;

procedure TDOMVisitorFrm.Chromium1DevToolsMethodResult(Sender: TObject;
  const browser: ICefBrowser; message_id: Integer; success: Boolean;
  const result: ICefValue);
begin
  case FStatus of
    dvsGettingDocNodeID :
      if HandleGetDocumentRslt(success, result) then
        begin
          FStatus := dvsQueryingSelector;
          exit;
        end;

    dvsQueryingSelector :
      if HandleQuerySelectorRslt(success, result) then
        begin
          FStatus := dvsSettingAttributeValue;
          exit;
        end;

    dvsSettingAttributeValue :
      HandleSetAttributeValueRslt(success, result);
  end;

  FStatus := dvsIdle;
end;

procedure TDOMVisitorFrm.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  Result := False;

  if (message = nil) or (message.ArgumentList = nil) then exit;

  // Message received from the DOMVISITOR in CEF

  if (message.Name = DOMVISITOR_MSGNAME_PARTIAL) then
    begin
      StatusText := 'DOM Visitor result text : ' + message.ArgumentList.GetString(0);
      Result := True;
    end
   else
    if (message.Name = DOMVISITOR_MSGNAME_FULL) then
      begin
        Clipboard.AsText := message.ArgumentList.GetString(0);
        StatusText := 'HTML copied to the clipboard';
        Result := True;
      end
     else
      if (message.Name = FRAMEIDS_MSGNAME) then
        begin
          Clipboard.AsText := message.ArgumentList.GetString(0);
          StatusText := 'Frame IDs copied to the clipboard in the render process.';
          Result := True;
        end;

  if Result then
    PostMessage(Handle, MINIBROWSER_SHOWSTATUSTEXT, 0, 0);
end;

procedure TDOMVisitorFrm.FormCloseQuery(Sender: TObject;
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

procedure TDOMVisitorFrm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
  FStatus   := dvsIdle;

  FCritSection := TCriticalSection.Create;
end;

procedure TDOMVisitorFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCritSection);
end;

procedure TDOMVisitorFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TDOMVisitorFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TDOMVisitorFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  AddressBarPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TDOMVisitorFrm.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TDOMVisitorFrm.VisitDOMBtnClick(Sender: TObject);
begin
  PostMessage(Handle, MINIBROWSER_VISITDOM_PARTIAL, 0, 0);
end;

procedure TDOMVisitorFrm.VisitDOMMsg(var aMessage : TMessage);
var
  TempMsg : ICefProcessMessage;
begin
  // Use the ArgumentList property if you need to pass some parameters.
  TempMsg := TCefProcessMessageRef.New(RETRIEVEDOM_MSGNAME_PARTIAL); // Same name than TCefCustomRenderProcessHandler.MessageName
  Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
end;

procedure TDOMVisitorFrm.VisitDOM2Msg(var aMessage : TMessage);
var
  TempMsg : ICefProcessMessage;
begin
  // Use the ArgumentList property if you need to pass some parameters.
  TempMsg := TCefProcessMessageRef.New(RETRIEVEDOM_MSGNAME_FULL); // Same name than TCefCustomRenderProcessHandler.MessageName
  Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
end;

procedure TDOMVisitorFrm.VisitDOM3Msg(var aMessage : TMessage);
var
  TempJSCode, TempMessage : string;
begin
  // Here we send the name and value of the element with the "console trick".
  // We execute "console.log" in JavaScript to send TempMessage with a
  // known preamble that will be used to identify the message in the
  // TChromium.OnConsoleMessage event.
  TempMessage := 'document.getElementById("' + NODE_ID + '").value';
  TempJSCode  := 'console.log("' + CONSOLE_MSG_PREAMBLE + '" + ' + TempMessage + ');';
  chromium1.ExecuteJavaScript(TempJSCode, 'about:blank');
end;

procedure TDOMVisitorFrm.CopyFrameIDs1(var aMessage : TMessage);
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
      ShowStatusText('Frame IDs copied to the clipboard in the browser process (' + inttostr(TempCount) + ')');
    end;
end;

procedure TDOMVisitorFrm.CopyFrameIDs2(var aMessage : TMessage);
var
  TempMsg : ICefProcessMessage;
begin
  TempMsg := TCefProcessMessageRef.New(FRAMEIDS_MSGNAME);
  Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
end;

procedure TDOMVisitorFrm.ShowMessageMsg(var aMessage : TMessage);
begin
  showmessage(MsgContents);
end;

procedure TDOMVisitorFrm.ShowStatusTextMsg(var aMessage : TMessage);
begin
  ShowStatusText(StatusText);
end;

procedure TDOMVisitorFrm.ShowErrorMsg(var aMessage : TMessage);
begin
  messagedlg(ErrorText, mtError, [mbOK], 0);
end;

procedure TDOMVisitorFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TDOMVisitorFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TDOMVisitorFrm.ShowStatusText(const aText : string);
begin
  StatusBar1.Panels[0].Text := aText;
end;

// https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelector
function TDOMVisitorFrm.QuerySelector(aNodeID : integer; const aSelector : string) : integer;
var
  TempParams : ICefDictionaryValue;
begin
  Result := 0;

  try
    if (length(aSelector) > 0) then
      begin
        TempParams := TCefDictionaryValueRef.New;
        TempParams.SetInt('nodeId', aNodeID);
        TempParams.SetString('selector', aSelector);
        Result := Chromium1.ExecuteDevToolsMethod(0, 'DOM.querySelector', TempParams);
      end;
  finally
    TempParams := nil;
  end;
end;

// https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-setAttributeValue
function TDOMVisitorFrm.SetAttributeValue(aNodeID : integer; const aName, aValue : string) : integer;
var
  TempParams : ICefDictionaryValue;
begin
  Result := 0;
  try
    if (aNodeID <> 0) then
      begin
        TempParams := TCefDictionaryValueRef.New;
        TempParams.SetInt('nodeId', aNodeID);
        TempParams.SetString('name', aName);
        TempParams.SetString('value', aValue);
        Result := Chromium1.ExecuteDevToolsMethod(0, 'DOM.setAttributeValue', TempParams);
      end;
  finally
    TempParams := nil;
  end;
end;

procedure TDOMVisitorFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

function TDOMVisitorFrm.GetMsgContents : string;
begin
  Result := '';
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      Result := FMsgContents;
    finally
      FCritSection.Release;
    end;
end;

procedure TDOMVisitorFrm.SetMsgContents(const aValue : string);
begin
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      FMsgContents := aValue;
    finally
      FCritSection.Release;
    end;
end;

function TDOMVisitorFrm.GetStatusText : string;
begin
  Result := '';
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      Result := FStatusText;
    finally
      FCritSection.Release;
    end;
end;

procedure TDOMVisitorFrm.SetStatusText(const aValue : string);
begin
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      FStatusText := aValue;
    finally
      FCritSection.Release;
    end;
end;

function TDOMVisitorFrm.GetErrorText : string;
begin
  Result := '';
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      Result := FErrorText;
    finally
      FCritSection.Release;
    end;
end;

procedure TDOMVisitorFrm.SetErrorText(const aValue : string);
begin
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      FErrorText := aValue;
    finally
      FCritSection.Release;
    end;
end;

end.
