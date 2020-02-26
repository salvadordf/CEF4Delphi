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
// https://www.briskbard.com/index.php?lang=en&pageid=cef
//
// Copyright © 2017 Salvador Diaz Fau. All rights reserved.
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

unit uCustomTitleBarExtension;

{$I cef.inc}

interface

uses
{$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
{$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes,
  uCEFConstants,
  uCEFWinControl, uCEFSentinel, uCEFChromiumCore;

const
  MINIBROWSER_SHOWTEXTVIEWER = WM_APP + $100;
  MINIBROWSER_JSBINPARAM = WM_APP + $103;

  MINIBROWSER_CONTEXTMENU_SETJSEVENT = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_JSVISITDOM = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_MUTATIONOBSERVER = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS = MENU_ID_USER_FIRST + 4;

  MOUSEOVER_MESSAGE_NAME = 'mousestate';
  WINDOW_MINIMIZE_MESSAGE = 'minimize';
  WINDOW_MAXIMIZE_MESSAGE = 'maximize';
  WINDOW_CLOSE_MESSAGE = 'close';
  BINARY_PARAM_JS = 'JSBinaryParameter';

type
  TCTBForm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    Timer1: TTimer;
    Timer2: TTimer;
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
      const browser: ICefBrowser; const frame: ICefFrame;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage;
      out Result: Boolean);
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
      var Result: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser;
      var aAction: TCefCloseBrowserAction);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);

    procedure executeJS(frame: ICefFrame);
    procedure Timer2Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
  protected

    tp, tpc: TPoint;
    mouseDrag: Boolean;

    FText: string;
    // Variables to control when can we destroy the form safely
    FCanClose: Boolean; // Set to True in TChromium.OnBeforeClose
    FClosing: Boolean; // Set to True in the CloseQuery event.

    procedure BrowserCreatedMsg(var aMessage: TMessage);
      message CEF_AFTERCREATED;
    procedure BrowserDestroyMsg(var aMessage: TMessage); message CEF_DESTROY;
    procedure ShowTextViewerMsg(var aMessage: TMessage);
      message MINIBROWSER_SHOWTEXTVIEWER;
    procedure EvalJSBinParamMsg(var aMessage: TMessage);
      message MINIBROWSER_JSBINPARAM;
    procedure WMMove(var aMessage: TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage: TMessage); message WM_MOVING;
  public
    { Public declarations }
  end;

var
  CTBForm: TCTBForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCefBinaryValue, uCefProcessMessage, uCEFv8Handler, uTestExtension,
  uCEFMiscFunctions;


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
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.OnWebKitInitialized := GlobalCEFApp_OnWebKitInitialized;
{$IFDEF DEBUG}
  GlobalCEFApp.LogFile := 'debug.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO;
{$ENDIF}
end;

procedure TCTBForm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL('file:///app_view.html');
end;

procedure TCTBForm.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
  CTBForm.executeJS(Chromium1.browser.MainFrame);
end;

procedure TCTBForm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  // Adding some custom context menu entries
  model.AddSeparator;
  model.AddItem(MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS, 'Show DevTools');
end;

procedure TCTBForm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TCTBForm.executeJS(frame: ICefFrame);
var

  TempJSCode: string;

begin
  if (frame <> nil) and frame.IsValid then
  begin
    TempJSCode := 'document.body.addEventListener("mousedown", function(evt){' +
      'myextension.mousestate(getComputedStyle(evt.target).webkitAppRegion)' +
      '});' + chr(13);

    TempJSCode := TempJSCode + ' setAppCaption("' + CTBForm.caption + '");';

    frame.ExecuteJavaScript(TempJSCode, 'about:blank', 0);
  end;
end;

procedure TCTBForm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer; eventFlags: Cardinal;
  out Result: Boolean);
const
  ELEMENT_ID = 'keywords';
  // ID attribute in the search box at https://www.briskbard.com/forum/
var
  TempPoint: TPoint;

begin
  Result := False;

  // Here is the code executed for each custom context menu entry
  case commandId of
    MINIBROWSER_CONTEXTMENU_SETJSEVENT:
      CTBForm.executeJS(frame);

    MINIBROWSER_CONTEXTMENU_SHOWDEVTOOLS:
      begin
        TempPoint.x := params.XCoord;
        TempPoint.y := params.YCoord;

        Chromium1.ShowDevTools(TempPoint, nil);
      end;
  end;
end;

procedure TCTBForm.EvalJSBinParamMsg(var aMessage: TMessage);
var
  TempMsg: ICefProcessMessage;
  TempOpenDialog: TOpenDialog;
  TempStream: TFileStream;
  TempBinValue: ICefBinaryValue;
  TempBuffer: TBytes;
  TempSize: NativeUInt;
  TempPointer: pointer;
begin
  TempOpenDialog := nil;
  TempStream := nil;

  try
    try
      TempOpenDialog := TOpenDialog.Create(nil);
      TempOpenDialog.Filter := 'JPEG files (*.jpg)|*.JPG';

      if TempOpenDialog.Execute then
      begin
        TempStream := TFileStream.Create(TempOpenDialog.FileName, fmOpenRead);
        TempSize := TempStream.Size;

        if (TempSize > 0) then
        begin
          SetLength(TempBuffer, TempSize);
          TempSize := TempStream.Read(TempBuffer, TempSize);

          if (TempSize > 0) then
          begin
            TempPointer := @TempBuffer[0];
            TempBinValue := TCefBinaryValueRef.New(TempPointer, TempSize);
            TempMsg := TCefProcessMessageRef.New(BINARY_PARAM_JS);

            if TempMsg.ArgumentList.SetBinary(0, TempBinValue) then
              Chromium1.SendProcessMessage(PID_RENDERER, TempMsg);
          end;
        end;
      end;
    except
      on e: exception do
        if CustomExceptionHandler('TCTBForm.EvalJSBinParamMsg', e) then
          raise;
    end;
  finally
    if (TempOpenDialog <> nil) then
      FreeAndNil(TempOpenDialog);
    if (TempStream <> nil) then
      FreeAndNil(TempStream);
    SetLength(TempBuffer, 0);
  end;
end;

procedure TCTBForm.Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer);
begin
  CTBForm.executeJS(Chromium1.browser.MainFrame);
end;

procedure TCTBForm.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage;
  out Result: Boolean);
begin
  Result := False;

  if (message = nil) or (message.ArgumentList = nil) then
    exit;

  // This function receives the messages with the JavaScript results

  // Many of these events are received in different threads and the VCL
  // doesn't like to create and destroy components in different threads.

  // It's safer to store the results and send a message to the main thread to show them.

  // The message names are defined in the extension or in JS code.

  if (message.Name = MOUSEOVER_MESSAGE_NAME) then
  begin
    tp := Mouse.CursorPos;
    tp := CTBForm.ScreenToClient(tp);
    mouseDrag := False;
    if message.ArgumentList.GetString(0) = 'drag' then
    begin
      mouseDrag := True;
      Timer2.Enabled := True;
    end;

    Result := True;
  end;

  if (message.Name = WINDOW_MINIMIZE_MESSAGE) then
  begin
    CTBForm.WindowState := wsMinimized;
  end;

  if (message.Name = WINDOW_MAXIMIZE_MESSAGE) then
  begin
    if CTBForm.WindowState = wsNormal then
    begin
      CTBForm.WindowState := wsMaximized;
    end
    else if CTBForm.WindowState = wsMaximized then
      CTBForm.WindowState := wsNormal;
  end;

  if (message.Name = WINDOW_CLOSE_MESSAGE) then
  begin
    CTBForm.close;
  end;
end;

procedure TCTBForm.FormShow(Sender: TObject);
begin
  Chromium1.DefaultURL := 'file:///app_view.html';

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then
    Timer1.Enabled := True;
end;

procedure TCTBForm.WMMove(var aMessage: TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TCTBForm.WMMoving(var aMessage: TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TCTBForm.ShowTextViewerMsg(var aMessage: TMessage);
begin

end;

procedure TCTBForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and
    not(Chromium1.Initialized) then
  begin
    Timer1.Enabled := True;
  end;
end;

procedure TCTBForm.BrowserCreatedMsg(var aMessage: TMessage);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure TCTBForm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TCTBForm.Chromium1Close(Sender: TObject; const browser: ICefBrowser;
  var aAction: TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TCTBForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
  begin
    FClosing := True;
    Visible := False;
    Chromium1.CloseBrowser(True);
  end;
end;

procedure TCTBForm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing := False;
end;

procedure TCTBForm.BrowserDestroyMsg(var aMessage: TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TCTBForm.Button1Click(Sender: TObject);
begin

end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TCTBForm.Timer2Timer(Sender: TObject);
var
  MouseLBtnDown: Boolean;
begin
  tpc := Mouse.CursorPos;

  MouseLBtnDown := (GetKeyState(VK_LBUTTON) < 0);

  if not MouseLBtnDown then
  begin
    mouseDrag := False;
    Timer2.Enabled := False;
  end;

  if mouseDrag then
  begin
    CTBForm.Left := tpc.x - tp.x;
    CTBForm.top := tpc.y - tp.y;
  end;

end;

end.
