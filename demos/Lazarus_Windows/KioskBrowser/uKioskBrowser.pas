unit uKioskBrowser;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, 
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFChromiumCore, uVirtualTouchKeyboard;

const
  HOMEPAGE_URL         = 'https://www.google.com';

  SHOWKEYBOARD_PROCMSG = 'showkeyboard';
  HIDEKEYBOARD_PROCMSG = 'hidekeyboard';

  CEF_SHOWKEYBOARD     = WM_APP + $B01;
  CEF_HIDEKEYBOARD     = WM_APP + $B02;

  KIOSKBROWSER_CONTEXTMENU_EXIT         = MENU_ID_USER_FIRST + 1;
  KIOSKBROWSER_CONTEXTMENU_HIDEKEYBOARD = MENU_ID_USER_FIRST + 2;
  KIOSKBROWSER_CONTEXTMENU_SHOWKEYBOARD = MENU_ID_USER_FIRST + 3;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;

    procedure Timer1Timer(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1CanFocus(Sender: TObject);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1KeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1PreKeyEvent(Sender: TObject; const browser: ICefBrowser; const event: PCefKeyEvent; osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
    procedure Chromium1ProcessMessageReceived(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure FormDestroy(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FVirtualTouchKeyboard : TVirtualTouchKeyboard;

    procedure HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
    procedure HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);

    procedure ShowKeyboardMsg(var aMessage : TMessage); message CEF_SHOWKEYBOARD;
    procedure HideKeyboardMsg(var aMessage : TMessage); message CEF_HIDEKEYBOARD;
    procedure FocusEnabledMsg(var aMessage : TMessage); message CEF_FOCUSENABLED;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uCEFApplication, uCEFMiscFunctions, uCEFProcessMessage;

// This is a simplified Kiosk browser using a virtual keyboard.
// The default URL is defined in the HOMEPAGE_URL constant.

// To close this app press the ESC key or select the 'Exit' option in the context menu.

// This demo uses a TChromium and a TCEFWindowParent

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

function NodeIsTextArea(const aNode : ICefDomNode) : boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := (CompareText(aNode.ElementTagName, 'textarea') = 0);
end;

function NodeIsInput(const aNode : ICefDomNode) : boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := (CompareText(aNode.ElementTagName, 'input') = 0);
end;

function InputNeedsKeyboard(const aNode : ICefDomNode) : boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
var
  TempType : string;
begin
  if not(aNode.HasElementAttribute('type')) then
    Result := True
   else
    begin
      TempType := aNode.GetElementAttribute('type');
      Result   := (CompareText(TempType, 'date')           = 0) or
                  (CompareText(TempType, 'datetime-local') = 0) or
                  (CompareText(TempType, 'email')          = 0) or
                  (CompareText(TempType, 'month')          = 0) or
                  (CompareText(TempType, 'number')         = 0) or
                  (CompareText(TempType, 'password')       = 0) or
                  (CompareText(TempType, 'search')         = 0) or
                  (CompareText(TempType, 'tel')            = 0) or
                  (CompareText(TempType, 'text')           = 0) or
                  (CompareText(TempType, 'time')           = 0) or
                  (CompareText(TempType, 'url')            = 0) or
                  (CompareText(TempType, 'week')           = 0);
    end;
end;

function NodeNeedsKeyboard(const aNode : ICefDomNode) : boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := NodeIsTextArea(aNode) or
            (NodeIsInput(aNode) and InputNeedsKeyboard(aNode));
end;

procedure GlobalCEFApp_OnFocusedNodeChanged(const browser: ICefBrowser; const frame: ICefFrame; const node: ICefDomNode);
var
  TempMsg : ICefProcessMessage;
begin
  if assigned(frame) and frame.IsValid then
    begin
      // This procedure is called in the Render process and checks if the focused node is an
      // INPUT or TEXTAREA to show or hide the virtual keyboard.
      // It sends a process message to the browser process to handle the virtual keyboard.
      if (node <> nil) and NodeNeedsKeyboard(node) then
        TempMsg := TCefProcessMessageRef.New(SHOWKEYBOARD_PROCMSG)
       else
        TempMsg := TCefProcessMessageRef.New(HIDEKEYBOARD_PROCMSG);

      frame.SendProcessMessage(PID_BROWSER, TempMsg);
    end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.RootCache                  := 'RootCache';
  GlobalCEFApp.EnablePrintPreview         := True;
  GlobalCEFApp.TouchEvents                := STATE_ENABLED;
  GlobalCEFApp.AddCustomCommandLine('--kiosk');
  GlobalCEFApp.EnableGPU                  := True;
  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {$ENDIF}
  GlobalCEFApp.OnFocusedNodeChanged       := GlobalCEFApp_OnFocusedNodeChanged;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
      CEFWindowParent1.Free;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // We use a normal form while debugging.
  {$IFNDEF DEBUG}
  BorderIcons := [];
  BorderStyle := bsNone;
  BorderWidth := 0;
  Caption     := '';
  {$ENDIF}
  FCanClose := False;
  FClosing  := False;
  Chromium1.DefaultURL := HOMEPAGE_URL;
  FVirtualTouchKeyboard := TVirtualTouchKeyboard.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FVirtualTouchKeyboard.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // We use a normal form while debugging.
  {$IFNDEF DEBUG}
  self.WindowState := wsMaximized;
  {$ENDIF}

  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddSeparator;

  if FVirtualTouchKeyboard.Visible then
    model.AddItem(KIOSKBROWSER_CONTEXTMENU_HIDEKEYBOARD, 'Hide virtual keyboard')
   else
    model.AddItem(KIOSKBROWSER_CONTEXTMENU_SHOWKEYBOARD, 'Show virtual keyboard');

  model.AddSeparator;
  model.AddItem(KIOSKBROWSER_CONTEXTMENU_EXIT, 'Exit');
end;

procedure TForm1.Chromium1CanFocus(Sender: TObject);
begin
  // The browser required some time to create associated internal objects
  // before being able to accept the focus. Now we can set the focus on the
  // TBufferPanel control
  PostMessage(Handle, CEF_FOCUSENABLED, 0, 0);
end;

procedure TForm1.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; out Result: Boolean);
begin
  Result := False;

  case commandId of
    KIOSKBROWSER_CONTEXTMENU_EXIT         : PostMessage(Handle, WM_CLOSE, 0, 0);
    KIOSKBROWSER_CONTEXTMENU_HIDEKEYBOARD : PostMessage(Handle, CEF_HIDEKEYBOARD, 0, 0);
    KIOSKBROWSER_CONTEXTMENU_SHOWKEYBOARD : PostMessage(Handle, CEF_SHOWKEYBOARD, 0, 0);
  end;
end;

procedure TForm1.Chromium1KeyEvent(Sender: TObject; const browser: ICefBrowser;
  const event: PCefKeyEvent; osEvent: TCefEventHandle; out Result: Boolean);
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

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer;
  const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1PreKeyEvent(Sender: TObject;
  const browser: ICefBrowser; const event: PCefKeyEvent;
  osEvent: TCefEventHandle; out isKeyboardShortcut, Result: Boolean);
begin
  Result := False;

  if (event <> nil) and
     (event.kind in [KEYEVENT_KEYDOWN, KEYEVENT_KEYUP]) and
     (event.windows_key_code = VK_ESCAPE) then
    isKeyboardShortcut := True;
end;

procedure TForm1.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage;
  out Result: Boolean);
begin
  // This function receives the process message from the render process to show or hide the virtual keyboard.
  // This event is not executed in the main thread so it has to send a custom windows message to the form
  // to handle the keyboard in the main thread.

  if (message.Name = SHOWKEYBOARD_PROCMSG) then
    begin
      PostMessage(Handle, CEF_SHOWKEYBOARD, 0 ,0);
      Result := True;
    end
   else
    if (message.Name = HIDEKEYBOARD_PROCMSG) then
      begin
        PostMessage(Handle, CEF_HIDEKEYBOARD, 0 ,0);
        Result := True;
      end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

procedure TForm1.ShowKeyboardMsg(var aMessage : TMessage);
begin
  FVirtualTouchKeyboard.Show;
end;

procedure TForm1.HideKeyboardMsg(var aMessage : TMessage);
begin
  FVirtualTouchKeyboard.Hide;
end;

procedure TForm1.FocusEnabledMsg(var aMessage : TMessage);
begin
  Chromium1.SetFocus(True);
end;

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
end;

procedure TForm1.HandleKeyUp(const aMsg : TMsg; var aHandled : boolean);
var
  TempMessage : TMessage;
  TempKeyMsg  : TWMKey;
begin
  TempMessage.Msg     := aMsg.message;
  TempMessage.wParam  := aMsg.wParam;
  TempMessage.lParam  := aMsg.lParam;
  TempKeyMsg          := TWMKey(TempMessage);

  if (TempKeyMsg.CharCode = VK_ESCAPE) then
    begin
      aHandled := True;

      PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TForm1.HandleKeyDown(const aMsg : TMsg; var aHandled : boolean);
var
  TempMessage : TMessage;
  TempKeyMsg  : TWMKey;
begin
  TempMessage.Msg     := aMsg.message;
  TempMessage.wParam  := aMsg.wParam;
  TempMessage.lParam  := aMsg.lParam;
  TempKeyMsg          := TWMKey(TempMessage);

  if (TempKeyMsg.CharCode = VK_ESCAPE) then aHandled := True;
end;

end.
