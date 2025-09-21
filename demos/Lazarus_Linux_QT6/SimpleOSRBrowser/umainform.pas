unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LMessages, StdCtrls, LCLType, ComCtrls, Types, SyncObjs, 
  {$IFDEF LCLQT}qt4,{$ENDIF}
  {$IFDEF LCLQT5}qt5,{$ENDIF}
  {$IFDEF LCLQT6}qt6,{$ENDIF}
  uCEFBufferPanel, uCEFChromium, uCEFInterfaces, uCEFTypes;
        
const
  // Set this constant to True and load "file:///<path-to-CEF4Delphi>/bin/transparency.html" to test a
  // transparent browser.
  TRANSPARENT_BROWSER      = True;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddressCb: TComboBox;
    AddressPnl: TPanel;
    Panel1: TBufferPanel;
    Chromium1: TChromium;
    GoBtn: TButton;

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);   
    procedure Panel1QTKeyPress(Sender: TObject; Event_: QEventH);
    procedure Panel1QTKeyRelease(Sender: TObject; Event_: QEventH);
    procedure Panel1Resize(Sender: TObject);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1CursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure Chromium1GetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure Chromium1GetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure Chromium1GetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1Paint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
    procedure Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
    procedure Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure Chromium1SetFocus(Sender: TObject; const browser: ICefBrowser; source: TCefFocusSource; out Result: Boolean);
    procedure Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);

    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);   

    procedure Application_OnActivate(Sender: TObject);
    procedure Application_OnDeactivate(Sender: TObject);

    procedure AddressCbEnter(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);
  private

  protected
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FResizing        : boolean;
    FPendingResize   : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;
    FResizeCS        : TCriticalSection;
    FBrowserCS       : TCriticalSection;
    FPanelCursor     : TCursor;
    FPanelHint       : ustring;
    FPanelOffset     : TPoint;

    function  GetPanelCursor : TCursor;
    function  GetPanelHint : ustring;

    procedure SetPanelCursor(aValue : TCursor);
    procedure SetPanelHint(const aValue : ustring);

    procedure SendCompMessage(aMsg : cardinal; aData: PtrInt = 0);
    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;
    procedure UpdatePanelOffset;

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure PendingResizeMsg(Data: PtrInt);
    procedure PendingInvalidateMsg(Data: PtrInt);
    procedure PendingCursorUpdateMsg(Data: PtrInt);
    procedure PendingHintUpdateMsg(Data: PtrInt);

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    property PanelCursor  : TCursor   read GetPanelCursor   write SetPanelCursor;
    property PanelHint    : ustring   read GetPanelHint     write SetPanelHint;

  public

  end;

var
  MainForm: TMainForm;  

procedure CreateGlobalCEFApp;
function StartMainProcess: boolean;

implementation

{$R *.lfm}     

// This is a simple CEF browser in "off-screen rendering" mode (a.k.a OSR mode)

// It uses the default CEF configuration with a multithreaded message loop and
// that means that the TChromium events are executed in a CEF thread.

// QT is not thread safe so we have to save all the information given in the
// TChromium events and use it later in the main application thread. We use
// critical sections to protect all that information.

// For example, the browser updates the mouse cursor in the
// TChromium.OnCursorChange event so we have to save the cursor in FPanelCursor
// and use Application.QueueAsyncCall to update the Panel1.Cursor value in the
// main application thread.

// The raw bitmap information given in the TChromium.OnPaint event also needs to
// be stored in a TCEFBitmapBitBuffer class instead of a simple TBitmap to avoid
// issues with QT.

// Chromium renders the web contents asynchronously. It uses multiple processes
// and threads which makes it complicated to keep the correct browser size.

// In one hand you have the main application thread where the form is resized by
// the user. On the other hand, Chromium renders the contents asynchronously
// with the last browser size available, which may have changed by the time
// Chromium renders the page.

// For this reason we need to keep checking the real size and call
// TChromium.WasResized when we detect that Chromium has an incorrect size.

// TChromium.WasResized triggers the TChromium.OnGetViewRect event to let CEF
// read the current browser size and then it triggers TChromium.OnPaint when the
// contents are finally rendered.

// TChromium.WasResized --> (time passes) --> TChromium.OnGetViewRect --> (time passes) --> TChromium.OnPaint

// You have to assume that the real browser size can change between those calls
// and events.

// This demo uses a couple of fields called "FResizing" and "FPendingResize" to
// reduce the number of TChromium.WasResized calls.

// FResizing is set to True before the TChromium.WasResized call and it's set to
// False at the end of the TChromium.OnPaint event.

// FPendingResize is set to True when the browser changed its size while
// FResizing was True. The FPendingResize value is checked at the end of
// TChromium.OnPaint to check the browser size again because it changed while
// Chromium was rendering the page.

// The TChromium.OnPaint event in the demo also calls
// TBufferPanel.UpdateOrigBufferDimensions and TBufferPanel.BufferIsResized to check
// the width and height of the buffer parameter, and the internal buffer size in
// the TBufferPanel component.

// Lazarus usually initializes the QT WidgetSet in the initialization section
// of the "Interfaces" unit which is included in the LPR file. This causes
// initialization problems in CEF and we need to call "CreateWidgetset" after
// the GlobalCEFApp.StartMainProcess call.

// Lazarus shows a warning if we remove the "Interfaces" unit from the LPR file
// so we created a custom unit with the same name that includes two procedures
// to initialize and finalize the WidgetSet at the right time.

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and
//    calls Chromium1.CloseBrowser(True) which will destroy the internal browser
//    immediately.
// 2- Chromium1.OnBeforeClose is triggered because the internal browser was
//    destroyed. FCanClose is set to True and calls
//    SendCompMessage(CEF_BEFORECLOSE) to close the form asynchronously.

uses
  Math,
  uCEFMiscFunctions, uCEFApplication, uCEFConstants, uCEFBitmapBitBuffer,
  uCEFLinuxFunctions;

const
  CEF_UPDATE_CURSOR   = $A1D;
  CEF_UPDATE_HINT     = $A1E;

var
  MainAppEvent : TEventObject;

{GlobalCEFApp functions}
{%Region}
procedure GlobalCEFApp_OnContextInitialized();
begin
  MainAppEvent.SetEvent;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.RootCache                  := 'RootCache';
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.OnContextInitialized       := @GlobalCEFApp_OnContextInitialized;          

  // If you need transparency leave the GlobalCEFApp.BackgroundColor property
  // with the default value or set the alpha channel to 0
  if TRANSPARENT_BROWSER then
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($00, $00, $00, $00)
   else
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
end;

function StartMainProcess: boolean;
begin
  Result := False;

  if GlobalCEFApp.StartMainProcess then
    begin
      // Wait until the context is initialized before initializing GTK.
      if (MainAppEvent.WaitFor(10000) = wrTimeout) then
        CefDebugLog('CEF initialization failure!')
       else
        Result := True;
    end;
end;
{%Endregion}

{TBufferPanel events}
{%Region}
procedure TMainForm.Panel1Enter(Sender: TObject);
begin
  Chromium1.SetFocus(True);
end;

procedure TMainForm.Panel1Exit(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TMainForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  Panel1.SetFocus;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
end;

procedure TMainForm.Panel1MouseEnter(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  TempPoint           := Panel1.ScreenToClient(mouse.CursorPos);
  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := EVENTFLAG_NONE;
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TMainForm.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  TempPoint           := Panel1.ScreenToClient(mouse.CursorPos);
  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := EVENTFLAG_NONE;
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseMoveEvent(@TempEvent, True);
end;

procedure TMainForm.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TMainForm.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent  : TCefMouseEvent;
begin
  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);
end;

procedure TMainForm.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
begin
  TempEvent.x         := MousePos.x;
  TempEvent.y         := MousePos.y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
end;

procedure TMainForm.Panel1QTKeyPress(Sender: TObject; Event_: QEventH);   
var
  TempCefEvent : TCefKeyEvent;
begin
  QTKeyEventToCEFKeyEvent(QKeyEventH(Event_), TempCefEvent);

  TempCefEvent.kind := KEYEVENT_RAWKEYDOWN;
  //CefKeyEventLog(TempCefEvent);
  Chromium1.SendKeyEvent(@TempCefEvent);

  if AdjustCefKeyCharEvent(QKeyEventH(Event_), TempCefEvent) then
    begin
      TempCefEvent.kind := KEYEVENT_CHAR;
      //CefKeyEventLog(TempCefEvent);
      Chromium1.SendKeyEvent(@TempCefEvent);
    end;
end;

procedure TMainForm.Panel1QTKeyRelease(Sender: TObject; Event_: QEventH);  
var
  TempCefEvent : TCefKeyEvent;
begin
  QTKeyEventToCEFKeyEvent(QKeyEventH(Event_), TempCefEvent);

  TempCefEvent.kind := KEYEVENT_KEYUP;
  //CefKeyEventLog(TempCefEvent);
  Chromium1.SendKeyEvent(@TempCefEvent);
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;
{%Endregion}

{TChromium events}
{%Region}
procedure TMainForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TMainForm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TMainForm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer;
  const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.Chromium1CursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor_: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo;
  var aResult : boolean);
begin
  PanelCursor := CefCursorToWindowsCursor(cursorType);
  aResult     := True;

  SendCompMessage(CEF_UPDATE_CURSOR);
end;

procedure TMainForm.Chromium1GetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
  Result: Boolean);
var
  TempRect  : TCEFRect;
  TempScale : single;
begin
  TempScale       := Panel1.ScreenScale;
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  TempRect.height := DeviceToLogical(Panel1.Height, TempScale);

  screenInfo.device_scale_factor := TempScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TMainForm.Chromium1GetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
begin
  try
    FBrowserCS.Acquire;
    screenX := LogicalToDevice(viewX, Panel1.ScreenScale) + FPanelOffset.x;
    screenY := LogicalToDevice(viewY, Panel1.ScreenScale) + FPanelOffset.y;
    Result  := True;
  finally
    FBrowserCS.Release;
  end;
end;

procedure TMainForm.Chromium1GetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
var
  TempScale : single;
begin
  TempScale   := Panel1.ScreenScale;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  rect.height := DeviceToLogical(Panel1.Height, TempScale);
end;

procedure TMainForm.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.Chromium1Paint(Sender: TObject; const browser: ICefBrowser;
  type_: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer
  );
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempBitmap : TCEFBitmapBitBuffer;
  TempSrcRect : TRect;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      begin
        if (type_ = PET_POPUP) then
          begin
            Panel1.UpdateOrigPopupBufferDimensions(aWidth, aHeight);

            TempBitmap := Panel1.OrigPopupBuffer;
            TempWidth  := Panel1.OrigPopupBufferWidth;
            TempHeight := Panel1.OrigPopupBufferHeight;
          end
         else
          begin
            TempForcedResize := Panel1.UpdateOrigBufferDimensions(aWidth, aHeight) or
                                not(Panel1.BufferIsResized(False));

            TempBitmap := Panel1.OrigBuffer;
            TempWidth  := Panel1.OrigBufferWidth;
            TempHeight := Panel1.OrigBufferHeight;
          end;

        SrcStride := aWidth * SizeOf(TRGBQuad);
        n         := 0;

        while (n < dirtyRectsCount) do
          begin
            if (dirtyRects^[n].x >= 0) and (dirtyRects^[n].y >= 0) then
              begin
                TempLineSize := min(dirtyRects^[n].width, TempWidth - dirtyRects^[n].x) * SizeOf(TRGBQuad);

                if (TempLineSize > 0) then
                  begin
                    TempSrcOffset := ((dirtyRects^[n].y * aWidth) + dirtyRects^[n].x) * SizeOf(TRGBQuad);
                    TempDstOffset := (dirtyRects^[n].x * SizeOf(TRGBQuad));

                    src := @PByte(buffer)[TempSrcOffset];

                    i := 0;
                    j := min(dirtyRects^[n].height, TempHeight - dirtyRects^[n].y);

                    while (i < j) do
                      begin
                        TempBufferBits := TempBitmap.Scanline[dirtyRects^[n].y + i];
                        dst            := @PByte(TempBufferBits)[TempDstOffset];

                        Move(src^, dst^, TempLineSize);

                        Inc(src, SrcStride);
                        inc(i);
                      end;
                  end;
              end;

            inc(n);
          end;

        if FShowPopup then
          begin
            TempSrcRect := Rect(0, 0,
                                FPopUpRect.Right  - FPopUpRect.Left,
                                FPopUpRect.Bottom - FPopUpRect.Top);

            Panel1.DrawOrigPopupBuffer(TempSrcRect, FPopUpRect);
          end;

        Panel1.EndBufferDraw;

        SendCompMessage(CEF_PENDINGINVALIDATE);

        if (type_ = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then
              SendCompMessage(CEF_PENDINGRESIZE);

            FResizing      := False;
            FPendingResize := False;
          end;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TMainForm.Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
begin
  if aShow then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      if (Chromium1 <> nil) then Chromium1.Invalidate(PET_VIEW);
    end;
end;

procedure TMainForm.Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
begin
  LogicalToDevice(rect^, Panel1.ScreenScale);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TMainForm.Chromium1SetFocus(Sender: TObject; const browser: ICefBrowser;
  source: TCefFocusSource; out Result: Boolean);
begin
  Result := not(Panel1.Focused);
end;

procedure TMainForm.Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
begin
  PanelHint := aText;
  Result    := True;

  SendCompMessage(CEF_UPDATE_HINT);
end;
{%Endregion}

{TForm events}
{%Region}
procedure TMainForm.FormActivate(Sender: TObject);
begin
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.

  // Linux needs a visible form to create a browser so we need to use the
  // TForm.OnActivate event instead of the TForm.OnShow event

  if not(Chromium1.Initialized) then
    begin
      // We have to update the DeviceScaleFactor here to get the scale of the
      // monitor where the main application form is located.
      GlobalCEFApp.UpdateDeviceScaleFactor;

      UpdatePanelOffset;

      // If you need transparency leave the Chromium1.Options.BackgroundColor property
      // with the default value or set the alpha channel to 0
      if TRANSPARENT_BROWSER then
        Chromium1.Options.BackgroundColor := CefColorSetARGB($00, $00, $00, $00)
       else
        Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

      Chromium1.DefaultURL := UTF8Decode(AddressCb.Text);

      Chromium1.CreateBrowser;
    end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPopUpRect        := rect(0, 0, 0, 0);
  FShowPopUp        := False;
  FResizing         := False;
  FPendingResize    := False;
  FCanClose         := False;
  FClosing          := False;
  FResizeCS         := TCriticalSection.Create;
  FBrowserCS        := TCriticalSection.Create;

  Panel1.CopyOriginalBuffer := True;
  Panel1.OnQtKeyPress       := @Panel1QTKeyPress;
  Panel1.OnQtKeyRelease     := @Panel1QTKeyRelease;

  Application.OnActivate    := @Application_OnActivate;
  Application.OnDeactivate  := @Application_OnDeactivate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if (FResizeCS   <> nil) then FreeAndNil(FResizeCS);
  if (FBrowserCS  <> nil) then FreeAndNil(FBrowserCS);
end;

procedure TMainForm.FormHide(Sender: TObject);
begin
  Chromium1.SetFocus(False);
  Chromium1.WasHidden(True);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Chromium1.WasHidden(False);
  Chromium1.SetFocus(Panel1.Focused);
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsMinimized) then
    begin
      Chromium1.SetFocus(False);
      Chromium1.WasHidden(True);
    end
   else
    begin
      Chromium1.WasHidden(False);
      Chromium1.SetFocus(Panel1.Focused);
    end;
end;
{%Endregion}

{TApplication events}
{%Region}
procedure TMainForm.Application_OnActivate(Sender: TObject);
begin
  Chromium1.SetFocus(Panel1.Focused);
end;

procedure TMainForm.Application_OnDeactivate(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;
{%Endregion}

{Other events}
{%Region}
procedure TMainForm.AddressCbEnter(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  Chromium1.LoadURL(UTF8Decode(AddressCb.Text))
end;

procedure TMainForm.GoBtnEnter(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;
{%Endregion}

{Getters and setters}
{%Region}
function TMainForm.GetPanelCursor : TCursor;
begin
  try
    FBrowserCS.Acquire;
    Result := FPanelCursor;
  finally
    FBrowserCS.Release;
  end;
end;

function TMainForm.GetPanelHint : ustring;
begin
  try
    FBrowserCS.Acquire;
    Result := FPanelHint;
  finally
    FBrowserCS.Release;
  end;
end;

procedure TMainForm.SetPanelCursor(aValue : TCursor);
begin
  try
    FBrowserCS.Acquire;
    FPanelCursor := aValue;
  finally
    FBrowserCS.Release;
  end;
end;

procedure TMainForm.SetPanelHint(const aValue : ustring);
begin
  try
    FBrowserCS.Acquire;
    FPanelHint := aValue;
  finally
    FBrowserCS.Release;
  end;
end;
{%Endregion}

{Misc functions}
{%Region}
procedure TMainForm.SendCompMessage(aMsg : cardinal; aData: PtrInt);
begin
  case aMsg of
    CEF_AFTERCREATED      : Application.QueueAsyncCall(@BrowserCreatedMsg, aData);
    CEF_BEFORECLOSE       : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
    CEF_PENDINGRESIZE     : Application.QueueAsyncCall(@PendingResizeMsg, aData);
    CEF_PENDINGINVALIDATE : Application.QueueAsyncCall(@PendingInvalidateMsg, aData);
    CEF_UPDATE_CURSOR     : Application.QueueAsyncCall(@PendingCursorUpdateMsg, aData);
    CEF_UPDATE_HINT       : Application.QueueAsyncCall(@PendingHintUpdateMsg, aData);
  end;
end;

function TMainForm.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TMainForm.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TMainForm.DoResize;
begin
  try
    FResizeCS.Acquire;

    if FResizing then
      FPendingResize := True
     else
      if Panel1.BufferIsResized then
        Chromium1.Invalidate(PET_VIEW)
       else
        begin
          FResizing := True;
          Chromium1.WasResized;
        end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TMainForm.UpdatePanelOffset;
var
  TempPoint : TPoint;
begin
  try
    FBrowserCS.Acquire;
    TempPoint.x  := 0;
    TempPoint.y  := 0;
    FPanelOffset := Panel1.ClientToScreen(TempPoint);
  finally
    FBrowserCS.Release;
  end;
end;
{%Endregion}

{Message handlers}
{%Region}
procedure TMainForm.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption            := 'Simple OSR Browser';
  AddressPnl.Enabled := True;

  Chromium1.SetFocus(Panel1.Focused);
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TMainForm.PendingResizeMsg(Data: PtrInt);
begin
  DoResize;
end;

procedure TMainForm.PendingInvalidateMsg(Data: PtrInt);
begin
  Panel1.Invalidate;
end;

procedure TMainForm.PendingCursorUpdateMsg(Data: PtrInt);
begin
  Panel1.Cursor := PanelCursor;
end;

procedure TMainForm.PendingHintUpdateMsg(Data: PtrInt);
begin
  Panel1.hint     := UTF8Encode(PanelHint);
  Panel1.ShowHint := (length(Panel1.hint) > 0);
end;

procedure TMainForm.WMMove(var Message: TLMMove);
begin
  inherited;
  UpdatePanelOffset;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMSize(var Message: TLMSize);
begin
  inherited;
  UpdatePanelOffset;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  UpdatePanelOffset;
  Chromium1.NotifyMoveOrResizeStarted;
end;
{%Endregion}

initialization
  MainAppEvent := TEventObject.Create(nil, True, False, 'MainAppEvent');

finalization
  if assigned(MainAppEvent) then
    FreeAndNil(MainAppEvent);

end.

