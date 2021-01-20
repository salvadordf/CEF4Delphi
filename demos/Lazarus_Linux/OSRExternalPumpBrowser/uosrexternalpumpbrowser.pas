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

unit uOSRExternalPumpBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLType, ComCtrls, Types, SyncObjs, LMessages,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFBufferPanel,
  uCEFChromiumEvents;

type
  { TForm1 }
  TForm1 = class(TForm)
    AddressEdt: TEdit;
    SaveDialog1: TSaveDialog;
    SnapshotBtn: TButton;
    GoBtn: TButton;
    Panel1: TBufferPanel;
    Chromium1: TChromium;
    AddressPnl: TPanel;
    Panel2: TPanel;

    procedure Panel1Click(Sender: TObject);
    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1Resize(Sender: TObject);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1CursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure Chromium1GetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure Chromium1GetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure Chromium1GetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1Paint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
    procedure Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
    procedure Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);     
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);
    procedure SnapshotBtnClick(Sender: TObject);

    procedure AddressEdtEnter(Sender: TObject);
  private             

  protected                      
    FPopUpBitmap     : TBitmap;
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FResizing        : boolean;
    FPendingResize   : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;
    FFirstLoad       : boolean;
    FResizeCS        : TCriticalSection;     

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

  public
    procedure SendCEFKeyEvent(const aCefEvent : TCefKeyEvent);
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

// This is a simple CEF browser in "off-screen rendering" mode (a.k.a OSR mode)
// with a different executable for the Chromium subprocesses and an external
// message pump

// Chromium needs the key press data available in the GDK signals
// "key-press-event" and "key-release-event" but Lazarus doesn't expose that
// information so we have to call g_signal_connect to receive that information
// in the GTKKeyPress function.

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
// TBufferPanel.UpdateBufferDimensions and TBufferPanel.BufferIsResized to check
// the width and height of the buffer parameter, and the internal buffer size in
// the TBufferPanel component.

// Lazarus usually initializes the GTK WidgetSet in the initialization section
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
//    destroyed. FCanClose is set to True and we can close the form safely.

uses
  Math, gtk2, glib2, gdk2, gtk2proc, gtk2int,
  uCEFMiscFunctions, uCEFApplication, uCEFBitmapBitBuffer, uCEFWorkScheduler;

var
  CEFContextInitEvent : TSimpleEvent = nil;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then
     GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;     

procedure GlobalCEFApp_OnContextInitialized;
begin
  if (CEFContextInitEvent <> nil) then
     CEFContextInitEvent.SetEvent;
end;

procedure CreateGlobalCEFApp;
begin               
  // TCEFWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  // We use CreateDelayed in order to have a single thread in the process while
  // CEF is initialized.
  GlobalCEFWorkScheduler := TCEFWorkScheduler.CreateDelayed;
  CEFContextInitEvent    := TSimpleEvent.Create;

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  GlobalCEFApp.BrowserSubprocessPath      := 'OSRExternalPumpBrowser_sp';
  GlobalCEFApp.BackgroundColor            := CefColorSetARGB($FF, $FF, $FF, $FF);  
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := @GlobalCEFApp_OnScheduleMessagePumpWork;

  // This is a workaround for the 'GPU is not usable error' issue :
  // https://bitbucket.org/chromiumembedded/cef/issues/2964/gpu-is-not-usable-error-during-cef
  GlobalCEFApp.DisableZygote := True; // this property adds the "--no-zygote" command line switch

  GlobalCEFApp.LogFile             := 'debug.log';
  GlobalCEFApp.LogSeverity         := LOGSEVERITY_INFO;

  if GlobalCEFApp.StartMainProcess then
    begin
      // Wait until the context is initialized
      CEFContextInitEvent.WaitFor(5000);
      // Now we can create the GlobalCEFWorkScheduler background thread
      GlobalCEFWorkScheduler.CreateThread;
    end;
end;

function GTKKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer) : GBoolean; cdecl;
var
  TempCefEvent : TCefKeyEvent;
begin
  GdkEventKeyToCEFKeyEvent(Event, TempCefEvent);

  if (Event^._type = GDK_KEY_PRESS) then
    begin
      TempCefEvent.kind := KEYEVENT_RAWKEYDOWN;
      Form1.SendCEFKeyEvent(TempCefEvent);
      TempCefEvent.kind := KEYEVENT_CHAR;
      Form1.SendCEFKeyEvent(TempCefEvent);
    end
   else
    begin
      TempCefEvent.kind := KEYEVENT_KEYUP;
      Form1.SendCEFKeyEvent(TempCefEvent);
    end;

  Result := True;
end;

procedure ConnectKeyPressReleaseEvents(const aWidget : PGtkWidget);
begin
  g_signal_connect(aWidget, 'key-press-event',   TGTKSignalFunc(@GTKKeyPress), nil);
  g_signal_connect(aWidget, 'key-release-event', TGTKSignalFunc(@GTKKeyPress), nil);
end;

{ TForm1 }

procedure TForm1.SendCEFKeyEvent(const aCefEvent : TCefKeyEvent);
begin
  Chromium1.SendKeyEvent(@aCefEvent);
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  Caption            := 'OSR External Pump Browser';
  AddressPnl.Enabled := True;

  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.AddressEdtEnter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
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

      // opaque white background color
      Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
      Chromium1.DefaultURL              := UTF8Decode(AddressEdt.Text);
      Chromium1.CreateBrowser;
    end;
end;

procedure TForm1.Panel1Enter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(True);
end;

procedure TForm1.Panel1Exit(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  Panel1.SetFocus;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);
end;

procedure TForm1.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
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

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;

function TForm1.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;       

function TForm1.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  Close;
end;

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean; var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1CursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor_: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; 
  var aResult : boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TForm1.Chromium1GetScreenInfo(Sender: TObject;
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

procedure TForm1.Chromium1GetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
  TempScale : single;
begin
  TempScale    := Panel1.ScreenScale;
  TempViewPt.x := LogicalToDevice(viewX, TempScale);
  TempViewPt.y := LogicalToDevice(viewY, TempScale);
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TForm1.Chromium1GetViewRect(Sender: TObject;
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

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1Paint(Sender: TObject; const browser: ICefBrowser;
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
  TempBitmap : TBitmap;
  TempSrcRect : TRect;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      begin
        if (type_ = PET_POPUP) then
          begin
            if (FPopUpBitmap = nil) then
               begin
                 FPopUpBitmap             := TBitmap.Create;
                 FPopUpBitmap.PixelFormat := pf32bit;
                 FPopUpBitmap.HandleType  := bmDIB;
                 FPopUpBitmap.Width       := aWidth;
                 FPopUpBitmap.Height      := aHeight;

                 FPopUpBitmap.Canvas.Brush.Color := clWhite;
                 FPopUpBitmap.Canvas.FillRect(rect(0, 0, aWidth, aHeight));
               end;

            if (aWidth  <> FPopUpBitmap.Width) or
               (aHeight <> FPopUpBitmap.Height) then
              begin
                FPopUpBitmap.Width  := aWidth;
                FPopUpBitmap.Height := aHeight;

                FPopUpBitmap.Canvas.Brush.Color := clWhite;
                FPopUpBitmap.Canvas.FillRect(rect(0, 0, aWidth, aHeight));
              end;

            TempBitmap := FPopUpBitmap;
            TempBitmap.BeginUpdate;

            TempWidth  := FPopUpBitmap.Width;
            TempHeight := FPopUpBitmap.Height;
          end
         else
          begin
            TempForcedResize := Panel1.UpdateBufferDimensions(aWidth, aHeight) or not(Panel1.BufferIsResized(False));

            TempBitmap := Panel1.Buffer;
            TempBitmap.BeginUpdate;

            TempWidth  := Panel1.BufferWidth;
            TempHeight := Panel1.BufferHeight;
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

        TempBitmap.EndUpdate;

        if FShowPopup and (FPopUpBitmap <> nil) then
          begin
            TempSrcRect := Rect(0, 0,
                                min(FPopUpRect.Right  - FPopUpRect.Left, FPopUpBitmap.Width),
                                min(FPopUpRect.Bottom - FPopUpRect.Top,  FPopUpBitmap.Height));

            Panel1.BufferDraw(FPopUpBitmap, TempSrcRect, FPopUpRect);
          end;

        Panel1.EndBufferDraw;
        Panel1.InvalidatePanel;

        if (type_ = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then
              TThread.Queue(nil, @DoResize);

            FResizing      := False;
            FPendingResize := False;
          end;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TForm1.Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
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

procedure TForm1.Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
begin
  LogicalToDevice(rect^, Panel1.ScreenScale);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TForm1.Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
begin
  Panel1.hint     := aText;
  Panel1.ShowHint := (length(aText) > 0);
  Result          := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin                             
  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FFirstLoad      := True;
  FResizeCS       := TCriticalSection.Create;

  ConnectKeyPressReleaseEvents(PGtkWidget(Panel1.Handle));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin                                              
  if (FPopUpBitmap        <> nil) then FreeAndNil(FPopUpBitmap);
  if (FResizeCS           <> nil) then FreeAndNil(FResizeCS);
  if (CEFContextInitEvent <> nil) then FreeAndNil(CEFContextInitEvent);
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
  Chromium1.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Chromium1.WasHidden(False);
  Chromium1.SendFocusEvent(True);
end;

procedure TForm1.GoBtnEnter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

procedure TForm1.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.DoResize;
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

procedure TForm1.WMMove(var Message: TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMSize(var Message: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

end.

