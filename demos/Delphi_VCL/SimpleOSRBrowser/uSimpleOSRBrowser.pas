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

unit uSimpleOSRBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.SyncObjs, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.AppEvnts, WinApi.imm, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, SyncObjs,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, AppEvnts, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFBufferPanel,
  uCEFChromiumCore;

const
  // Set this constant to True and load "file://transparency.html" to test a
  // transparent browser.
  TRANSPARENT_BROWSER = False;

type
  TForm1 = class(TForm)
    NavControlPnl: TPanel;
    chrmosr: TChromium;
    AppEvents: TApplicationEvents;
    AddressCb: TComboBox;
    Panel2: TPanel;
    GoBtn: TButton;
    SnapshotBtn: TButton;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Panel1: TBufferPanel;

    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1PaintParentBkg(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1IMECancelComposition(Sender: TObject);
    procedure Panel1IMECommitText(Sender: TObject; const aText: ustring; const replacement_range: PCefRange; relative_cursor_pos: Integer);
    procedure Panel1IMESetComposition(Sender: TObject; const aText: ustring; const underlines: TCefCompositionUnderlineDynArray; const replacement_range, selection_range: TCefRange);
    procedure Panel1CustomTouch(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
    procedure Panel1PointerDown(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
    procedure Panel1PointerUp(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
    procedure Panel1PointerUpdate(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrIMECompositionRangeChanged(Sender: TObject; const browser: ICefBrowser; const selected_range: PCefRange; character_boundsCount: NativeUInt; const character_bounds: PCefRect);

    procedure SnapshotBtnClick(Sender: TObject);
    procedure SnapshotBtnEnter(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);
    procedure AddressCbEnter(Sender: TObject);

  protected
    FPopUpBitmap     : TBitmap;
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FResizing        : boolean;
    FPendingResize   : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;
    FResizeCS        : TCriticalSection;
    FIMECS           : TCriticalSection;
    FDeviceBounds    : TCefRectDynArray;
    FSelectedRange   : TCefRange;
    FAtLeastWin8     : boolean;

    FLastClickCount  : integer;
    FLastClickTime   : integer;
    FLastClickPoint  : TPoint;
    FLastClickButton : TMouseButton;

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;
    procedure InitializeLastClick;
    function  CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;
    function  ArePointerEventsSupported : boolean;
    {$IFDEF DELPHI14_UP}
    function  HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
    function  HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean;
    function  HandlePointerEvent(var aMessage : TMessage) : boolean;
    {$ENDIF}

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMCaptureChanged(var aMessage : TMessage); message WM_CAPTURECHANGED;
    procedure WMCancelMode(var aMessage : TMessage); message WM_CANCELMODE;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure PendingResizeMsg(var aMessage : TMessage); message CEF_PENDINGRESIZE;
    procedure RangeChangedMsg(var aMessage : TMessage); message CEF_IMERANGECHANGED;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplication;

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

// **********************************************************************************************
// ************************************** ATTENTION! ********************************************
// **********************************************************************************************
// *                                                                                            *
// * If your Delphi/Lazarus version doesn't have full touch and pen support you will have       *
// * issues building this demo.                                                                 *
// *                                                                                            *
// * Some constants and types like POINTER_INPUT_TYPE, POINTER_PEN_INFO, etc. need to be        *
// * defined in order to test touch screens and pens on Windows.                                *
// *                                                                                            *
// * Try adding this unit made by Andreas Hausladen to the "uses" section if you get            *
// * "Undeclared identifier" errors :                                                           *
// * https://github.com/ahausladen/ObjectPascal-WinAPIs/blob/master/WinApi/WinApi.WMPointer.pas *
// *                                                                                            *
// **********************************************************************************************

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and
//    calls chrmosr.CloseBrowser(True).
// 2- chrmosr.CloseBrowser(True) will trigger chrmosr.OnClose and we have to
//    set "Result" to false and CEF will destroy the internal browser immediately.
// 3- chrmosr.OnBeforeClose is triggered because the internal browser was destroyed.
//    Sets FCanClose := True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  GlobalCEFApp.TouchEvents                := STATE_ENABLED;
  //GlobalCEFApp.EnableGPU                  := True;
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_VERBOSE;

  // If you need transparency leave the GlobalCEFApp.BackgroundColor property
  // with the default value or set the alpha channel to 0
  if TRANSPARENT_BROWSER then
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($00, $00, $00, $00)
   else
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
end;

procedure TForm1.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent   : TCefKeyEvent;
  TempMouseEvent : TCefMouseEvent;
begin
  case Msg.message of
    WM_SYSCHAR :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(True);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          CefCheckAltGrPressed(Msg.wParam, TempKeyEvent);
          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_SYSKEYDOWN :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(True);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_SYSKEYUP :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_KEYUP;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(True);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_KEYDOWN :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := (Msg.wParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB]);
        end;

    WM_KEYUP :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_KEYUP;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_CHAR :
      if Panel1.Focused then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempKeyEvent.windows_key_code        := Msg.wParam;
          TempKeyEvent.native_key_code         := Msg.lParam;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          CefCheckAltGrPressed(Msg.wParam, TempKeyEvent);
          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_MOUSEWHEEL :
      if Panel1.Focused then
        begin
          TempMouseEvent.x         := Msg.lParam and $FFFF;
          TempMouseEvent.y         := Msg.lParam shr 16;
          TempMouseEvent.modifiers := GetCefMouseModifiers(Msg.wParam);
          DeviceToLogical(TempMouseEvent, Panel1.ScreenScale);
          chrmosr.SendMouseWheelEvent(@TempMouseEvent, 0, smallint(Msg.wParam shr 16));
        end;
  end;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  chrmosr.LoadURL(AddressCb.Text);
end;

procedure TForm1.GoBtnEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TForm1.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TForm1.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.chrmosrBeforePopup(      Sender             : TObject;
                                    const browser            : ICefBrowser;
                                    const frame              : ICefFrame;
                                    const targetUrl          : ustring;
                                    const targetFrameName    : ustring;
                                          targetDisposition  : TCefWindowOpenDisposition;
                                          userGesture        : Boolean;
                                    const popupFeatures      : TCefPopupFeatures;
                                    var   windowInfo         : TCefWindowInfo;
                                    var   client             : ICefClient;
                                    var   settings           : TCefBrowserSettings;
                                    var   extra_info         : ICefDictionaryValue;
                                    var   noJavascriptAccess : Boolean;
                                    var   Result             : Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.chrmosrCursorChange(      Sender           : TObject;
                                     const browser          : ICefBrowser;
                                           cursor           : HICON;
                                           cursorType       : TCefCursorType;
                                     const customCursorInfo : PCefCursorInfo;
                                     var   aResult          : boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TForm1.chrmosrGetScreenInfo(      Sender     : TObject;
                                      const browser    : ICefBrowser;
                                      var   screenInfo : TCefScreenInfo;
                                      out   Result     : Boolean);
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

procedure TForm1.chrmosrGetScreenPoint(      Sender  : TObject;
                                       const browser : ICefBrowser;
                                             viewX   : Integer;
                                             viewY   : Integer;
                                       var   screenX : Integer;
                                       var   screenY : Integer;
                                       out   Result  : Boolean);
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

procedure TForm1.chrmosrGetViewRect(      Sender  : TObject;
                                    const browser : ICefBrowser;
                                    var   rect    : TCefRect);
var
  TempScale : single;
begin
  TempScale   := Panel1.ScreenScale;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  rect.height := DeviceToLogical(Panel1.Height, TempScale);
end;

procedure TForm1.chrmosrPaint(      Sender          : TObject;
                              const browser         : ICefBrowser;
                                    kind            : TCefPaintElementType;
                                    dirtyRectsCount : NativeUInt;
                              const dirtyRects      : PCefRectArray;
                              const buffer          : Pointer;
                                    width           : Integer;
                                    height          : Integer);
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride, DstStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight, TempScanlineSize : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempSrcRect : TRect;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      begin
        if (kind = PET_POPUP) then
          begin
            if (FPopUpBitmap = nil) or
               (width  <> FPopUpBitmap.Width) or
               (height <> FPopUpBitmap.Height) then
              begin
                if (FPopUpBitmap <> nil) then FPopUpBitmap.Free;

                FPopUpBitmap             := TBitmap.Create;
                FPopUpBitmap.PixelFormat := pf32bit;
                FPopUpBitmap.HandleType  := bmDIB;
                FPopUpBitmap.Width       := width;
                FPopUpBitmap.Height      := height;
              end;

            TempWidth        := FPopUpBitmap.Width;
            TempHeight       := FPopUpBitmap.Height;
            TempScanlineSize := FPopUpBitmap.Width * SizeOf(TRGBQuad);
            TempBufferBits   := FPopUpBitmap.Scanline[pred(FPopUpBitmap.Height)];
          end
         else
          begin
            TempForcedResize := Panel1.UpdateBufferDimensions(Width, Height) or not(Panel1.BufferIsResized(False));
            TempWidth        := Panel1.BufferWidth;
            TempHeight       := Panel1.BufferHeight;
            TempScanlineSize := Panel1.ScanlineSize;
            TempBufferBits   := Panel1.BufferBits;
          end;

        if (TempBufferBits <> nil) then
          begin
            SrcStride := Width * SizeOf(TRGBQuad);
            DstStride := - TempScanlineSize;

            n := 0;

            while (n < dirtyRectsCount) do
              begin
                if (dirtyRects[n].x >= 0) and (dirtyRects[n].y >= 0) then
                  begin
                    TempLineSize := min(dirtyRects[n].width, TempWidth - dirtyRects[n].x) * SizeOf(TRGBQuad);

                    if (TempLineSize > 0) then
                      begin
                        TempSrcOffset := ((dirtyRects[n].y * Width) + dirtyRects[n].x) * SizeOf(TRGBQuad);
                        TempDstOffset := ((TempScanlineSize * pred(TempHeight)) - (dirtyRects[n].y * TempScanlineSize)) +
                                         (dirtyRects[n].x * SizeOf(TRGBQuad));

                        src := @PByte(buffer)[TempSrcOffset];
                        dst := @PByte(TempBufferBits)[TempDstOffset];

                        i := 0;
                        j := min(dirtyRects[n].height, TempHeight - dirtyRects[n].y);

                        while (i < j) do
                          begin
                            Move(src^, dst^, TempLineSize);

                            Inc(dst, DstStride);
                            Inc(src, SrcStride);
                            inc(i);
                          end;
                      end;
                  end;

                inc(n);
              end;

            if FShowPopup and (FPopUpBitmap <> nil) then
              begin
                TempSrcRect := Rect(0, 0,
                                    min(FPopUpRect.Right  - FPopUpRect.Left, FPopUpBitmap.Width),
                                    min(FPopUpRect.Bottom - FPopUpRect.Top,  FPopUpBitmap.Height));

                Panel1.BufferDraw(FPopUpBitmap, TempSrcRect, FPopUpRect);
              end;
          end;

        Panel1.EndBufferDraw;
        Panel1.InvalidatePanel;

        if (kind = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then PostMessage(Handle, CEF_PENDINGRESIZE, 0, 0);

            FResizing      := False;
            FPendingResize := False;
          end;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TForm1.chrmosrPopupShow(      Sender  : TObject;
                                  const browser : ICefBrowser;
                                        show    : Boolean);
begin
  if show then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      if (chrmosr <> nil) then chrmosr.Invalidate(PET_VIEW);
    end;
end;

procedure TForm1.chrmosrPopupSize(      Sender  : TObject;
                                  const browser : ICefBrowser;
                                  const rect    : PCefRect);
begin
  LogicalToDevice(rect^, Panel1.ScreenScale);

  FPopUpRect.Left   := rect.x;
  FPopUpRect.Top    := rect.y;
  FPopUpRect.Right  := rect.x + rect.width  - 1;
  FPopUpRect.Bottom := rect.y + rect.height - 1;
end;

procedure TForm1.chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
begin
  Panel1.hint     := text;
  Panel1.ShowHint := (length(text) > 0);
  Result          := True;
end;

procedure TForm1.AddressCbEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
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
    mbRight  : Result := MBT_RIGHT;
    mbMiddle : Result := MBT_MIDDLE;
    else       Result := MBT_LEFT;
  end;
end;

procedure TForm1.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMCaptureChanged(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TForm1.WMCancelMode(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption               := 'Simple OSR Browser';
  NavControlPnl.Enabled := True;
end;

procedure TForm1.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  if (GlobalCEFApp <> nil) then
    GlobalCEFApp.UpdateDeviceScaleFactor;

  if (chrmosr <> nil) then
    begin
      chrmosr.NotifyScreenInfoChanged;
      chrmosr.WasResized;
    end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      chrmosr.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  TempMajorVer, TempMinorVer : DWORD;
begin
  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FDeviceBounds   := nil;

  FAtLeastWin8    := GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and
                     ((TempMajorVer > 6) or
                      ((TempMajorVer = 6) and (TempMinorVer >= 2)));

  FSelectedRange.from   := 0;
  FSelectedRange.to_    := 0;

  FResizeCS       := TCriticalSection.Create;
  FIMECS          := TCriticalSection.Create;

  Panel1.Transparent := TRANSPARENT_BROWSER;

  InitializeLastClick;

  chrmosr.DefaultURL := trim(AddressCb.Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  chrmosr.ShutdownDragAndDrop;

  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);
  if (FResizeCS    <> nil) then FreeAndNil(FResizeCS);
  if (FIMECS       <> nil) then FreeAndNil(FIMECS);

  if (FDeviceBounds <> nil) then
    begin
      Finalize(FDeviceBounds);
      FDeviceBounds := nil;
    end;
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
  chrmosr.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if chrmosr.Initialized then
    begin
      chrmosr.WasHidden(False);
      chrmosr.SendFocusEvent(True);
    end
   else
    begin
      // If you need transparency leave the chrmosr.Options.BackgroundColor property
      // with the default value or set the alpha channel to 0
      if TRANSPARENT_BROWSER then
        chrmosr.Options.BackgroundColor := CefColorSetARGB($00, $00, $00, $00)
       else
        chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

      // The IME handler needs to be created when Panel1 has a valid handle
      // and before the browser creation.
      // You can skip this if the user doesn't need an "Input Method Editor".
      Panel1.CreateIMEHandler;

      {$IFDEF DELPHI14_UP}
      if not(ArePointerEventsSupported) then
        RegisterTouchWindow(Panel1.Handle, 0);
      {$ENDIF}

      if chrmosr.CreateBrowser(nil, '') then
        chrmosr.InitializeDragAndDrop(Panel1)
       else
        Timer1.Enabled := True;
    end;
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TForm1.Panel1CustomTouch(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
{$IFDEF DELPHI14_UP}
var
  TempScale       : single;
  TempTouchEvent  : TCefTouchEvent;
  TempHTOUCHINPUT : HTOUCHINPUT;
  TempNumPoints   : integer;
  i               : integer;
  TempTouchInputs : array of TTouchInput;
  TempPoint       : TPoint;
  TempLParam      : LPARAM;
  TempResult      : LRESULT;
{$ENDIF}
begin
{$IFDEF DELPHI14_UP}
  if not(Panel1.Focused) or (GlobalCEFApp = nil) then exit;

  TempNumPoints := LOWORD(aMessage.wParam);

  // Chromium only supports upto 16 touch points.
  if (TempNumPoints < 1) or (TempNumPoints > 16) then exit;

  SetLength(TempTouchInputs, TempNumPoints);
  TempHTOUCHINPUT := HTOUCHINPUT(aMessage.lParam);
  TempScale       := Panel1.ScreenScale;

  if GetTouchInputInfo(TempHTOUCHINPUT, TempNumPoints, @TempTouchInputs[0], SizeOf(TTouchInput)) then
    begin
      i := 0;
      while (i < TempNumPoints) do
        begin
          TempPoint := TouchPointToPoint(Panel1.Handle, TempTouchInputs[i]);

          if not(FAtLeastWin8) then
            begin
              // Windows 7 sends touch events for touches in the non-client area,
              // whereas Windows 8 does not. In order to unify the behaviour, always
              // ignore touch events in the non-client area.

              TempLParam := MakeLParam(TempPoint.x, TempPoint.y);
              TempResult := SendMessage(Panel1.Handle, WM_NCHITTEST, 0, TempLParam);

              if (TempResult <> HTCLIENT) then
                begin
                  SetLength(TempTouchInputs, 0);
                  exit;
                end;
            end;

          TempPoint        := Panel1.ScreenToClient(TempPoint);
          TempTouchEvent.x := DeviceToLogical(TempPoint.x, TempScale);
          TempTouchEvent.y := DeviceToLogical(TempPoint.y, TempScale);

          // Touch point identifier stays consistent in a touch contact sequence
          TempTouchEvent.id := TempTouchInputs[i].dwID;

          if      ((TempTouchInputs[i].dwFlags and TOUCHEVENTF_DOWN) <> 0) then TempTouchEvent.type_ := CEF_TET_PRESSED
          else if ((TempTouchInputs[i].dwFlags and TOUCHEVENTF_MOVE) <> 0) then TempTouchEvent.type_ := CEF_TET_MOVED
          else if ((TempTouchInputs[i].dwFlags and TOUCHEVENTF_UP)   <> 0) then TempTouchEvent.type_ := CEF_TET_RELEASED;

          TempTouchEvent.radius_x       := 0;
          TempTouchEvent.radius_y       := 0;
          TempTouchEvent.rotation_angle := 0;
          TempTouchEvent.pressure       := 0;
          TempTouchEvent.modifiers      := EVENTFLAG_NONE;
          TempTouchEvent.pointer_type   := CEF_POINTER_TYPE_TOUCH;

          chrmosr.SendTouchEvent(@TempTouchEvent);

          inc(i);
        end;

      CloseTouchInputHandle(TempHTOUCHINPUT);
      aHandled := True;
    end;

  SetLength(TempTouchInputs, 0);
{$ENDIF}
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  {$IFDEF DELPHI14_UP}
  if (ssTouch in Shift) then exit;
  {$ENDIF}

  Panel1.SetFocus;

  if not(CancelPreviousClick(x, y, TempTime)) and (Button = FLastClickButton) then
    inc(FLastClickCount)
   else
    begin
      FLastClickPoint.x := x;
      FLastClickPoint.y := y;
      FLastClickCount   := 1;
    end;

  FLastClickTime      := TempTime;
  FLastClickButton    := Button;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
end;

procedure TForm1.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
  TempTime  : integer;
begin
  GetCursorPos(TempPoint);
  TempPoint := Panel1.ScreenToclient(TempPoint);

  if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := GetCefMouseModifiers;
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseMoveEvent(@TempEvent, True);
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  {$IFDEF DELPHI14_UP}
  if (ssTouch in Shift) then exit;
  {$ENDIF}

  if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  {$IFDEF DELPHI14_UP}
  if (ssTouch in Shift) then exit;
  {$ENDIF}

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
end;

procedure TForm1.Panel1PaintParentBkg(Sender: TObject);
begin
  // This event should only be used if you enabled transparency in the browser
  if TRANSPARENT_BROWSER then
    begin
      // This event should copy the background image into Panel1.Canvas
      // The TBufferPanel uses "AlphaBlend" to draw the browser contents over
      // this background image.
      // For simplicity, we just paint it green.
      Panel1.Canvas.Brush.Color := clGreen;
      Panel1.Canvas.Brush.Style := bsSolid;
      Panel1.Canvas.FillRect(Rect(0, 0, Panel1.Width, Panel1.Height));
    end;
end;

procedure TForm1.Panel1PointerDown(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
begin
  {$IFDEF DELPHI14_UP}
  aHandled := Panel1.Focused and
              (GlobalCEFApp <> nil) and
              ArePointerEventsSupported and
              HandlePointerEvent(aMessage);
  {$ELSE}
  aHandled := False;
  {$ENDIF}
end;

procedure TForm1.Panel1PointerUp(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
begin
  {$IFDEF DELPHI14_UP}
  aHandled := Panel1.Focused and
              (GlobalCEFApp <> nil) and
              ArePointerEventsSupported and
              HandlePointerEvent(aMessage);
  {$ELSE}
  aHandled := False;
  {$ENDIF}
end;

procedure TForm1.Panel1PointerUpdate(Sender: TObject; var aMessage: TMessage; var aHandled: Boolean);
begin
  {$IFDEF DELPHI14_UP}
  aHandled := Panel1.Focused and
              (GlobalCEFApp <> nil) and
              ArePointerEventsSupported and
              HandlePointerEvent(aMessage);
  {$ELSE}
  aHandled := False;
  {$ENDIF}
end;

{$IFDEF DELPHI14_UP}
function TForm1.HandlePointerEvent(var aMessage : TMessage) : boolean;
const
  PT_TOUCH = 2;
  PT_PEN   = 3;
var
  TempID   : uint32;
  TempType : POINTER_INPUT_TYPE;
begin
  Result := False;
  TempID := LoWord(aMessage.wParam);

  if GetPointerType(TempID, @TempType) then
    case TempType of
      PT_PEN   : Result := HandlePenEvent(TempID, aMessage.Msg);
      PT_TOUCH : Result := HandleTouchEvent(TempID, aMessage.Msg);
    end;
end;

function TForm1.HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
var
  TempPenInfo    : POINTER_PEN_INFO;
  TempTouchEvent : TCefTouchEvent;
  TempPoint      : TPoint;
  TempScale      : single;
begin
  Result := False;

  if not(GetPointerPenInfo(aID, @TempPenInfo)) then exit;

  TempTouchEvent.id        := aID;
  TempTouchEvent.x         := 0;
  TempTouchEvent.y         := 0;
  TempTouchEvent.radius_x  := 0;
  TempTouchEvent.radius_y  := 0;
  TempTouchEvent.type_     := CEF_TET_RELEASED;
  TempTouchEvent.modifiers := EVENTFLAG_NONE;

  if ((TempPenInfo.penFlags and PEN_FLAG_ERASER) <> 0) then
    TempTouchEvent.pointer_type := CEF_POINTER_TYPE_ERASER
   else
    TempTouchEvent.pointer_type := CEF_POINTER_TYPE_PEN;

  if ((TempPenInfo.penMask and PEN_MASK_PRESSURE) <> 0) then
    TempTouchEvent.pressure := TempPenInfo.pressure / 1024
   else
    TempTouchEvent.pressure := 0;

  if ((TempPenInfo.penMask and PEN_MASK_ROTATION) <> 0) then
    TempTouchEvent.rotation_angle := TempPenInfo.rotation / 180 * Pi
   else
    TempTouchEvent.rotation_angle := 0;

  Result := True;

  case aMsg of
    WM_POINTERDOWN :
      TempTouchEvent.type_ := CEF_TET_PRESSED;

    WM_POINTERUPDATE :
      if ((TempPenInfo.pointerInfo.pointerFlags and POINTER_FLAG_INCONTACT) <> 0) then
        TempTouchEvent.type_ := CEF_TET_MOVED
       else
        exit; // Ignore hover events.

    WM_POINTERUP :
      TempTouchEvent.type_ := CEF_TET_RELEASED;
  end;

  if ((TempPenInfo.pointerInfo.pointerFlags and POINTER_FLAG_CANCELED) <> 0) then
    TempTouchEvent.type_ := CEF_TET_CANCELLED;

  TempScale        := Panel1.ScreenScale;
  TempPoint        := Panel1.ScreenToClient(TempPenInfo.pointerInfo.ptPixelLocation);
  TempTouchEvent.x := DeviceToLogical(TempPoint.x, TempScale);
  TempTouchEvent.y := DeviceToLogical(TempPoint.y, TempScale);

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;

function TForm1.HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean;
var
  TempTouchInfo  : POINTER_TOUCH_INFO;
  TempTouchEvent : TCefTouchEvent;
  TempPoint      : TPoint;
  TempScale      : single;
begin
  Result := False;

  if not(GetPointerTouchInfo(aID, @TempTouchInfo)) then exit;

  TempTouchEvent.id             := aID;
  TempTouchEvent.x              := 0;
  TempTouchEvent.y              := 0;
  TempTouchEvent.radius_x       := 0;
  TempTouchEvent.radius_y       := 0;
  TempTouchEvent.rotation_angle := 0;
  TempTouchEvent.pressure       := 0;
  TempTouchEvent.type_          := CEF_TET_RELEASED;
  TempTouchEvent.modifiers      := EVENTFLAG_NONE;
  TempTouchEvent.pointer_type   := CEF_POINTER_TYPE_TOUCH;

  Result := True;

  case aMsg of
    WM_POINTERDOWN :
      TempTouchEvent.type_ := CEF_TET_PRESSED;

    WM_POINTERUPDATE :
      if ((TempTouchInfo.pointerInfo.pointerFlags and POINTER_FLAG_INCONTACT) <> 0) then
        TempTouchEvent.type_ := CEF_TET_MOVED
       else
        exit; // Ignore hover events.

    WM_POINTERUP :
      TempTouchEvent.type_ := CEF_TET_RELEASED;
  end;

  if ((TempTouchInfo.pointerInfo.pointerFlags and POINTER_FLAG_CANCELED) <> 0) then
    TempTouchEvent.type_ := CEF_TET_CANCELLED;

  TempScale        := Panel1.ScreenScale;
  TempPoint        := Panel1.ScreenToClient(TempTouchInfo.pointerInfo.ptPixelLocation);
  TempTouchEvent.x := DeviceToLogical(TempPoint.x, TempScale);
  TempTouchEvent.y := DeviceToLogical(TempPoint.y, TempScale);

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;
{$ENDIF}

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;

procedure TForm1.PendingResizeMsg(var aMessage : TMessage);
begin
  DoResize;
end;

procedure TForm1.RangeChangedMsg(var aMessage : TMessage);
begin
  try
    FIMECS.Acquire;
    Panel1.ChangeCompositionRange(FSelectedRange, FDeviceBounds);
  finally
    FIMECS.Release;
  end;
end;

procedure TForm1.DoResize;
begin
  try
    FResizeCS.Acquire;

    if FResizing then
      FPendingResize := True
     else
      if Panel1.BufferIsResized then
        chrmosr.Invalidate(PET_VIEW)
       else
        begin
          FResizing := True;
          chrmosr.WasResized;
        end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TForm1.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := mbLeft;
end;

function TForm1.CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;
begin
  aCurrentTime := GetMessageTime;

  Result := (abs(FLastClickPoint.x - x) > (GetSystemMetrics(SM_CXDOUBLECLK) div 2)) or
            (abs(FLastClickPoint.y - y) > (GetSystemMetrics(SM_CYDOUBLECLK) div 2)) or
            (cardinal(aCurrentTime - FLastClickTime) > GetDoubleClickTime);
end;

function TForm1.ArePointerEventsSupported : boolean;
begin
  {$IFDEF DELPHI14_UP}
  Result := FAtLeastWin8 and
            (@GetPointerType      <> nil) and
            (@GetPointerTouchInfo <> nil) and
            (@GetPointerPenInfo   <> nil);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TForm1.Panel1Enter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(True);
end;

procedure TForm1.Panel1Exit(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TForm1.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.SnapshotBtnEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if chrmosr.CreateBrowser(nil, '') then
    chrmosr.InitializeDragAndDrop(Panel1)
   else
    if not(chrmosr.Initialized) then Timer1.Enabled := True;
end;

procedure TForm1.chrmosrIMECompositionRangeChanged(      Sender                : TObject;
                                                   const browser               : ICefBrowser;
                                                   const selected_range        : PCefRange;
                                                         character_boundsCount : NativeUInt;
                                                   const character_bounds      : PCefRect);
var
  TempPRect : PCefRect;
  i         : NativeUInt;
  TempScale : single;
begin
  try
    FIMECS.Acquire;

    // TChromium.OnIMECompositionRangeChanged is triggered in a different thread
    // and all functions using a IMM context need to be executed in the same
    // thread, in this case the main thread. We need to save the parameters and
    // send a message to the form to execute Panel1.ChangeCompositionRange in
    // the main thread.

    if (FDeviceBounds <> nil) then
      begin
        Finalize(FDeviceBounds);
        FDeviceBounds := nil;
      end;

    FSelectedRange := selected_range^;

    if (character_boundsCount > 0) then
      begin
        SetLength(FDeviceBounds, character_boundsCount);

        i         := 0;
        TempPRect := character_bounds;
        TempScale := Panel1.ScreenScale;

        while (i < character_boundsCount) do
          begin
            FDeviceBounds[i] := TempPRect^;
            LogicalToDevice(FDeviceBounds[i], TempScale);

            inc(TempPRect);
            inc(i);
          end;
      end;

    PostMessage(Handle, CEF_IMERANGECHANGED, 0, 0);
  finally
    FIMECS.Release;
  end;
end;

procedure TForm1.Panel1IMECancelComposition(Sender: TObject);
begin
  chrmosr.IMECancelComposition;
end;

procedure TForm1.Panel1IMECommitText(      Sender              : TObject;
                                     const aText               : ustring;
                                     const replacement_range   : PCefRange;
                                           relative_cursor_pos : Integer);
begin
  chrmosr.IMECommitText(aText, replacement_range, relative_cursor_pos);
end;

procedure TForm1.Panel1IMESetComposition(      Sender            : TObject;
                                         const aText             : ustring;
                                         const underlines        : TCefCompositionUnderlineDynArray;
                                         const replacement_range : TCefRange;
                                         const selection_range   : TCefRange);
begin
  chrmosr.IMESetComposition(aText, underlines, @replacement_range, @selection_range);
end;

end.
