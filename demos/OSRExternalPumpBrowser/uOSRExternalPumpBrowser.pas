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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.SyncObjs, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.AppEvnts,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, SyncObjs,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, AppEvnts,
  {$ENDIF}
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uBufferPanel, uCEFWorkScheduler;

type
  TOSRExternalPumpBrowserFrm = class(TForm)
    NavControlPnl: TPanel;
    chrmosr: TChromium;
    AppEvents: TApplicationEvents;
    ComboBox1: TComboBox;
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
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseLeave(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect; out Result: Boolean);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
    procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);

    procedure SnapshotBtnClick(Sender: TObject);
    procedure SnapshotBtnEnter(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);

  protected
    FPopUpBitmap    : TBitmap;
    FPopUpRect      : TRect;
    FShowPopUp      : boolean;
    FResizing       : boolean;
    FPendingResize  : boolean;
    FCanClose       : boolean;
    FClosing        : boolean;
    FResizeCS       : TCriticalSection;

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMCaptureChanged(var aMessage : TMessage); message WM_CAPTURECHANGED;
    procedure WMCancelMode(var aMessage : TMessage); message WM_CANCELMODE;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure PendingResizeMsg(var aMessage : TMessage); message CEF_PENDINGRESIZE;

  public
    { Public declarations }
  end;

var
  OSRExternalPumpBrowserFrm : TOSRExternalPumpBrowserFrm;
  GlobalCEFWorkScheduler    : TCEFWorkScheduler = nil;

// This is a simple browser in OSR mode (off-screen rendering).
// It was necessary to destroy the browser following the destruction sequence described in
// the MDIBrowser demo but in OSR mode there are some modifications.

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and calls chrmosr.CloseBrowser(True).
// 2- chrmosr.CloseBrowser(True) will trigger chrmosr.OnClose and we have to
//    set "Result" to false and CEF3 will destroy the internal browser immediately.
// 3- chrmosr.OnBeforeClose is triggered because the internal browser was destroyed.
//    Now we set FCanClose to True and send WM_CLOSE to the form.

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplication;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure TOSRExternalPumpBrowserFrm.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent   : TCefKeyEvent;
  TempMouseEvent : TCefMouseEvent;
begin
  case Msg.message of
    WM_SYSCHAR :
      if Panel1.Focused and (Msg.wParam in [VK_BACK..VK_HELP]) then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
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

    WM_SYSKEYDOWN :
      if Panel1.Focused and (Msg.wParam in [VK_BACK..VK_HELP]) then
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
      if Panel1.Focused and (Msg.wParam in [VK_BACK..VK_HELP]) then
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
      if Panel1.Focused and (Msg.wParam in [VK_BACK..VK_HELP]) then
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
          Handled := True;
        end;

    WM_KEYUP :
      if Panel1.Focused and (Msg.wParam in [VK_BACK..VK_HELP]) then
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

          chrmosr.SendKeyEvent(@TempKeyEvent);
          Handled := True;
        end;

    WM_MOUSEWHEEL :
      if Panel1.Focused and (GlobalCEFApp <> nil) then
        begin
          TempMouseEvent.x         := Msg.lParam and $FFFF;
          TempMouseEvent.y         := Msg.lParam shr 16;
          TempMouseEvent.modifiers := GetCefMouseModifiers(Msg.wParam);
          DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);
          chrmosr.SendMouseWheelEvent(@TempMouseEvent, 0, int16(Msg.wParam shr 16));
        end;
  end;
end;

procedure TOSRExternalPumpBrowserFrm.GoBtnClick(Sender: TObject);
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  chrmosr.LoadURL(ComboBox1.Text);
end;

procedure TOSRExternalPumpBrowserFrm.GoBtnEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  Result := False;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrCursorChange(Sender : TObject;
                                                         const browser          : ICefBrowser;
                                                               cursor           : HICON;
                                                               cursorType       : TCefCursorType;
                                                         const customCursorInfo : PCefCursorInfo);
begin
  Panel1.Cursor := GefCursorToWindowsCursor(cursorType);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrGetScreenInfo(Sender : TObject;
                                                          const browser    : ICefBrowser;
                                                          var   screenInfo : TCefScreenInfo;
                                                          out   Result     : Boolean);
var
  TempRect : TCEFRect;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := DeviceToLogical(Panel1.Width,  GlobalCEFApp.DeviceScaleFactor);
      TempRect.height := DeviceToLogical(Panel1.Height, GlobalCEFApp.DeviceScaleFactor);

      screenInfo.device_scale_factor := GlobalCEFApp.DeviceScaleFactor;
      screenInfo.depth               := 0;
      screenInfo.depth_per_component := 0;
      screenInfo.is_monochrome       := Ord(False);
      screenInfo.rect                := TempRect;
      screenInfo.available_rect      := TempRect;

      Result := True;
    end
   else
    Result := False;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrGetScreenPoint(Sender : TObject;
                                                           const browser : ICefBrowser;
                                                                 viewX   : Integer;
                                                                 viewY   : Integer;
                                                           var   screenX : Integer;
                                                           var   screenY : Integer;
                                                           out   Result  : Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempViewPt.x := LogicalToDevice(viewX, GlobalCEFApp.DeviceScaleFactor);
      TempViewPt.y := LogicalToDevice(viewY, GlobalCEFApp.DeviceScaleFactor);
      TempScreenPt := Panel1.ClientToScreen(TempViewPt);
      screenX      := TempScreenPt.x;
      screenY      := TempScreenPt.y;
      Result       := True;
    end
   else
    Result := False;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrGetViewRect(Sender : TObject;
                                                        const browser : ICefBrowser;
                                                        var   rect    : TCefRect;
                                                        out   Result  : Boolean);
begin
  if (GlobalCEFApp <> nil) then
    begin
      rect.x      := 0;
      rect.y      := 0;
      rect.width  := DeviceToLogical(Panel1.Width,  GlobalCEFApp.DeviceScaleFactor);
      rect.height := DeviceToLogical(Panel1.Height, GlobalCEFApp.DeviceScaleFactor);
      Result      := True;
    end
   else
    Result := False;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrPaint(Sender : TObject;
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
              Panel1.BufferDraw(FPopUpRect.Left, FPopUpRect.Top, FPopUpBitmap);
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

procedure TOSRExternalPumpBrowserFrm.chrmosrPopupShow(Sender : TObject;
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

procedure TOSRExternalPumpBrowserFrm.chrmosrPopupSize(Sender : TObject;
                                                      const browser : ICefBrowser;
                                                      const rect    : PCefRect);
begin
  if (GlobalCEFApp <> nil) then
    begin
      LogicalToDevice(rect^, GlobalCEFApp.DeviceScaleFactor);

      FPopUpRect.Left   := rect.x;
      FPopUpRect.Top    := rect.y;
      FPopUpRect.Right  := rect.x + rect.width  - 1;
      FPopUpRect.Bottom := rect.y + rect.height - 1;
    end;
end;

procedure TOSRExternalPumpBrowserFrm.ComboBox1Enter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

function TOSRExternalPumpBrowserFrm.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TOSRExternalPumpBrowserFrm.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TOSRExternalPumpBrowserFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TOSRExternalPumpBrowserFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TOSRExternalPumpBrowserFrm.WMCaptureChanged(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TOSRExternalPumpBrowserFrm.WMCancelMode(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TOSRExternalPumpBrowserFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption               := 'OSR External Pump Browser';
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TOSRExternalPumpBrowserFrm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  if (chrmosr <> nil) then
    begin
      chrmosr.NotifyScreenInfoChanged;
      chrmosr.WasResized;
    end;
end;

procedure TOSRExternalPumpBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing              := True;
      Visible               := False;
      NavControlPnl.Enabled := False;
      chrmosr.CloseBrowser(True);
    end;
end;

procedure TOSRExternalPumpBrowserFrm.FormCreate(Sender: TObject);
begin
  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FResizeCS       := TCriticalSection.Create;
end;

procedure TOSRExternalPumpBrowserFrm.FormDestroy(Sender: TObject);
begin
  chrmosr.ShutdownDragAndDrop;

  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);
end;

procedure TOSRExternalPumpBrowserFrm.FormHide(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
  chrmosr.WasHidden(True);
end;

procedure TOSRExternalPumpBrowserFrm.FormShow(Sender: TObject);
begin
  if chrmosr.Initialized then
    begin
      chrmosr.WasHidden(False);
      chrmosr.SendFocusEvent(True);
    end
   else
    begin
      // opaque white background color
      chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

      if chrmosr.CreateBrowser(nil, '') then
        chrmosr.InitializeDragAndDrop(Panel1)
       else
        Timer1.Enabled := True;
    end;
end;

procedure TOSRExternalPumpBrowserFrm.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TOSRExternalPumpBrowserFrm.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      Panel1.SetFocus;

      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
    end;
end;

procedure TOSRExternalPumpBrowserFrm.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  if (GlobalCEFApp <> nil) then
    begin
      GetCursorPos(TempPoint);
      TempPoint           := Panel1.ScreenToclient(TempPoint);
      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
      TempEvent.modifiers := GetCefMouseModifiers;
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseMoveEvent(@TempEvent, True);
    end;
end;

procedure TOSRExternalPumpBrowserFrm.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseMoveEvent(@TempEvent, False);
    end;
end;

procedure TOSRExternalPumpBrowserFrm.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);
    end;
end;

procedure TOSRExternalPumpBrowserFrm.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;

procedure TOSRExternalPumpBrowserFrm.PendingResizeMsg(var aMessage : TMessage);
begin
  DoResize;
end;

procedure TOSRExternalPumpBrowserFrm.DoResize;
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

procedure TOSRExternalPumpBrowserFrm.Panel1Enter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(True);
end;

procedure TOSRExternalPumpBrowserFrm.Panel1Exit(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TOSRExternalPumpBrowserFrm.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TOSRExternalPumpBrowserFrm.SnapshotBtnEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TOSRExternalPumpBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if chrmosr.CreateBrowser(nil, '') then
    chrmosr.InitializeDragAndDrop(Panel1)
   else
    if not(chrmosr.Initialized) then Timer1.Enabled := True;
end;

end.
