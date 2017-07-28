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

unit uSimpleOSRBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.AppEvnts,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, AppEvnts,
  {$ENDIF}
  GR32_Image, // You need the Graphics32 components for this demo available at http://graphics32.org
  uCEFChromium, uCEFTypes, uCEFInterfaces;

const
  MINIBROWSER_CREATED       = WM_APP + $100;

type
  TForm1 = class(TForm)
    NavControlPnl: TPanel;
    chrmosr: TChromium;
    AppEvents: TApplicationEvents;
    Panel1: TPanel;    // This is just a quick and dirty hack to receive some events that the PaintBox can't receive.
    PaintBox: TPaintBox32;
    ComboBox1: TComboBox;
    Panel2: TPanel;
    GoBtn: TButton;

    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);

    procedure GoBtnClick(Sender: TObject);

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);

    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseLeave(Sender: TObject);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect; out Result: Boolean);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);

  private
    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMCaptureChanged(var aMessage : TMessage); message WM_CAPTURECHANGED;
    procedure WMCancelMode(var aMessage : TMessage); message WM_CANCELMODE;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message MINIBROWSER_CREATED;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uCEFMiscFunctions, uCEFConstants, uCEFApplication;

procedure TForm1.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  TempEvent : TCefKeyEvent;
begin
  case Msg.message of
    WM_SYSCHAR :
      if (Panel1.Focused or chrmosr.FrameIsFocused) and
         (Msg.wParam in [VK_BACK..VK_HELP]) then
        begin
          TempEvent.kind                    := KEYEVENT_CHAR;
          TempEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempEvent.windows_key_code        := Msg.wParam;
          TempEvent.native_key_code         := Msg.lParam;
          TempEvent.is_system_key           := ord(True);
          TempEvent.character               := #0;
          TempEvent.unmodified_character    := #0;
          TempEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempEvent);
          Handled := True;
        end;

    WM_SYSKEYDOWN :
      if (Panel1.Focused or chrmosr.FrameIsFocused) and
         (Msg.wParam in [VK_BACK..VK_HELP]) then
        begin
          TempEvent.kind                    := KEYEVENT_RAWKEYDOWN;
          TempEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempEvent.windows_key_code        := Msg.wParam;
          TempEvent.native_key_code         := Msg.lParam;
          TempEvent.is_system_key           := ord(True);
          TempEvent.character               := #0;
          TempEvent.unmodified_character    := #0;
          TempEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempEvent);
          Handled := True;
        end;

    WM_SYSKEYUP :
      if (Panel1.Focused or chrmosr.FrameIsFocused) and
         (Msg.wParam in [VK_BACK..VK_HELP]) then
        begin
          TempEvent.kind                    := KEYEVENT_KEYUP;
          TempEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempEvent.windows_key_code        := Msg.wParam;
          TempEvent.native_key_code         := Msg.lParam;
          TempEvent.is_system_key           := ord(True);
          TempEvent.character               := #0;
          TempEvent.unmodified_character    := #0;
          TempEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempEvent);
          Handled := True;
        end;

    WM_KEYDOWN :
      if (Panel1.Focused or chrmosr.FrameIsFocused) and
         (Msg.wParam in [VK_BACK..VK_HELP]) then
        begin
          TempEvent.kind                    := KEYEVENT_RAWKEYDOWN;
          TempEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempEvent.windows_key_code        := Msg.wParam;
          TempEvent.native_key_code         := Msg.lParam;
          TempEvent.is_system_key           := ord(False);
          TempEvent.character               := #0;
          TempEvent.unmodified_character    := #0;
          TempEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempEvent);
          Handled := True;
        end;

    WM_KEYUP :
      if (Panel1.Focused or chrmosr.FrameIsFocused) and
         (Msg.wParam in [VK_BACK..VK_HELP]) then
        begin
          TempEvent.kind                    := KEYEVENT_KEYUP;
          TempEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempEvent.windows_key_code        := Msg.wParam;
          TempEvent.native_key_code         := Msg.lParam;
          TempEvent.is_system_key           := ord(False);
          TempEvent.character               := #0;
          TempEvent.unmodified_character    := #0;
          TempEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempEvent);
          Handled := True;
        end;

    WM_CHAR :
      if Panel1.Focused or chrmosr.FrameIsFocused then
        begin
          TempEvent.kind                    := KEYEVENT_CHAR;
          TempEvent.modifiers               := GetCefKeyboardModifiers(Msg.wParam, Msg.lParam);
          TempEvent.windows_key_code        := Msg.wParam;
          TempEvent.native_key_code         := Msg.lParam;
          TempEvent.is_system_key           := ord(False);
          TempEvent.character               := #0;
          TempEvent.unmodified_character    := #0;
          TempEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempEvent);
          Handled := True;
        end;
  end;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  chrmosr.LoadURL(ComboBox1.Text);
end;

procedure TForm1.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, MINIBROWSER_CREATED, 0, 0);
end;

procedure TForm1.chrmosrCursorChange(Sender : TObject;
                                     const browser          : ICefBrowser;
                                           cursor           : HICON;
                                           cursorType       : TCefCursorType;
                                     const customCursorInfo : PCefCursorInfo);
begin
  PaintBox.Cursor := GefCursorToWindowsCursor(cursorType);
end;

procedure TForm1.chrmosrGetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo;
  out Result: Boolean);
var
  TempRect : TCEFRect;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempRect.x      := 0;
      TempRect.y      := 0;
      TempRect.width  := DeviceToLogical(PaintBox.Width,  GlobalCEFApp.DeviceScaleFactor);
      TempRect.height := DeviceToLogical(PaintBox.Height, GlobalCEFApp.DeviceScaleFactor);

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

procedure TForm1.chrmosrGetScreenPoint(Sender : TObject;
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
      TempScreenPt := PaintBox.ClientToScreen(TempViewPt);
      screenX      := TempScreenPt.x;
      screenY      := TempScreenPt.y;
      Result       := True;
    end
   else
    Result := False;
end;

procedure TForm1.chrmosrGetViewRect(Sender : TObject;
                                    const browser : ICefBrowser;
                                    var   rect    : TCefRect;
                                    out   Result  : Boolean);
begin
  if (GlobalCEFApp <> nil) then
    begin
      rect.x      := 0;
      rect.y      := 0;
      rect.width  := DeviceToLogical(PaintBox.Width,  GlobalCEFApp.DeviceScaleFactor);
      rect.height := DeviceToLogical(PaintBox.Height, GlobalCEFApp.DeviceScaleFactor);
      Result      := True;
    end
   else
    Result := False;
end;

procedure TForm1.chrmosrPaint(Sender : TObject;
                              const browser         : ICefBrowser;
                                    kind            : TCefPaintElementType;
                                    dirtyRectsCount : NativeUInt;
                              const dirtyRects      : PCefRectArray;
                              const buffer          : Pointer;
                                    width           : Integer;
                                    height          : Integer);
var
  src, dst: PByte;
  offset, i, j, w: Integer;
begin
  if (width <> PaintBox.Width) or (height <> PaintBox.Height) then Exit;

  // ====================
  // === WARNING !!!! ===
  // ====================
  // This is a simple and basic function that copies the buffer passed from
  // CEF into the PaintBox canvas. If you have a high DPI monitor you may
  // have rounding problems resulting in a black screen.
  // CEF and this demo use a device_scale_factor to calculate screen logical
  // and real sizes. If there's a rounding error CEF and this demo will have
  // slightly different sizes and this function will exit.
  // If you need to support high DPI, you'll have to use a better function
  // to copy the buffer.

  with PaintBox.Buffer do
    begin
      PaintBox.Canvas.Lock;
      Lock;
      try
        for j := 0 to dirtyRectsCount - 1 do
        begin
          w := Width * 4;
          offset := ((dirtyRects[j].y * Width) + dirtyRects[j].x) * 4;
          src := @PByte(buffer)[offset];
          dst := @PByte(Bits)[offset];
          offset := dirtyRects[j].width * 4;
          for i := 0 to dirtyRects[j].height - 1 do
          begin
            Move(src^, dst^, offset);
            Inc(dst, w);
            Inc(src, w);
          end;
          PaintBox.Flush(Rect(dirtyRects[j].x, dirtyRects[j].y,
            dirtyRects[j].x + dirtyRects[j].width,  dirtyRects[j].y + dirtyRects[j].height));
        end;
      finally
        Unlock;
        PaintBox.Canvas.Unlock;
      end;
    end;
end;

procedure TForm1.chrmosrPopupShow(Sender : TObject;
                                  const browser : ICefBrowser;
                                        show : Boolean);
begin
  // TO DO : Needed to draw the "select" items
end;

procedure TForm1.chrmosrPopupSize(Sender : TObject;
                                  const browser : ICefBrowser;
                                  const rect    : PCefRect);
begin
  // TO DO : Needed to draw the "select" items
  // The rect also needs to be converted.
  // LogicalToDevice(rect, GlobalCEFApp.DeviceScaleFactor);
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

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TForm1.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  if (chrmosr <> nil) then
    begin
      chrmosr.NotifyScreenInfoChanged;
      chrmosr.WasResized;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF); // opaque white background color
  chrmosr.CreateBrowser(nil, '');
  chrmosr.InitializeDragAndDrop(PaintBox);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  chrmosr.ShutdownDragAndDrop;
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
  chrmosr.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  chrmosr.WasHidden(False);
  chrmosr.SendFocusEvent(True);
end;

procedure TForm1.PaintBoxClick(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TForm1.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
    end;
end;

procedure TForm1.PaintBoxMouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  if (GlobalCEFApp <> nil) then
    begin
      GetCursorPos(TempPoint);
      TempPoint           := PaintBox.ScreenToclient(TempPoint);
      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
      TempEvent.modifiers := GetCefMouseModifiers;
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseMoveEvent(@TempEvent, not PaintBox.MouseInControl);
    end;
end;

procedure TForm1.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseMoveEvent(@TempEvent, not PaintBox.MouseInControl);
    end;
end;

procedure TForm1.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TForm1.PaintBoxMouseWheel(Sender      : TObject;
                                    Shift       : TShiftState;
                                    WheelDelta  : Integer;
                                    MousePos    : TPoint;
                                    var Handled : Boolean);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempEvent.x         := MousePos.X;
      TempEvent.y         := MousePos.Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      chrmosr.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
    end;
end;

procedure TForm1.PaintBoxResize(Sender: TObject);
begin
  PaintBox.Buffer.SetSize(PaintBox.Width, PaintBox.Height);
  chrmosr.WasResized;
end;

procedure TForm1.Panel1Enter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(True);
end;

procedure TForm1.Panel1Exit(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

end.
