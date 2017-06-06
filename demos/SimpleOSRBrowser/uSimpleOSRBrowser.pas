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

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    PaintBox: TPaintBox32;
    chrmosr: TChromium;
    AppEvents: TApplicationEvents;
    procedure FormShow(Sender: TObject);
    procedure PaintBoxResize(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser;
      kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
      const dirtyRects: PCefRectArray; const buffer: Pointer; width,
      height: Integer);
    procedure chrmosrGetRootScreenRect(Sender: TObject;
      const browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
    procedure chrmosrCursorChange(Sender: TObject;
      const browser: ICefBrowser; cursor: HICON;
      cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure chrmosrGetViewRect(Sender: TObject;
      const browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
    procedure chrmosrGetScreenPoint(Sender: TObject;
      const browser: ICefBrowser; viewX, viewY: Integer; screenX,
      screenY: PInteger; out Result: Boolean);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser;
      show: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser;
      const rect: PCefRect);
  private
    function getModifiers(Shift: TShiftState): TCefEventFlags;
    function GetButton(Button: TMouseButton): TCefMouseButtonType;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uCEFMiscFunctions;

procedure TForm1.AppEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  event: TCefKeyEvent;
begin
  case Msg.message of
    WM_CHAR:
     begin
       FillChar(event, SizeOf(TCefKeyEvent), 0);
       event.kind := KEYEVENT_CHAR;
       event.windows_key_code := Msg.wParam;
       event.native_key_code := Msg.lParam;
       chrmosr.SendKeyEvent(@event);
     end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  chrmosr.LoadURL(Edit1.Text);
end;

procedure TForm1.chrmosrCursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo);
begin
  case cursorType of
    CT_POINTER: PaintBox.Cursor := crArrow;
    CT_CROSS: PaintBox.Cursor:= crCross;
    CT_HAND: PaintBox.Cursor := crHandPoint;
    CT_IBEAM: PaintBox.Cursor := crIBeam;
    CT_WAIT: PaintBox.Cursor := crHourGlass;
    CT_HELP: PaintBox.Cursor := crHelp;
    CT_EASTRESIZE: PaintBox.Cursor := crSizeWE;
    CT_NORTHRESIZE: PaintBox.Cursor := crSizeNS;
    CT_NORTHEASTRESIZE: PaintBox.Cursor:= crSizeNESW;
    CT_NORTHWESTRESIZE: PaintBox.Cursor:= crSizeNWSE;
    CT_SOUTHRESIZE: PaintBox.Cursor:= crSizeNS;
    CT_SOUTHEASTRESIZE: PaintBox.Cursor:= crSizeNWSE;
    CT_SOUTHWESTRESIZE: PaintBox.Cursor:= crSizeNESW;
    CT_WESTRESIZE: PaintBox.Cursor := crSizeWE;
    CT_NORTHSOUTHRESIZE: PaintBox.Cursor:= crSizeNS;
    CT_EASTWESTRESIZE: PaintBox.Cursor := crSizeWE;
    CT_NORTHEASTSOUTHWESTRESIZE: PaintBox.Cursor:= crSizeNESW;
    CT_NORTHWESTSOUTHEASTRESIZE: PaintBox.Cursor:= crSizeNWSE;
    CT_COLUMNRESIZE: PaintBox.Cursor:= crHSplit;
    CT_ROWRESIZE: PaintBox.Cursor:= crVSplit;
    CT_MOVE: PaintBox.Cursor := crSizeAll;
    CT_PROGRESS: PaintBox.Cursor := crAppStart;
    CT_NODROP: PaintBox.Cursor:= crNo;
    CT_NONE: PaintBox.Cursor:= crNone;
    CT_NOTALLOWED: PaintBox.Cursor:= crNo;
  else
    PaintBox.Cursor := crArrow;
  end;
end;

procedure TForm1.chrmosrGetRootScreenRect(Sender: TObject;
  const browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
begin
  rect.x := 0;
  rect.y := 0;
  rect.width := PaintBox.Width;
  rect.height := PaintBox.Height;
  Result := True;
end;

procedure TForm1.chrmosrGetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; screenX,
  screenY: PInteger; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
begin
  TempViewPt.x := viewX;
  TempViewPt.y := viewY;
  TempScreenPt := PaintBox.ClientToScreen(TempViewPt);
  screenX^     := TempScreenPt.x;
  screenY^     := TempScreenPt.y;
  Result       := True;
end;

procedure TForm1.chrmosrGetViewRect(Sender: TObject;
  const browser: ICefBrowser; rect: PCefRect; out Result: Boolean);
begin
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := PaintBox.Width;
  rect.height := PaintBox.Height;
  Result      := True;
end;

procedure TForm1.chrmosrPaint(Sender: TObject; const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; width,
  height: Integer);
var
  src, dst: PByte;
  offset, i, j, w: Integer;
begin
  if (width <> PaintBox.Width) or (height <> PaintBox.Height) then Exit;

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

procedure TForm1.chrmosrPopupShow(Sender: TObject;
  const browser: ICefBrowser; show: Boolean);
begin
  // TO DO : Needed to draw the "select" items
end;

procedure TForm1.chrmosrPopupSize(Sender: TObject;
  const browser: ICefBrowser; const rect: PCefRect);
begin
  // TO DO : Needed to draw the "select" items
end;

function TForm1.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := [];
  if ssShift in Shift then Include(Result, EVENTFLAG_SHIFT_DOWN);
  if ssAlt in Shift then Include(Result, EVENTFLAG_ALT_DOWN);
  if ssCtrl in Shift then Include(Result, EVENTFLAG_CONTROL_DOWN);
  if ssLeft in Shift then Include(Result, EVENTFLAG_LEFT_MOUSE_BUTTON);
  if ssRight in Shift then Include(Result, EVENTFLAG_RIGHT_MOUSE_BUTTON);
  if ssMiddle in Shift then Include(Result, EVENTFLAG_MIDDLE_MOUSE_BUTTON);
end;

function TForm1.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight: Result := MBT_RIGHT;
    TMouseButton.mbMiddle: Result := MBT_MIDDLE;
    else  Result := MBT_LEFT;
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

procedure TForm1.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  if (chrmosr <> nil) then
    begin
      chrmosr.NotifyScreenInfoChanged;
      chrmosr.WasResized;
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF); // opaque white background color
  chrmosr.CreateBrowser(nil, '');
end;

procedure TForm1.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  event: TCefMouseEvent;
begin
  event.x := X;
  event.y := Y;
  event.modifiers := getModifiers(Shift);
  chrmosr.SendMouseClickEvent(@event, GetButton(Button), False, 1);
end;

procedure TForm1.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  event: TCefMouseEvent;
begin
  event.x := X;
  event.y := Y;
  event.modifiers := getModifiers(Shift);
  chrmosr.SendMouseMoveEvent(@event, not PaintBox.MouseInControl);
end;

procedure TForm1.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  event: TCefMouseEvent;
begin
  event.x := X;
  event.y := Y;
  event.modifiers := getModifiers(Shift);
  chrmosr.SendMouseClickEvent(@event, GetButton(Button), True, 1);
end;

procedure TForm1.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  event: TCefMouseEvent;
begin
  event.x := MousePos.X;
  event.y := MousePos.Y;
  event.modifiers := getModifiers(Shift);
  chrmosr.SendMouseWheelEvent(@event, 0, WheelDelta);
end;

procedure TForm1.PaintBoxResize(Sender: TObject);
begin
  PaintBox.Buffer.SetSize(PaintBox.Width, PaintBox.Height);
  chrmosr.WasResized;
  chrmosr.SendFocusEvent(True);
end;

end.
