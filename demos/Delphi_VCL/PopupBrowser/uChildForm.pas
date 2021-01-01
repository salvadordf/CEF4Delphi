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

unit uChildForm;

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
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFBufferPanel,
  uCEFChromiumCore;

const
  CEF_SHOWCHILD = WM_APP + $A52;

type
  TChildForm = class(TForm)
    Chromium1: TChromium;
    Panel1: TBufferPanel;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1Paint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure Chromium1CursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure Chromium1GetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure Chromium1GetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure Chromium1GetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
    procedure Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);

   protected
    FPopUpBitmap       : TBitmap;
    FPopUpRect         : TRect;
    FShowPopUp         : boolean;
    FResizing          : boolean;
    FPendingResize     : boolean;
    FCanClose          : boolean;
    FClosing           : boolean;
    FClientInitialized : boolean;
    FResizeCS          : TCriticalSection;
    FPopupFeatures     : TCefPopupFeatures;

    FLastClickCount  : integer;
    FLastClickTime   : integer;
    FLastClickPoint  : TPoint;
    FLastClickButton : TMouseButton;

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;
    procedure InitializeLastClick;
    function  CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMCaptureChanged(var aMessage : TMessage); message WM_CAPTURECHANGED;
    procedure WMCancelMode(var aMessage : TMessage); message WM_CANCELMODE;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure PendingResizeMsg(var aMessage : TMessage); message CEF_PENDINGRESIZE;
    procedure ShowChildMsg(var aMessage : TMessage); message CEF_SHOWCHILD;

  public
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
    procedure ApplyPopupFeatures;
    procedure HandleSysCharMsg(var Msg: tagMSG; var Handled: Boolean);
    procedure HandleSysKeyDownMsg(var Msg: tagMSG; var Handled: Boolean);
    procedure HandleSysKeyUpMsg(var Msg: tagMSG; var Handled: Boolean);
    procedure HandleKeyDownMsg(var Msg: tagMSG; var Handled: Boolean);
    procedure HandleKeyUpMsg(var Msg: tagMSG; var Handled: Boolean);
    procedure HandleCharMsg(var Msg: tagMSG; var Handled: Boolean);
    procedure HandleMouseWheelMsg(var Msg: tagMSG; var Handled: Boolean);

    property  ClientInitialized : boolean   read FClientInitialized;
    property  Closing           : boolean   read FClosing;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI16_UP}
  System.Math,
  {$ELSE}
  Math,
  {$ENDIF}
  uCEFMiscFunctions, uCEFApplication, uMainForm;

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and calls chrmosr.CloseBrowser(True).
// 2- chrmosr.CloseBrowser(True) will trigger chrmosr.OnClose and we have to
//    set "Result" to false and CEF will destroy the internal browser immediately.
// 3- chrmosr.OnBeforeClose is triggered because the internal browser was destroyed.
//    Now we set FCanClose to True and send WM_CLOSE to the form.

procedure TChildForm.HandleSysCharMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent : TCefKeyEvent;
begin
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
      Chromium1.SendKeyEvent(@TempKeyEvent);
      Handled := True;
    end
   else
    Handled := False;
end;

procedure TChildForm.HandleSysKeyDownMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent : TCefKeyEvent;
begin
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

      Chromium1.SendKeyEvent(@TempKeyEvent);
      Handled := True;
    end
   else
    Handled := False;
end;

procedure TChildForm.HandleSysKeyUpMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent : TCefKeyEvent;
begin
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

      Chromium1.SendKeyEvent(@TempKeyEvent);
      Handled := True;
    end
   else
    Handled := False;
end;

procedure TChildForm.HandleKeyDownMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent : TCefKeyEvent;
begin
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

      Chromium1.SendKeyEvent(@TempKeyEvent);
      Handled := (Msg.wParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB]);
    end
   else
    Handled := False;
end;

procedure TChildForm.HandleKeyUpMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent : TCefKeyEvent;
begin
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

      Chromium1.SendKeyEvent(@TempKeyEvent);
      Handled := True;
    end
   else
    Handled := False;
end;

procedure TChildForm.HandleCharMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempKeyEvent : TCefKeyEvent;
begin
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
      Chromium1.SendKeyEvent(@TempKeyEvent);
      Handled := True;
    end
   else
    Handled := False;
end;

procedure TChildForm.HandleMouseWheelMsg(var Msg: tagMSG; var Handled: Boolean);
var
  TempMouseEvent : TCefMouseEvent;
begin
  if Panel1.Focused and (GlobalCEFApp <> nil) then
    begin
      TempMouseEvent.x         := Msg.lParam and $FFFF;
      TempMouseEvent.y         := Msg.lParam shr 16;
      TempMouseEvent.modifiers := GetCefMouseModifiers(Msg.wParam);
      DeviceToLogical(TempMouseEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseWheelEvent(@TempMouseEvent, 0, int16(Msg.wParam shr 16));
      Handled := False;
    end;
end;

function TChildForm.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                        var   client          : ICefClient;
                                        const targetFrameName : string;
                                        const popupFeatures   : TCefPopupFeatures) : boolean;
begin
  WindowInfoAsWindowless(windowInfo, 0, targetFrameName);
  FPopupFeatures     := popupFeatures;
  FClientInitialized := Chromium1.CreateClientHandler(client);
  Result             := FClientInitialized;
end;

procedure TChildForm.ApplyPopupFeatures;
begin
  if (FPopupFeatures.xset      <> 0) then Chromium1.SetFormLeftTo(FPopupFeatures.x);
  if (FPopupFeatures.yset      <> 0) then Chromium1.SetFormTopTo(FPopupFeatures.y);
  if (FPopupFeatures.widthset  <> 0) then Chromium1.ResizeFormWidthTo(FPopupFeatures.width);
  if (FPopupFeatures.heightset <> 0) then Chromium1.ResizeFormHeightTo(FPopupFeatures.height);
end;

procedure TChildForm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TChildForm.Chromium1BeforePopup(      Sender             : TObject;
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
  case targetDisposition of
    WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB,
    WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    WOD_NEW_POPUP : Result := not(TMainForm(Owner).CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

procedure TChildForm.Chromium1CursorChange(      Sender           : TObject;
                                           const browser          : ICefBrowser;
                                                 cursor           : HICON;
                                                 cursorType       : TCefCursorType;
                                           const customCursorInfo : PCefCursorInfo;
                                           var   aResult          : boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TChildForm.Chromium1GetScreenInfo(      Sender     : TObject;
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

procedure TChildForm.Chromium1GetScreenPoint(      Sender  : TObject;
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

procedure TChildForm.Chromium1GetViewRect(      Sender  : TObject;
                                          const browser : ICefBrowser;
                                          var   rect    : TCefRect);
begin
  if (GlobalCEFApp <> nil) then
    begin
      rect.x      := 0;
      rect.y      := 0;
      rect.width  := DeviceToLogical(Panel1.Width,  GlobalCEFApp.DeviceScaleFactor);
      rect.height := DeviceToLogical(Panel1.Height, GlobalCEFApp.DeviceScaleFactor);
    end;
end;

procedure TChildForm.Chromium1Paint(      Sender          : TObject;
                                    const browser         : ICefBrowser;
                                          type_           : TCefPaintElementType;
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
        if (type_ = PET_POPUP) then
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

        if (type_ = PET_VIEW) then
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

procedure TChildForm.Chromium1PopupShow(      Sender  : TObject;
                                        const browser : ICefBrowser;
                                              show    : Boolean);
begin
  if show then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      Chromium1.Invalidate(PET_VIEW);
    end;
end;

procedure TChildForm.Chromium1PopupSize(      Sender  : TObject;
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

procedure TChildForm.Chromium1TitleChange(      Sender  : TObject;
                                          const browser : ICefBrowser;
                                          const title   : ustring);
begin
  Caption := title;
end;

procedure TChildForm.Chromium1Tooltip(      Sender  : TObject;
                                      const browser : ICefBrowser;
                                      var   text    : ustring;
                                      out   Result  : Boolean);
begin
  Panel1.hint     := text;
  Panel1.ShowHint := (length(text) > 0);
  Result          := True;
end;

function TChildForm.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TChildForm.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TChildForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.WMCaptureChanged(var aMessage : TMessage);
begin
  inherited;

  Chromium1.SendCaptureLostEvent;
end;

procedure TChildForm.WMCancelMode(var aMessage : TMessage);
begin
  inherited;

  Chromium1.SendCaptureLostEvent;
end;

procedure TChildForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TChildForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TChildForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  if (GlobalCEFApp <> nil) then
    GlobalCEFApp.UpdateDeviceScaleFactor;

  Chromium1.NotifyScreenInfoChanged;
  Chromium1.WasResized;
end;

procedure TChildForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TChildForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;

      if Chromium1.Initialized then
        Chromium1.CloseBrowser(True)
       else
        CanClose := True;
    end;
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  FPopUpBitmap       := nil;
  FPopUpRect         := rect(0, 0, 0, 0);
  FShowPopUp         := False;
  FResizing          := False;
  FPendingResize     := False;
  FCanClose          := False;
  FClosing           := False;
  FClientInitialized := False;
  FResizeCS          := TCriticalSection.Create;

  InitializeLastClick;
end;

procedure TChildForm.FormDestroy(Sender: TObject);
begin
  Chromium1.ShutdownDragAndDrop;

  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);

  if FClientInitialized and TMainForm(Owner).HandleAllocated then
    PostMessage(TMainForm(Owner).Handle, CEF_CHILDDESTROYED, 0, 0);
end;

procedure TChildForm.FormHide(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
  Chromium1.WasHidden(True);
end;

procedure TChildForm.FormShow(Sender: TObject);
begin
  Chromium1.InitializeDragAndDrop(Panel1);
  Chromium1.WasHidden(False);
  Chromium1.SendFocusEvent(True);
end;

procedure TChildForm.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TChildForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) then
    begin
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
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
    end;
end;

procedure TChildForm.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) then
    begin
      GetCursorPos(TempPoint);
      TempPoint := Panel1.ScreenToclient(TempPoint);

      if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
      TempEvent.modifiers := GetCefMouseModifiers;
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseMoveEvent(@TempEvent, True);
    end;
end;

procedure TChildForm.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) then
    begin
      if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseMoveEvent(@TempEvent, False);
    end;
end;

procedure TChildForm.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) then
    begin
      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
    end;
end;

procedure TChildForm.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;

procedure TChildForm.PendingResizeMsg(var aMessage : TMessage);
begin
  DoResize;
end;

procedure TChildForm.ShowChildMsg(var aMessage : TMessage);
begin
  ApplyPopupFeatures;
  Show;
end;

procedure TChildForm.DoResize;
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

procedure TChildForm.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := mbLeft;
end;

function TChildForm.CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;
begin
  aCurrentTime := GetMessageTime;

  Result := (abs(FLastClickPoint.x - x) > (GetSystemMetrics(SM_CXDOUBLECLK) div 2)) or
            (abs(FLastClickPoint.y - y) > (GetSystemMetrics(SM_CYDOUBLECLK) div 2)) or
            (cardinal(aCurrentTime - FLastClickTime) > GetDoubleClickTime);
end;

procedure TChildForm.Panel1Enter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(True);
end;

procedure TChildForm.Panel1Exit(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

end.
