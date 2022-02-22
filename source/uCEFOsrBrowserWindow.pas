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
//        Copyright © 2022 Salvador Diaz Fau. All rights reserved.
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

(*

  === State of Implementation ===

  On MacOS the keyboard support is currently incomplete


*)

unit uCEFOsrBrowserWindow;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$i cef.inc}

interface

uses
  {$IFDEF DARWIN}  // $IFDEF MACOSX
  uCEFLazarusCocoa,
  {$ENDIF}
  {$IFDEF FPC}
  LResources, PropEdits,
  {$ENDIF}
  uCEFApplication, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces, uCEFChromium,
  uCEFLinkedWinControlBase, uCEFBufferPanel,
  uCEFBrowserWindow, uCEFBitmapBitBuffer, uCEFMiscFunctions,
  uCEFConstants, uCEFChromiumEvents, Forms, ExtCtrls, LCLType, Graphics,
  Controls, syncobjs, Classes, sysutils, math;

type

  TBrowserMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer;
    var AHandled: Boolean) of Object;
  TBrowserMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer;
    var AHandled: Boolean) of Object;
  TBrowserMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint;
    var AHandled: Boolean) of Object;
  TBrowserKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState; var AHandled: Boolean) of Object;
  //TBrowserKeyPressEvent = procedure(Sender: TObject; var Key: char; var AHandled: Boolean) of Object;
  TBrowserUTF8KeyPressEvent = procedure(Sender: TObject; var UTF8Key: TUTF8Char; var AHandled: Boolean) of Object;

  (* TEmbeddedOsrChromium

     Hides (THiddenPropertyEditor) any published event that is used by TOsrBrowserWindow
     * Hidden events must also not be used by user code *
  *)

  TEmbeddedOsrChromium = class(TEmbeddedChromium)
  end;

  { TOsrBrowserWindow - Off-Screen-Rendering

    A simple "drop on the Form" component for an full embedded browser.

    See notes an TBrowserWindow for requirements in user code.
    Further:
    - Some keystrokes may not be sent to KeyDown/KeyPress by the LCL.
      They may be available as WM_SYSKEYDOWN/UP message on the containing Form.


    This component is still experimental.
    - On MacOS Keyboard support is not complete

  }

  TOsrBrowserWindow = class(TBufferPanel)
    private
      FPopUpBitmap     : TBitmap;
      FPopUpRect       : TRect;
      FShowPopUp       : boolean;
      FResizing        : boolean;
      FPendingResize   : boolean;
      FResizeCS        : syncobjs.TCriticalSection;

      //FIMECS           : TCriticalSection;
      FDeviceBounds    : TCefRectDynArray;
      FSelectedRange   : TCefRange;

      FLastKeyDown: Word;

      procedure AsyncInvalidate(Data: PtrInt);
      procedure AsyncResize(Data: PtrInt);
      procedure SyncIMERangeChanged;

      procedure DoGetChromiumBeforePopup(Sender: TObject;
        const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
        targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
        userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
        var windowInfo: TCefWindowInfo; var client: ICefClient;
        var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
        var noJavascriptAccess: Boolean; var Result: Boolean);
      procedure DoGetChromiumPopupShow(Sender: TObject;
        const browser: ICefBrowser; AShow: Boolean);
      procedure DoGetChromiumPopupSize(Sender: TObject;
        const browser: ICefBrowser; const rect: PCefRect);
      procedure DoGetChromiumTooltip(Sender: TObject;
        const browser: ICefBrowser; var AText: ustring; out Result: Boolean);
      procedure DoGetChromiumIMECompositionRangeChanged(Sender: TObject;
        const browser: ICefBrowser; const selected_range: PCefRange;
        character_boundsCount: NativeUInt; const character_bounds: PCefRect);
      procedure DoGetChromiumCursorChange(Sender: TObject;
        const browser: ICefBrowser; cursor_: TCefCursorHandle;
        cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo;
        var aResult: boolean);
      procedure DoGetChromiumGetScreenInfo(Sender: TObject;
        const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
        Result: Boolean);
      procedure DoGetChromiumGetScreenPoint(Sender: TObject;
        const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
        screenY: Integer; out Result: Boolean);
      procedure DoGetChromiumViewRect(Sender: TObject;
        const browser: ICefBrowser; var rect: TCefRect);
      procedure DoChromiumPaint(Sender: TObject; const browser: ICefBrowser;
        kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
        const dirtyRects: PCefRectArray; const ABuffer: Pointer; AWidth,
        AHeight: Integer);

    private
      FChromium  : TEmbeddedOsrChromium;

      FOnBrowserClosed  : TNotifyEvent;
      FOnBrowserCreated : TNotifyEvent;
      FOnKeyDown: TBrowserKeyEvent;
      FOnKeyUp: TBrowserKeyEvent;
      FOnMouseDown: TBrowserMouseEvent;
      FOnMouseMove: TBrowserMouseMoveEvent;
      FOnMouseUp: TBrowserMouseEvent;
      FOnMouseWheel: TBrowserMouseWheelEvent;
      FOnUtf8KeyPress: TBrowserUTF8KeyPressEvent;

      procedure DoCreateBrowserAfterContext(Sender: TObject);

    protected
      function    GetChromium: TEmbeddedOsrChromium;
      function    getModifiers(Shift: TShiftState): TCefEventFlags;
      function    getKeyModifiers(Shift: TShiftState): TCefEventFlags;
      function    GetButton(Button: TMouseButton): TCefMouseButtonType;
      procedure   DestroyHandle; override;
      procedure   RealizeBounds; override;

      procedure   DoEnter; override;
      procedure   DoExit; override;
      procedure   Click; override;
      procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure   MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure   MouseEnter; override;
      procedure   MouseLeave; override;
      function    DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

      (* Key input works only for windows.
      *)
      procedure   KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure   UTF8KeyPress(var UTF8Key: TUTF8Char); override;
      procedure   KeyUp(var Key: Word; Shift: TShiftState); override;
      {$IFDEF MSWINDOWS}
      procedure DoOnIMECancelComposition; override;
      procedure DoOnIMECommitText(const aText : ustring; const replacement_range : PCefRange; relative_cursor_pos : integer); override;
      procedure DoOnIMESetComposition(const aText : ustring; const underlines : TCefCompositionUnderlineDynArray; const replacement_range, selection_range : TCefRange); override;
      {$ENDIF}
      procedure CaptureChanged; override;

      procedure   DoOnCreated(Sender: TObject);
      procedure   DoOnClosed(Sender: TObject);
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   CreateHandle; override;

      procedure   CloseBrowser(aForceClose: boolean);
      procedure   WaitForBrowserClosed;
      function    IsClosed: boolean;
      procedure   LoadURL(aURL: ustring);
    //
    published
      property    Chromium : TEmbeddedOsrChromium    read GetChromium;

      property    OnBrowserCreated : TNotifyEvent read FOnBrowserCreated write FOnBrowserCreated;
      property    OnBrowserClosed  : TNotifyEvent read FOnBrowserClosed write FOnBrowserClosed;

      (* Mouse/Key events
         The below events can be used to see mouse/key input before it is sent to CEF.
         All events have a "AHandled" parameter, which can be used to prevent the event
         from being sent to CEF.
      *)
      property    OnMouseDown:    TBrowserMouseEvent      read FOnMouseDown write FOnMouseDown;
      property    OnMouseUp:      TBrowserMouseEvent      read FOnMouseUp write FOnMouseUp;
      property    OnMouseMove:    TBrowserMouseMoveEvent  read FOnMouseMove write FOnMouseMove;
      property    OnMouseWheel:   TBrowserMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
      property    OnKeyDown:      TBrowserKeyEvent        read FOnKeyDown write FOnKeyDown;
      property    OnKeyUp:        TBrowserKeyEvent        read FOnKeyUp write FOnKeyUp;
      property    OnUtf8KeyPress: TBrowserUTF8KeyPressEvent read FOnUtf8KeyPress write FOnUtf8KeyPress;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}


implementation

{ TOsrBrowserWindow }

procedure TOsrBrowserWindow.AsyncInvalidate(Data: PtrInt);
begin
  Invalidate;
end;

procedure TOsrBrowserWindow.AsyncResize(Data: PtrInt);
begin
  try
    FResizeCS.Acquire;

    if FResizing then
      FPendingResize := True
     else
      if BufferIsResized then
        Chromium.Invalidate(PET_VIEW)
       else
        begin
          FResizing := True;
          Chromium.WasResized;
        end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TOsrBrowserWindow.SyncIMERangeChanged;
begin
  ChangeCompositionRange(FSelectedRange, FDeviceBounds);
end;

procedure TOsrBrowserWindow.DoGetChromiumBeforePopup(Sender: TObject;
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

procedure TOsrBrowserWindow.DoGetChromiumPopupShow(Sender: TObject;
  const browser: ICefBrowser; AShow: Boolean);
begin
  if aShow then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      if (Chromium <> nil) then Chromium.Invalidate(PET_VIEW);
    end;
end;

procedure TOsrBrowserWindow.DoGetChromiumPopupSize(Sender: TObject;
  const browser: ICefBrowser; const rect: PCefRect);
begin
  LogicalToDevice(rect^, ScreenScale);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TOsrBrowserWindow.DoGetChromiumTooltip(Sender: TObject;
  const browser: ICefBrowser; var AText: ustring; out Result: Boolean);
begin
  hint     := aText;
  ShowHint := (length(aText) > 0);
  Result   := True;
end;

procedure TOsrBrowserWindow.DoGetChromiumIMECompositionRangeChanged(
  Sender: TObject; const browser: ICefBrowser; const selected_range: PCefRange;
  character_boundsCount: NativeUInt; const character_bounds: PCefRect);
var
  TempPRect : PCefRect;
  i         : NativeUInt;
  TempScale : single;
begin
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
      TempScale := ScreenScale;

      while (i < character_boundsCount) do
        begin
          FDeviceBounds[i] := TempPRect^;
          LogicalToDevice(FDeviceBounds[i], TempScale);

          inc(TempPRect);
          inc(i);
        end;
    end;

  TThread.Synchronize(nil, @SyncIMERangeChanged);
end;

procedure TOsrBrowserWindow.DoGetChromiumCursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor_: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo;
  var aResult: boolean);
begin
  Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TOsrBrowserWindow.DoGetChromiumGetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
  Result: Boolean);
var
  TempRect  : TCEFRect;
  TempScale : single;
begin
  TempScale       := ScreenScale;
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := DeviceToLogical(Width,  TempScale);
  TempRect.height := DeviceToLogical(Height, TempScale);

  screenInfo.device_scale_factor := TempScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TOsrBrowserWindow.DoGetChromiumGetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
  TempScale : single;
begin
  TempScale    := ScreenScale;
  TempViewPt.x := LogicalToDevice(viewX, TempScale);
  TempViewPt.y := LogicalToDevice(viewY, TempScale);
  TempScreenPt := ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TOsrBrowserWindow.DoGetChromiumViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
var
  TempScale : single;
begin
  TempScale   := ScreenScale;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(Width,  TempScale);
  rect.height := DeviceToLogical(Height, TempScale);
end;

procedure TOsrBrowserWindow.DoChromiumPaint(Sender: TObject;
  const browser: ICefBrowser; kind: TCefPaintElementType;
  dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
  const ABuffer: Pointer; AWidth, AHeight: Integer);
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight: integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempBitmap : TBitmap;
  TempSrcRect : TRect;
  {$IFDEF DARWIN}
  s: PByte;
  ls: integer;
  {$ENDIF}
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if BeginBufferDraw then
      begin
        if (kind = PET_POPUP) then
          begin
            if (FPopUpBitmap = nil) or
               (aWidth  <> FPopUpBitmap.Width) or
               (aHeight <> FPopUpBitmap.Height) then
              begin
                if (FPopUpBitmap <> nil) then FPopUpBitmap.Free;

                FPopUpBitmap             := TBitmap.Create;
                FPopUpBitmap.PixelFormat := pf32bit;
                FPopUpBitmap.HandleType  := bmDIB;
                FPopUpBitmap.Width       := aWidth;
                FPopUpBitmap.Height      := aHeight;
              end;

            TempBitmap := FPopUpBitmap;
            TempBitmap.BeginUpdate;

            TempWidth  := FPopUpBitmap.Width;
            TempHeight := FPopUpBitmap.Height;
          end
         else
          begin
            TempForcedResize := UpdateBufferDimensions(aWidth, aHeight) or not(BufferIsResized(False));

            TempBitmap := Buffer;
            TempBitmap.BeginUpdate;

            TempWidth  := BufferWidth;
            TempHeight := BufferHeight;
          end;

        SrcStride := aWidth * SizeOf(TRGBQuad);
        n         := 0;

        while (n < dirtyRectsCount) do
          begin
            if (dirtyRects^[n].x >= 0) and (dirtyRects^[n].y >= 0) then
              begin
                TempLineSize := min(dirtyRects^[n].width, TempWidth - dirtyRects^[n].x) {$IFnDEF DARWIN}* SizeOf(TRGBQuad){$ENDIF};

                if (TempLineSize > 0) then
                  begin
                    TempSrcOffset := ((dirtyRects^[n].y * aWidth) + dirtyRects^[n].x) * SizeOf(TRGBQuad);
                    TempDstOffset := (dirtyRects^[n].x * SizeOf(TRGBQuad));

                    src := @PByte(ABuffer)[TempSrcOffset];

                    i := 0;
                    j := min(dirtyRects^[n].height, TempHeight - dirtyRects^[n].y);

                    while (i < j) do
                      begin
                        TempBufferBits := TempBitmap.Scanline[dirtyRects^[n].y + i];
                        dst            := @PByte(TempBufferBits)[TempDstOffset];

                        {$IFDEF DARWIN}
                        ls := TempLineSize;
                        s  := src;
                        while ls > 0 do begin
                          PCardinal(dst)^ := (s[0] shl 24) or (s[1] shl 16) or (s[2] shl 8) or s[3];
                          inc(dst, 4);
                          inc(s, 4);
                          dec(ls);
                        end;
                        {$ELSE}
                        Move(src^, dst^, TempLineSize);
                        {$ENDIF}


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

            BufferDraw(FPopUpBitmap, TempSrcRect, FPopUpRect);
          end;

        EndBufferDraw;

        if HandleAllocated then
          //PostMessage(Handle, CEF_PENDINGINVALIDATE, 0, 0);
          Application.QueueAsyncCall(@AsyncInvalidate, 0);

        if (kind = PET_VIEW) then
          begin
            if (TempForcedResize or FPendingResize) and
               HandleAllocated then
              Application.QueueAsyncCall(@AsyncResize, 0);
              //PostMessage(Handle, CEF_PENDINGRESIZE, 0, 0);

            FResizing      := False;
            FPendingResize := False;
          end;
      end;
  finally
    FResizeCS.Release;
  end;
end;

function TOsrBrowserWindow.GetChromium: TEmbeddedOsrChromium;
begin
  Result := FChromium;
end;

function TOsrBrowserWindow.getModifiers(Shift: TShiftState
  ): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssMeta   in Shift) then Result := Result or EVENTFLAG_COMMAND_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TOsrBrowserWindow.getKeyModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssMeta   in Shift) then Result := Result or EVENTFLAG_COMMAND_DOWN;
  if (ssNum    in Shift) then Result := Result or EVENTFLAG_NUM_LOCK_ON;
  if (ssCaps   in Shift) then Result := Result or EVENTFLAG_CAPS_LOCK_ON;
end;

function TOsrBrowserWindow.GetButton(Button: TMouseButton
  ): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TOsrBrowserWindow.DoCreateBrowserAfterContext(Sender: TObject);
begin
  FChromium.CreateBrowser(nil);
end;

procedure TOsrBrowserWindow.CreateHandle;
begin
  inherited CreateHandle;
  if not (csDesigning in ComponentState) then begin
    GlobalCEFApp.AddContextInitializedHandler(@DoCreateBrowserAfterContext);
  end;
end;

procedure TOsrBrowserWindow.DestroyHandle;
begin
  if (GlobalCEFApp = nil) or
     (not FChromium.HasBrowser) or
     (csDesigning in ComponentState)
  then begin
    inherited DestroyHandle;
    exit;
  end;

  FChromium.CloseBrowser(True);
  inherited DestroyHandle;
end;

procedure TOsrBrowserWindow.RealizeBounds;
begin
  inherited RealizeBounds;
  Chromium.NotifyMoveOrResizeStarted;
  AsyncResize(0);
end;

procedure TOsrBrowserWindow.DoEnter;
begin
  inherited DoEnter;
  Chromium.SetFocus(True);
end;

procedure TOsrBrowserWindow.DoExit;
begin
  inherited DoExit;
  Chromium.SetFocus(False);
end;

procedure TOsrBrowserWindow.Click;
begin
  inherited Click;
  SetFocus;
end;

procedure TOsrBrowserWindow.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  LastClickCount: integer;
  IsHandled: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  IsHandled := False;
  if FOnMouseDown <> nil then
    FOnMouseDown(Self, Button, Shift, X, Y, IsHandled);
  if IsHandled then
    exit;

  SetFocus;

  LastClickCount := 1;
  if ssDouble in Shift then LastClickCount := 2
  else if ssTriple in Shift then LastClickCount := 3
  else if ssQuad in Shift then LastClickCount := 4;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, ScreenScale);
  Chromium.SendMouseClickEvent(@TempEvent, GetButton(Button), False, LastClickCount);
end;

procedure TOsrBrowserWindow.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  LastClickCount: integer;
  IsHandled: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  IsHandled := False;
  if FOnMouseDown <> nil then
    FOnMouseDown(Self, Button, Shift, X, Y, IsHandled);
  if IsHandled then
    exit;

  LastClickCount := 1;
  if ssDouble in Shift then LastClickCount := 2
  else if ssTriple in Shift then LastClickCount := 3
  else if ssQuad in Shift then LastClickCount := 4;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, ScreenScale);
  Chromium.SendMouseClickEvent(@TempEvent, GetButton(Button), True, LastClickCount);
end;

procedure TOsrBrowserWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  IsHandled: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  IsHandled := False;
  if FOnMouseMove <> nil then
    FOnMouseMove(Self, Shift, X, Y, IsHandled);
  if IsHandled then
    exit;

  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, ScreenScale);
  Chromium.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TOsrBrowserWindow.MouseEnter;
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  inherited MouseEnter;

  TempPoint           := ScreenToClient(mouse.CursorPos);
  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := EVENTFLAG_NONE;
  DeviceToLogical(TempEvent, ScreenScale);
  Chromium.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TOsrBrowserWindow.MouseLeave;
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
  TempTime  : integer;
begin
  inherited MouseLeave;

  TempPoint := ScreenToClient(mouse.CursorPos);
  TempPoint := ScreenToclient(TempPoint);
  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  {$IFDEF MSWINDOWS}
  TempEvent.modifiers := GetCefMouseModifiers;
  {$ELSE}
  TempEvent.modifiers := EVENTFLAG_NONE;
  {$ENDIF}
  DeviceToLogical(TempEvent, ScreenScale);
  Chromium.SendMouseMoveEvent(@TempEvent, True);
end;

function TOsrBrowserWindow.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  TempEvent  : TCefMouseEvent;
  IsHandled: Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  IsHandled := False;
  if FOnMouseWheel <> nil then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, IsHandled);
  if IsHandled then
    exit;

  TempEvent.x         := MousePos.x;
  TempEvent.y         := MousePos.y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, ScreenScale);
  {$IFDEF MSWINDOWS}
  if CefIsKeyDown(VK_SHIFT) then
    Chromium.SendMouseWheelEvent(@TempEvent, WheelDelta, 0)
  else
  {$ENDIF}
    Chromium.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
end;

procedure TOsrBrowserWindow.KeyDown(var Key: Word; Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
  IsHandled: Boolean;
begin
  IsHandled := False;
  if FOnKeyDown <> nil then
    FOnKeyDown(Self, Key, Shift, IsHandled);
  if IsHandled then begin
    inherited KeyDown(Key, Shift);
    exit;
  end;

  FLastKeyDown := Key;
  if (Key <> 0) and (Chromium <> nil) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := getModifiers(Shift);
      TempKeyEvent.windows_key_code        := Key;
      {$IFDEF DARWIN}  // $IFDEF MACOSX
      TempKeyEvent.native_key_code         := LastMacOsKeyDownCode;
      {$ELSE}
      TempKeyEvent.native_key_code         := 0;
      {$ENDIF}
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      Chromium.SendKeyEvent(@TempKeyEvent);

      if (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB]) then Key := 0;
    end;

  inherited KeyDown(Key, Shift);
end;

procedure TOsrBrowserWindow.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  TempKeyEvent : TCefKeyEvent;
  TempString   : UnicodeString;
  IsHandled: Boolean;
begin
  IsHandled := False;
  if FOnUtf8KeyPress <> nil then
    FOnUtf8KeyPress(Self, UTF8Key, IsHandled);
  if IsHandled then begin
    inherited UTF8KeyPress(UTF8Key);
    exit;
  end;

  if Focused then
    begin
      TempString := UTF8Decode(UTF8Key);

      if (length(TempString) > 0) then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          {$IFDEF MSWINDOWS}
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(WParam(TempString[1]), 0);
          TempKeyEvent.windows_key_code        := ord(TempString[1]);
          {$ELSE}
          TempKeyEvent.modifiers               := getKeyModifiers(GetKeyShiftState);
          TempKeyEvent.windows_key_code        := FLastKeyDown;
          {$ENDIF}
          TempKeyEvent.native_key_code         := 0;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := TempString[1];
          TempKeyEvent.unmodified_character    := TempString[1];
          TempKeyEvent.focus_on_editable_field := ord(False);

          Chromium.SendKeyEvent(@TempKeyEvent);
        end;
    end;

  inherited UTF8KeyPress(UTF8Key);
end;

procedure TOsrBrowserWindow.KeyUp(var Key: Word; Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
  IsHandled: Boolean;
begin
  IsHandled := False;
  if FOnKeyUp <> nil then
    FOnKeyUp(Self, Key, Shift, IsHandled);
  if IsHandled then begin
    inherited KeyUp(Key, Shift);
    exit;
  end;

  if (Key <> 0) and (Chromium <> nil) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_KEYUP;
      TempKeyEvent.modifiers               := getModifiers(Shift);
      TempKeyEvent.windows_key_code        := Key;
      TempKeyEvent.native_key_code         := 0;
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      Chromium.SendKeyEvent(@TempKeyEvent);
    end;

  inherited KeyUp(Key, Shift);
end;

{$IFDEF MSWINDOWS}
procedure TOsrBrowserWindow.DoOnIMECancelComposition;
begin
  inherited DoOnIMECancelComposition;
  Chromium.IMECancelComposition;
end;

procedure TOsrBrowserWindow.DoOnIMECommitText(const aText: ustring;
  const replacement_range: PCefRange; relative_cursor_pos: integer);
begin
  inherited DoOnIMECommitText(aText, replacement_range, relative_cursor_pos);
  Chromium.IMECommitText(aText, replacement_range, relative_cursor_pos);;
end;

procedure TOsrBrowserWindow.DoOnIMESetComposition(const aText: ustring;
  const underlines: TCefCompositionUnderlineDynArray; const replacement_range,
  selection_range: TCefRange);
begin
  inherited DoOnIMESetComposition(aText, underlines, replacement_range, selection_range);
  Chromium.IMESetComposition(aText, underlines, @replacement_range, @selection_range);
end;
{$ENDIF}

procedure TOsrBrowserWindow.CaptureChanged;
begin
  inherited CaptureChanged;

  if (Chromium <> nil) then Chromium.SendCaptureLostEvent;
end;

procedure TOsrBrowserWindow.DoOnCreated(Sender: TObject);
begin
  if Assigned(FOnBrowserCreated) then
    FOnBrowserCreated(Self);
end;

procedure TOsrBrowserWindow.DoOnClosed(Sender: TObject);
begin
  if (not(csDestroying in ComponentState)) and
     Assigned(FOnBrowserClosed)
  then
    FOnBrowserClosed(Self);
end;

constructor TOsrBrowserWindow.Create(AOwner: TComponent);
begin
  FResizeCS       := TCriticalSection.Create;

  FDeviceBounds   := nil;
  FSelectedRange.from   := 0;
  FSelectedRange.to_    := 0;

  FChromium := TEmbeddedOsrChromium.Create(Self);
  FChromium.InternalOnBrowserClosed              := {$IFDEF FPC}@{$ENDIF}DoOnClosed;
  FChromium.InternalOnBrowserCreated             := {$IFDEF FPC}@{$ENDIF}DoOnCreated;

  FChromium.OnPaint                      := {$IFDEF FPC}@{$ENDIF}DoChromiumPaint;
  FChromium.OnGetViewRect                := {$IFDEF FPC}@{$ENDIF}DoGetChromiumViewRect;
  FChromium.OnCursorChange               := {$IFDEF FPC}@{$ENDIF}DoGetChromiumCursorChange;
  FChromium.OnGetScreenPoint             := {$IFDEF FPC}@{$ENDIF}DoGetChromiumGetScreenPoint;
  FChromium.OnGetScreenInfo              := {$IFDEF FPC}@{$ENDIF}DoGetChromiumGetScreenInfo;
  FChromium.OnPopupShow                  := {$IFDEF FPC}@{$ENDIF}DoGetChromiumPopupShow;
  FChromium.OnPopupSize                  := {$IFDEF FPC}@{$ENDIF}DoGetChromiumPopupSize;
  FChromium.OnTooltip                    := {$IFDEF FPC}@{$ENDIF}DoGetChromiumTooltip;
  FChromium.OnBeforePopup                := {$IFDEF FPC}@{$ENDIF}DoGetChromiumBeforePopup;
  FChromium.OnIMECompositionRangeChanged := @DoGetChromiumIMECompositionRangeChanged;

  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOwnedChildrenNotSelectable];
  CopyOriginalBuffer := true;
end;

destructor TOsrBrowserWindow.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FResizeCS);
  if (FDeviceBounds <> nil) then
    begin
      Finalize(FDeviceBounds);
      FDeviceBounds := nil;
    end;
end;

procedure TOsrBrowserWindow.CloseBrowser(aForceClose: boolean);
begin
  FChromium.CloseBrowser(aForceClose);
end;

procedure TOsrBrowserWindow.WaitForBrowserClosed;
begin
  if not FChromium.HasBrowser then
    exit;
  FChromium.CloseBrowser(True);

  while FChromium.HasBrowser do begin
    Application.ProcessMessages;
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;
    sleep(5);
  end;

  // TODO : sent closed?
end;

function TOsrBrowserWindow.IsClosed: boolean;
begin
  Result := not FChromium.HasBrowser;
end;

procedure TOsrBrowserWindow.LoadURL(aURL: ustring);
begin
  FChromium.LoadURL(aURL);
end;


{$IFDEF FPC}
procedure Register;
begin
  {$I res/TOsrBrowserWindow.lrs}
  RegisterComponents('Chromium', [TOsrBrowserWindow]);
  RegisterClass(TEmbeddedOsrChromium);
  RegisterPropertyEditor(TypeInfo(TOnClose),                      TEmbeddedOsrChromium,'OnClose',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnPaint),                      TEmbeddedOsrChromium,'OnPaint',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnGetViewRect),                TEmbeddedOsrChromium,'OnGetViewRect',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnCursorChange),               TEmbeddedOsrChromium,'OnCursorChange',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnGetScreenPoint),             TEmbeddedOsrChromium,'OnGetScreenPoint',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnGetScreenInfo),              TEmbeddedOsrChromium,'OnGetScreenInfo',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnPopupShow),                  TEmbeddedOsrChromium,'OnPopupShow',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnPopupSize),                  TEmbeddedOsrChromium,'OnPopupSize',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnTooltip),                    TEmbeddedOsrChromium,'OnTooltip',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnBeforePopup),                TEmbeddedOsrChromium,'OnBeforePopup',THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TOnIMECompositionRangeChanged), TEmbeddedOsrChromium,'OnIMECompositionRangeChanged',THiddenPropertyEditor);
end;
{$ENDIF}

end.

