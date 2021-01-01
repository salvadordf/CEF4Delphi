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

unit uBrowserFrame;

{$I cef.inc}

interface

uses
  Winapi.Windows, System.SysUtils, System.Types, System.UITypes, System.Classes,
  WinApi.Messages, System.Variants, FMX.Types, {$IFDEF DELPHI17_UP}FMX.Graphics,{$ENDIF}
  System.SyncObjs, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation, FMX.TabControl,
  uCEFChromiumCore, uCEFFMXChromium, uCEFFMXWindowParent, uCEFInterfaces,
  uCEFTypes, uCEFConstants, uCEFFMXBufferPanel;

type
  TBrowserTitleEvent = procedure(Sender: TObject; const aTitle : string) of object;

  TBrowserFrame = class(TFrame)
      FMXChromium1: TFMXChromium;
      StatusBar: TStatusBar;
      StatusLbl: TLabel;
      AddressLay: TLayout;
      GoBtn: TSpeedButton;
      NavButtonLay: TLayout;
      BackBtn: TSpeedButton;
      ForwardBtn: TSpeedButton;
      ReloadBtn: TSpeedButton;
      StopBtn: TSpeedButton;
      URLEdt: TEdit;
      FMXBufferPanel1: TFMXBufferPanel;

      procedure BackBtnClick(Sender: TObject);
      procedure ForwardBtnClick(Sender: TObject);
      procedure ReloadBtnClick(Sender: TObject);
      procedure StopBtnClick(Sender: TObject);
      procedure GoBtnClick(Sender: TObject);

      procedure FMXBufferPanel1Enter(Sender: TObject);
      procedure FMXBufferPanel1Exit(Sender: TObject);
      procedure FMXBufferPanel1Resize(Sender: TObject);
      procedure FMXBufferPanel1Click(Sender: TObject);
      procedure FMXBufferPanel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
      procedure FMXBufferPanel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
      procedure FMXBufferPanel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
      procedure FMXBufferPanel1MouseLeave(Sender: TObject);
      procedure FMXBufferPanel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
      procedure FMXBufferPanel1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
      procedure FMXBufferPanel1DialogKey(Sender: TObject; var Key: Word; Shift: TShiftState);

      procedure FMXChromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure FMXChromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
      procedure FMXChromium1CursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult: Boolean);
      procedure FMXChromium1GetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
      procedure FMXChromium1GetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
      procedure FMXChromium1GetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
      procedure FMXChromium1Paint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
      procedure FMXChromium1PopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
      procedure FMXChromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
      procedure FMXChromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
      procedure FMXChromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
      procedure FMXChromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
      procedure FMXChromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
      procedure FMXChromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
      procedure FMXChromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
      procedure FMXChromium1StatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);

    protected
      FPopUpBitmap          : TBitmap;
      FPopUpRect            : TRect;
      FShowPopUp            : boolean;
      FResizing             : boolean;
      FPendingResize        : boolean;
      FResizeCS             : TCriticalSection;
      FAtLeastWin8          : boolean;
      FClosing              : boolean;   // Indicates that this frame is destroying the browser
      FHomepage             : string;    // Used to set the TChromium.DefaultURL property
      FOnBrowserDestroyed   : TNotifyEvent;
      FOnBrowserNeedsResize : TNotifyEvent;
      FOnBrowserTitleChange : TBrowserTitleEvent;

      FLastClickCount       : integer;
      FLastClickTime        : integer;
      FLastClickPoint       : TPointF;
      FLastClickButton      : TMouseButton;

      function  GetParentForm : TCustomForm;
      function  GetParentTab : TTabItem;

      function  getModifiers(Shift: TShiftState): TCefEventFlags;
      function  GetButton(Button: TMouseButton): TCefMouseButtonType;
      function  GetMousePosition(var aPoint : TPointF) : boolean;
      procedure InitializeLastClick;
      function  CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;
      {$IFDEF MSWINDOWS}
      function  PostFormMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
      function  ArePointerEventsSupported : boolean;
      function  HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
      function  HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean; overload;
      function  HandlePointerEvent(const aMessage : TMsg) : boolean;
      {$ENDIF}

    public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;

      procedure   NotifyMoveOrResizeStarted;
      procedure   SendCaptureLostEvent;
      {$IFDEF MSWINDOWS}
      procedure   HandleSYSCHAR(const aMessage : TMsg);
      procedure   HandleSYSKEYDOWN(const aMessage : TMsg);
      procedure   HandleSYSKEYUP(const aMessage : TMsg);
      procedure   HandleKEYDOWN(const aMessage : TMsg);
      procedure   HandleKEYUP(const aMessage : TMsg);
      function    HandlePOINTER(const aMessage : TMsg) : boolean;
      {$ENDIF}

      procedure   CreateBrowser;
      procedure   CloseBrowser;
      procedure   ResizeBrowser;
      procedure   FocusBrowser;

      property    ParentForm           : TCustomForm         read GetParentForm;
      property    ParentTab            : TTabItem            read GetParentTab;
      property    Closing              : boolean             read FClosing;
      property    Homepage             : string              read FHomepage              write FHomepage;
      property    OnBrowserDestroyed   : TNotifyEvent        read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property    OnBrowserTitleChange : TBrowserTitleEvent  read FOnBrowserTitleChange  write FOnBrowserTitleChange;
      property    OnBrowserNeedsResize : TNotifyEvent        read FOnBrowserNeedsResize  write FOnBrowserNeedsResize;
  end;

implementation

{$R *.fmx}

uses
  FMX.Platform, {$IFDEF MSWINDOWS}FMX.Platform.Win,{$ENDIF} System.Math,
  uCEFMiscFunctions, uCEFApplication, uBrowserTab, uMainForm;

procedure TBrowserFrame.BackBtnClick(Sender: TObject);
begin
  FMXChromium1.GoBack;
end;

constructor TBrowserFrame.Create(AOwner : TComponent);
var
  TempMajorVer, TempMinorVer : DWORD;
begin
  inherited Create(AOwner);

  FClosing               := False;
  FHomepage              := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;

  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FClosing        := False;
  FResizeCS       := TCriticalSection.Create;

  FAtLeastWin8 := GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and
                  ((TempMajorVer > 6) or
                   ((TempMajorVer = 6) and (TempMinorVer >= 2)));

  InitializeLastClick;
end;

destructor TBrowserFrame.Destroy;
begin
  FResizeCS.Free;
  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);

  inherited Destroy;
end;

procedure TBrowserFrame.ReloadBtnClick(Sender: TObject);
begin
  FMXChromium1.Reload;
end;

procedure TBrowserFrame.FocusBrowser;
begin
  FMXBufferPanel1.SetFocus;
end;

procedure TBrowserFrame.ResizeBrowser;
begin
  try
    if (FResizeCS <> nil) then
      begin
        FResizeCS.Acquire;

        if FResizing then
          FPendingResize := True
         else
          if FMXBufferPanel1.BufferIsResized then
            FMXChromium1.Invalidate(PET_VIEW)
           else
            begin
              FResizing := True;
              FMXChromium1.WasResized;
            end;
      end;
  finally
    if (FResizeCS <> nil) then FResizeCS.Release;
  end;
end;

procedure TBrowserFrame.NotifyMoveOrResizeStarted;
begin
  FMXChromium1.NotifyMoveOrResizeStarted;
end;

procedure TBrowserFrame.SendCaptureLostEvent;
begin
  FMXChromium1.SendCaptureLostEvent;
end;

procedure TBrowserFrame.StopBtnClick(Sender: TObject);
begin
  FMXChromium1.StopLoad;
end;

procedure TBrowserFrame.FMXBufferPanel1Click(Sender: TObject);
begin
  FocusBrowser;
end;

procedure TBrowserFrame.FMXBufferPanel1DialogKey(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vkTab) then Key := 0;
end;

procedure TBrowserFrame.FMXBufferPanel1Enter(Sender: TObject);
begin
  FMXChromium1.SendFocusEvent(True);
end;

procedure TBrowserFrame.FMXBufferPanel1Exit(Sender: TObject);
begin
  FMXChromium1.SendFocusEvent(False);
end;

procedure TBrowserFrame.FMXBufferPanel1KeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if not(FMXBufferPanel1.IsFocused) then exit;

  if (Key = 0) and (KeyChar <> #0) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_CHAR;
      TempKeyEvent.modifiers               := getModifiers(Shift);
      TempKeyEvent.windows_key_code        := ord(KeyChar);
      TempKeyEvent.native_key_code         := 0;
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      FMXChromium1.SendKeyEvent(@TempKeyEvent);
    end
   else
    if (Key <> 0) and (KeyChar = #0) and
       (Key in [vkLeft, vkRight, vkUp, vkDown]) then
      Key := 0;
end;

procedure TBrowserFrame.FMXBufferPanel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if not(ssTouch in Shift) then
    begin
      FocusBrowser;

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

      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Shift);
      FMXChromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
    end;
end;

procedure TBrowserFrame.FMXBufferPanel1MouseLeave(Sender: TObject);
var
  TempEvent  : TCefMouseEvent;
  TempPoint  : TPoint;
  TempPointF : TPointF;
  TempTime   : integer;
begin
  if GetMousePosition(TempPointF) then
    begin
      TempPoint.x := round(TempPointF.x);
      TempPoint.y := round(TempPointF.y);
      TempPoint   := FMXBufferPanel1.ScreenToclient(TempPoint);

      if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
      TempEvent.modifiers := GetCefMouseModifiers;
      FMXChromium1.SendMouseMoveEvent(@TempEvent, True);
    end;
end;

procedure TBrowserFrame.FMXBufferPanel1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if not(ssTouch in Shift) then
    begin
      if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Shift);
      FMXChromium1.SendMouseMoveEvent(@TempEvent, False);
    end;
end;

procedure TBrowserFrame.FMXBufferPanel1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  TempEvent : TCefMouseEvent;
begin
  if not(ssTouch in Shift) then
    begin
      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Shift);
      FMXChromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
    end;
end;

procedure TBrowserFrame.FMXBufferPanel1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
  TempPoint  : TPoint;
  TempPointF : TPointF;
begin
  if FMXBufferPanel1.IsFocused and GetMousePosition(TempPointF) then
    begin
      TempPoint.x := round(TempPointF.x);
      TempPoint.y := round(TempPointF.y);
      TempPoint   := FMXBufferPanel1.ScreenToClient(TempPoint);

      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
      TempEvent.modifiers := getModifiers(Shift);
      FMXChromium1.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
    end;
end;

procedure TBrowserFrame.FMXBufferPanel1Resize(Sender: TObject);
begin
  ResizeBrowser;
end;

procedure TBrowserFrame.FMXChromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  URLEdt.Text := url;
end;

procedure TBrowserFrame.FMXChromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  AddressLay.Enabled := True;
  FocusBrowser;
end;

procedure TBrowserFrame.FMXChromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then FOnBrowserDestroyed(Sender);
end;

procedure TBrowserFrame.FMXChromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TBrowserFrame.FMXChromium1CursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType;
  const customCursorInfo: PCefCursorInfo; var aResult: Boolean);
begin
  FMXBufferPanel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult                := True;
end;

procedure TBrowserFrame.FMXChromium1GetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo;
  out Result: Boolean);
var
  TempRect : TCEFRect;
begin
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := round(FMXBufferPanel1.Width);
  TempRect.height := round(FMXBufferPanel1.Height);

  screenInfo.device_scale_factor := FMXBufferPanel1.ScreenScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TBrowserFrame.FMXChromium1GetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
begin
  // TFMXBufferPanel.ClientToScreen applies the scale factor. No need to call LogicalToDevice to set TempViewPt.
  TempViewPt.x := viewX;
  TempViewPt.y := viewY;
  TempScreenPt := FMXBufferPanel1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TBrowserFrame.FMXChromium1GetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
begin
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := round(FMXBufferPanel1.Width);
  rect.height := round(FMXBufferPanel1.Height);
end;

procedure TBrowserFrame.FMXChromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
var
  TempString : string;
begin
  if (errorCode = ERR_ABORTED) then exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  FMXChromium1.LoadString(TempString, frame);
end;

procedure TBrowserFrame.FMXChromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  BackBtn.Enabled    := canGoBack;
  ForwardBtn.Enabled := canGoForward;

  if isLoading then
    begin
      ReloadBtn.Enabled := False;
      StopBtn.Enabled   := True;
    end
   else
    begin
      ReloadBtn.Enabled := True;
      StopBtn.Enabled   := False;
    end;
end;

procedure TBrowserFrame.FMXChromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TBrowserFrame.FMXChromium1Paint(Sender: TObject;
  const browser: ICefBrowser; type_: TCefPaintElementType;
  dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray;
  const buffer: Pointer; width, height: Integer);
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride, TempWidth, TempHeight : Integer;
  n : NativeUInt;
  {$IFNDEF DELPHI17_UP}
  TempScanlineSize, DstStride : integer;
  {$ENDIF}
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempBitmapData : TBitmapData;
  TempBitmap : TBitmap;
  TempSrcRect, TempDstRect : TRectF;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if FMXBufferPanel1.BeginBufferDraw then
      try
        if (type_ = PET_POPUP) then
          begin
            if (FPopUpBitmap = nil) or
               (width  <> FPopUpBitmap.Width) or
               (height <> FPopUpBitmap.Height) then
              begin
                if (FPopUpBitmap <> nil) then FPopUpBitmap.Free;

                FPopUpBitmap             := TBitmap.Create(width, height);
                {$IFDEF DELPHI17_UP}
                FPopUpBitmap.BitmapScale := FMXBufferPanel1.ScreenScale;
                {$ENDIF}
              end;

            TempWidth        := FPopUpBitmap.Width;
            TempHeight       := FPopUpBitmap.Height;
            {$IFNDEF DELPHI17_UP}
            TempScanlineSize := FPopUpBitmap.BytesPerLine;
            {$ENDIF}
            TempBitmap       := FPopUpBitmap;
          end
         else
          begin
            TempForcedResize := FMXBufferPanel1.UpdateBufferDimensions(Width, Height) or not(FMXBufferPanel1.BufferIsResized(False));
            TempWidth        := FMXBufferPanel1.BufferWidth;
            TempHeight       := FMXBufferPanel1.BufferHeight;
            {$IFNDEF DELPHI17_UP}
            TempScanlineSize := FMXBufferPanel1.ScanlineSize;
            {$ENDIF}
            TempBitmap       := FMXBufferPanel1.Buffer;
          end;


        if (TempBitmap <> nil) {$IFDEF DELPHI17_UP}and TempBitmap.Map(TMapAccess.ReadWrite, TempBitmapData){$ENDIF} then
          begin
            try
              {$IFNDEF DELPHI17_UP}
              TempBufferBits := TempBitmapData.StartLine;
              DstStride      := TempScanlineSize;
              {$ENDIF}
              SrcStride      := Width * SizeOf(TRGBQuad);

              n := 0;

              while (n < dirtyRectsCount) do
                begin
                  if (dirtyRects[n].x >= 0) and (dirtyRects[n].y >= 0) then
                    begin
                      TempLineSize := min(dirtyRects[n].width, TempWidth - dirtyRects[n].x) * SizeOf(TRGBQuad);

                      if (TempLineSize > 0) then
                        begin
                          TempSrcOffset := ((dirtyRects[n].y * Width) + dirtyRects[n].x) * SizeOf(TRGBQuad);

                          {$IFDEF DELPHI17_UP}
                          TempDstOffset := (dirtyRects[n].x * SizeOf(TRGBQuad));
                          {$ELSE}
                          TempDstOffset := (dirtyRects[n].y * TempScanlineSize) + (dirtyRects[n].x * SizeOf(TRGBQuad));
                          {$ENDIF}

                          src := @PByte(buffer)[TempSrcOffset];
                          {$IFNDEF DELPHI17_UP}
                          dst := @PByte(TempBufferBits)[TempDstOffset];
                          {$ENDIF}

                          i := 0;
                          j := min(dirtyRects[n].height, TempHeight - dirtyRects[n].y);

                          while (i < j) do
                            begin
                              {$IFDEF DELPHI17_UP}
                              TempBufferBits := TempBitmapData.GetScanline(dirtyRects[n].y + i);
                              dst            := @PByte(TempBufferBits)[TempDstOffset];
                              {$ENDIF}

                              System.Move(src^, dst^, TempLineSize);

                              {$IFNDEF DELPHI17_UP}
                              inc(dst, DstStride);
                              {$ENDIF}
                              inc(src, SrcStride);
                              inc(i);
                            end;
                        end;
                    end;

                  inc(n);
                end;

              FMXBufferPanel1.InvalidatePanel;
            finally
              {$IFDEF DELPHI17_UP}
              TempBitmap.Unmap(TempBitmapData);
              {$ENDIF}
            end;

            if FShowPopup and (FPopUpBitmap <> nil) then
              begin
                TempSrcRect := RectF(0, 0,
                                     min(FPopUpRect.Width,  FPopUpBitmap.Width),
                                     min(FPopUpRect.Height, FPopUpBitmap.Height));

                TempDstRect.Left   := FPopUpRect.Left / GlobalCEFApp.DeviceScaleFactor;
                TempDstRect.Top    := FPopUpRect.Top  / GlobalCEFApp.DeviceScaleFactor;
                TempDstRect.Right  := TempDstRect.Left + (TempSrcRect.Width  / GlobalCEFApp.DeviceScaleFactor);
                TempDstRect.Bottom := TempDstRect.Top  + (TempSrcRect.Height / GlobalCEFApp.DeviceScaleFactor);

                FMXBufferPanel1.BufferDraw(FPopUpBitmap, TempSrcRect, TempDstRect);
              end;
          end;

        if (type_ = PET_VIEW) then
          begin
            if (TempForcedResize or FPendingResize) and
               assigned(FOnBrowserNeedsResize) then
              FOnBrowserNeedsResize(self);

            FResizing      := False;
            FPendingResize := False;
          end;
      finally
        FMXBufferPanel1.EndBufferDraw;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TBrowserFrame.FMXChromium1PopupShow(Sender: TObject;
  const browser: ICefBrowser; show: Boolean);
begin
  if show then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      FMXChromium1.Invalidate(PET_VIEW);
    end;
end;

procedure TBrowserFrame.FMXChromium1PopupSize(Sender: TObject;
  const browser: ICefBrowser; const rect: PCefRect);
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

procedure TBrowserFrame.FMXChromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  StatusLbl.Text := value;
end;

procedure TBrowserFrame.FMXChromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if assigned(FOnBrowserTitleChange) then FOnBrowserTitleChange(Sender, title);
end;

procedure TBrowserFrame.FMXChromium1Tooltip(Sender: TObject;
  const browser: ICefBrowser; var text: ustring; out Result: Boolean);
begin
  FMXBufferPanel1.Hint     := text;
  FMXBufferPanel1.ShowHint := (length(text) > 0);
  Result                   := True;
end;

procedure TBrowserFrame.ForwardBtnClick(Sender: TObject);
begin
  FMXChromium1.GoForward;
end;

function TBrowserFrame.GetParentForm : TCustomForm;
var
  TempParent : TTabItem;
begin
  Result     := nil;
  TempParent := ParentTab;

  if (TempParent <> nil) and (TempParent is TBrowserTab) then
    Result := TBrowserTab(TempParent).ParentForm;
end;

function TBrowserFrame.GetParentTab : TTabItem;
var
  TempParent : TFMXObject;
begin
  Result     := nil;
  TempParent := Parent;

  while (TempParent <> nil) and not(TempParent is TTabItem) do
    TempParent := TempParent.Parent;

  if (TempParent <> nil) and (TempParent is TTabItem) then
    Result := TTabItem(TempParent);
end;

procedure TBrowserFrame.GoBtnClick(Sender: TObject);
begin
  FMXChromium1.LoadURL(URLEdt.Text);
end;

procedure TBrowserFrame.CreateBrowser;
begin
  if not(FMXChromium1.Initialized) then
    begin
      FMXChromium1.DefaultUrl := FHomepage;
      FMXChromium1.CreateBrowser;
    end;
end;

procedure TBrowserFrame.CloseBrowser;
begin
  if not(FClosing) then
    begin
      FClosing           := True;
      AddressLay.Enabled := False;
      FMXChromium1.CloseBrowser(True);
    end;
end;

function TBrowserFrame.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TBrowserFrame.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

function TBrowserFrame.GetMousePosition(var aPoint : TPointF) : boolean;
var
  TempForm : TCustomForm;
begin
  TempForm := ParentForm;
  Result   := (TempForm <> nil) and
              (TempForm is TMainForm) and
              TMainForm(TempForm).GetMousePosition(aPoint);
end;

procedure TBrowserFrame.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := TMouseButton.mbLeft;
end;

function TBrowserFrame.CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;
begin
  {$IFDEF MSWINDOWS}
  aCurrentTime := GetMessageTime;

  Result := (abs(FLastClickPoint.x - x) > (GetSystemMetrics(SM_CXDOUBLECLK) div 2)) or
            (abs(FLastClickPoint.y - y) > (GetSystemMetrics(SM_CYDOUBLECLK) div 2)) or
            (cardinal(aCurrentTime - FLastClickTime) > GetDoubleClickTime);
  {$ELSE}
  aCurrentTime := 0;
  Result       := False;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function TBrowserFrame.PostFormMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempTab : TTabItem;

begin
  TempTab := ParentTab;
  Result  := (TempTab <> nil) and
             (TempTab is TBrowserTab) and
             TBrowserTab(TempTab).PostFormMessage(aMsg, aWParam, aLParam);
end;

function TBrowserFrame.ArePointerEventsSupported : boolean;
begin
  Result := FAtLeastWin8 and
            (@GetPointerType      <> nil) and
            (@GetPointerTouchInfo <> nil) and
            (@GetPointerPenInfo   <> nil);
end;

function TBrowserFrame.HandlePointerEvent(const aMessage : TMsg) : boolean;
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
      PT_PEN   : Result := HandlePenEvent(TempID, aMessage.message);
      PT_TOUCH : Result := HandleTouchEvent(TempID, aMessage.message);
    end;
end;

function TBrowserFrame.HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
var
  TempPenInfo    : POINTER_PEN_INFO;
  TempTouchEvent : TCefTouchEvent;
  TempPoint      : TPoint;
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

  TempPoint        := FMXBufferPanel1.ScreenToClient(TempPenInfo.pointerInfo.ptPixelLocation);
  // TFMXBufferPanel.ScreenToClient applies the scale factor. No need to call DeviceToLogical to set TempTouchEvent.
  TempTouchEvent.x := TempPoint.x;
  TempTouchEvent.y := TempPoint.y;

  FMXChromium1.SendTouchEvent(@TempTouchEvent);
end;

function TBrowserFrame.HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean;
var
  TempTouchInfo  : POINTER_TOUCH_INFO;
  TempTouchEvent : TCefTouchEvent;
  TempPoint      : TPoint;
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

  TempPoint        := FMXBufferPanel1.ScreenToClient(TempTouchInfo.pointerInfo.ptPixelLocation);
  // TFMXBufferPanel.ScreenToClient applies the scale factor. No need to call DeviceToLogical to set TempTouchEvent.
  TempTouchEvent.x := TempPoint.x;
  TempTouchEvent.y := TempPoint.y;

  FMXChromium1.SendTouchEvent(@TempTouchEvent);
end;

procedure TBrowserFrame.HandleSYSCHAR(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if FMXBufferPanel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_CHAR;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      FMXChromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TBrowserFrame.HandleSYSKEYDOWN(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if FMXBufferPanel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      FMXChromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TBrowserFrame.HandleSYSKEYUP(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if FMXBufferPanel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_KEYUP;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      FMXChromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TBrowserFrame.HandleKEYDOWN(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if FMXBufferPanel1.IsFocused then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      FMXChromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TBrowserFrame.HandleKEYUP(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if FMXBufferPanel1.IsFocused then
    begin
      if (aMessage.wParam = vkReturn) then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
          TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
          TempKeyEvent.native_key_code         := integer(aMessage.lParam);
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          FMXChromium1.SendKeyEvent(@TempKeyEvent);
        end;

      TempKeyEvent.kind                    := KEYEVENT_KEYUP;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      FMXChromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

function TBrowserFrame.HandlePOINTER(const aMessage : TMsg) : boolean;
begin
  Result := FMXBufferPanel1.IsFocused and
            (GlobalCEFApp <> nil) and
            ArePointerEventsSupported and
            HandlePointerEvent(aMessage);
end;
{$ENDIF}


end.
