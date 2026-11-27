unit uFMXExternalPumpBrowser;

{$I ..\..\..\source\cef.inc}

interface

uses
  Winapi.Messages, Winapi.Windows,
  System.Types, System.UITypes, System.Classes, System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation,
  {$IFDEF DELPHI17_UP}
  FMX.Graphics,
  {$ENDIF}
  uCEFFMXChromium, uCEFFMXBufferPanel, uCEFFMXWorkScheduler,
  uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFChromiumCore, FMX.Layouts,
  FMX.ComboEdit;

type
  TFMXExternalPumpBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    chrmosr: TFMXChromium;
    Timer1: TTimer;
    SaveDialog1: TSaveDialog;
    Panel1: TFMXBufferPanel;
    Layout1: TLayout;
    GoBtn: TButton;
    SnapshotBtn: TButton;
    AddressCb: TComboEdit;

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Panel1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure Panel1DialogKey(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult: Boolean);
    procedure chrmosrCanFocus(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);
    procedure AddressEdtEnter(Sender: TObject);

    procedure SnapshotBtnClick(Sender: TObject);
    procedure SnapshotBtnEnter(Sender: TObject);

  protected
    FPopUpBitmap       : TBitmap;
    FPopUpRect         : TRect;
    FShowPopUp         : boolean;
    FResizing          : boolean;
    FPendingResize     : boolean;
    FCanClose          : boolean;
    FClosing           : boolean;
    FResizeCS          : TCriticalSection;
    FAtLeastWin8       : boolean;
    {$IFDEF DELPHI17_UP}
    FMouseWheelService : IFMXMouseService;
    {$ENDIF}

    FLastClickCount  : integer;
    FLastClickTime   : integer;
    FLastClickPoint  : TPointF;
    FLastClickButton : TMouseButton;

    procedure LoadURL;
    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    function  GetMousePosition(var aPoint : TPointF) : boolean;
    procedure InitializeLastClick;
    function  CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;
    procedure SendMouseInput(const mouse : TCefMouseEvent; type_ : TCefMouseInputEventType; button : TCefMouseButtonType; click_count: integer; X, Y: single);
    function  SendCompMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    function  ArePointerEventsSupported : boolean;
    function  HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
    function  HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean; overload;
    function  HandlePointerEvent(const aMessage : TMsg) : boolean;

  public
    procedure DoResize;
    procedure NotifyMoveOrResizeStarted;
    procedure SendCaptureLostEvent;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure HandleKeyboardMsg(const aMessage : TMsg);
    function  HandlePOINTER(const aMessage : TMsg) : boolean;
  end;

var
  FMXExternalPumpBrowserFrm : TFMXExternalPumpBrowserFrm;

// This is a simple browser using FireMonkey components in OSR mode (off-screen rendering)
// and a external message pump.

// It's recomemded to understand the code in the SimpleOSRBrowser and OSRExternalPumpBrowser demos before
// reading the code in this demo.

// Due to the Firemonkey code structure, this demo uses a IFMXApplicationService interface implemented in
// uFMXApplicationService.pas to intercept some windows messages needed to make a CEF browser work.

// The TFMXApplicationService.HandleMessages function receives many of the messages that the
// OSRExternalPumpBrowser demo hadled in the main form or in the GlobalFMXWorkScheduler.

// It was necessary to destroy the browser following the destruction sequence described in
// the MDIBrowser demo but in OSR mode there are some modifications.

// All FMX applications using CEF4Delphi should add the $(FrameworkType) conditional define
// in the project options to avoid duplicated resources.
// This demo has that define in the menu option :
// Project -> Options -> Building -> Delphi compiler -> Conditional defines (All configurations)

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and
//    calls chrmosr.CloseBrowser(True).
// 2- chrmosr.CloseBrowser(True) will trigger chrmosr.OnClose and the default
//    implementation will destroy the internal browser immediately, which will
//    trigger the chrmosr.OnBeforeClose event.
// 3- chrmosr.OnBeforeClose sets FCanClose to True and closes the form.

procedure CreateGlobalCEFApp;

implementation

{$R *.fmx}

uses
  System.SysUtils, System.Math, FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uCEFApplication, uFMXApplicationService;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalFMXWorkScheduler <> nil) then
    GlobalFMXWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;
  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {$ENDIF}

  // TFMXWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalFMXWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalFMXWorkScheduler := TFMXWorkScheduler.Create(nil);
end;

procedure TFMXExternalPumpBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing           := True;
      Visible            := False;
      AddressPnl.Enabled := False;
      chrmosr.CloseBrowser(True);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.FormCreate(Sender: TObject);
var
  TempMajorVer, TempMinorVer : DWORD;
begin
  TFMXApplicationService.AddPlatformService;

  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FResizeCS       := TCriticalSection.Create;

  FAtLeastWin8 := GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and
                  ((TempMajorVer > 6) or
                   ((TempMajorVer = 6) and (TempMinorVer >= 2)));

  chrmosr.DefaultURL := AddressCb.Text;

  InitializeLastClick;

  {$IFDEF DELPHI17_UP}
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService) then
    FMouseWheelService := TPlatformServices.Current.GetPlatformService(IFMXMouseService) as IFMXMouseService;
  {$ENDIF}
end;

procedure TFMXExternalPumpBrowserFrm.FormDestroy(Sender: TObject);
begin
  FResizeCS.Free;
  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);
end;

procedure TFMXExternalPumpBrowserFrm.FormHide(Sender: TObject);
begin
  chrmosr.SetFocus(False);
  chrmosr.WasHidden(True);
end;

procedure TFMXExternalPumpBrowserFrm.FormShow(Sender: TObject);
begin
  if chrmosr.Initialized then
    begin
      chrmosr.WasHidden(False);
      chrmosr.SetFocus(True);
    end
   else
    begin
      // opaque white background color
      chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

      if not(chrmosr.CreateBrowser) then Timer1.Enabled := True;
    end;
end;

procedure TFMXExternalPumpBrowserFrm.GoBtnClick(Sender: TObject);
begin
  LoadURL;
end;

procedure TFMXExternalPumpBrowserFrm.LoadURL;
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  chrmosr.LoadURL(AddressCb.Text);
end;

procedure TFMXExternalPumpBrowserFrm.GoBtnEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1DialogKey(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = vkTab) then Key := 0;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1Enter(Sender: TObject);
begin
  chrmosr.SetFocus(True);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1Exit(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1KeyDown(    Sender  : TObject;
                                                   var Key     : Word;
                                                   var KeyChar : Char;
                                                       Shift   : TShiftState);
var
  TempMessage : TMsg;
begin
  if not(Panel1.IsFocused) then exit;

  if (Key = 0) and (KeyChar <> #0) then
    begin
      TempMessage.message := WM_CHAR;
      TempMessage.wParam  := ord(KeyChar);
      TempMessage.lParam  := 0;
      HandleKeyboardMsg(TempMessage);
    end
   else
    if (Key <> 0) and (KeyChar = #0) and
       (Key in [vkLeft, vkRight, vkUp, vkDown]) then
      Key := 0;
end;

function TFMXExternalPumpBrowserFrm.GetMousePosition(var aPoint : TPointF) : boolean;
begin
  {$IFDEF DELPHI17_UP}
  if (FMouseWheelService <> nil) then
    begin
      aPoint := FMouseWheelService.GetMousePos;
      Result := True;
    end
   else
    begin
      aPoint.x := 0;
      aPoint.y := 0;
      Result   := False;
    end;
  {$ELSE}
  TempPointF := Platform.GetMousePos;
  Result     := True;
  {$ENDIF}
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseDown(Sender : TObject;
                                                     Button : TMouseButton;
                                                     Shift  : TShiftState;
                                                     X, Y   : Single);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if not(ssTouch in Shift) then
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

      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Shift);
      SendMouseInput(TempEvent, CEF_MOUSE_EVENT_DOWN, GetButton(Button), FLastClickCount, x, y);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseLeave(Sender: TObject);
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
      TempPoint   := Panel1.ScreenToclient(TempPoint);

      if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
      TempEvent.modifiers := GetCefMouseModifiers;
      SendMouseInput(TempEvent, CEF_MOUSE_EVENT_LEAVE, CEF_MOUSE_BUTTON_NONE, 0, TempPoint.x, TempPoint.y);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseMove(Sender : TObject;
                                                     Shift  : TShiftState;
                                                     X, Y   : Single);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if not(ssTouch in Shift) then
    begin
      if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

      TempEvent.x         := round(x);
      TempEvent.y         := round(y);
      TempEvent.modifiers := getModifiers(Shift);
      SendMouseInput(TempEvent, CEF_MOUSE_EVENT_MOVE, CEF_MOUSE_BUTTON_NONE, 0, x, y);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseUp(Sender : TObject;
                                                   Button : TMouseButton;
                                                   Shift  : TShiftState;
                                                   X, Y   : Single);
var
  TempEvent : TCefMouseEvent;
begin
  if not(ssTouch in Shift) then
    begin
      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Shift);
      SendMouseInput(TempEvent, CEF_MOUSE_EVENT_UP, GetButton(Button), FLastClickCount, x, y);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseWheel(    Sender      : TObject;
                                                          Shift       : TShiftState;
                                                          WheelDelta  : Integer;
                                                      var Handled     : Boolean);
var
  TempMouseEvent : TCefMouseEvent;
  TempPoint, TempScreenPt : TPointF;
  horizontal : boolean;
  screen : TCefPoint;
  input : TCefWheelInputEvent;
  scroll_amount : cardinal;
  ticks : single;
begin
  if not(Panel1.IsFocused) or
     not(GetMousePosition(TempPoint)) then
    exit;

  horizontal := (ssShift in Shift);

  TempPoint                := Panel1.ScreenToclient(TempPoint);
  TempMouseEvent.x         := round(TempPoint.x);
  TempMouseEvent.y         := round(TempPoint.y);
  TempMouseEvent.modifiers := getModifiers(Shift);

  TempScreenPt := Panel1.ClientToScreen(TempPoint);
  screen.x     := round(TempScreenPt.X);
  screen.y     := round(TempScreenPt.Y);

  input := MakeOsrWheelInputEvent(TempMouseEvent, screen.x, screen.y);

  DeviceToLogical(TempMouseEvent, Panel1.ScreenScale);

  input.x := TempMouseEvent.x;
  input.y := TempMouseEvent.y;

  if horizontal then
    begin
      scroll_amount := 1;
      SystemParametersInfo(SPI_GETWHEELSCROLLCHARS, 0, @scroll_amount, 0);
    end
   else
    begin
      scroll_amount := 3;
      SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @scroll_amount, 0);
    end;

  ticks := WheelDelta / WHEEL_DELTA;
  SetOsrWindowsWheelDelta(input, ticks, horizontal, scroll_amount);
  chrmosr.SendWheelEvent(input);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1Resize(Sender: TObject);
begin
  DoResize;
end;

procedure TFMXExternalPumpBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if not(chrmosr.CreateBrowser) and not(chrmosr.Initialized) then
    Timer1.Enabled := True;
end;

procedure TFMXExternalPumpBrowserFrm.AddressEdtEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can enable the UI.
  Caption := 'FMX External Pump Browser';
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCompMessage(WM_CLOSE);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrBeforePopup(      Sender             : TObject;
                                                        const browser            : ICefBrowser;
                                                        const frame              : ICefFrame;
                                                              popup_id           : Integer;
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
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrCanFocus(Sender: TObject);
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      // The browser required some time to create associated internal objects
      // before being able to accept the focus. Now we can set the focus on the
      // TBufferPanel control
      if Panel1.IsFocused then
        chrmosr.SetFocus(True)
       else
        Panel1.SetFocus;
    end);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrCursorChange(      Sender           : TObject;
                                                         const browser          : ICefBrowser;
                                                               cursor_          : TCefCursorHandle;
                                                               cursorType       : TCefCursorType;
                                                         const customCursorInfo : PCefCursorInfo;
                                                         var   aResult          : Boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrGetScreenInfo(      Sender     : TObject;
                                                          const browser    : ICefBrowser;
                                                          var   screenInfo : TCefScreenInfo;
                                                          out   Result     : Boolean);
var
  TempRect : TCEFRect;
begin
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := round(Panel1.Width);
  TempRect.height := round(Panel1.Height);

  screenInfo.device_scale_factor := Panel1.ScreenScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrGetScreenPoint(      Sender  : TObject;
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
  TempViewPt.x := viewX;
  TempViewPt.y := viewY;
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screenX      := LogicalToDevice(TempScreenPt.x, TempScale);
  screenY      := LogicalToDevice(TempScreenPt.y, TempScale);
  Result       := True;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrGetViewRect(      Sender  : TObject;
                                                        const browser : ICefBrowser;
                                                        var   rect    : TCefRect);
begin
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := round(Panel1.Width);
  rect.height := round(Panel1.Height);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrPaint(      Sender          : TObject;
                                                  const browser         : ICefBrowser;
                                                        type_           : TCefPaintElementType;
                                                        dirtyRectsCount : NativeUInt;
                                                  const dirtyRects      : PCefRectArray;
                                                  const buffer          : Pointer;
                                                        width           : Integer;
                                                        height          : Integer);
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

    if Panel1.BeginBufferDraw then
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
                FPopUpBitmap.BitmapScale := Panel1.ScreenScale;
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
            TempForcedResize := Panel1.UpdateBufferDimensions(Width, Height) or not(Panel1.BufferIsResized(False));
            TempWidth        := Panel1.BufferWidth;
            TempHeight       := Panel1.BufferHeight;
            {$IFNDEF DELPHI17_UP}
            TempScanlineSize := Panel1.ScanlineSize;
            {$ENDIF}
            TempBitmap       := Panel1.Buffer;
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

                              Move(src^, dst^, TempLineSize);

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

              Panel1.InvalidatePanel;
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

                Panel1.BufferDraw(FPopUpBitmap, TempSrcRect, TempDstRect);
              end;
          end;

        if (type_ = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then
              SendCompMessage(CEF_PENDINGRESIZE);

            FResizing      := False;
            FPendingResize := False;
          end;
      finally
        Panel1.EndBufferDraw;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrPopupShow(      Sender  : TObject;
                                                      const browser : ICefBrowser;
                                                            show    : Boolean);
begin
  if show then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      chrmosr.Invalidate(PET_VIEW);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrPopupSize(      Sender  : TObject;
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

procedure TFMXExternalPumpBrowserFrm.chrmosrTooltip(      Sender  : TObject;
                                                    const browser : ICefBrowser;
                                                    var   text    : ustring;
                                                    out   Result  : Boolean);
begin
  Panel1.Hint     := text;
  Panel1.ShowHint := (length(text) > 0);
  Result          := True;
end;

procedure TFMXExternalPumpBrowserFrm.DoResize;
begin
  if (FResizeCS <> nil) then
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

procedure TFMXExternalPumpBrowserFrm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  PositionChanged: Boolean;
begin
  PositionChanged := (ALeft <> Left) or (ATop <> Top);

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if PositionChanged then NotifyMoveOrResizeStarted;
end;

procedure TFMXExternalPumpBrowserFrm.NotifyMoveOrResizeStarted;
begin
  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TFMXExternalPumpBrowserFrm.SendCaptureLostEvent;
begin
  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

function TFMXExternalPumpBrowserFrm.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift  in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt    in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl   in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft   in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight  in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
end;

function TFMXExternalPumpBrowserFrm.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := CEF_MOUSE_BUTTON_RIGHT;
    TMouseButton.mbMiddle : Result := CEF_MOUSE_BUTTON_MIDDLE;
    else                    Result := CEF_MOUSE_BUTTON_LEFT;
  end;
end;

procedure TFMXExternalPumpBrowserFrm.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := TMouseButton.mbLeft;
end;

function TFMXExternalPumpBrowserFrm.CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;
begin
  aCurrentTime := GetMessageTime;

  Result := (abs(FLastClickPoint.x - x) > (GetSystemMetrics(SM_CXDOUBLECLK) div 2)) or
            (abs(FLastClickPoint.y - y) > (GetSystemMetrics(SM_CYDOUBLECLK) div 2)) or
            (cardinal(aCurrentTime - FLastClickTime) > GetDoubleClickTime);
end;

procedure TFMXExternalPumpBrowserFrm.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TFMXExternalPumpBrowserFrm.SnapshotBtnEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

function TFMXExternalPumpBrowserFrm.HandlePOINTER(const aMessage : TMsg) : boolean;
begin
  Result := Panel1.IsFocused and
            (GlobalCEFApp <> nil) and
            ArePointerEventsSupported and
            HandlePointerEvent(aMessage);
end;

function TFMXExternalPumpBrowserFrm.SendCompMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempHandle : TWinWindowHandle;
begin
  TempHandle := WindowHandleToPlatform(Handle);
  Result     := WinApi.Windows.PostMessage(TempHandle.Wnd, aMsg, aWParam, aLParam);
end;

procedure TFMXExternalPumpBrowserFrm.SendMouseInput(const mouse       : TCefMouseEvent;
                                                          type_       : TCefMouseInputEventType;
                                                          button      : TCefMouseButtonType;
                                                          click_count : integer;
                                                          X, Y        : single);
var
  screen : TCefPoint;
  TempScreenPt, TempViewPt : TPointF;
begin
  TempViewPt.x := x;
  TempViewPt.y := y;
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screen.x     := round(TempScreenPt.X);
  screen.y     := round(TempScreenPt.Y);
  chrmosr.SendMouseEvent(MakeOsrMouseInputEvent(mouse, screen, type_, button, click_count));
end;

procedure TFMXExternalPumpBrowserFrm.HandleKeyboardMsg(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyboardInputEvent;
begin
  if Panel1.IsFocused then
    begin
      FillChar(TempKeyEvent, SizeOf(TCefKeyboardInputEvent), 0);
      TempKeyEvent.size := SizeOf(TCefKeyboardInputEvent);
      if CefKeyboardInputEventFromWindowsMessage(aMessage.message, aMessage.wParam, aMessage.lParam, TempKeyEvent) then
        chrmosr.SendKeyboardEvent(TempKeyEvent);
    end;
end;

function TFMXExternalPumpBrowserFrm.ArePointerEventsSupported : boolean;
begin
  Result := FAtLeastWin8 and
            (@GetPointerType      <> nil) and
            (@GetPointerTouchInfo <> nil) and
            (@GetPointerPenInfo   <> nil);
end;

function TFMXExternalPumpBrowserFrm.HandlePointerEvent(const aMessage : TMsg) : boolean;
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

function TFMXExternalPumpBrowserFrm.HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
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

  TempScale   := Panel1.ScreenScale;
  TempPoint.x := DeviceToLogical(TempPenInfo.pointerInfo.ptPixelLocation.x, TempScale);
  TempPoint.y := DeviceToLogical(TempPenInfo.pointerInfo.ptPixelLocation.y, TempScale);

  TempPoint        := Panel1.ScreenToClient(TempPoint);
  TempTouchEvent.x := TempPoint.x;
  TempTouchEvent.y := TempPoint.y;

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;

function TFMXExternalPumpBrowserFrm.HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean;
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

  TempScale   := Panel1.ScreenScale;
  TempPoint.x := DeviceToLogical(TempTouchInfo.pointerInfo.ptPixelLocation.x, TempScale);
  TempPoint.y := DeviceToLogical(TempTouchInfo.pointerInfo.ptPixelLocation.y, TempScale);

  TempPoint        := Panel1.ScreenToClient(TempPoint);
  TempTouchEvent.x := TempPoint.x;
  TempTouchEvent.y := TempPoint.y;

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;

end.
