unit uMainForm;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}Winapi.Messages, Winapi.Windows,{$ENDIF}
  System.Types, System.UITypes, System.Classes, System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Edit, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ComboEdit, {$IFDEF DELPHI17_UP}FMX.Graphics,{$ENDIF}
  uCEFFMXChromium, uCEFFMXBufferPanel, uCEFFMXWorkScheduler,
  uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFChromiumCore,
  // The following units come from the Skia4Delphi project
  Skia, FMX.Skia;


type
  TMainForm = class(TForm)
    AddressPnl: TPanel;
    chrmosr: TFMXChromium;
    Timer1: TTimer;
    GoBtn: TButton;
    AddressCb: TComboEdit;
    SkPaintBox1: TSkPaintBox;
    Panel1: TPanel;

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Panel1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure Panel1Resize(Sender: TObject);

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
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult: Boolean);
    procedure chrmosrCanFocus(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);
    procedure AddressEdtEnter(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);

  protected
    FPopUpBitmap       : TBitmap;
    FPopUpRect         : TRect;
    FShowPopUp         : boolean;
    FCanClose          : boolean;
    FClosing           : boolean;
    FAtLeastWin8       : boolean;
    FImage             : ISkImage;
    FPopupImage        : ISkImage;
    FImageInfo         : TSkImageInfo;
    {$IFDEF DELPHI17_UP}
    FMouseWheelService : IFMXMouseService;
    {$ENDIF}

    FLastClickCount  : integer;
    FLastClickTime   : integer;
    FLastClickPoint  : TPointF;
    FLastClickButton : TMouseButton;

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    function  GetMousePosition(var aPoint : TPointF) : boolean;
    procedure InitializeLastClick;
    function  CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;
    procedure DoRedraw;
    procedure DoResize;
    function  RealScreenScale: single;
    {$IFDEF MSWINDOWS}
    function  SendCompMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    function  ArePointerEventsSupported : boolean;
    function  HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
    function  HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean; overload;
    function  HandlePointerEvent(const aMessage : TMsg) : boolean;
    {$ENDIF}

  public
    procedure NotifyMoveOrResizeStarted;
    procedure SendCaptureLostEvent;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    {$IFDEF MSWINDOWS}
    procedure HandleSYSCHAR(const aMessage : TMsg);
    procedure HandleSYSKEYDOWN(const aMessage : TMsg);
    procedure HandleSYSKEYUP(const aMessage : TMsg);
    procedure HandleKEYDOWN(const aMessage : TMsg);
    procedure HandleKEYUP(const aMessage : TMsg);
    function  HandlePOINTER(const aMessage : TMsg) : boolean;
    {$ENDIF}
  end;

var
  MainForm : TMainForm;

// This is a simple browser using the FireMonkey framework in OSR mode (off-screen rendering).
// It uses a external message pump, a different executable for the CEF subprocesses and
// the components from the Skia4Delphi project.

// In order to build this demo it's necessary to download and install the latest
// version of Skia4Delphi from here :
// https://www.skia4delphi.org/
// https://github.com/skia4delphi/skia4delphi

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
  System.SysUtils, System.Math, FMX.Platform, FMX.Platform.Win, FMX.Helpers.Win,
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
  GlobalCEFApp.EnableGPU                  := True;
  GlobalCEFApp.BrowserSubprocessPath      := 'FMXSkiaBrowser_sp.exe';
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;
  {$IFDEF DEBUG}
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {$ENDIF}

  // TFMXWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalFMXWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalFMXWorkScheduler := TFMXWorkScheduler.Create(nil);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

procedure TMainForm.FormCreate(Sender: TObject);
var
  TempMajorVer, TempMinorVer : DWORD;
begin
  TFMXApplicationService.AddPlatformService;

  // We need a control that can be focused when the browser has the focus.
  // The background panel can focus and receive all keyboard and mouse events.
  Panel1.CanFocus := True;

  FImage          := nil;
  FPopupImage     := nil;
  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FCanClose       := False;
  FClosing        := False;

  {$IFDEF MSWINDOWS}
  FAtLeastWin8 := GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and
                  ((TempMajorVer > 6) or
                   ((TempMajorVer = 6) and (TempMinorVer >= 2)));
  {$ELSE}
  FAtLeastWin8 := False;
  {$ENDIF}

  chrmosr.DefaultURL := AddressCb.Text;

  InitializeLastClick;

  {$IFDEF DELPHI17_UP}
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService) then
    FMouseWheelService := TPlatformServices.Current.GetPlatformService(IFMXMouseService) as IFMXMouseService;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);

  FImage      := nil;
  FPopupImage := nil;
end;

procedure TMainForm.FormHide(Sender: TObject);
begin
  chrmosr.SetFocus(False);
  chrmosr.WasHidden(True);
end;

procedure TMainForm.FormShow(Sender: TObject);
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

function TMainForm.GetMousePosition(var aPoint : TPointF) : boolean;
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

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  chrmosr.LoadURL(AddressCb.Text);
end;

procedure TMainForm.GoBtnEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TMainForm.Panel1Enter(Sender: TObject);
begin
  chrmosr.SetFocus(True);
end;

procedure TMainForm.Panel1Exit(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  chrmosr.WasResized;
end;

procedure TMainForm.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TMainForm.Panel1MouseDown(Sender : TObject;
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
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
    end;
end;

procedure TMainForm.Panel1MouseUp(Sender : TObject;
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
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
    end;
end;

procedure TMainForm.Panel1MouseMove(Sender : TObject;
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
      chrmosr.SendMouseMoveEvent(@TempEvent, False);
    end;
end;

procedure TMainForm.Panel1MouseLeave(Sender: TObject);
var
  TempEvent  : TCefMouseEvent;
  TempPoint  : TPointF;
  TempTime   : integer;
begin
  if GetMousePosition(TempPoint) then
    begin
      TempPoint := Panel1.ScreenToLocal(TempPoint);

      if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

      TempEvent.x         := round(TempPoint.x);
      TempEvent.y         := round(TempPoint.y);
      TempEvent.modifiers := GetCefMouseModifiers;
      chrmosr.SendMouseMoveEvent(@TempEvent, True);
    end;
end;

procedure TMainForm.Panel1MouseWheel(    Sender      : TObject;
                                         Shift       : TShiftState;
                                         WheelDelta  : Integer;
                                     var Handled     : Boolean);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPointF;
begin
  if Panel1.IsFocused and GetMousePosition(TempPoint) then
    begin
      TempPoint           := Panel1.ScreenToLocal(TempPoint);
      TempEvent.x         := round(TempPoint.x);
      TempEvent.y         := round(TempPoint.y);
      TempEvent.modifiers := getModifiers(Shift);
      chrmosr.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
    end;
end;

procedure TMainForm.Panel1KeyDown(    Sender  : TObject;
                                  var Key     : Word;
                                  var KeyChar : Char;
                                      Shift   : TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if not(Panel1.IsFocused) then exit;

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

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end
   else
    if (Key <> 0) and (KeyChar = #0) and
       (Key in [vkLeft, vkRight, vkUp, vkDown]) then
      Key := 0;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if not(chrmosr.CreateBrowser) and not(chrmosr.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMainForm.AddressEdtEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TMainForm.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can enable the UI.
  Caption := 'FMX Skia Browser';
end;

procedure TMainForm.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;

  {$IFDEF MSWINDOWS}
  SendCompMessage(WM_CLOSE);
  {$ELSE}
  TThread.ForceQueue(nil,
    procedure
    begin
      close;
    end);
  {$ENDIF}
end;

procedure TMainForm.chrmosrBeforePopup(      Sender             : TObject;
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
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.chrmosrCanFocus(Sender: TObject);
begin
  // The browser required some time to create associated internal objects
  // before being able to accept the focus. Now we can set the focus on the
  // TBufferPanel control
  if Panel1.IsFocused then
    chrmosr.SetFocus(True)
   else
    Panel1.SetFocus;
end;

procedure TMainForm.chrmosrCursorChange(      Sender           : TObject;
                                        const browser          : ICefBrowser;
                                              cursor_          : TCefCursorHandle;
                                              cursorType       : TCefCursorType;
                                        const customCursorInfo : PCefCursorInfo;
                                        var   aResult          : Boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TMainForm.chrmosrGetScreenInfo(      Sender     : TObject;
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

  screenInfo.device_scale_factor := GlobalCEFApp.DeviceScaleFactor;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TMainForm.chrmosrGetScreenPoint(      Sender  : TObject;
                                          const browser : ICefBrowser;
                                                viewX   : Integer;
                                                viewY   : Integer;
                                          var   screenX : Integer;
                                          var   screenY : Integer;
                                          out   Result  : Boolean);
var
  TempScreenPt, TempViewPt : TPointF;
  TempScale : single;
begin
  TempScale    := RealScreenScale;
  TempViewPt.x := viewX;
  TempViewPt.y := viewY;
  TempScreenPt := Panel1.LocalToScreen(TempViewPt);
  screenX      := LogicalToDevice(round(TempScreenPt.x), TempScale);
  screenY      := LogicalToDevice(round(TempScreenPt.y), TempScale);
  Result       := True;
end;

procedure TMainForm.chrmosrGetViewRect(      Sender  : TObject;
                                       const browser : ICefBrowser;
                                       var   rect    : TCefRect);
begin
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := round(Panel1.Width);
  rect.height := round(Panel1.Height);
end;

procedure TMainForm.chrmosrPaint(      Sender          : TObject;
                                 const browser         : ICefBrowser;
                                       type_           : TCefPaintElementType;
                                       dirtyRectsCount : NativeUInt;
                                 const dirtyRects      : PCefRectArray;
                                 const buffer          : Pointer;
                                       width           : Integer;
                                       height          : Integer);
var
  TempMustResize : boolean;
  TempImageInfo  : TSkImageInfo;
begin
  case type_ of
    PET_VIEW :
      begin
        TempMustResize := (FImageInfo.Width <> width) or (FImageInfo.Height <> height);
        FImageInfo     := TSkImageInfo.Create(width, height);
        FImage         := TSkImage.MakeRasterCopy(FImageInfo, buffer, FImageInfo.MinRowBytes);

        TThread.ForceQueue(nil, DoRedraw);

        if TempMustResize then
          TThread.ForceQueue(nil, DoResize);
      end;

    PET_POPUP :
      begin
        TempImageInfo := TSkImageInfo.Create(width, height);
        FPopupImage   := TSkImage.MakeRasterCopy(TempImageInfo, buffer, TempImageInfo.MinRowBytes);
        TThread.ForceQueue(nil, DoRedraw);
      end;
  end;
end;

procedure TMainForm.chrmosrPopupShow(      Sender  : TObject;
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

procedure TMainForm.chrmosrPopupSize(      Sender  : TObject;
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

procedure TMainForm.chrmosrTooltip(      Sender  : TObject;
                                   const browser : ICefBrowser;
                                   var   text    : ustring;
                                   out   Result  : Boolean);
begin
  Panel1.Hint     := text;
  Panel1.ShowHint := (length(text) > 0);
  Result          := True;
end;

procedure TMainForm.DoRedraw;
begin
  SkPaintBox1.Redraw;
end;

procedure TMainForm.DoResize;
begin
  chrmosr.WasResized;
end;

function TMainForm.RealScreenScale: single;
var
  TempHandle: TCefWindowHandle;
begin
  if assigned(GlobalCEFApp) then
    result := GlobalCEFApp.DeviceScaleFactor
   else
    result := 1;

  TempHandle := FmxHandleToHWND(Handle);

  if (TempHandle <> 0) then
    Result := GetWndScale(TempHandle);
end;

procedure TMainForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  PositionChanged: Boolean;
begin
  PositionChanged := (ALeft <> Left) or (ATop <> Top);

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if PositionChanged then NotifyMoveOrResizeStarted;
end;

procedure TMainForm.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  TempRect : TRectF;
begin
  if not(assigned(FImage)) then exit;

  ACanvas.Save;
  try
    TempRect.Left   := 0;
    TempRect.Top    := 0;
    TempRect.Right  := FImage.Width  / GlobalCEFApp.DeviceScaleFactor;
    TempRect.Bottom := FImage.Height / GlobalCEFApp.DeviceScaleFactor;

    ACanvas.ClipRect(ADest);
    ACanvas.DrawImageRect(FImage, TempRect);

    if FShowPopUp and assigned(FPopupImage) then
      begin
        TempRect.Left   := FPopUpRect.Left / GlobalCEFApp.DeviceScaleFactor;
        TempRect.Top    := FPopUpRect.Top  / GlobalCEFApp.DeviceScaleFactor;
        TempRect.Right  := TempRect.Left + (FPopupImage.Width  / GlobalCEFApp.DeviceScaleFactor);
        TempRect.Bottom := TempRect.Top  + (FPopupImage.Height / GlobalCEFApp.DeviceScaleFactor);

        ACanvas.DrawImageRect(FPopupImage, TempRect);
      end;
  finally
    ACanvas.Restore;
  end;
end;

procedure TMainForm.NotifyMoveOrResizeStarted;
begin
  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.SendCaptureLostEvent;
begin
  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
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

procedure TMainForm.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := TMouseButton.mbLeft;
end;

function TMainForm.CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;
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
procedure TMainForm.HandleSYSCHAR(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if Panel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_CHAR;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TMainForm.HandleSYSKEYDOWN(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if Panel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TMainForm.HandleSYSKEYUP(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if Panel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_KEYUP;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TMainForm.HandleKEYDOWN(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if Panel1.IsFocused then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TMainForm.HandleKEYUP(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if Panel1.IsFocused then
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

          chrmosr.SendKeyEvent(@TempKeyEvent);
        end;

      TempKeyEvent.kind                    := KEYEVENT_KEYUP;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := integer(aMessage.wParam);
      TempKeyEvent.native_key_code         := integer(aMessage.lParam);
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

function TMainForm.HandlePOINTER(const aMessage : TMsg) : boolean;
begin
  Result := Panel1.IsFocused and
            (GlobalCEFApp <> nil) and
            ArePointerEventsSupported and
            HandlePointerEvent(aMessage);
end;

function TMainForm.SendCompMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
var
  TempHandle : TWinWindowHandle;
begin
  TempHandle := WindowHandleToPlatform(Handle);
  Result     := WinApi.Windows.PostMessage(TempHandle.Wnd, aMsg, aWParam, aLParam);
end;

function TMainForm.ArePointerEventsSupported : boolean;
begin
  Result := FAtLeastWin8 and
            (@GetPointerType      <> nil) and
            (@GetPointerTouchInfo <> nil) and
            (@GetPointerPenInfo   <> nil);
end;

function TMainForm.HandlePointerEvent(const aMessage : TMsg) : boolean;
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

function TMainForm.HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
var
  TempPenInfo    : POINTER_PEN_INFO;
  TempTouchEvent : TCefTouchEvent;
  TempPointF     : TPointF;
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

  TempScale    := RealScreenScale;
  TempPointF.x := DeviceToLogical(TempPenInfo.pointerInfo.ptPixelLocation.x, TempScale);
  TempPointF.y := DeviceToLogical(TempPenInfo.pointerInfo.ptPixelLocation.y, TempScale);

  TempPointF       := Panel1.ScreenToLocal(TempPointF);
  TempTouchEvent.x := round(TempPointF.x);
  TempTouchEvent.y := round(TempPointF.y);

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;

function TMainForm.HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean;
var
  TempTouchInfo  : POINTER_TOUCH_INFO;
  TempTouchEvent : TCefTouchEvent;
  TempPointF     : TPointF;
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

  TempScale    := RealScreenScale;
  TempPointF.x := DeviceToLogical(TempTouchInfo.pointerInfo.ptPixelLocation.x, TempScale);
  TempPointF.y := DeviceToLogical(TempTouchInfo.pointerInfo.ptPixelLocation.y, TempScale);

  TempPointF       := Panel1.ScreenToLocal(TempPointF);
  TempTouchEvent.x := round(TempPointF.x);
  TempTouchEvent.y := round(TempPointF.y);

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;
{$ENDIF}

end.
