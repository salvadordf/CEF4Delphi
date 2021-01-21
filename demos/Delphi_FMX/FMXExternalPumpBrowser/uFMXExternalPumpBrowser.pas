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

unit uFMXExternalPumpBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  System.Types, System.UITypes, System.Classes, System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation,
  {$IFDEF DELPHI17_UP}
  FMX.Graphics,
  {$ENDIF}
  uCEFFMXChromium, uCEFFMXBufferPanel, uCEFFMXWorkScheduler,
  uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFChromiumCore, FMX.Layouts;

type
  TFMXExternalPumpBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    chrmosr: TFMXChromium;
    Timer1: TTimer;
    SaveDialog1: TSaveDialog;
    Panel1: TFMXBufferPanel;
    Layout1: TLayout;
    GoBtn: TButton;
    SnapshotBtn: TButton;

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
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult: Boolean);

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
    {$IFDEF MSWINDOWS}
    function  SendCompMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    function  ArePointerEventsSupported : boolean;
    function  HandlePenEvent(const aID : uint32; aMsg : cardinal) : boolean;
    function  HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean; overload;
    function  HandlePointerEvent(const aMessage : TMsg) : boolean;
    {$ENDIF}

  public
    procedure DoResize;
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
  if (GlobalFMXWorkScheduler <> nil) then GlobalFMXWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;
begin
  // TFMXWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalFMXWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalFMXWorkScheduler := TFMXWorkScheduler.Create(nil);

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;
  //GlobalCEFApp.EnableGPU                  := True;
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
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

  {$IFDEF MSWINDOWS}
  FAtLeastWin8 := GetWindowsMajorMinorVersion(TempMajorVer, TempMinorVer) and
                  ((TempMajorVer > 6) or
                   ((TempMajorVer = 6) and (TempMinorVer >= 2)));
  {$ELSE}
  FAtLeastWin8 := False;
  {$ENDIF}

  chrmosr.DefaultURL := AddressEdt.Text;

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
  chrmosr.SendFocusEvent(False);
  chrmosr.WasHidden(True);
end;

procedure TFMXExternalPumpBrowserFrm.FormShow(Sender: TObject);
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

  chrmosr.LoadURL(AddressEdt.Text);
end;

procedure TFMXExternalPumpBrowserFrm.GoBtnEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
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
  chrmosr.SendFocusEvent(True);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1Exit(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1KeyDown(    Sender  : TObject;
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
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
    end;
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

procedure TFMXExternalPumpBrowserFrm.Panel1MouseLeave(Sender: TObject);
var
  TempEvent  : TCefMouseEvent;
  TempPoint  : TPointF;
  TempTime   : integer;
begin
  if GetMousePosition(TempPoint) then
    begin
      TempPoint := Panel1.ScreenToClient(TempPoint);

      if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

      TempEvent.x         := round(TempPoint.x);
      TempEvent.y         := round(TempPoint.y);
      TempEvent.modifiers := GetCefMouseModifiers;
      chrmosr.SendMouseMoveEvent(@TempEvent, True);
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
      chrmosr.SendMouseMoveEvent(@TempEvent, False);
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
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseWheel(    Sender      : TObject;
                                                          Shift       : TShiftState;
                                                          WheelDelta  : Integer;
                                                      var Handled     : Boolean);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPointF;
begin
  if Panel1.IsFocused and GetMousePosition(TempPoint) then
    begin
      TempPoint           := Panel1.ScreenToClient(TempPoint);
      TempEvent.x         := round(TempPoint.x);
      TempEvent.y         := round(TempPoint.y);
      TempEvent.modifiers := getModifiers(Shift);
      chrmosr.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
    end;
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
  chrmosr.SendFocusEvent(False);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can enable the UI.
  Caption            := 'FMX External Pump Browser';
  AddressPnl.Enabled := True;
  Panel1.SetFocus;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;

  {$IFDEF MSWINDOWS}
  SendCompMessage(WM_CLOSE);
  {$ELSE}
  TThread.Queue(nil, procedure
                     begin
                       close
                     end);
  {$ENDIF}
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrBeforePopup(      Sender             : TObject;
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

// TO-DO: The "cursor" parameter should be TCefCursorHandle but Delphi shows a warning if it's not declared as HICON.
procedure TFMXExternalPumpBrowserFrm.chrmosrCursorChange(      Sender           : TObject;
                                                         const browser          : ICefBrowser;
                                                               cursor           : HICON;
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
begin
  // TFMXBufferPanel.ClientToScreen applies the scale factor. No need to call LogicalToDevice to set TempViewPt.
  TempViewPt.x := viewX;
  TempViewPt.y := viewY;
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
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
              begin
                {$IFDEF MSWINDOWS}
                SendCompMessage(CEF_PENDINGRESIZE);
                {$ELSE}
                TThread.Queue(nil, DoResize);
                {$ENDIF}
              end;

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
  try
    if (FResizeCS <> nil) then
      begin
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
      end;
  finally
    if (FResizeCS <> nil) then FResizeCS.Release;
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
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
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

procedure TFMXExternalPumpBrowserFrm.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TFMXExternalPumpBrowserFrm.SnapshotBtnEnter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

{$IFDEF MSWINDOWS}
procedure TFMXExternalPumpBrowserFrm.HandleSYSCHAR(const aMessage : TMsg);
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

procedure TFMXExternalPumpBrowserFrm.HandleSYSKEYDOWN(const aMessage : TMsg);
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

procedure TFMXExternalPumpBrowserFrm.HandleSYSKEYUP(const aMessage : TMsg);
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

procedure TFMXExternalPumpBrowserFrm.HandleKEYDOWN(const aMessage : TMsg);
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

procedure TFMXExternalPumpBrowserFrm.HandleKEYUP(const aMessage : TMsg);
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

  TempPoint        := Panel1.ScreenToClient(TempPenInfo.pointerInfo.ptPixelLocation);
  // TFMXBufferPanel.ScreenToClient applies the scale factor. No need to call DeviceToLogical to set TempTouchEvent.
  TempTouchEvent.x := TempPoint.x;
  TempTouchEvent.y := TempPoint.y;

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;

function TFMXExternalPumpBrowserFrm.HandleTouchEvent(const aID : uint32; aMsg : cardinal) : boolean;
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

  TempPoint        := Panel1.ScreenToClient(TempTouchInfo.pointerInfo.ptPixelLocation);
  // TFMXBufferPanel.ScreenToClient applies the scale factor. No need to call DeviceToLogical to set TempTouchEvent.
  TempTouchEvent.x := TempPoint.x;
  TempTouchEvent.y := TempPoint.y;

  chrmosr.SendTouchEvent(@TempTouchEvent);
end;
{$ENDIF}

end.
