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
//        Copyright © 2019 Salvador Diaz Fau. All rights reserved.
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
  uCEFInterfaces, uCEFTypes, uCEFConstants;

type
  TFMXExternalPumpBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    chrmosr: TFMXChromium;
    Timer1: TTimer;
    Panel2: TPanel;
    GoBtn: TButton;
    SnapshotBtn: TButton;
    SaveDialog1: TSaveDialog;
    Panel1: TFMXBufferPanel;

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
    procedure Panel1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure Panel1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; width, height: Integer);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);

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
    function  SendCompMessage(aMsg : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;
    procedure InitializeLastClick;
    function  CancelPreviousClick(const x, y : single; var aCurrentTime : integer) : boolean;

  public
    procedure DoResize;
    procedure DoBrowserCreated;
    procedure NotifyMoveOrResizeStarted;
    procedure SendCaptureLostEvent;
    procedure HandleSYSCHAR(const aMessage : TMsg);
    procedure HandleSYSKEYDOWN(const aMessage : TMsg);
    procedure HandleSYSKEYUP(const aMessage : TMsg);
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
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and calls chrmosr.CloseBrowser(True).
// 2- chrmosr.CloseBrowser(True) will trigger chrmosr.OnClose and we have to
//    set "Result" to false and CEF3 will destroy the internal browser immediately.
// 3- chrmosr.OnBeforeClose is triggered because the internal browser was destroyed.
//    Now we set FCanClose to True and send WM_CLOSE to the form.

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
  GlobalCEFApp.FlashEnabled               := False;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;
  GlobalCEFApp.DisableFeatures            := 'NetworkService,OutOfBlinkCors';
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

procedure TFMXExternalPumpBrowserFrm.Panel1Enter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(True);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1Exit(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1KeyDown(Sender : TObject;
                                                   var Key     : Word;
                                                   var KeyChar : Char;
                                                       Shift   : TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if not(Panel1.IsFocused) or (chrmosr = nil) then exit;

  if (Key <> 0) and (KeyChar = #0) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := getModifiers(Shift);
      TempKeyEvent.windows_key_code        := Key;
      TempKeyEvent.native_key_code         := 0;
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);

      if (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB]) then Key := 0;
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1KeyUp(Sender : TObject;
                                                 var Key     : Word;
                                                 var KeyChar : Char;
                                                     Shift   : TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if not(Panel1.IsFocused) or (chrmosr = nil) then exit;

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
    if (Key <> 0) and (KeyChar = #0) then
      begin
        TempKeyEvent.kind                    := KEYEVENT_KEYUP;
        TempKeyEvent.modifiers               := getModifiers(Shift);
        TempKeyEvent.windows_key_code        := Key;
        TempKeyEvent.native_key_code         := 0;
        TempKeyEvent.is_system_key           := ord(False);
        TempKeyEvent.character               := #0;
        TempKeyEvent.unmodified_character    := #0;
        TempKeyEvent.focus_on_editable_field := ord(False);

        chrmosr.SendKeyEvent(@TempKeyEvent);
      end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseDown(Sender : TObject;
                                                     Button : TMouseButton;
                                                     Shift  : TShiftState;
                                                     X, Y   : Single);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) and (chrmosr <> nil) then
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

procedure TFMXExternalPumpBrowserFrm.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) and (chrmosr <> nil) then
    begin
      GetCursorPos(TempPoint);
      TempPoint := Panel1.ScreenToclient(TempPoint);

      if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

      TempEvent.x         := TempPoint.x;
      TempEvent.y         := TempPoint.y;
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
  if (GlobalCEFApp <> nil) and (chrmosr <> nil) then
    begin
      if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
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
  if (GlobalCEFApp <> nil) and (chrmosr <> nil) then
    begin
      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Shift);
      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseWheel(Sender      : TObject;
                                                      Shift       : TShiftState;
                                                      WheelDelta  : Integer;
                                                      var Handled : Boolean);
var
  TempEvent  : TCefMouseEvent;
  TempPointF : TPointF;
begin
  if Panel1.IsFocused and (GlobalCEFApp <> nil) and (chrmosr <> nil) then
    begin
      {$IFDEF DELPHI17_UP}
      if (FMouseWheelService <> nil) then
        TempPointF := FMouseWheelService.GetMousePos
       else
        exit;
      {$ELSE}
      TempPointF := Platform.GetMousePos;
      {$ENDIF}

      TempEvent.x         := round(TempPointF.x);
      TempEvent.y         := round(TempPointF.y);
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

procedure TFMXExternalPumpBrowserFrm.chrmosrAfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the
  // main form to load the initial web page.
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCompMessage(WM_CLOSE);
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

procedure TFMXExternalPumpBrowserFrm.chrmosrCursorChange(Sender : TObject;
                                                         const browser          : ICefBrowser;
                                                               cursor           : HICON;
                                                               cursorType       : TCefCursorType;
                                                         const customCursorInfo : PCefCursorInfo);
begin
  Panel1.Cursor := GefCursorToWindowsCursor(cursorType);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrGetScreenInfo(Sender : TObject;
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
      TempRect.width  := round(Panel1.Width);
      TempRect.height := round(Panel1.Height);

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

procedure TFMXExternalPumpBrowserFrm.chrmosrGetScreenPoint(Sender : TObject;
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

procedure TFMXExternalPumpBrowserFrm.chrmosrGetViewRect(Sender : TObject;
                                                        const browser : ICefBrowser;
                                                        var   rect    : TCefRect);
begin
  if (GlobalCEFApp <> nil) then
    begin
      rect.x      := 0;
      rect.y      := 0;
      rect.width  := round(Panel1.Width);
      rect.height := round(Panel1.Height);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrPaint(Sender : TObject;
                                                  const browser         : ICefBrowser;
                                                        kind            : TCefPaintElementType;
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
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      try
        if (kind = PET_POPUP) then
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
              Panel1.BufferDraw(FPopUpRect.Left, FPopUpRect.Top, FPopUpBitmap);
          end;

        if (kind = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then SendCompMessage(CEF_PENDINGRESIZE);

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

procedure TFMXExternalPumpBrowserFrm.chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; show: Boolean);
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

procedure TFMXExternalPumpBrowserFrm.chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
begin
  if (GlobalCEFApp <> nil) then
    begin
      FPopUpRect.Left   := rect.x;
      FPopUpRect.Top    := rect.y;
      FPopUpRect.Right  := rect.x + LogicalToDevice(rect.width,  GlobalCEFApp.DeviceScaleFactor)  - 1;
      FPopUpRect.Bottom := rect.y + LogicalToDevice(rect.height, GlobalCEFApp.DeviceScaleFactor) - 1;
    end;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var text: ustring; out Result: Boolean);
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

procedure TFMXExternalPumpBrowserFrm.NotifyMoveOrResizeStarted;
begin
  if (chrmosr <> nil) then chrmosr.NotifyMoveOrResizeStarted;
end;

procedure TFMXExternalPumpBrowserFrm.SendCaptureLostEvent;
begin
  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TFMXExternalPumpBrowserFrm.HandleSYSCHAR(const aMessage : TMsg);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if Panel1.IsFocused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_CHAR;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := aMessage.wParam;
      TempKeyEvent.native_key_code         := aMessage.lParam;
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
      TempKeyEvent.windows_key_code        := aMessage.wParam;
      TempKeyEvent.native_key_code         := aMessage.lParam;
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
      TempKeyEvent.windows_key_code        := aMessage.wParam;
      TempKeyEvent.native_key_code         := aMessage.lParam;
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.DoBrowserCreated;
begin
  Caption            := 'FMX External Pump Browser';
  AddressPnl.Enabled := True;
  Panel1.SetFocus;
  LoadURL;
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

function TFMXExternalPumpBrowserFrm.SendCompMessage(aMsg, wParam : cardinal; lParam : integer) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHandle : TWinWindowHandle;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TempHandle := WindowHandleToPlatform(Handle);
  Result     := WinApi.Windows.PostMessage(TempHandle.Wnd, aMsg, wParam, lParam);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TFMXExternalPumpBrowserFrm.InitializeLastClick;
begin
  FLastClickCount   := 0;
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

end.
