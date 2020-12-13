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
//        Copyright © 2020 Salvador Diaz Fau. All rights reserved.
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

{$MODE OBJFPC}{$H+}

{$I cef.inc}

interface

uses
  Classes, SysUtils, LCLType, Variants, SyncObjs, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Types,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFBufferPanel,
  uCEFWorkScheduler;

type

  { TOSRExternalPumpBrowserFrm }

  TOSRExternalPumpBrowserFrm = class(TForm)
    NavControlPnl: TPanel;
    chrmosr: TChromium;
    ComboBox1: TComboBox;
    Panel2: TPanel;
    GoBtn: TButton;
    SnapshotBtn: TButton;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Panel1: TBufferPanel;

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Panel1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; aCursor: HICON; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);

    procedure SnapshotBtnClick(Sender: TObject);
    procedure SnapshotBtnEnter(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);

  protected
    FbFirst          : boolean;
    FPopUpBitmap     : TBitmap;
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FResizing        : boolean;
    FPendingResize   : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;
    FResizeCS        : TCriticalSection;

    function  SendCompMessage(aMsg : cardinal) : boolean;
    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;

    procedure BrowserCreated(Data: PtrInt);
    procedure BrowserCloseForm(Data: PtrInt);
    procedure PendingResize(Data: PtrInt);
    procedure PendingInvalidate(Data: PtrInt);

  public
    { Public declarations }
  end;

var
  OSRExternalPumpBrowserFrm : TOSRExternalPumpBrowserFrm;

// This is a simple browser in OSR mode (off-screen rendering).
// It was necessary to destroy the browser following the destruction sequence described in
// the MDIBrowser demo but in OSR mode there are some modifications.

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and calls chrmosr.CloseBrowser(True).
// 2- chrmosr.CloseBrowser(True) will trigger chrmosr.OnClose and we have to
//    set "Result" to false and CEF will destroy the internal browser immediately.
// 3- chrmosr.OnBeforeClose is triggered because the internal browser was destroyed.
//    FCanClose is set to True and sends WM_CLOSE to the form.

procedure CreateGlobalCEFApp;
procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);

implementation

{$R *.lfm}

uses
  Math,
  uCEFMiscFunctions, uCEFApplication;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then
     GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;     
var
  TempHome, TempBinDir : ustring;
begin
  TempHome     := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
  TempBinDir   := TempHome + 'Lazarus/CEF4Delphi/bin';

  // TCEFWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalCEFWorkScheduler := TCEFWorkScheduler.Create(nil);

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := @GlobalCEFApp_OnScheduleMessagePumpWork;      
  GlobalCEFApp.SetCurrentDir              := True;

  if DirectoryExists(TempBinDir) then
    begin
      GlobalCEFApp.FrameworkDirPath := TempBinDir;
      GlobalCEFApp.ResourcesDirPath := TempBinDir;
      GlobalCEFApp.LocalesDirPath   := TempBinDir + '/locales';
    end;

  // Add a debug log in the BIN directory
  GlobalCEFApp.LogFile     := 'cef.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_VERBOSE;
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
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TOSRExternalPumpBrowserFrm.Panel1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  TempKeyEvent : TCefKeyEvent;
  TempString   : UnicodeString;
begin
  if Panel1.Focused then
    begin
      TempString := UTF8Decode(UTF8Key);

      if (length(TempString) > 0) then
        begin
          TempKeyEvent.kind                    := KEYEVENT_CHAR;
          TempKeyEvent.modifiers               := EVENTFLAG_NONE;
          TempKeyEvent.windows_key_code        := ord(TempString[1]);
          TempKeyEvent.native_key_code         := 0;
          TempKeyEvent.is_system_key           := ord(False);
          TempKeyEvent.character               := #0;
          TempKeyEvent.unmodified_character    := #0;
          TempKeyEvent.focus_on_editable_field := ord(False);

          chrmosr.SendKeyEvent(@TempKeyEvent);
        end;
    end;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrCursorChange(Sender : TObject;
                                     const browser          : ICefBrowser;
                                           aCursor          : HICON;
                                           cursorType       : TCefCursorType;
                                     const customCursorInfo : PCefCursorInfo;
                                     var   aResult          : boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrGetScreenInfo(Sender : TObject;
                                      const browser    : ICefBrowser;
                                      var   screenInfo : TCefScreenInfo;
                                      out   Result     : Boolean);
var
  TempRect  : TCEFRect;
  TempScale : single;
begin
  TempScale       := Panel1.ScreenScale;
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  TempRect.height := DeviceToLogical(Panel1.Height, TempScale);

  screenInfo.device_scale_factor := TempScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrGetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
  TempScale : single;
begin
  TempScale    := Panel1.ScreenScale;
  TempViewPt.x := LogicalToDevice(viewX, TempScale);
  TempViewPt.y := LogicalToDevice(viewY, TempScale);
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrGetViewRect(Sender : TObject;
                                    const browser : ICefBrowser;
                                    var   rect    : TCefRect);
var
  TempScale : single;
begin
  TempScale   := Panel1.ScreenScale;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  rect.height := DeviceToLogical(Panel1.Height, TempScale);
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrPaint(Sender: TObject; const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth,
  aHeight: Integer);
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempBitmap : TBitmap;           
  TempSrcRect : TRect;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
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
            TempForcedResize := Panel1.UpdateBufferDimensions(aWidth, aHeight) or not(Panel1.BufferIsResized(False));

            TempBitmap := Panel1.Buffer;
            TempBitmap.BeginUpdate;

            TempWidth  := Panel1.BufferWidth;
            TempHeight := Panel1.BufferHeight;
          end;

        SrcStride := aWidth * SizeOf(TRGBQuad);
        n         := 0;

        while (n < dirtyRectsCount) do
          begin
            if (dirtyRects^[n].x >= 0) and (dirtyRects^[n].y >= 0) then
              begin
                TempLineSize := min(dirtyRects^[n].width, TempWidth - dirtyRects^[n].x) * SizeOf(TRGBQuad);

                if (TempLineSize > 0) then
                  begin
                    TempSrcOffset := ((dirtyRects^[n].y * aWidth) + dirtyRects^[n].x) * SizeOf(TRGBQuad);
                    TempDstOffset := (dirtyRects^[n].x * SizeOf(TRGBQuad));

                    src := @PByte(buffer)[TempSrcOffset];

                    i := 0;
                    j := min(dirtyRects^[n].height, TempHeight - dirtyRects^[n].y);

                    while (i < j) do
                      begin
                        TempBufferBits := TempBitmap.Scanline[dirtyRects^[n].y + i];
                        dst            := @PByte(TempBufferBits)[TempDstOffset];

                        Move(src^, dst^, TempLineSize);

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

            Panel1.BufferDraw(FPopUpBitmap, TempSrcRect, FPopUpRect);
          end;

        Panel1.EndBufferDraw;

        SendCompMessage(CEF_PENDINGINVALIDATE);

        if (kind = PET_VIEW) then
          begin
            if TempForcedResize or FPendingResize then
              SendCompMessage(CEF_PENDINGRESIZE);

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
                                        aShow   : Boolean);
begin
  if aShow then
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
  LogicalToDevice(rect^, Panel1.ScreenScale);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TOSRExternalPumpBrowserFrm.chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
begin
  Panel1.hint     := aText;
  Panel1.ShowHint := (length(aText) > 0);
  Result          := True;
end;

procedure TOSRExternalPumpBrowserFrm.ComboBox1Enter(Sender: TObject);
begin
  chrmosr.SendFocusEvent(False);
end;

function TOSRExternalPumpBrowserFrm.SendCompMessage(aMsg : cardinal) : boolean;
begin
  case aMsg of
    CEF_AFTERCREATED      : Application.QueueAsyncCall(@BrowserCreated, 0);
    CEF_BEFORECLOSE       : Application.QueueAsyncCall(@BrowserCloseForm, 0);
    CEF_PENDINGRESIZE     : Application.QueueAsyncCall(@PendingResize, 0);
    CEF_PENDINGINVALIDATE : Application.QueueAsyncCall(@PendingInvalidate, 0);
  end;
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

procedure TOSRExternalPumpBrowserFrm.BrowserCreated(Data: PtrInt);
begin
  Caption               := 'Simple Lazarus OSR Browser';
  NavControlPnl.Enabled := True;
end;

procedure TOSRExternalPumpBrowserFrm.BrowserCloseForm(Data: PtrInt);
begin
  Close;
end;

procedure TOSRExternalPumpBrowserFrm.PendingResize(Data: PtrInt);
begin
  DoResize;
end;

procedure TOSRExternalPumpBrowserFrm.PendingInvalidate(Data: PtrInt);
begin
  Panel1.Invalidate;
end;

procedure TOSRExternalPumpBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      chrmosr.CloseBrowser(True);
    end;
end;

procedure TOSRExternalPumpBrowserFrm.FormCreate(Sender: TObject);
begin
  FbFirst         := False;
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
  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);
  if (FResizeCS    <> nil) then FreeAndNil(FResizeCS);
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
      chrmosr.DefaultURL              := ComboBox1.Text;

      if not(chrmosr.CreateBrowser(nil, '')) then
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
  Panel1.SetFocus;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
end;

procedure TOSRExternalPumpBrowserFrm.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TOSRExternalPumpBrowserFrm.Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);
end;

procedure TOSRExternalPumpBrowserFrm.Panel1Resize(Sender: TObject);
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

procedure TOSRExternalPumpBrowserFrm.Panel1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if (Key <> 0) and (chrmosr <> nil) then
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

procedure TOSRExternalPumpBrowserFrm.Panel1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if (Key <> 0) and (chrmosr <> nil) then
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

procedure TOSRExternalPumpBrowserFrm.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
begin
  TempEvent.x         := MousePos.x;
  TempEvent.y         := MousePos.y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  chrmosr.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
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

  if not(chrmosr.CreateBrowser(nil, '')) and
     not(chrmosr.Initialized) then
    Timer1.Enabled := True;
end;

end.
