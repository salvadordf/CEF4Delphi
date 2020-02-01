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

unit uSimpleOSRBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLType, Types, SyncObjs,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFBufferPanel,
  uCEFChromiumEvents;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddressEdt: TEdit;
    Panel1: TBufferPanel;
    Chromium1: TChromium;
    GoBtn: TButton;
    AddressPnl: TPanel;
    Timer1: TTimer;
    procedure AddressEdtEnter(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Panel1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser
      );
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const targetUrl, targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
      var client: ICefClient; var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
      var Result: Boolean);
    procedure Chromium1CursorChange(Sender: TObject;
      const browser: ICefBrowser; cursor_: TCefCursorHandle;
      cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
    procedure Chromium1GetScreenInfo(Sender: TObject;
      const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
      Result: Boolean);
    procedure Chromium1GetScreenPoint(Sender: TObject;
      const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
      screenY: Integer; out Result: Boolean);
    procedure Chromium1GetViewRect(Sender: TObject; const browser: ICefBrowser;
      var rect: TCefRect);
    procedure Chromium1OpenUrlFromTab(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; out Result: Boolean);
    procedure Chromium1Paint(Sender: TObject; const browser: ICefBrowser;
      type_: TCefPaintElementType; dirtyRectsCount: NativeUInt;
      const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth,
      aHeight: Integer);
    procedure Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser;
      aShow: Boolean);
    procedure Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser;
      const rect: PCefRect);
    procedure Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser;
      var aText: ustring; out Result: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private             

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
    FDeviceBounds    : TCefRectDynArray;
    FSelectedRange   : TCefRange;

    function  SendCompMessage(aMsg : cardinal) : boolean;
    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;

    procedure BrowserCreated;
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure PendingResizeMsg(Data: PtrInt);
    procedure PendingInvalidateMsg(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  Math,
  uCEFMiscFunctions, uCEFApplication;

procedure CreateGlobalCEFApp;
var
  TempHome, TempBinDir : ustring;
begin
  TempHome     := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
  TempBinDir   := TempHome + 'Lazarus/CEF4Delphi/bin';

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
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

{ TForm1 }

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  TThread.Queue(nil, @BrowserCreated);
end;

procedure TForm1.Panel1UTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
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

          Chromium1.SendKeyEvent(@TempKeyEvent);
        end;
    end;
end;

procedure TForm1.AddressEdtEnter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TForm1.Panel1Enter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(True);
end;

procedure TForm1.Panel1Exit(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

procedure TForm1.Panel1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if (Key <> 0) and (Chromium1 <> nil) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_RAWKEYDOWN;
      TempKeyEvent.modifiers               := getModifiers(Shift);
      TempKeyEvent.windows_key_code        := Key;
      TempKeyEvent.native_key_code         := 0;
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      Chromium1.SendKeyEvent(@TempKeyEvent);

      if (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB]) then Key := 0;
    end;
end;

procedure TForm1.Panel1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if (Key <> 0) and (Chromium1 <> nil) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_KEYUP;
      TempKeyEvent.modifiers               := getModifiers(Shift);
      TempKeyEvent.windows_key_code        := Key;
      TempKeyEvent.native_key_code         := 0;
      TempKeyEvent.is_system_key           := ord(False);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      Chromium1.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) and (Chromium1 <> nil) then
    begin
      Panel1.SetFocus;

      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
    end;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if (GlobalCEFApp <> nil) and (Chromium1 <> nil) then
    begin
      TempEvent.x         := x;
      TempEvent.y         := y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseMoveEvent(@TempEvent, False);
    end;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) and (Chromium1 <> nil) then
    begin
      TempEvent.x         := X;
      TempEvent.y         := Y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);
    end;
end;

procedure TForm1.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
begin
  if (GlobalCEFApp <> nil) and (Chromium1 <> nil) then
    begin
      TempEvent.x         := MousePos.x;
      TempEvent.y         := MousePos.y;
      TempEvent.modifiers := getModifiers(Shift);
      DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
      Chromium1.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
    end;
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  DoResize;
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

procedure TForm1.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
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

procedure TForm1.Chromium1CursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor_: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
end;

procedure TForm1.Chromium1GetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
  Result: Boolean);
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

procedure TForm1.Chromium1GetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
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

procedure TForm1.Chromium1GetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
begin
  if (GlobalCEFApp <> nil) then
    begin
      rect.x      := 0;
      rect.y      := 0;
      rect.width  := DeviceToLogical(Panel1.Width,  GlobalCEFApp.DeviceScaleFactor);
      rect.height := DeviceToLogical(Panel1.Height, GlobalCEFApp.DeviceScaleFactor);
    end;
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1Paint(Sender: TObject; const browser: ICefBrowser;
  type_: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer
  );
var
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempBitmap : TBitmap;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      begin
        if (type_ = PET_POPUP) then
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

        if (TempBufferBits <> nil) then
          begin
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
              Panel1.BufferDraw(FPopUpRect.Left, FPopUpRect.Top, FPopUpBitmap);
          end;

        Panel1.EndBufferDraw;

        if HandleAllocated then SendCompMessage(CEF_PENDINGINVALIDATE);

        if (type_ = PET_VIEW) then
          begin
            if (TempForcedResize or FPendingResize) and
               HandleAllocated then
              SendCompMessage(CEF_PENDINGRESIZE);

            FResizing      := False;
            FPendingResize := False;
          end;
      end;
  finally
    FResizeCS.Release;
  end;
end;

procedure TForm1.Chromium1PopupShow(Sender: TObject;
  const browser: ICefBrowser; aShow: Boolean);
begin
  if aShow then
    FShowPopUp := True
   else
    begin
      FShowPopUp := False;
      FPopUpRect := rect(0, 0, 0, 0);

      if (Chromium1 <> nil) then Chromium1.Invalidate(PET_VIEW);
    end;
end;

procedure TForm1.Chromium1PopupSize(Sender: TObject;
  const browser: ICefBrowser; const rect: PCefRect);
begin
  if (GlobalCEFApp <> nil) then
    begin
      LogicalToDevice(rect^, GlobalCEFApp.DeviceScaleFactor);

      FPopUpRect.Left   := rect^.x;
      FPopUpRect.Top    := rect^.y;
      FPopUpRect.Right  := rect^.x + rect^.width  - 1;
      FPopUpRect.Bottom := rect^.y + rect^.height - 1;
    end;
end;

procedure TForm1.Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser;
  var aText: ustring; out Result: Boolean);
begin
  Panel1.hint     := aText;
  Panel1.ShowHint := (length(aText) > 0);
  Result          := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FbFirst         := False;
  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FDeviceBounds   := nil;      
  FResizeCS       := TCriticalSection.Create;

  FSelectedRange.from   := 0;
  FSelectedRange.to_    := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if (FPopUpBitmap <> nil) then FreeAndNil(FPopUpBitmap);
  if (FResizeCS    <> nil) then FreeAndNil(FResizeCS);

  if (FDeviceBounds <> nil) then
    begin
      Finalize(FDeviceBounds);
      FDeviceBounds := nil;
    end;
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
  Chromium1.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if Chromium1.Initialized then
    begin
      Chromium1.WasHidden(False);
      Chromium1.SendFocusEvent(True);
    end
   else
    begin
      // opaque white background color
      Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);  
      Chromium1.DefaultURL := UTF8Decode(AddressEdt.Text);

      if not(Chromium1.CreateBrowser(nil, '')) then Timer1.Enabled := True;
    end;
end;

procedure TForm1.GoBtnEnter(Sender: TObject);
begin
  Chromium1.SendFocusEvent(False);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if not(Chromium1.CreateBrowser(nil, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.BrowserCreated;
begin
  Caption            := 'Simple OSR Browser';
  AddressPnl.Enabled := True;
end;

procedure TForm1.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;  

procedure TForm1.PendingResizeMsg(Data: PtrInt);
begin
  DoResize;
end;

procedure TForm1.PendingInvalidateMsg(Data: PtrInt);
begin
  Panel1.Invalidate;
end;

function TForm1.SendCompMessage(aMsg : cardinal) : boolean;
begin
  case aMsg of
    CEF_BEFORECLOSE       : Application.QueueAsyncCall(@BrowserCloseFormMsg, 0);
    CEF_PENDINGRESIZE     : Application.QueueAsyncCall(@PendingResizeMsg, 0);
    CEF_PENDINGINVALIDATE : Application.QueueAsyncCall(@PendingInvalidateMsg, 0);
  end;
end;    

procedure TForm1.DoResize;
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

end.

