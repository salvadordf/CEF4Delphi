unit usimplelazosrbrowser2;

{$MODE OBJFPC}{$H+}

interface

uses
  Windows, LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types,
  OpenGLContext, GL, GLext,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants;

type

  { TForm1 }

  TForm1 = class(TForm)
    NavControlPnl: TPanel;
    chrmosr: TChromium;
    ComboBox1: TComboBox;
    OpenGLControl1: TOpenGLControl;
    Panel2: TPanel;
    GoBtn: TButton;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);

    procedure OpenGLControl1Enter(Sender: TObject);
    procedure OpenGLControl1Exit(Sender: TObject);
    procedure OpenGLControl1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenGLControl1KeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControl1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseLeave(Sender: TObject);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure OpenGLControl1Click(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure chrmosrPaint(Sender: TObject; const browser: ICefBrowser; kind: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
    procedure chrmosrCursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure chrmosrGetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure chrmosrGetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure chrmosrGetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure chrmosrPopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
    procedure chrmosrPopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
    procedure chrmosrBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);

    procedure Timer1Timer(Sender: TObject);
    procedure ComboBox1Enter(Sender: TObject);

  protected
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;

    FLastClickCount  : integer;
    FLastClickTime   : integer;
    FLastClickPoint  : TPoint;
    FLastClickButton : TMouseButton;

    FGLInitialized   : boolean;
    FTextureID       : GLuint;
    FTextureWidth    : integer;
    FTextureHeight   : integer;
    FUpdateRect      : TCefRect;

    function  InitializeOpenGL : boolean;
    procedure VerifyOpenGLErrors;

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure InitializeLastClick;
    function  CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;

    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMCaptureChanged(var aMessage : TMessage); message WM_CAPTURECHANGED;
    procedure WMCancelMode(var aMessage : TMessage); message WM_CANCELMODE;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
    procedure WMSysChar(var aMessage: TMessage); message WM_SYSCHAR;
    procedure WMSysKeyDown(var aMessage: TMessage); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp(var aMessage: TMessage); message WM_SYSKEYUP;
    procedure WMDpiChanged(var Message: TMessage); message WM_DPICHANGED;
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uCEFMiscFunctions, uCEFApplication, uCEFWorkScheduler;


// This demo uses a CEF browser in "off-screen rendering" mode (a.k.a OSR mode)
// but instead of copying the raw bitmap information it uses an OpenGL texture
// to accelerate drawing the contents.

// It's recommemded to avoid using OpenGL in multiple threads so this demo uses
// a external message pump to execute all CEF events in the main appliction
// thread.

// This demo uses a TOpenGLControl control from the "LazOpenGLContext" package.

// Follow these steps to install the LazOpenGLContext package in Lazarus:
// 1.- Click on the "Package" option of the main Lazarus menu.
// 2.- Select the "Install/Uninstall packages" option.
// 3.- Choose "LazOpenGLContext" on the right side panel.
// 4.- Click on the "Install selection" button.
// 5.- Click on the "Save and rebuild IDE" button.
// 6.- Click on the "Continue" button.
// 7.- Wait a moment until Lazarus restarts.

// The code in this demo is a translation of the official CEF sample application
// called "cefsimple" and it also copies a few code fragments from the OSRDemo
// in the fpCEF3 project.

// In case you run this demo inside a virtual machine or using an unsupported
// graphics card you may get a white screen and/or OpenGL errors.

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and
//    calls chrmosr.CloseBrowser(True) which will destroy the internal browser
//    immediately.
// 2- chrmosr.OnBeforeClose is triggered because the internal browser was
//    destroyed. FCanClose is set to True and we can close the form safely.


procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;
begin          
  // TCEFWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalCEFWorkScheduler := TCEFWorkScheduler.Create(nil);

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableGPU                  := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := @GlobalCEFApp_OnScheduleMessagePumpWork;      
  GlobalCEFApp.SetCurrentDir              := True;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  chrmosr.LoadURL(UTF8Decode(ComboBox1.Text));
end;

procedure TForm1.GoBtnEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TForm1.OpenGLControl1Click(Sender: TObject);
begin
  OpenGLControl1.SetFocus;
end;

procedure TForm1.OpenGLControl1Enter(Sender: TObject);
begin
  chrmosr.SetFocus(True);
end;

procedure TForm1.OpenGLControl1Exit(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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

procedure TForm1.OpenGLControl1KeyPress(Sender: TObject; var Key: char);
var
  TempKeyEvent : TCefKeyEvent;
begin
  TempKeyEvent.kind                    := KEYEVENT_CHAR;
  TempKeyEvent.modifiers               := GetCefKeyboardModifiers(WParam(Key), 0);
  TempKeyEvent.windows_key_code        := Integer(Key);
  TempKeyEvent.native_key_code         := 0;
  TempKeyEvent.is_system_key           := ord(False);
  TempKeyEvent.character               := #0;
  TempKeyEvent.unmodified_character    := #0;
  TempKeyEvent.focus_on_editable_field := ord(False);

  chrmosr.SendKeyEvent(@TempKeyEvent);
end;

procedure TForm1.OpenGLControl1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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

procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  OpenGLControl1.SetFocus;

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
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, FLastClickCount);
end;

procedure TForm1.OpenGLControl1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
  TempTime  : integer;
begin
  GetCursorPos(TempPoint);
  TempPoint := OpenGLControl1.ScreenToclient(TempPoint);

  if CancelPreviousClick(TempPoint.x, TempPoint.y, TempTime) then InitializeLastClick;

  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := GetCefMouseModifiers;
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  chrmosr.SendMouseMoveEvent(@TempEvent, True);
end;

procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
  TempTime  : integer;
begin
  if CancelPreviousClick(x, y, TempTime) then InitializeLastClick;

  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  chrmosr.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, FLastClickCount);
end;

procedure TForm1.OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
begin
  TempEvent.x         := MousePos.x;
  TempEvent.y         := MousePos.y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);

  if CefIsKeyDown(VK_SHIFT) then
    chrmosr.SendMouseWheelEvent(@TempEvent, WheelDelta, 0)
   else
    chrmosr.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
end;

procedure TForm1.VerifyOpenGLErrors;
Var
  TempError : GLenum;
begin
  TempError := glGetError();
  if (TempError <> GL_NO_ERROR) then
    OutputDebugString(PAnsiChar('OpenGL error: 0x' + IntToHex(TempError, 8) + chr(0)));
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Type
  TVertex = packed record
    tu, tv: Single;
    x, y, z: Single;
  end;
Const
  Vertices : array[0..3] of TVertex = (
    (tu: 0.0; tv: 1.0; x: -1.0; y: -1.0; z: 0.0),
    (tu: 1.0; tv: 1.0; x:  1.0; y: -1.0; z: 0.0),
    (tu: 1.0; tv: 0.0; x:  1.0; y:  1.0; z: 0.0),
    (tu: 0.0; tv: 0.0; x: -1.0; y:  1.0; z: 0.0)
   );                                            
begin
  if (FTextureWidth = 0) or (FTextureHeight = 0) or not(FGLInitialized) then exit;
  //if not(FGLInitialized) then exit; // *****

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  VerifyOpenGLErrors;

  glMatrixMode(GL_MODELVIEW);
  VerifyOpenGLErrors;

  glLoadIdentity();
  VerifyOpenGLErrors;

  // Match GL units to screen coordinates.
  glViewport(0, 0, FTextureWidth, FTextureHeight);
  //glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);  // *****
  VerifyOpenGLErrors;

  glMatrixMode(GL_PROJECTION);
  VerifyOpenGLErrors;

  glLoadIdentity();
  VerifyOpenGLErrors;

  // Draw the background gradient.
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  VerifyOpenGLErrors;

  // Don't check for errors until glEnd().
  glBegin(GL_QUADS);
  glColor4f(1.0, 0.0, 0.0, 1.0);  // red
  glVertex2f(-1.0, -1.0);
  glVertex2f(1.0, -1.0);
  glColor4f(0.0, 0.0, 1.0, 1.0);  // blue
  glVertex2f(1.0, 1.0);
  glVertex2f(-1.0, 1.0);
  glEnd();
  VerifyOpenGLErrors;

  glPopAttrib();
  VerifyOpenGLErrors;

  // ***** vvvv
  // alpha blending style: texture values have premultiplied alpha
  //glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);

  // enable alpha blending
  //glEnable(GL_BLEND);
  // ***** ^^^^

  // Enable 2D textures.
  glEnable(GL_TEXTURE_2D);
  VerifyOpenGLErrors;

  // Draw the facets with the texture.
  glBindTexture(GL_TEXTURE_2D, FTextureID);
  VerifyOpenGLErrors;

  glInterleavedArrays(GL_T2F_V3F, 0, @Vertices);
  VerifyOpenGLErrors;

  glDrawArrays(GL_QUADS, 0, 4);
  VerifyOpenGLErrors;

  // Disable 2D textures.
  glDisable(GL_TEXTURE_2D);
  VerifyOpenGLErrors;

  // ***** vvvv
  // disable alpha blending
  //glDisable(GL_BLEND);
  // ***** ^^^^

  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.OpenGLControl1Resize(Sender: TObject);
begin
  chrmosr.WasResized;
end;

procedure TForm1.chrmosrAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TForm1.chrmosrBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.chrmosrBeforePopup(Sender: TObject;
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
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.chrmosrCursorChange(Sender : TObject;
                                     const browser          : ICefBrowser;
                                           cursor_          : TCefCursorHandle;
                                           cursorType       : TCefCursorType;
                                     const customCursorInfo : PCefCursorInfo;
                                     var   aResult          : boolean);
begin
  OpenGLControl1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult := True;
end;

procedure TForm1.chrmosrGetScreenInfo(Sender : TObject;
                                      const browser    : ICefBrowser;
                                      var   screenInfo : TCefScreenInfo;
                                      out   Result     : Boolean);
var
  TempRect  : TCEFRect;
  TempScale : single;
begin
  TempScale       := GlobalCEFApp.DeviceScaleFactor;
  TempRect.x      := 0;
  TempRect.y      := 0;
  TempRect.width  := DeviceToLogical(OpenGLControl1.Width,  TempScale);
  TempRect.height := DeviceToLogical(OpenGLControl1.Height, TempScale);

  screenInfo.device_scale_factor := TempScale;
  screenInfo.depth               := 0;
  screenInfo.depth_per_component := 0;
  screenInfo.is_monochrome       := Ord(False);
  screenInfo.rect                := TempRect;
  screenInfo.available_rect      := TempRect;

  Result := True;
end;

procedure TForm1.chrmosrGetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;  
  TempScale : single;
begin
  TempScale    := GlobalCEFApp.DeviceScaleFactor;
  TempViewPt.x := LogicalToDevice(viewX, TempScale);
  TempViewPt.y := LogicalToDevice(viewY, TempScale);
  TempScreenPt := OpenGLControl1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TForm1.chrmosrGetViewRect(Sender : TObject;
                                    const browser : ICefBrowser;
                                    var   rect    : TCefRect);
var
  TempScale : single;
begin
  TempScale   := GlobalCEFApp.DeviceScaleFactor;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(OpenGLControl1.Width,  TempScale);
  rect.height := DeviceToLogical(OpenGLControl1.Height, TempScale);
end;

procedure TForm1.chrmosrPaint(Sender: TObject; const browser: ICefBrowser;
  kind: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth,
  aHeight: Integer);
Var
  TempRect : TCefRect;                         
  i, TempOldWidth, TempOldHeight, TempSkipPixels, TempSkipRows, x, y, w, h : integer;
begin
  if not(InitializeOpenGL) then exit;

  // Enable 2D textures.
  glEnable(GL_TEXTURE_2D);
  VerifyOpenGLErrors;

  glBindTexture(GL_TEXTURE_2D, FTextureID);
  VerifyOpenGLErrors;

  case kind of
    PET_VIEW :
      begin
        TempOldWidth   := FTextureWidth;
        TempOldHeight  := FTextureHeight;
        FTextureWidth  := aWidth;
        FTextureHeight := aHeight;

        glPixelStorei(GL_UNPACK_ROW_LENGTH, FTextureWidth);
        VerifyOpenGLErrors;

        If (FTextureWidth  <> TempOldWidth)  or
           (FTextureHeight <> TempOldHeight) or
           ((dirtyRectsCount  = 1) and
            (dirtyRects^[0].x = 0) and
            (dirtyRects^[0].y = 0) and
            (dirtyRects^[0].width  = FTextureWidth) and
            (dirtyRects^[0].height = FTextureHeight)) then
          begin
            // Update/resize the whole texture.
            glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
            VerifyOpenGLErrors;

            glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
            VerifyOpenGLErrors;

            glTexImage2D(GL_TEXTURE_2D,
                         0, GL_RGBA, FTextureWidth, FTextureHeight,
                         0, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, buffer);
            VerifyOpenGLErrors;
          end
         else
          begin
            // Update just the dirty rectangles.
            i := 0;
            while (i < dirtyRectsCount) do
              begin
                TempRect := dirtyRects^[i];

                if (TempRect.x + TempRect.width  <= FTextureWidth) and
                   (TempRect.y + TempRect.height <= FTextureHeight) then
                  begin
                    glPixelStorei(GL_UNPACK_SKIP_PIXELS, TempRect.x);
                    VerifyOpenGLErrors;

                    glPixelStorei(GL_UNPACK_SKIP_ROWS, TempRect.y);
                    VerifyOpenGLErrors;

                    glTexSubImage2D(GL_TEXTURE_2D, 0,
                                    TempRect.x, TempRect.y,
                                    TempRect.width, TempRect.height,
                                    GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV,
                                    buffer);
                    VerifyOpenGLErrors;
                  end;

                inc(i);
              end;
          end;
      end;

    PET_POPUP :
      if (FPopUpRect.Right  > FPopUpRect.Left) and
         (FPopUpRect.Bottom > FPopUpRect.Top)  then
        begin
          TempSkipPixels := 0;
          TempSkipRows   := 0;
          x              := FPopUpRect.Left;
          y              := FPopUpRect.Top;
          w              := aWidth;
          h              := aHeight;

          // Adjust the popup to fit inside the view.
          if (x < 0) then
            begin
              TempSkipPixels := -x;
              x := 0;
            end;

          if (y < 0) then
            begin
              TempSkipRows := -y;
              y := 0;
            end;

          if (x + w > FTextureWidth) then
            w := FTextureWidth - x;

          if (y + h > FTextureHeight) then
            h := FTextureHeight - y;

          // Update the popup rectangle.
          glPixelStorei(GL_UNPACK_ROW_LENGTH, aWidth);
          VerifyOpenGLErrors;

          glPixelStorei(GL_UNPACK_SKIP_PIXELS, TempSkipPixels);
          VerifyOpenGLErrors;

          glPixelStorei(GL_UNPACK_SKIP_ROWS, TempSkipRows);
          VerifyOpenGLErrors;

          glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, w, h, GL_BGRA,
                          GL_UNSIGNED_INT_8_8_8_8_REV, buffer);
          VerifyOpenGLErrors;
        end;
  end;

  // Disable 2D textures.
  glDisable(GL_TEXTURE_2D);
  VerifyOpenGLErrors;

  OpenGLControl1.Invalidate;
end;

procedure TForm1.chrmosrPopupShow(Sender : TObject;
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

procedure TForm1.chrmosrPopupSize(Sender : TObject;
                                  const browser : ICefBrowser;
                                  const rect    : PCefRect);
begin
  LogicalToDevice(rect^, GlobalCEFApp.DeviceScaleFactor);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TForm1.chrmosrTooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
begin
  OpenGLControl1.hint     := UTF8Encode(aText);
  OpenGLControl1.ShowHint := (length(aText) > 0);
  Result                  := True;
end;

procedure TForm1.ComboBox1Enter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
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

procedure TForm1.WMCaptureChanged(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TForm1.WMCancelMode(var aMessage : TMessage);
begin
  inherited;

  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

procedure TForm1.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TForm1.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

procedure TForm1.WMSysChar(var aMessage: TMessage);
var
  TempKeyEvent : TCefKeyEvent;
begin
  inherited;

  if OpenGLControl1.Focused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
    begin
      TempKeyEvent.kind                    := KEYEVENT_CHAR;
      TempKeyEvent.modifiers               := GetCefKeyboardModifiers(aMessage.wParam, aMessage.lParam);
      TempKeyEvent.windows_key_code        := aMessage.wParam;
      TempKeyEvent.native_key_code         := aMessage.lParam;
      TempKeyEvent.is_system_key           := ord(True);
      TempKeyEvent.character               := #0;
      TempKeyEvent.unmodified_character    := #0;
      TempKeyEvent.focus_on_editable_field := ord(False);

      CefCheckAltGrPressed(aMessage.wParam, TempKeyEvent);
      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TForm1.WMSysKeyDown(var aMessage: TMessage);
var
  TempKeyEvent : TCefKeyEvent;
begin
  inherited;

  if OpenGLControl1.Focused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
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

procedure TForm1.WMSysKeyUp(var aMessage: TMessage);
var
  TempKeyEvent : TCefKeyEvent;
begin
  inherited;

  if OpenGLControl1.Focused and (aMessage.wParam in [VK_BACK..VK_HELP]) then
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

procedure TForm1.WMDpiChanged(var Message: TMessage);
begin
  inherited;

  if (GlobalCEFApp <> nil) then
    GlobalCEFApp.UpdateDeviceScaleFactor;

  if (chrmosr <> nil) then
    begin
      chrmosr.NotifyScreenInfoChanged;
      chrmosr.WasResized;
    end;
end;

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption               := 'Simple Lazarus OSR Browser 2';
  NavControlPnl.Enabled := True;
end;

procedure TForm1.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  if (chrmosr <> nil) then
    begin
      chrmosr.NotifyScreenInfoChanged;
      chrmosr.WasResized;
    end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      chrmosr.CloseBrowser(True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTextureID      := 0;
  FTextureWidth   := 0;
  FTextureHeight  := 0;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FCanClose       := False;
  FClosing        := False;

  InitializeLastClick;

  chrmosr.DefaultURL := UTF8Decode(ComboBox1.Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  chrmosr.ShutdownDragAndDrop;

  If (FTextureID <> 0) then
     glDeleteTextures(1, @FTextureID);
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  chrmosr.SetFocus(False);
  chrmosr.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if chrmosr.Initialized then
    begin
      chrmosr.WasHidden(False);
      chrmosr.SetFocus(True);
    end
   else
    begin
      // Clear OpenGL error cache.
      glGetError();

      // opaque white background color
      chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

      if chrmosr.CreateBrowser(nil, '') then
        chrmosr.InitializeDragAndDrop(OpenGLControl1)
       else
        Timer1.Enabled := True;
    end;
end;

procedure TForm1.InitializeLastClick;
begin
  FLastClickCount   := 1;
  FLastClickTime    := 0;
  FLastClickPoint.x := 0;
  FLastClickPoint.y := 0;
  FLastClickButton  := mbLeft;
end;

function TForm1.CancelPreviousClick(x, y : integer; var aCurrentTime : integer) : boolean;
begin
  aCurrentTime := GetMessageTime;

  Result := (abs(FLastClickPoint.x - x) > (GetSystemMetrics(SM_CXDOUBLECLK) div 2)) or
            (abs(FLastClickPoint.y - y) > (GetSystemMetrics(SM_CYDOUBLECLK) div 2)) or
            (cardinal(aCurrentTime - FLastClickTime) > GetDoubleClickTime);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if chrmosr.CreateBrowser(nil, '') then
    chrmosr.InitializeDragAndDrop(OpenGLControl1)
   else
    Timer1.Enabled := True;
end;

function TForm1.InitializeOpenGL : boolean;
begin
  if FGLInitialized then
    Result := True
   else
    begin
      Result := False;

      glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
      VerifyOpenGLErrors;

      glClearColor(CefColorGetR(chrmosr.Options.BackgroundColor) / 255,
                   CefColorGetG(chrmosr.Options.BackgroundColor) / 255,
                   CefColorGetB(chrmosr.Options.BackgroundColor) / 255,
                   1);
      VerifyOpenGLErrors;

      // necessary for non-power-of-2 textures to render correctly
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
      VerifyOpenGLErrors;

      // create the texture
      glGenTextures(1, @FTextureID);
      VerifyOpenGLErrors;
      if (FTextureID = 0) then exit;

      glBindTexture(GL_TEXTURE_2D, FTextureID);
      VerifyOpenGLErrors;

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      VerifyOpenGLErrors;

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      VerifyOpenGLErrors;

      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      VerifyOpenGLErrors;

      FGLInitialized := True;
      Result         := True;
    end;
end;

end.
