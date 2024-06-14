unit uOSRExternalPumpBrowser2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLType, ComCtrls, Types, SyncObjs, eventlog, LMessages, OpenGLContext, GL,
  GLext, uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants,
  uCEFBufferPanel, uCEFChromiumEvents;

type
  { TForm1 }
  TForm1 = class(TForm)
    AddressEdt: TEdit;
    Panel1: TOpenGLControl;
    SaveDialog1: TSaveDialog;
    GoBtn: TButton;
    Chromium1: TChromium;
    AddressPnl: TPanel;
    Panel2: TPanel;

    procedure Panel1Click(Sender: TObject);
    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1Paint(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1CursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure Chromium1GetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure Chromium1GetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure Chromium1GetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1Paint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
    procedure Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
    procedure Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);     
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);

    procedure AddressEdtEnter(Sender: TObject);
  private             

  protected                      
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;
    FFirstLoad       : boolean;

    FGLInitialized   : boolean;
    FTextureID       : GLuint;
    FTextureWidth    : integer;
    FTextureHeight   : integer;
    FUpdateRect      : TCefRect;

    function  InitializeOpenGL : boolean;
    procedure VerifyOpenGLErrors;

    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

  public
    procedure SendCEFKeyEvent(const aCefEvent : TCefKeyEvent);
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

// ATTENTION
// =========
// Since CEF 102 the Linux demos with the GlobalCEFApp.MultiThreadedMessageLoop
// property set to FALSE and using GTK2 require a custom build of CEF binaries
// with use_gtk=false set via GN_DEFINES.

// This demo uses a CEF browser in "off-screen rendering" mode (a.k.a OSR mode)
// but instead of copying the raw bitmap information it uses an OpenGL texture
// to accelerate drawing the contents.

// It's recommemded to avoid using OpenGL in multiple threads so this demo uses
// a external message pump to execute all CEF events in the main appliction
// thread.

// This demo uses a TOpenGLControl control from the "LazOpenGLContext" package.
//
// Before you install the LazOpenGLContext package you need to install the
// libgl1-mesa-dev package in Linux.

// If you use Ubuntu you would have to open a Terminal and execute the following
// commands :
//    sudo apt-get update -y
//    sudo apt-get install -y libgl1-mesa-dev
//
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

// Lazarus usually initializes the GTK WidgetSet in the initialization section
// of the "Interfaces" unit which is included in the LPR file. This causes
// initialization problems in CEF and we need to call "CreateWidgetset" after
// the GlobalCEFApp.StartMainProcess call.

// Lazarus shows a warning if we remove the "Interfaces" unit from the LPR file
// so we created a custom unit with the same name that includes two procedures
// to initialize and finalize the WidgetSet at the right time.

// This is the destruction sequence in OSR mode :
// 1- FormCloseQuery sets CanClose to the initial FCanClose value (False) and
//    calls Chromium1.CloseBrowser(True) which will destroy the internal browser
//    immediately.
// 2- Chromium1.OnBeforeClose is triggered because the internal browser was
//    destroyed. FCanClose is set to True and we can close the form safely.

uses
  Math, gtk2, glib2, gdk2, gtk2proc, gtk2int,
  uCEFMiscFunctions, uCEFApplication, uCEFBitmapBitBuffer, uCEFWorkScheduler,
  uCEFLinuxFunctions;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFWorkScheduler <> nil) then
    GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;     

procedure CreateGlobalCEFApp;
begin               
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.BrowserSubprocessPath      := 'OSRExternalPumpBrowser2_sp';
  GlobalCEFApp.EnableGPU                  := True;
  GlobalCEFApp.BackgroundColor            := CefColorSetARGB($FF, $FF, $FF, $FF);  
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.OnScheduleMessagePumpWork  := @GlobalCEFApp_OnScheduleMessagePumpWork;

  // This is a workaround for the 'GPU is not usable error' issue :
  // https://bitbucket.org/chromiumembedded/cef/issues/2964/gpu-is-not-usable-error-during-cef
  GlobalCEFApp.DisableZygote := True; // this property adds the "--no-zygote" command line switch

  // TCEFWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  // We use CreateDelayed in order to have a single thread in the process while
  // CEF is initialized.
  GlobalCEFWorkScheduler := TCEFWorkScheduler.CreateDelayed;

  GlobalCEFApp.StartMainProcess;
  GlobalCEFWorkScheduler.CreateThread;
end;

function GTKKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer) : GBoolean; cdecl;
var
  TempCefEvent : TCefKeyEvent;
begin
  GdkEventKeyToCEFKeyEvent(Event, TempCefEvent);

  if (Event^._type = GDK_KEY_PRESS) then
    begin
      TempCefEvent.kind := KEYEVENT_RAWKEYDOWN;
      Form1.SendCEFKeyEvent(TempCefEvent);
      TempCefEvent.kind := KEYEVENT_CHAR;
      Form1.SendCEFKeyEvent(TempCefEvent);
    end
   else
    begin
      TempCefEvent.kind := KEYEVENT_KEYUP;
      Form1.SendCEFKeyEvent(TempCefEvent);
    end;

  Result := True;
end;

procedure ConnectKeyPressReleaseEvents(const aWidget : PGtkWidget);
begin
  g_signal_connect(aWidget, 'key-press-event',   TGTKSignalFunc(@GTKKeyPress), nil);
  g_signal_connect(aWidget, 'key-release-event', TGTKSignalFunc(@GTKKeyPress), nil);
end;

{ TForm1 }

procedure TForm1.SendCEFKeyEvent(const aCefEvent : TCefKeyEvent);
begin
  Chromium1.SendKeyEvent(@aCefEvent);
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(UTF8Decode(AddressEdt.Text));
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  Caption            := 'OSR External Pump Browser 2';
  AddressPnl.Enabled := True;

  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.AddressEdtEnter(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TForm1.Panel1Click(Sender: TObject);
begin
  Panel1.SetFocus;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.

  // Linux needs a visible form to create a browser so we need to use the
  // TForm.OnActivate event instead of the TForm.OnShow event

  if not(Chromium1.Initialized) then
    begin
      // We have to update the DeviceScaleFactor here to get the scale of the
      // monitor where the main application form is located.
      GlobalCEFApp.UpdateDeviceScaleFactor;

      // opaque white background color
      Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
      Chromium1.DefaultURL              := UTF8Decode(AddressEdt.Text);
      Chromium1.CreateBrowser;
    end;
end;

procedure TForm1.Panel1Enter(Sender: TObject);
begin
  Chromium1.SetFocus(True);
end;

procedure TForm1.Panel1Exit(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  Panel1.SetFocus;

  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), False, 1);
end;

procedure TForm1.Panel1MouseEnter(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  TempPoint           := Panel1.ScreenToClient(mouse.CursorPos);
  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := EVENTFLAG_NONE;
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  Chromium1.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TForm1.Panel1MouseLeave(Sender: TObject);
var
  TempEvent : TCefMouseEvent;
  TempPoint : TPoint;
begin
  TempPoint           := Panel1.ScreenToClient(mouse.CursorPos);
  TempEvent.x         := TempPoint.x;
  TempEvent.y         := TempPoint.y;
  TempEvent.modifiers := EVENTFLAG_NONE;
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  Chromium1.SendMouseMoveEvent(@TempEvent, True);
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := x;
  TempEvent.y         := y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  Chromium1.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent : TCefMouseEvent;
begin
  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);
end;

procedure TForm1.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
begin
  TempEvent.x         := MousePos.x;
  TempEvent.y         := MousePos.y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, GlobalCEFApp.DeviceScaleFactor);
  Chromium1.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
end;

procedure TForm1.Panel1Paint(Sender: TObject);
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
  if (FTextureWidth = 0) or (FTextureHeight = 0) or not(FGLInitialized) then
     exit;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  VerifyOpenGLErrors;

  glMatrixMode(GL_MODELVIEW);
  VerifyOpenGLErrors;

  glLoadIdentity();
  VerifyOpenGLErrors;

  // Match GL units to screen coordinates.
  glViewport(0, 0, FTextureWidth, FTextureHeight);
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

  Panel1.SwapBuffers;
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  Chromium1.WasResized;
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
  Close;
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
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1CursorChange(Sender: TObject;
  const browser: ICefBrowser; cursor_: TCefCursorHandle;
  cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; 
  var aResult : boolean);
begin
  Panel1.Cursor := CefCursorToWindowsCursor(cursorType);
  aResult       := True;
end;

procedure TForm1.Chromium1GetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
  Result: Boolean);
var
  TempRect  : TCEFRect;
  TempScale : single;
begin           
  TempScale       := GlobalCEFApp.DeviceScaleFactor;
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

procedure TForm1.Chromium1GetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
var
  TempScreenPt, TempViewPt : TPoint;
  TempScale : single;
begin
  TempScale    := GlobalCEFApp.DeviceScaleFactor;
  TempViewPt.x := LogicalToDevice(viewX, TempScale);
  TempViewPt.y := LogicalToDevice(viewY, TempScale);
  TempScreenPt := Panel1.ClientToScreen(TempViewPt);
  screenX      := TempScreenPt.x;
  screenY      := TempScreenPt.y;
  Result       := True;
end;

procedure TForm1.Chromium1GetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
var
  TempScale : single;
begin                     
  TempScale   := GlobalCEFApp.DeviceScaleFactor;
  rect.x      := 0;
  rect.y      := 0;
  rect.width  := DeviceToLogical(Panel1.Width,  TempScale);
  rect.height := DeviceToLogical(Panel1.Height, TempScale);
end;

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TForm1.Chromium1Paint(Sender: TObject; const browser: ICefBrowser;
  type_: TCefPaintElementType; dirtyRectsCount: NativeUInt;
  const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer
  );
var
  TempRect : TCefRect;
  i, TempOldWidth, TempOldHeight, TempSkipPixels, TempSkipRows, x, y, w, h : integer;
begin
  if not(InitializeOpenGL) then exit;

  // Enable 2D textures.
  glEnable(GL_TEXTURE_2D);
  VerifyOpenGLErrors;

  glBindTexture(GL_TEXTURE_2D, FTextureID);
  VerifyOpenGLErrors;

  case type_ of
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

  Panel1.Invalidate;
end;

procedure TForm1.Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
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

procedure TForm1.Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
begin
  LogicalToDevice(rect^, GlobalCEFApp.DeviceScaleFactor);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TForm1.Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
begin
  Panel1.hint     := UTF8Encode(aText);
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
  FTextureID      := 0;
  FTextureWidth   := 0;
  FTextureHeight  := 0;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FCanClose       := False;
  FClosing        := False;
  FFirstLoad      := True;

  ConnectKeyPressReleaseEvents(PGtkWidget(Panel1.Handle));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin                                              
  If (FTextureID <> 0) then
     glDeleteTextures(1, @FTextureID);
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  Chromium1.SetFocus(False);
  Chromium1.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if Chromium1.Initialized then
    begin
      Chromium1.WasHidden(False);
      Chromium1.SetFocus(True);
    end
   else
    glGetError(); // Clear OpenGL error cache.
end;

procedure TForm1.GoBtnEnter(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TForm1.WMMove(var Message: TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMSize(var Message: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.VerifyOpenGLErrors;
Var
  TempError : GLenum;
begin
  TempError := glGetError();
  if (TempError <> GL_NO_ERROR) then
    WriteLn(StdErr, 'OpenGL error: 0x' + IntToHex(TempError, 8));
end;

procedure TForm1.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
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

      glClearColor(CefColorGetR(Chromium1.Options.BackgroundColor) / 255,
                   CefColorGetG(Chromium1.Options.BackgroundColor) / 255,
                   CefColorGetB(Chromium1.Options.BackgroundColor) / 255,
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

