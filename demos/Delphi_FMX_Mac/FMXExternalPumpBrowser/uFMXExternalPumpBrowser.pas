unit uFMXExternalPumpBrowser;

{$I ..\..\..\source\cef.inc}

interface

uses
  System.Types, System.UITypes, System.Classes, System.SyncObjs,
  Macapi.AppKit, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation,
  {$IFDEF DELPHI17_UP}FMX.Graphics,{$ENDIF}
  uCEFFMXChromium, uCEFFMXBufferPanel, uCEFTimerWorkScheduler,
  uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFChromiumCore, FMX.Layouts,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Menus, FMX.ComboEdit;

type
  tagRGBQUAD = record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
          rgbReserved : BYTE;
       end;
  TRGBQuad = tagRGBQUAD;

  TJSDialogInfo = record
    OriginUrl         : ustring;
    MessageText       : ustring;
    DefaultPromptText : ustring;
    DialogType        : TCefJsDialogType;
    Callback          : ICefJsDialogCallback;
  end;

  TFMXExternalPumpBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    chrmosr: TFMXChromium;
    Timer1: TTimer;
    SaveDialog1: TSaveDialog;
    Panel1: TFMXBufferPanel;
    Layout1: TLayout;
    GoBtn: TButton;
    SnapshotBtn: TButton;
    MainMenu1: TMainMenu;
    EditMenu: TMenuItem;
    UndoMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SeparatorMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    AddressCb: TComboEdit;
    PopupMenu1: TPopupMenu;
    BackMenuItem: TMenuItem;
    ForwardMenuItem: TMenuItem;
    StatusBar: TStatusBar;
    StatusLbl: TLabel;

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
    procedure Panel1DialogKey(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormActivate(Sender: TObject);
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
    procedure chrmosrBeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure chrmosrJsdialog(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring; dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring; const callback: ICefJsDialogCallback; out suppressMessage, Result: Boolean);

    procedure Timer1Timer(Sender: TObject);
    procedure AddressEdtEnter(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);

    procedure SnapshotBtnClick(Sender: TObject);
    procedure SnapshotBtnEnter(Sender: TObject);

    procedure CopyMenuItemClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure RedoMenuItemClick(Sender: TObject);
    procedure SelectAllMenuItemClick(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure BackMenuItemClick(Sender: TObject);
    procedure ForwardMenuItemClick(Sender: TObject);
    procedure chrmosrTitleChange(Sender: TObject; const browser: ICefBrowser;
      const title: ustring);
    procedure chrmosrStatusMessage(Sender: TObject; const browser: ICefBrowser;
      const value: ustring);

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

    FJSDialogInfo      : TJSDialogInfo;
    FLastClickPoint    : TPointF;

    procedure GlobalCEFTimerWorkScheduler_OnAllowDoWork(Sender: TObject; var allow : boolean);

    procedure LoadURL;
    function  getModifiers(Shift: TShiftState): TCefEventFlags; overload;
    function  getModifiers(Shift: TShiftState; KeyCode: integer): TCefEventFlags; overload;
    function  getModifiers(Button: TMouseButton; Shift: TShiftState): TCefEventFlags; overload;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    function  GetMousePosition(var aPoint : TPointF) : boolean;
    procedure ShowPendingJSDialog;
    procedure ShowPendingPopupMenu;

  public
    procedure DoResize;
    procedure NotifyMoveOrResizeStarted;
    procedure SendCaptureLostEvent;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  end;

var
  FMXExternalPumpBrowserFrm : TFMXExternalPumpBrowserFrm;

// This is a simple browser using FireMonkey components in OSR mode (off-screen
// rendering) and a external message pump for MacOS.

// It's recomemded to understand the code in the SimpleOSRBrowser and
// OSRExternalPumpBrowser demos before reading the code in this demo.

// All FMX applications using CEF4Delphi should add the $(FrameworkType)
// conditional define in the project options to avoid duplicated resources.
// This demo has that define in the menu option :
// Project -> Options -> Building -> Delphi compiler -> Conditional defines
// (All configurations)

// The subprocesses may need to use "FMX" instead of the $(FrameworkType)
// conditional define

// As mentioned in the CEF4Delphi information page, Chromium in MacOS requires
// 4 helper bundles used for the subprocesses. The helpers must be copied inside
// the "Contents/Frameworks" directory along with the CEF binaries.
// Read this for more details :
// https://www.briskbard.com/index.php?lang=en&pageid=cef#builddemo

// The Helpers *MUST* have these names :
// <appname> Helper.app
// <appname> Helper (GPU).app
// <appname> Helper (Plugin).app
// <appname> Helper (Renderer).app
// <appname> Helper (Alerts).app

// Delphi doesn't allow project names with spaces so you need to rename all the
// helper bundles and the executable inside them. The "AppHelperRenamer" tool
// can be used for that purpose.

// The CopyCEFFramework and CopyCEFHelpers calls in the DPR file will copy
// the CEF binaries and the helper bundles automatically but those functions
// should only be used during development because the final build should have
// all the bundle contents signed using your "Apple developer certificate".

// All the helpers in this demo have extra information in the info.plist file.
// Open the "Project -> Options..." menu option and select "Application -> Version Info"
// in the left tree to edit the information in the info.plist file.
// As you can see in the helper projects, these keys have the final helper name :
// CFBundleName, CFBundleDisplayName and CFBundleExecutable.
// You also need to add a new key called "NSBGOnly" with a value "1" because the
// helper projects are regular multidevice applications without forms and
// the NSBGOnly key hides the app bundle icon from the dock.

// All the helpers use the uCEFLoader.pas unit to initialize and finalize CEF in
// the "initialization" and "finalization" sections of that unit.

// Adding the CEF binaries and the helpers to the "Contents/Frameworks"
// directory while the main application is deployed is possible but then Delphi
// runs codesign to sign all those files. You need to setup your
// "Apple developer certificate" details in the project options.
// Open the "Project -> Options..." menu option and select "Deployment -> Provisioning"
// to fill the certificate details needed to sign your application.

// Chromium requires subclassing NSApplication and implementing CrAppProtocol in
// NSApplication but the Firemonkey framework only allows to do that partially.
// This is a known cause of issues that can be avoided using custom popup menus
// and dialogs. This demo shows how to use a custom popup menu to replace the
// context menu and Firemonkey dialogs to replace JavaScript dialogs.
// If you detect some other issues when the browser shows some native user
// interface controls then replace them with custom Firemonkey controls.

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
  System.SysUtils, System.Math, System.IOUtils,
  FMX.Platform, FMX.DialogService, FMX.DialogService.Async,
  uCEFMiscFunctions, uCEFApplication, uFMXApplicationService,
  uCEFMacOSConstants, uCEFMacOSFunctions;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  if (GlobalCEFTimerWorkScheduler <> nil) then
    GlobalCEFTimerWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.UseMockKeyChain            := True;
  GlobalCEFApp.OnScheduleMessagePumpWork  := GlobalCEFApp_OnScheduleMessagePumpWork;

  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile     := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'debug.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO;
  {$ENDIF}

  // TCEFTimerWorkScheduler will call cef_do_message_loop_work when
  // it's told in the GlobalCEFApp.OnScheduleMessagePumpWork event.
  // GlobalCEFTimerWorkScheduler needs to be created before the
  // GlobalCEFApp.StartMainProcess call.
  GlobalCEFTimerWorkScheduler := TCEFTimerWorkScheduler.Create;
end;

procedure TFMXExternalPumpBrowserFrm.FormActivate(Sender: TObject);
begin
  if not(chrmosr.Initialized) then
    begin
      // opaque white background color
      chrmosr.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

      if not(chrmosr.CreateBrowser) then
        Timer1.Enabled := True;
    end;
end;

procedure TFMXExternalPumpBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing               := True;
      Visible                := False;
      AddressPnl.Enabled     := False;
      FJSDialogInfo.Callback := nil;

      chrmosr.CloseBrowser(True);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.FormCreate(Sender: TObject);
begin
  TFMXApplicationService.AddPlatformService;

  // Enable this code line in case there's an unexpected crash when
  // cef_do_message_loop_work is called.
  //GlobalCEFTimerWorkScheduler.OnAllowDoWork := GlobalCEFTimerWorkScheduler_OnAllowDoWork;

  FPopUpBitmap    := nil;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FResizeCS       := TCriticalSection.Create;

  chrmosr.DefaultURL := AddressCb.Text;

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
    end;
end;

procedure TFMXExternalPumpBrowserFrm.ForwardMenuItemClick(Sender: TObject);
begin
  chrmosr.GoForward;
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

procedure TFMXExternalPumpBrowserFrm.Panel1DialogKey(    Sender : TObject;
                                                     var Key    : Word;
                                                         Shift  : TShiftState);
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

procedure TFMXExternalPumpBrowserFrm.Panel1KeyUp(    Sender  : TObject;
                                                 var Key     : Word;
                                                 var KeyChar : Char;
                                                     Shift   : TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if not(Panel1.IsFocused) or (KeyChar = #0) then
    exit;

  TempKeyEvent.kind                    := KEYEVENT_KEYUP;
  TempKeyEvent.native_key_code         := KeyToMacOSKeyCode(Key);
  TempKeyEvent.modifiers               := getModifiers(Shift, TempKeyEvent.native_key_code);
  TempKeyEvent.windows_key_code        := 0;
  TempKeyEvent.is_system_key           := ord(False);
  TempKeyEvent.character               := KeyChar;
  TempKeyEvent.unmodified_character    := KeyChar;
  TempKeyEvent.focus_on_editable_field := ord(False);

  chrmosr.SendKeyEvent(@TempKeyEvent);
end;

procedure TFMXExternalPumpBrowserFrm.Panel1KeyDown(    Sender  : TObject;
                                                   var Key     : Word;
                                                   var KeyChar : Char;
                                                       Shift   : TShiftState);
var
  TempKeyEvent : TCefKeyEvent;
begin
  if not(Panel1.IsFocused) then exit;

  TempKeyEvent.kind                    := KEYEVENT_KEYDOWN;
  TempKeyEvent.native_key_code         := KeyToMacOSKeyCode(Key);
  TempKeyEvent.modifiers               := getModifiers(Shift, TempKeyEvent.native_key_code);
  TempKeyEvent.windows_key_code        := 0;
  TempKeyEvent.is_system_key           := ord(False);
  TempKeyEvent.character               := KeyChar;
  TempKeyEvent.unmodified_character    := KeyChar;
  TempKeyEvent.focus_on_editable_field := ord(False);

  chrmosr.SendKeyEvent(@TempKeyEvent);

  if not(TempKeyEvent.native_key_code in CEF_MACOS_KEYPAD_KEYS +
                                         CEF_MACOS_ARROW_KEYS +
                                         CEF_MACOS_FUNCTION_KEYS) then
    begin
      TempKeyEvent.kind := KEYEVENT_CHAR;
      chrmosr.SendKeyEvent(@TempKeyEvent);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.GlobalCEFTimerWorkScheduler_OnAllowDoWork(Sender: TObject; var allow : boolean);
begin
  allow := not(TFMXApplicationService(TFMXApplicationService.NewFMXApplicationService).GetHandlingSendEvent);
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
begin
  if GetMousePosition(TempPoint) then
    begin
      TempPoint           := Panel1.ScreenToClient(TempPoint);
      TempEvent.x         := round(TempPoint.x);
      TempEvent.y         := round(TempPoint.y);
      TempEvent.modifiers := EVENTFLAG_NONE;

      chrmosr.SendMouseMoveEvent(@TempEvent, True);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseMove(Sender : TObject;
                                                     Shift  : TShiftState;
                                                     X, Y   : Single);
var
  TempEvent : TCefMouseEvent;
begin
  if not(ssTouch in Shift) then
    begin
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
  TempCount : integer;
begin
  if not(ssTouch in Shift) then
    begin
      FLastClickPoint.x   := x;
      FLastClickPoint.y   := y;
      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Button, Shift);

      if (ssDouble in Shift) then
        TempCount := 2
       else
        TempCount := 1;

      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), True, TempCount);
    end;
end;

procedure TFMXExternalPumpBrowserFrm.Panel1MouseDown(Sender : TObject;
                                                     Button : TMouseButton;
                                                     Shift  : TShiftState;
                                                     X, Y   : Single);
var
  TempEvent : TCefMouseEvent;
  TempCount : integer;
begin
  if not(ssTouch in Shift) then
    begin
      TempEvent.x         := round(X);
      TempEvent.y         := round(Y);
      TempEvent.modifiers := getModifiers(Button, Shift);

      if (ssDouble in Shift) then
        TempCount := 2
       else
        TempCount := 1;

      chrmosr.SendMouseClickEvent(@TempEvent, GetButton(Button), False, TempCount);
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

procedure TFMXExternalPumpBrowserFrm.PasteMenuItemClick(Sender: TObject);
begin
  chrmosr.ClipboardPaste;
end;

procedure TFMXExternalPumpBrowserFrm.PopupMenu1Popup(Sender: TObject);
begin
  BackMenuItem.Enabled    := chrmosr.CanGoBack;
  ForwardMenuItem.Enabled := chrmosr.CanGoForward;
end;

procedure TFMXExternalPumpBrowserFrm.RedoMenuItemClick(Sender: TObject);
begin
  chrmosr.ClipboardRedo;
end;

procedure TFMXExternalPumpBrowserFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if not(chrmosr.CreateBrowser) then
    Timer1.Enabled := True;
end;

procedure TFMXExternalPumpBrowserFrm.UndoMenuItemClick(Sender: TObject);
begin
  chrmosr.ClipboardUndo;
end;

procedure TFMXExternalPumpBrowserFrm.AddressEdtEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

procedure TFMXExternalPumpBrowserFrm.BackMenuItemClick(Sender: TObject);
begin
  chrmosr.GoBack;
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

  TThread.ForceQueue(nil, procedure
                          begin
                            close
                          end);
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrBeforeContextMenu(      Sender  : TObject;
                                                              const browser : ICefBrowser;
                                                              const frame   : ICefFrame;
                                                              const params  : ICefContextMenuParams;
                                                              const model   : ICefMenuModel);
begin
  // Disable the context menu to avoid crashes and show a custom FMX popup menu instead.
  // You can call the methods in "model" to populate the custom popup menu with the original menu options.
  if (model <> nil) then model.Clear;

  TThread.ForceQueue(nil, ShowPendingPopupMenu);
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
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
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

procedure TFMXExternalPumpBrowserFrm.chrmosrJsdialog(      Sender            : TObject;
                                                     const browser           : ICefBrowser;
                                                     const originUrl         : ustring;
                                                           dialogType        : TCefJsDialogType;
                                                     const messageText       : ustring;
                                                     const defaultPromptText : ustring;
                                                     const callback          : ICefJsDialogCallback;
                                                     out   suppressMessage   : Boolean;
                                                     out   Result            : Boolean);
begin
  FJSDialogInfo.OriginUrl         := originUrl;
  FJSDialogInfo.DialogType        := dialogType;
  FJSDialogInfo.MessageText       := messageText;
  FJSDialogInfo.DefaultPromptText := defaultPromptText;
  FJSDialogInfo.Callback          := callback;

  Result             := True;
  suppressMessage    := False;

  TThread.ForceQueue(nil, ShowPendingJSDialog);
end;

procedure TFMXExternalPumpBrowserFrm.ShowPendingJSDialog;
var
  TempCaption : string;
begin
  TempCaption := 'JavaScript message from : ' + FJSDialogInfo.OriginUrl;

  case FJSDialogInfo.DialogType of
    JSDIALOGTYPE_CONFIRM :
      begin
        TempCaption := TempCaption + CRLF + CRLF + FJSDialogInfo.MessageText;
        TDialogServiceAsync.MessageDialog(TempCaption,
                                          TMsgDlgType.mtConfirmation,
                                          [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                                          TMsgDlgBtn.mbYes,
                                          0,
                                          procedure(const AResult: TModalResult)
                                          begin
                                            FJSDialogInfo.Callback.cont(AResult in [mrOk, mrYes], '');
                                            FJSDialogInfo.Callback := nil;
                                          end);
      end;

    JSDIALOGTYPE_PROMPT :
      TDialogServiceAsync.InputQuery(TempCaption,
                                     [FJSDialogInfo.MessageText],
                                     [FJSDialogInfo.DefaultPromptText],
                                     procedure(const AResult: TModalResult; const AValues: array of string)
                                     begin
                                       FJSDialogInfo.Callback.cont(AResult in [mrOk, mrYes], AValues[0]);
                                       FJSDialogInfo.Callback := nil;
                                     end);

    else // JSDIALOGTYPE_ALERT
      begin
        TempCaption := TempCaption + CRLF + CRLF + FJSDialogInfo.MessageText;
        TDialogServiceAsync.ShowMessage(TempCaption);
        FJSDialogInfo.Callback := nil;
      end;
  end;
end;

procedure TFMXExternalPumpBrowserFrm.ShowPendingPopupMenu;
var
  TempPoint : TPointF;
begin
  if not(GetMousePosition(TempPoint)) then
    TempPoint := Panel1.ClientToScreen(FLastClickPoint);

  PopupMenu1.Popup(TempPoint.X, TempPoint.Y);
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
  src, dst, srcPixel, dstPixel: PByte;
  i, j, k, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride, TempWidth, TempHeight : Integer;
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
                      TempLineSize := min(dirtyRects[n].width, TempWidth - dirtyRects[n].x);

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

                              srcPixel := src;
                              dstPixel := dst;
                              k        := TempLineSize;

                              while (k > 0) do
                                begin
                                  // Switch the red and blue channels
                                  dstPixel[0] := srcPixel[2];
                                  dstPixel[1] := srcPixel[1];
                                  dstPixel[2] := srcPixel[0];
                                  dstPixel[3] := srcPixel[3];
                                  inc(dstPixel, SizeOf(TRGBQuad));
                                  inc(srcPixel, SizeOf(TRGBQuad));
                                  dec(k);
                                end;

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
              TThread.ForceQueue(nil, DoResize);

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

procedure TFMXExternalPumpBrowserFrm.chrmosrStatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  StatusLbl.Text := value;
end;

procedure TFMXExternalPumpBrowserFrm.chrmosrTitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  Caption := 'FMX External Pump Browser - ' + title;
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

procedure TFMXExternalPumpBrowserFrm.CopyMenuItemClick(Sender: TObject);
begin
  chrmosr.ClipboardCopy;
end;

procedure TFMXExternalPumpBrowserFrm.CutMenuItemClick(Sender: TObject);
begin
  chrmosr.ClipboardCut;
end;

procedure TFMXExternalPumpBrowserFrm.DeleteMenuItemClick(Sender: TObject);
begin
  chrmosr.ClipboardDel;
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

procedure TFMXExternalPumpBrowserFrm.SelectAllMenuItemClick(Sender: TObject);
begin
  chrmosr.SelectAll;
end;

procedure TFMXExternalPumpBrowserFrm.SendCaptureLostEvent;
begin
  if (chrmosr <> nil) then chrmosr.SendCaptureLostEvent;
end;

function TFMXExternalPumpBrowserFrm.getModifiers(Shift: TShiftState): TCefEventFlags;
begin
  Result := EVENTFLAG_NONE;

  if (ssShift   in Shift) then Result := Result or EVENTFLAG_SHIFT_DOWN;
  if (ssAlt     in Shift) then Result := Result or EVENTFLAG_ALT_DOWN;
  if (ssCtrl    in Shift) then Result := Result or EVENTFLAG_CONTROL_DOWN;
  if (ssLeft    in Shift) then Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
  if (ssRight   in Shift) then Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
  if (ssMiddle  in Shift) then Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
  if (ssCommand in Shift) then Result := Result or EVENTFLAG_COMMAND_DOWN;
end;

function TFMXExternalPumpBrowserFrm.getModifiers(Shift: TShiftState; KeyCode: integer): TCefEventFlags;
begin
  Result := getModifiers(Shift);

  if (KeyCode in CEF_MACOS_KEYPAD_KEYS) then
    Result := Result or EVENTFLAG_IS_KEY_PAD;
end;

function TFMXExternalPumpBrowserFrm.getModifiers(Button: TMouseButton; Shift: TShiftState): TCefEventFlags;
begin
  Result := getModifiers(shift);

  case Button of
    TMouseButton.mbLeft   : Result := Result or EVENTFLAG_LEFT_MOUSE_BUTTON;
    TMouseButton.mbRight  : Result := Result or EVENTFLAG_RIGHT_MOUSE_BUTTON;
    TMouseButton.mbMiddle : Result := Result or EVENTFLAG_MIDDLE_MOUSE_BUTTON;
  end;
end;

function TFMXExternalPumpBrowserFrm.GetButton(Button: TMouseButton): TCefMouseButtonType;
begin
  case Button of
    TMouseButton.mbRight  : Result := MBT_RIGHT;
    TMouseButton.mbMiddle : Result := MBT_MIDDLE;
    else                    Result := MBT_LEFT;
  end;
end;

procedure TFMXExternalPumpBrowserFrm.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TFMXExternalPumpBrowserFrm.SnapshotBtnEnter(Sender: TObject);
begin
  chrmosr.SetFocus(False);
end;

end.
