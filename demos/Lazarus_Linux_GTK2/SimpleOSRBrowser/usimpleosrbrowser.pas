unit uSimpleOSRBrowser;

{$mode objfpc}{$H+}

// Enable this DEFINE in case you need to use the IME
{$DEFINE CEF_USE_IME}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LCLType, ComCtrls, Types, SyncObjs, LMessages,
  gtk2, glib2, gdk2, gtk2proc, gtk2int,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants,
  {$IFDEF CEF_USE_IME}uCEFLinuxOSRIMEHandler,{$ENDIF} uCEFBufferPanel, uCEFChromiumEvents;

type
  TDevToolsStatus = (dtsIdle, dtsGettingNodeID, dtsGettingNodeInfo, dtsGettingNodeRect);

  { TForm1 }
  TForm1 = class(TForm)
    AddressCb: TComboBox;
    SaveDialog1: TSaveDialog;
    SnapshotBtn: TButton;
    GoBtn: TButton;
    Panel1: TBufferPanel;
    Chromium1: TChromium;
    AddressPnl: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;

    procedure Panel1Enter(Sender: TObject);
    procedure Panel1Exit(Sender: TObject);
    procedure Panel1IMECommit(Sender: TObject; const aCommitText: ustring);
    procedure Panel1IMEPreEditChanged(Sender: TObject; aFlag: cardinal; const aPreEditText: ustring);
    procedure Panel1IMEPreEditEnd(Sender: TObject);
    procedure Panel1IMEPreEditStart(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseEnter(Sender: TObject);
    procedure Panel1MouseLeave(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Panel1Resize(Sender: TObject);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1CursorChange(Sender: TObject; const browser: ICefBrowser; cursor_: TCefCursorHandle; cursorType: TCefCursorType; const customCursorInfo: PCefCursorInfo; var aResult : boolean);
    procedure Chromium1GetScreenInfo(Sender: TObject; const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out Result: Boolean);
    procedure Chromium1GetScreenPoint(Sender: TObject; const browser: ICefBrowser; viewX, viewY: Integer; var screenX, screenY: Integer; out Result: Boolean);
    procedure Chromium1GetViewRect(Sender: TObject; const browser: ICefBrowser; var rect: TCefRect);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1Paint(Sender: TObject; const browser: ICefBrowser; type_: TCefPaintElementType; dirtyRectsCount: NativeUInt; const dirtyRects: PCefRectArray; const buffer: Pointer; aWidth, aHeight: Integer);
    procedure Chromium1PopupShow(Sender: TObject; const browser: ICefBrowser; aShow: Boolean);
    procedure Chromium1PopupSize(Sender: TObject; const browser: ICefBrowser; const rect: PCefRect);
    procedure Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);   
    procedure Chromium1ProcessMessageReceived(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: integer; success: boolean; const result: ICefValue);
    procedure Chromium1SetFocus(Sender: TObject; const browser: ICefBrowser; source: TCefFocusSource; out Result: Boolean);

    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);

    procedure Application_OnActivate(Sender: TObject);
    procedure Application_OnDeactivate(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);
    procedure GoBtnEnter(Sender: TObject);
    procedure SnapshotBtnClick(Sender: TObject);

    procedure Timer1Timer(Sender: TObject);       
    procedure AddressCbEnter(Sender: TObject);
  private             

  protected
    FPopUpRect       : TRect;
    FShowPopUp       : boolean;
    FResizing        : boolean;
    FPendingResize   : boolean;
    FCanClose        : boolean;
    FClosing         : boolean;
    FResizeCS        : TCriticalSection;     
    FBrowserCS       : TCriticalSection;
    FPanelCursor     : TCursor;
    FPanelHint       : ustring;
    FPanelOffset     : TPoint;
    FIsEditing       : boolean;
    FElementBounds   : TRect;
    FDevToolsStatus  : TDevToolsStatus;
    FCheckEditable   : boolean;
    FWasEditing      : boolean;
    FBrowserIsFocused : boolean;
    {$IFDEF CEF_USE_IME}
    FIMEHandler      : TCEFLinuxOSRIMEHandler;
    {$ENDIF}

    function  GetPanelCursor : TCursor;
    function  GetPanelHint : ustring;
    function  GetIsEditing : boolean;

    procedure SetPanelCursor(aValue : TCursor);
    procedure SetPanelHint(const aValue : ustring);
    procedure SetIsEditing(aValue : boolean);

    procedure SendCompMessage(aMsg : cardinal; aData: PtrInt = 0);
    function  getModifiers(Shift: TShiftState): TCefEventFlags;
    function  GetButton(Button: TMouseButton): TCefMouseButtonType;
    procedure DoResize;
    procedure UpdatePanelOffset;
    procedure UpdateElementBounds(const aArgumentList : ICefListValue); overload;
    procedure UpdateElementBounds(const aRect : TRect); overload;
    function  CopyElementBounds(var aBounds : TRect) : boolean; 
    function  SetIMECursorLocation : boolean;
    function  HandleIMEKeyEvent(Event: PGdkEventKey) : boolean;    
    function  HandleGettingNodeIdResult(aSuccess : boolean; const aResult: ICefValue) : boolean;
    function  HandleGettingNodeInfoResult(aSuccess : boolean; const aResult: ICefValue) : boolean;
    function  HandleGettingNodeRectResult(aSuccess : boolean; const aResult: ICefValue) : boolean;

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure PendingResizeMsg(Data: PtrInt);
    procedure PendingInvalidateMsg(Data: PtrInt);
    procedure PendingCursorUpdateMsg(Data: PtrInt);
    procedure PendingHintUpdateMsg(Data: PtrInt);           
    procedure FocusEnabledMsg(Data: PtrInt);

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    property PanelCursor  : TCursor   read GetPanelCursor   write SetPanelCursor;
    property PanelHint    : ustring   read GetPanelHint     write SetPanelHint;
    property IsEditing    : boolean   read GetIsEditing     write SetIsEditing;

  public
    function  HandleKeyEvent(Event: PGdkEventKey) : boolean;
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

// This is a simple CEF browser in "off-screen rendering" mode (a.k.a OSR mode)

// It uses the default CEF configuration with a multithreaded message loop and
// that means that the TChromium events are executed in a CEF thread.

// GTK is not thread safe so we have to save all the information given in the
// TChromium events and use it later in the main application thread. We use
// critical sections to protect all that information.

// For example, the browser updates the mouse cursor in the
// TChromium.OnCursorChange event so we have to save the cursor in FPanelCursor
// and use Application.QueueAsyncCall to update the Panel1.Cursor value in the
// main application thread.

// The raw bitmap information given in the TChromium.OnPaint event also needs to
// be stored in a TCEFBitmapBitBuffer class instead of a simple TBitmap to avoid
// issues with GTK.

// Chromium needs the key press data available in the GDK signals
// "key-press-event" and "key-release-event" but Lazarus doesn't expose that
// information so we have to call g_signal_connect to receive that information
// in the GTKKeyPress function.

// Chromium renders the web contents asynchronously. It uses multiple processes
// and threads which makes it complicated to keep the correct browser size.

// In one hand you have the main application thread where the form is resized by
// the user. On the other hand, Chromium renders the contents asynchronously
// with the last browser size available, which may have changed by the time
// Chromium renders the page.

// For this reason we need to keep checking the real size and call
// TChromium.WasResized when we detect that Chromium has an incorrect size.

// TChromium.WasResized triggers the TChromium.OnGetViewRect event to let CEF
// read the current browser size and then it triggers TChromium.OnPaint when the
// contents are finally rendered.

// TChromium.WasResized --> (time passes) --> TChromium.OnGetViewRect --> (time passes) --> TChromium.OnPaint

// You have to assume that the real browser size can change between those calls
// and events.

// This demo uses a couple of fields called "FResizing" and "FPendingResize" to
// reduce the number of TChromium.WasResized calls.

// FResizing is set to True before the TChromium.WasResized call and it's set to
// False at the end of the TChromium.OnPaint event.

// FPendingResize is set to True when the browser changed its size while
// FResizing was True. The FPendingResize value is checked at the end of
// TChromium.OnPaint to check the browser size again because it changed while
// Chromium was rendering the page.

// The TChromium.OnPaint event in the demo also calls
// TBufferPanel.UpdateBufferDimensions and TBufferPanel.BufferIsResized to check
// the width and height of the buffer parameter, and the internal buffer size in
// the TBufferPanel component.

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
//    destroyed. FCanClose is set to True and calls
//    SendCompMessage(CEF_BEFORECLOSE) to close the form asynchronously.

uses
  Math, mouseandkeyinput,
  uCEFMiscFunctions, uCEFApplication, uCEFBitmapBitBuffer, uCEFLinuxFunctions,
  uCEFProcessMessage, uCEFDictionaryValue, uCEFJson;

const
  CEF_UPDATE_CURSOR   = $A1D;
  CEF_UPDATE_HINT     = $A1E;
  {$IFDEF CEF_USE_IME}
  EDITABLE_MSGNAME    = 'editable';
  NONEDITABLE_MSGNAME = 'noneditable';
  {$ENDIF}

{$IFDEF CEF_USE_IME}
procedure GlobalCEFApp_OnFocusedNodeChanged(const browser : ICefBrowser;
                                            const frame   : ICefFrame;
                                            const node    : ICefDomNode);
var
  TempMsg : ICefProcessMessage;
begin
  if (frame <> nil) and frame.IsValid then
    try
      if (node <> nil) and node.IsEditable then
        begin
          TempMsg := TCefProcessMessageRef.New(EDITABLE_MSGNAME);
          TempMsg.ArgumentList.SetSize(4);
          TempMsg.ArgumentList.SetInt(0, node.ElementBounds.x);
          TempMsg.ArgumentList.SetInt(1, node.ElementBounds.y);
          TempMsg.ArgumentList.SetInt(2, node.ElementBounds.width);
          TempMsg.ArgumentList.SetInt(3, node.ElementBounds.height);
        end
       else
        TempMsg := TCefProcessMessageRef.New(NONEDITABLE_MSGNAME);


      frame.SendProcessMessage(PID_BROWSER, TempMsg);
    finally
      TempMsg := nil;
    end;
end;
{$ENDIF}

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.BackgroundColor            := CefColorSetARGB($FF, $FF, $FF, $FF);
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {$IFDEF CEF_USE_IME}
  GlobalCEFApp.OnFocusedNodeChanged       := @GlobalCEFApp_OnFocusedNodeChanged;
  {$ENDIF}
end;

function GTKKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer) : GBoolean; cdecl;
begin
  Result := Form1.HandleKeyEvent(Event);
end;

procedure ConnectKeyPressReleaseEvents(const aWidget : PGtkWidget);
begin
  g_signal_connect(aWidget, 'key-press-event',   G_CALLBACK(@GTKKeyPress), nil);
  g_signal_connect(aWidget, 'key-release-event', G_CALLBACK(@GTKKeyPress), nil);
end;

{ TForm1 }

function TForm1.HandleKeyEvent(Event: PGdkEventKey) : boolean;
var
  TempCefEvent : TCefKeyEvent;
begin
  Result := True;

  if not(HandleIMEKeyEvent(Event)) then
    begin
      GdkEventKeyToCEFKeyEvent(Event, TempCefEvent);

      if (Event^._type = GDK_KEY_PRESS) then
        begin
          TempCefEvent.kind := KEYEVENT_RAWKEYDOWN;
          Chromium1.SendKeyEvent(@TempCefEvent);
          TempCefEvent.kind := KEYEVENT_CHAR;
          Chromium1.SendKeyEvent(@TempCefEvent);
        end
       else
        begin
          TempCefEvent.kind := KEYEVENT_KEYUP;
          Chromium1.SendKeyEvent(@TempCefEvent);
        end;
    end;
end;

function TForm1.HandleIMEKeyEvent(Event: PGdkEventKey) : boolean;
begin
  Result := False;
  {$IFDEF CEF_USE_IME}
  if SetIMECursorLocation then
    Result := FIMEHandler.FilterKeyPress(Event);
  {$ENDIF}
end;

function TForm1.SetIMECursorLocation : boolean;
{$IFDEF CEF_USE_IME}
var
  TempBounds : TRect;        
  TempPoint : TPoint;    
  TempScale : single;
{$ENDIF}
begin
  Result := False;
  {$IFDEF CEF_USE_IME}
  if CopyElementBounds(TempBounds) then
    begin
      TempScale   := Panel1.ScreenScale;
      TempPoint.x := DeviceToLogical(TempBounds.Left, TempScale);
      TempPoint.y := DeviceToLogical(integer(AddressPnl.Height + TempBounds.Bottom), TempScale);

      FIMEHandler.SetCursorLocation(TempPoint.x, TempPoint.y);
      Result := True;
    end;
  {$ENDIF}
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  FResizeCS.Acquire;
  FResizing      := False;
  FPendingResize := False;
  FResizeCS.Release;

  Chromium1.LoadURL(UTF8Decode(AddressCb.Text));
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);   
begin
  // Now the browser is fully initialized we can initialize the UI.
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TForm1.Chromium1SetFocus(Sender: TObject; const browser: ICefBrowser;
  source: TCefFocusSource; out Result: Boolean);
begin
  Result := not(FBrowserIsFocused);
end;

procedure TForm1.AddressCbEnter(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState = wsMinimized) then
    begin
      Chromium1.SetFocus(False);
      Chromium1.WasHidden(True);
    end
   else
    begin
      Chromium1.WasHidden(False);
      Chromium1.SetFocus(FBrowserIsFocused);
    end;
end;

procedure TForm1.Application_OnActivate(Sender: TObject);
begin
  IsEditing      := FWasEditing;
  FCheckEditable := True;
  Chromium1.SetFocus(FBrowserIsFocused);
end;      

procedure TForm1.Application_OnDeactivate(Sender: TObject);
begin
  FWasEditing := IsEditing;
  Chromium1.SetFocus(False);
  {$IFDEF CEF_USE_IME}
  FIMEHandler.Blur;
  {$ENDIF}
end;

function TForm1.HandleGettingNodeIdResult(aSuccess : boolean; const aResult: ICefValue) : boolean;
var
  TempParams, TempRsltDict : ICefDictionaryValue;
  TempNodeID : integer;
begin   
  Result := False;

  if aSuccess and (aResult <> nil) then
    try
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadInteger(TempRsltDict, 'backendNodeId', TempNodeID) then
        begin
          FDevToolsStatus := dtsGettingNodeInfo;

          TempParams := TCefDictionaryValueRef.New;
          TempParams.SetInt('backendNodeId', TempNodeID);

          Chromium1.ExecuteDevToolsMethod(0, 'DOM.describeNode', TempParams);
          Result := True;
        end
       else
        FDevToolsStatus := dtsIdle;
    finally
      TempParams := nil;
    end;
end;

function TForm1.HandleGettingNodeInfoResult(aSuccess : boolean; const aResult: ICefValue) : boolean;
var
  TempParams, TempRsltDict, TempNode : ICefDictionaryValue;
  TempAttribs : ICefListValue;
  TempName : ustring;
  TempNodeID : integer;

  function HasDisabledAttrib : boolean;
  var
    i : NativeUInt;
  begin
    Result := False;

    i := 0;
    while (i < TempAttribs.GetSize) do
      if (CompareText(TempAttribs.GetString(i), 'disabled') = 0) then
        begin
          Result := True;
          exit;
        end
       else
        inc(i, 2);
  end;

  function IsTextAreaNode : boolean;
  begin
    Result := (CompareText(TempName, 'textarea') = 0);
  end;

  function IsInputNode : boolean;
  var
    TempType : string;
    i : NativeUInt;
  begin
    Result := False;

    if (CompareText(TempName, 'input') = 0) then
      begin
        if assigned(TempAttribs) then
          begin
            i := 0;
            while (i < TempAttribs.GetSize) do
              if (CompareText(TempAttribs.GetString(i), 'type') = 0) then
                begin
                  if (i + 1 < TempAttribs.GetSize) then
                    begin
                      TempType := TempAttribs.GetString(i + 1);

                      if (CompareText(TempType, 'text') = 0) or
                         (CompareText(TempType, 'email') = 0) or
                         (CompareText(TempType, 'month') = 0) or
                         (CompareText(TempType, 'password') = 0) or
                         (CompareText(TempType, 'search') = 0) or
                         (CompareText(TempType, 'tel') = 0) or
                         (CompareText(TempType, 'url') = 0) or
                         (CompareText(TempType, 'week') = 0) or
                         (CompareText(TempType, 'datetime') = 0) then
                        begin
                          Result := True;
                          exit;
                        end
                       else
                        exit;
                    end
                   else
                    exit;
                end
               else
                inc(i, 2);

            Result := True; // Default input type is text
          end
         else
          Result := True; // Default input type is text
      end;
  end;
begin
  Result := False;

  if aSuccess and (aResult <> nil) then
    try
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadDictionary(TempRsltDict, 'node', TempNode) and
         TCEFJson.ReadString(TempNode, 'nodeName', TempName) and
         TCEFJson.ReadInteger(TempNode, 'backendNodeId', TempNodeID) and
         TCEFJson.ReadList(TempNode, 'attributes', TempAttribs) and
         not(HasDisabledAttrib) and
         (IsTextAreaNode or IsInputNode) then
        begin
          FDevToolsStatus := dtsGettingNodeRect;

          TempParams := TCefDictionaryValueRef.New;
          TempParams.SetInt('backendNodeId', TempNodeID);

          Chromium1.ExecuteDevToolsMethod(0, 'DOM.getContentQuads', TempParams);
          Result := True;
        end
       else
        FDevToolsStatus := dtsIdle;
    finally
      TempParams := nil;
    end;
end;

function TForm1.HandleGettingNodeRectResult(aSuccess : boolean; const aResult: ICefValue) : boolean;
var
  TempRsltDict : ICefDictionaryValue;
  TempList, TempQuads : ICefListValue;
  TempRect : TRect;
begin
  Result := False;

  if aSuccess and (aResult <> nil) then
    begin
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadList(TempRsltDict, 'quads', TempQuads) and
         (TempQuads.GetSize = 1) then
        begin
          TempList := TempQuads.GetList(0);

          if (TempList.GetSize = 8) then
            begin
              case TempList.GetType(0) of
                VTYPE_INT    : TempRect.Left   := TempList.GetInt(0);
                VTYPE_DOUBLE : TempRect.Left   := round(TempList.GetDouble(0));
              end;

              case TempList.GetType(1) of
                VTYPE_INT    : TempRect.Top    := TempList.GetInt(1);
                VTYPE_DOUBLE : TempRect.Top    := round(TempList.GetDouble(1));
              end;

              case TempList.GetType(4) of
                VTYPE_INT    : TempRect.Right  := TempList.GetInt(4);
                VTYPE_DOUBLE : TempRect.Right  := round(TempList.GetDouble(4));
              end;

              case TempList.GetType(5) of
                VTYPE_INT    : TempRect.Bottom := TempList.GetInt(5);
                VTYPE_DOUBLE : TempRect.Bottom := round(TempList.GetDouble(5));
              end;

              UpdateElementBounds(TempRect);

              Result := True;
            end;
        end;

      FDevToolsStatus := dtsIdle;
    end;
end;

procedure TForm1.Chromium1DevToolsMethodResult(Sender: TObject;
  const browser: ICefBrowser; message_id: integer; success: boolean;
  const result: ICefValue);
begin
  case FDevToolsStatus of
    dtsGettingNodeId   : HandleGettingNodeIdResult(success, result);
    dtsGettingNodeInfo : HandleGettingNodeInfoResult(success, result);
    dtsGettingNodeRect : HandleGettingNodeRectResult(success, result);
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.

  // Linux needs a visible form to create a browser so we need to use the
  // TForm.OnActivate event instead of the TForm.OnShow event

  {$IFDEF CEF_USE_IME}
  FIMEHandler.CreateContext;
  {$ENDIF}

  if not(Chromium1.Initialized) then
    begin
      // We have to update the DeviceScaleFactor here to get the scale of the
      // monitor where the main application form is located.
      GlobalCEFApp.UpdateDeviceScaleFactor;
      Panel1.UpdateDeviceScaleFactor;
      UpdatePanelOffset;

      // opaque white background color
      Chromium1.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
      Chromium1.DefaultURL              := UTF8Decode(AddressCb.Text);

      if not(Chromium1.CreateBrowser) then Timer1.Enabled := True;
    end;
end;

procedure TForm1.Panel1Enter(Sender: TObject);
begin
  IsEditing      := FWasEditing;
  FCheckEditable := True;
  FBrowserIsFocused := True;
  Chromium1.SetFocus(True);
end;

procedure TForm1.Panel1Exit(Sender: TObject);
begin
  FWasEditing := IsEditing;
  FBrowserIsFocused := False;
  Chromium1.SetFocus(False);
  {$IFDEF CEF_USE_IME}
  FIMEHandler.Blur;
  {$ENDIF}
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
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
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
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
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
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
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
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseMoveEvent(@TempEvent, False);
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TempEvent  : TCefMouseEvent;
  TempParams : ICefDictionaryValue;
begin
  TempEvent.x         := X;
  TempEvent.y         := Y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseClickEvent(@TempEvent, GetButton(Button), True, 1);

  // GlobalCEFApp.OnFocusedNodeChanged is not triggered the first time the
  // browser gets the focus. We need to call a series of DevTool methods to
  // check if the HTML element under the mouse is editable to save its position
  // and size. This information will be used to show the IME.
  if FCheckEditable then
    try
      FCheckEditable  := False;
      FDevToolsStatus := dtsGettingNodeId;

      TempParams := TCefDictionaryValueRef.New;
      TempParams.SetInt('x', DeviceToLogical(X, Panel1.ScreenScale));
      TempParams.SetInt('y', DeviceToLogical(Y, Panel1.ScreenScale));

      Chromium1.ExecuteDevToolsMethod(0, 'DOM.getNodeForLocation', TempParams);
    finally
      TempParams := nil;
    end;
end;

procedure TForm1.Panel1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  TempEvent  : TCefMouseEvent;
begin
  TempEvent.x         := MousePos.x;
  TempEvent.y         := MousePos.y;
  TempEvent.modifiers := getModifiers(Shift);
  DeviceToLogical(TempEvent, Panel1.ScreenScale);
  Chromium1.SendMouseWheelEvent(@TempEvent, 0, WheelDelta);
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
  const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer;
  const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
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
  PanelCursor := CefCursorToWindowsCursor(cursorType);
  aResult     := True;

  SendCompMessage(CEF_UPDATE_CURSOR);
end;

procedure TForm1.Chromium1GetScreenInfo(Sender: TObject;
  const browser: ICefBrowser; var screenInfo: TCefScreenInfo; out
  Result: Boolean);
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

procedure TForm1.Chromium1GetScreenPoint(Sender: TObject;
  const browser: ICefBrowser; viewX, viewY: Integer; var screenX,
  screenY: Integer; out Result: Boolean);
begin
  try
    FBrowserCS.Acquire;
    screenX := LogicalToDevice(viewX, Panel1.ScreenScale) + FPanelOffset.x;
    screenY := LogicalToDevice(viewY, Panel1.ScreenScale) + FPanelOffset.y;
    Result  := True;
  finally
    FBrowserCS.Release;
  end;
end;

procedure TForm1.Chromium1GetViewRect(Sender: TObject;
  const browser: ICefBrowser; var rect: TCefRect);
var
  TempScale : single;
begin                     
  TempScale   := Panel1.ScreenScale;
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
  src, dst: PByte;
  i, j, TempLineSize, TempSrcOffset, TempDstOffset, SrcStride : Integer;
  n : NativeUInt;
  TempWidth, TempHeight : integer;
  TempBufferBits : Pointer;
  TempForcedResize : boolean;
  TempBitmap : TCEFBitmapBitBuffer;
  TempSrcRect : TRect;
begin
  try
    FResizeCS.Acquire;
    TempForcedResize := False;

    if Panel1.BeginBufferDraw then
      begin
        if (type_ = PET_POPUP) then
          begin
            Panel1.UpdateOrigPopupBufferDimensions(aWidth, aHeight);

            TempBitmap := Panel1.OrigPopupBuffer;
            TempWidth  := Panel1.OrigPopupBufferWidth;
            TempHeight := Panel1.OrigPopupBufferHeight;
          end
         else
          begin
            TempForcedResize := Panel1.UpdateOrigBufferDimensions(aWidth, aHeight) or
                                not(Panel1.BufferIsResized(False));

            TempBitmap := Panel1.OrigBuffer;
            TempWidth  := Panel1.OrigBufferWidth;
            TempHeight := Panel1.OrigBufferHeight;
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

        if FShowPopup then
          begin
            TempSrcRect := Rect(0, 0,
                                FPopUpRect.Right  - FPopUpRect.Left,
                                FPopUpRect.Bottom - FPopUpRect.Top);

            Panel1.DrawOrigPopupBuffer(TempSrcRect, FPopUpRect);
          end;

        Panel1.EndBufferDraw;

        SendCompMessage(CEF_PENDINGINVALIDATE);

        if (type_ = PET_VIEW) then
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
  LogicalToDevice(rect^, Panel1.ScreenScale);

  FPopUpRect.Left   := rect^.x;
  FPopUpRect.Top    := rect^.y;
  FPopUpRect.Right  := rect^.x + rect^.width  - 1;
  FPopUpRect.Bottom := rect^.y + rect^.height - 1;
end;

procedure TForm1.Chromium1Tooltip(Sender: TObject; const browser: ICefBrowser; var aText: ustring; out Result: Boolean);
begin
  PanelHint := aText;
  Result    := True;

  SendCompMessage(CEF_UPDATE_HINT);
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
  FCheckEditable  := True;
  FWasEditing     := False;
  FDevToolsStatus := dtsIdle;
  FPopUpRect      := rect(0, 0, 0, 0);
  FShowPopUp      := False;
  FResizing       := False;
  FPendingResize  := False;
  FCanClose       := False;
  FClosing        := False;
  FBrowserIsFocused := False;
  FResizeCS       := TCriticalSection.Create;
  FBrowserCS      := TCriticalSection.Create;
  {$IFDEF CEF_USE_IME}
  FIMEHandler     := TCEFLinuxOSRIMEHandler.Create(Panel1);
  {$ENDIF}
  IsEditing       := False;

  Panel1.CopyOriginalBuffer := True;

  ConnectKeyPressReleaseEvents(PGtkWidget(Panel1.Handle));

  Application.OnActivate   := @Application_OnActivate;
  Application.OnDeactivate := @Application_OnDeactivate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if (FResizeCS   <> nil) then FreeAndNil(FResizeCS);
  if (FBrowserCS  <> nil) then FreeAndNil(FBrowserCS);
  {$IFDEF CEF_USE_IME}
  if (FIMEHandler <> nil) then FreeAndNil(FIMEHandler);
  {$ENDIF}
end;

procedure TForm1.FormHide(Sender: TObject);
begin
  Chromium1.SetFocus(False);
  Chromium1.WasHidden(True);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Chromium1.WasHidden(False);
  Chromium1.SetFocus(FBrowserIsFocused);
end;

procedure TForm1.GoBtnEnter(Sender: TObject);
begin
  Chromium1.SetFocus(False);
end;

procedure TForm1.SnapshotBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Panel1.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if not(Chromium1.CreateBrowser) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption            := 'Simple OSR Browser';
  AddressPnl.Enabled := True;

  Chromium1.SetFocus(FBrowserIsFocused);
  Chromium1.NotifyMoveOrResizeStarted;
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

procedure TForm1.PendingCursorUpdateMsg(Data: PtrInt);
begin
  Panel1.Cursor := PanelCursor;
end;

procedure TForm1.PendingHintUpdateMsg(Data: PtrInt);
begin
  Panel1.hint     := UTF8Encode(PanelHint);
  Panel1.ShowHint := (length(Panel1.hint) > 0);
end;

procedure TForm1.SendCompMessage(aMsg : cardinal; aData: PtrInt);
begin
  case aMsg of
    CEF_AFTERCREATED      : Application.QueueAsyncCall(@BrowserCreatedMsg, aData);
    CEF_BEFORECLOSE       : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
    CEF_PENDINGRESIZE     : Application.QueueAsyncCall(@PendingResizeMsg, aData);
    CEF_PENDINGINVALIDATE : Application.QueueAsyncCall(@PendingInvalidateMsg, aData);
    CEF_UPDATE_CURSOR     : Application.QueueAsyncCall(@PendingCursorUpdateMsg, aData);
    CEF_UPDATE_HINT       : Application.QueueAsyncCall(@PendingHintUpdateMsg, aData);
    CEF_FOCUSENABLED      : Application.QueueAsyncCall(@FocusEnabledMsg, aData);
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

procedure TForm1.UpdatePanelOffset;
var
  TempPoint : TPoint;
begin
  try
    FBrowserCS.Acquire;
    TempPoint.x  := 0;
    TempPoint.y  := 0;
    FPanelOffset := Panel1.ClientToScreen(TempPoint);
  finally
    FBrowserCS.Release;
  end;
end;

procedure TForm1.UpdateElementBounds(const aArgumentList : ICefListValue);
begin
  try
    FBrowserCS.Acquire;

    if assigned(aArgumentList) and (aArgumentList.GetSize = 4) then
      begin
        FIsEditing            := True;
        FElementBounds.Left   := aArgumentList.GetInt(0);
        FElementBounds.Top    := aArgumentList.GetInt(1);
        FElementBounds.Right  := FElementBounds.Left + aArgumentList.GetInt(2) - 1;
        FElementBounds.Bottom := FElementBounds.Top  + aArgumentList.GetInt(3) - 1;
      end
     else
      begin
        FIsEditing     := False;
        FElementBounds := rect(0, 0, 0, 0);
      end;

    SendCompMessage(CEF_FOCUSENABLED, ord(FIsEditing));
  finally
    FBrowserCS.Release;
  end;
end;

procedure TForm1.UpdateElementBounds(const aRect : TRect);
begin
  try
    FBrowserCS.Acquire;
    FIsEditing     := True;
    FElementBounds := aRect;
    SendCompMessage(CEF_FOCUSENABLED, ord(FIsEditing));
  finally
    FBrowserCS.Release;
  end;
end;

function TForm1.CopyElementBounds(var aBounds : TRect) : boolean;
begin
  Result  := False;
  aBounds := rect(0, 0, 0, 0);

  try
    FBrowserCS.Acquire;

    if FIsEditing then
      begin
        aBounds := FElementBounds;
        Result  := True;
      end;
  finally
    FBrowserCS.Release;
  end;
end;

function TForm1.GetPanelCursor : TCursor;
begin
  try
    FBrowserCS.Acquire;
    Result := FPanelCursor;
  finally
    FBrowserCS.Release;
  end;
end;

function TForm1.GetPanelHint : ustring;
begin
  try
    FBrowserCS.Acquire;
    Result := FPanelHint;
  finally
    FBrowserCS.Release;
  end;
end;

function TForm1.GetIsEditing : boolean;
begin
  try
    FBrowserCS.Acquire;
    Result := FIsEditing;
  finally
    FBrowserCS.Release;
  end;
end;

procedure TForm1.SetPanelCursor(aValue : TCursor);
begin
  try
    FBrowserCS.Acquire;
    FPanelCursor := aValue;
  finally
    FBrowserCS.Release;
  end;
end;

procedure TForm1.SetPanelHint(const aValue : ustring);
begin
  try
    FBrowserCS.Acquire;
    FPanelHint := aValue;
  finally
    FBrowserCS.Release;
  end;
end;    

procedure TForm1.SetIsEditing(aValue : boolean);
begin
  try
    FBrowserCS.Acquire;

    if aValue then
      FIsEditing := True
     else
      begin
        FIsEditing     := False;
        FElementBounds := rect(0, 0, 0, 0);
      end;

    SendCompMessage(CEF_FOCUSENABLED, ord(FIsEditing));
  finally
    FBrowserCS.Release;
  end;
end;

procedure TForm1.WMMove(var Message: TLMMove);
begin
  inherited;
  UpdatePanelOffset;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMSize(var Message: TLMSize);
begin
  inherited;
  UpdatePanelOffset;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  UpdatePanelOffset;
  Chromium1.NotifyMoveOrResizeStarted;
end;         

procedure TForm1.FocusEnabledMsg(Data: PtrInt);
begin
  {$IFDEF CEF_USE_IME}
  if (Data <> 0) then
    FIMEHandler.Focus // Set the client window for the input context when an editable HTML element is focused
   else
    FIMEHandler.Blur; // Reset the input context when the editable HTML is not focused
  {$ENDIF}
end;

procedure TForm1.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  sourceProcess: TCefProcessId; const message: ICefProcessMessage; out
  Result: Boolean);
begin
  Result := False;
  {$IFDEF CEF_USE_IME}
  if (message = nil) then exit;

  if (message.Name = EDITABLE_MSGNAME) then
    UpdateElementBounds(message.ArgumentList)
   else
    IsEditing := False;

  Result := True;
  {$ENDIF}
end;

procedure TForm1.Panel1IMECommit(Sender: TObject; const aCommitText: ustring);
{$IFDEF CEF_USE_IME}
const
  UINT32_MAX = high(cardinal);
var
  replacement_range : TCefRange;
{$ENDIF}
begin
  {$IFDEF CEF_USE_IME}
  replacement_range.from := UINT32_MAX;
  replacement_range.to_  := UINT32_MAX;

  Chromium1.IMECommitText(aCommitText, @replacement_range, 0);
  {$ENDIF}
end;

procedure TForm1.Panel1IMEPreEditChanged(Sender: TObject; aFlag: cardinal;
  const aPreEditText: ustring);
begin
  {$IFDEF CEF_USE_IME}
  SetIMECursorLocation;
  {$ENDIF}
end;

procedure TForm1.Panel1IMEPreEditEnd(Sender: TObject);
begin
  {$IFDEF CEF_USE_IME}         
  Chromium1.IMECancelComposition;
  {$ENDIF}
end;

procedure TForm1.Panel1IMEPreEditStart(Sender: TObject);
begin
  {$IFDEF CEF_USE_IME}
  SetIMECursorLocation;
  {$ENDIF}
end;

end.

