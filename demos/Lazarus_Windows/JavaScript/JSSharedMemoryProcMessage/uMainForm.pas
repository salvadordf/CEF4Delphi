unit uMainForm;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Menus, SyncObjs,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Types, ComCtrls, ClipBrd,
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes,
  uCEFConstants, uCEFWinControl, uCEFChromiumCore, uCEFChromiumEvents;

const
  MINIBROWSER_SHOWMESSAGE = WM_APP + 1;

  CUSTOMMESSAGENAME  = 'sharedmemorymsg';

type
  TDTVisitStatus = (dvsIdle, dvsGettingDocNodeID, dvsQueryingSelector, dvsSettingAttributeValue);

  TMainForm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    AddressBarPnl: TPanel;
    AddressEdt: TEdit;
    Timer1: TTimer;
    Panel1: TPanel;
    GoBtn: TButton;
    SendMessageBtn: TButton;

    procedure Timer1Timer(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure SendMessageBtnClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1ProcessMessageReceived(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    // Critical section and fields to show information received in CEF events safely.
    FCritSection : TCriticalSection;
    FMsgContents : string;

    function  GetMsgContents : string;
    procedure SetMsgContents(const aValue : string);

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure ShowMessageMsg(var aMessage : TMessage); message MINIBROWSER_SHOWMESSAGE;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;

    property  MsgContents : string   read GetMsgContents  write SetMsgContents;
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uCEFProcessMessage, uCEFMiscFunctions, uCEFSchemeRegistrar,
  uCEFRenderProcessHandler, uCEFTask, uCEFSharedProcessMessageBuilder;

// This demo sends process messages between the browser process and the render process.

// Read the code comments in the JSExtension and DOMVisitor demos to know all
// the details about the Chromium processes and how to send messages between them.

// This demo sends process messages with a shared memory buffer created by
// TCefSharedProcessMessageBuilderRef.CreateBuilder when you click on the
// "Send message" button.

// TCefSharedProcessMessageBuilderRef.CreateBuilder can be used to create messages
// with a large shared memory buffer but this demo only uses one byte.

// The render process receives the message in GlobalCEFApp_OnProcessMessageReceived
// and sends back another message to the browser process

// The browser process receives the message in Chromium1ProcessMessageReceived
// and sends a custom Windows message to the main form to show a text message in
// the main application thread.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                const frame         : ICefFrame;
                                                      sourceProcess : TCefProcessId;
                                                const message_      : ICefProcessMessage;
                                                var   aHandled      : boolean);
var
  TempRegion : ICefSharedMemoryRegion;
  TempData   : Pbyte;
  TempMsg    : ICefProcessMessage;
  TempResult : boolean;
begin
  aHandled := False;

  if (frame    <> nil) and
     frame.IsValid     and
     (message_ <> nil) and
     (message_.name = CUSTOMMESSAGENAME) then
    try
      aHandled   := True;
      TempResult := False;
      TempRegion := message_.SharedMemoryRegion;

      if (TempRegion <> nil) and TempRegion.IsValid and (TempRegion.Size > 0) then
        begin
          // We send a message back to the browser process with a boolean value
          // in the argument list.
          TempData   := TempRegion.Memory;
          TempResult := (TempData^ = 42);
        end;

      TempMsg  := TCefProcessMessageRef.New(CUSTOMMESSAGENAME);
      TempMsg.ArgumentList.SetBool(0, TempResult);
      frame.SendProcessMessage(PID_BROWSER, TempMsg);
    finally
      TempRegion := nil;
      TempMsg    := nil;
    end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                          := TCefApplication.Create;
  GlobalCEFApp.OnProcessMessageReceived := GlobalCEFApp_OnProcessMessageReceived;
  GlobalCEFApp.LogFile                  := 'debug.log';
  GlobalCEFApp.LogSeverity              := LOGSEVERITY_INFO;         
  GlobalCEFApp.SetCurrentDir            := True;

  // Delphi can only debug one process and it debugs the browser process by
  // default. If you need to debug code executed in the render process you will
  // need to use any of the methods described here :
  // https://www.briskbard.com/index.php?lang=en&pageid=cef#debugging

  // Using the "Single process" mode is one of the ways to debug all the code
  // because everything is executed in the browser process and Delphi won't have
  // any problems. However, The "Single process" mode is unsupported by CEF and
  // it causes unexpected issues. You should *ONLY* use it for debugging
  // purposses.
  //GlobalCEFApp.SingleProcess := True;
end;

procedure TMainForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TMainForm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TMainForm.Chromium1BeforePopup(Sender: TObject;
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

procedure TMainForm.Chromium1ProcessMessageReceived(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
begin
  Result := False;

  if (message <> nil) and (message.Name = CUSTOMMESSAGENAME) then
    begin
      if (message.ArgumentList <> nil) and
         message.ArgumentList.GetBool(0) then
        MsgContents := 'Messages received successfully!'
       else
        MsgContents := 'There was an error receiving the message';

      // This event is executed in a CEF thread so we send a message to the main
      // form to show the MsgContents value.
      PostMessage(Handle, MINIBROWSER_SHOWMESSAGE, 0, 0);
      Result := True;
    end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
      CEFWindowParent1.Free;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;

  FCritSection := TCriticalSection.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCritSection);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Chromium1.DefaultUrl := AddressEdt.Text;

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TMainForm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  AddressBarPnl.Enabled := True;
end;

procedure TMainForm.SendMessageBtnClick(Sender: TObject);
const
  BUFFER_SIZE = 1; // Random buffer size for this demo
var
  TempBuilder : ICefSharedProcessMessageBuilder;
  TempMessage : ICefProcessMessage;
  TempData    : Pbyte;
begin
  // The shared process builder can create process messages with a large buffer
  // size but this demo only sends a byte.
  TempBuilder := TCefSharedProcessMessageBuilderRef.CreateBuilder(CUSTOMMESSAGENAME, BUFFER_SIZE);
  TempMessage := nil;
  try
    if assigned(TempBuilder) then
      begin
        TempData    := TempBuilder.Memory;
        TempData^   := 42;  // Random value passed in the message
        TempMessage := TempBuilder.Build;
        Chromium1.SendProcessMessage(PID_RENDERER, TempMessage);
      end;
  finally
    TempMessage := nil;
    TempBuilder := nil;
  end;
end;

procedure TMainForm.ShowMessageMsg(var aMessage : TMessage);
begin
  showmessage(MsgContents);
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

function TMainForm.GetMsgContents : string;
begin
  Result := '';
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      Result := FMsgContents;
    finally
      FCritSection.Release;
    end;
end;

procedure TMainForm.SetMsgContents(const aValue : string);
begin
  if (FCritSection <> nil) then
    try
      FCritSection.Acquire;
      FMsgContents := aValue;
    finally
      FCritSection.Release;
    end;
end;

end.
