unit uMainForm;

{$I ..\..\..\source\cef.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, SyncObjs, 
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFWinControl, uCEFChromiumCore;

const
  DEVTOOLS_NETWORK_NESSAGE_ID = 1;

type
  TMainForm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    GoBtn: TButton;
    Timer1: TTimer;
    Chromium1: TChromium;
    CEFWindowParent1: TCEFWindowParent;
    LogMemo: TMemo;
    Splitter1: TSplitter;
    Timer2: TTimer;

    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);
    procedure Chromium1DevToolsRawEvent(Sender: TObject; const browser: ICefBrowser; const method: ustring; const params: Pointer; params_size: NativeUInt);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FLogLines    : TStringList;
    FCritSection : TCriticalSection;

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;

    procedure AddLogLine(const aLogLine: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation    

{$R *.lfm}

uses
  uCEFApplication, uCefMiscFunctions;

// This demo calls the "Network.enable" DevTools method to enable network events.
// Read this for more details :
//    https://chromedevtools.github.io/devtools-protocol/tot/Network/#method-enable

// TChromium will receive the network events in TChromium.OnDevToolsEvent or
// TChromium.OnDevToolsRawEvent

// This demo uses TChromium.OnDevToolsRawEvent to log the event messages easily but
// if you can also use TChromium.OnDevToolsEvent to get a ICefValue parameter with the parsed JSON event.
// See all the network events here :
//   https://chromedevtools.github.io/devtools-protocol/tot/Network/
// See the DOMVisitor demo to know how to read the values in the ICefValue parameter. For example, TDOMVisitorFrm.HandleErrorRslt

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
  Chromium1.DefaultURL := AddressEdt.Text;

  FLogLines    := TStringList.Create;
  FCritSection := TCriticalSection.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLogLines.Free;
  FCritSection.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TMainForm.AddLogLine(const aLogLine: string);
begin
  FCritSection.Acquire;
  FLogLines.Add(aLogLine);
  FCritSection.Release;
end;

procedure TMainForm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
  Chromium1.ExecuteDevToolsMethod(DEVTOOLS_NETWORK_NESSAGE_ID, 'Network.enable', nil);
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
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB,
                                   CEF_WOD_NEW_BACKGROUND_TAB,
                                   CEF_WOD_NEW_POPUP,
                                   CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.Chromium1DevToolsMethodResult(Sender: TObject;
  const browser: ICefBrowser; message_id: Integer; success: Boolean;
  const result: ICefValue);
begin
  if (message_id = DEVTOOLS_NETWORK_NESSAGE_ID) then
    AddLogLine('Network.enable result: ' + BoolToStr(success, True));
end;

procedure TMainForm.Chromium1DevToolsRawEvent(Sender: TObject;
  const browser: ICefBrowser; const method: ustring; const params: Pointer;
  params_size: NativeUInt);
var
  TempAnsiString : AnsiString;
begin
  if (params_size > 0) then
    begin
      SetLength(TempAnsiString, params_size);
      StrLCopy(PAnsiChar(TempAnsiString), PAnsiChar(params), params_size);
      AddLogLine('method: ' + method + ' -> ' + UTF8Decode(TempAnsiString));
    end;
end;

procedure TMainForm.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB,
                                   CEF_WOD_NEW_BACKGROUND_TAB,
                                   CEF_WOD_NEW_POPUP,
                                   CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption            := 'Network Tracker Browser';
  AddressPnl.Enabled := True;
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  // This will load the URL in the edit box
  Chromium1.LoadURL(AddressEdt.Text);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
begin
  FCritSection.Acquire;
  if FLogLines.Count > 0 then
    begin
      LogMemo.Lines.AddStrings(FLogLines);
      FLogLines.Clear;
    end;
  FCritSection.Release;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then
    Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

end.
