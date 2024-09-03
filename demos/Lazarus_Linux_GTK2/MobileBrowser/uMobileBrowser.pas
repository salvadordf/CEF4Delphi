unit uMobileBrowser;

{$I ../../../source/cef.inc}

interface

uses                                                      
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LMessages, MaskEdit, Spin, uCEFChromium, uCEFWindowParent, uCEFInterfaces,
  uCEFConstants, uCEFTypes, uCEFJson, uCEFWinControl, 
  uCEFChromiumCore, uCEFDictionaryValue, uCEFLinkedWindowParent, uCEFChromiumEvents;

type

  { TForm1 }

  TForm1 = class(TForm)
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    Timer1: TTimer;
    Chromium1: TChromium;
    Panel1: TPanel;
    Panel2: TPanel;
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    GoBtn: TButton;
    Splitter1: TSplitter;
    LogMem: TMemo;
    Panel3: TPanel;
    CanEmulateBtn: TButton;
    ClearDeviceMetricsOverrideBtn: TButton;
    Panel4: TPanel;
    GroupBox1: TGroupBox;
    UserAgentCb: TComboBox;
    OverrideUserAgentBtn: TButton;
    EmulateTouchChk: TCheckBox;
    Panel5: TPanel;
    GroupBox2: TGroupBox;
    Panel6: TPanel;
    Label1: TLabel;
    HeightEdt: TSpinEdit;
    Panel7: TPanel;
    Label2: TLabel;
    WidthEdt: TSpinEdit;
    OverrideDeviceMetricsBtn: TButton;
    Panel8: TPanel;
    Label3: TLabel;
    ScaleEdt: TMaskEdit;
    Panel9: TPanel;
    Label4: TLabel;
    OrientationCb: TComboBox;
    Panel10: TPanel;
    Label5: TLabel;
    AngleEdt: TSpinEdit;

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);

    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);

    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);
                                                           
    procedure CanEmulateBtnClick(Sender: TObject);
    procedure OverrideUserAgentBtnClick(Sender: TObject);
    procedure EmulateTouchChkClick(Sender: TObject);
    procedure ClearDeviceMetricsOverrideBtnClick(Sender: TObject);
    procedure OverrideDeviceMetricsBtnClick(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FPendingMsgID : integer;

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure SendCompMessage(aMsg : cardinal);

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserSetFocusMsg(Data: PtrInt);

    procedure HandleSetUserAgentResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleSetTouchEmulationEnabledResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleCanEmulateResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleClearDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleSetDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);

    procedure HandleSetUserAgentResultMsg(Data: PtrInt);
    procedure HandleSetTouchEmulationEnabledResultMsg(Data: PtrInt);
    procedure HandleCanEmulateResultMsg(Data: PtrInt);
    procedure HandleClearDeviceMetricsOverrideResultMsg(Data: PtrInt);
    procedure HandleSetDeviceMetricsOverrideResultMsg(Data: PtrInt);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uCEFApplication, uCEFMiscFunctions;

// This demo allows you to emulate a mobile browser using the "Emulation" namespace of the DevTools.
// It's necesary to reload the browser after using the controls in the right panel.

// This demo uses a TChromium and a TCEFWindowParent

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TChromium.CloseBrowser which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

const
  CEF_SETFOCUS = 1;

  DEVTOOLS_SETUSERAGENTOVERRIDE_MSGID       = 1;
  DEVTOOLS_SETTOUCHEMULATIONENABLED_MSGID   = 2;
  DEVTOOLS_CANEMULATE_MSGID                 = 3;
  DEVTOOLS_CLEARDEVICEMETRICSOVERRIDE_MSGID = 4;
  DEVTOOLS_SETDEVICEMETRICSOVERRIDE_MSGID   = 5;

  CANEMULATE_RESULT_FAIL                    = 0;
  CANEMULATE_RESULT_SUCCESS_SUPPORTED       = 1;
  CANEMULATE_RESULT_SUCCESS_UNSUPPORTED     = 2;


procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp             := TCefApplication.Create;
  {
  GlobalCEFApp.LogFile     := 'cef.log';
  GlobalCEFApp.LogSeverity := LOGSEVERITY_VERBOSE;
  }
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanClose            := False;
  FClosing             := False;
  FPendingMsgID        := 0;
  Chromium1.DefaultURL := UTF8Decode(AddressEdt.Text);
  Chromium1.RuntimeStyle := CEF_RUNTIME_STYLE_ALLOY;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(UTF8Decode(AddressEdt.Text));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, CEFLinkedWindowParent1.BoundsRect)) and
     not(Chromium1.Initialized) then
    Timer1.Enabled := True;
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

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
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

  if not(Chromium1.Initialized) and
     not(Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, CEFLinkedWindowParent1.BoundsRect)) then
    Timer1.Enabled := True;
end;

procedure TForm1.Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  // continue closing the browser
  aAction := cbaClose;
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // We must wait until all browsers trigger the TChromium.OnBeforeClose event
  // in order to close the application safely or we will have shutdown issues.
  FCanClose := True;
  SendCompMessage(CEF_BEFORECLOSE);
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  SendCompMessage(CEF_AFTERCREATED);
end;

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use CEFLinkedWindowParent1.OnEnter, CEFLinkedWindowParent1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// CEFLinkedWindowParent1.TabStop must be TRUE.
procedure TForm1.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;

procedure TForm1.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SetFocus(True);
end;

procedure TForm1.Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
begin
  SendCompMessage(CEF_SETFOCUS);
end;

procedure TForm1.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption            := 'Mobile Browser';
  AddressPnl.Enabled := True;
end;

procedure TForm1.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TForm1.BrowserSetFocusMsg(Data: PtrInt);
begin
  CEFLinkedWindowParent1.SetFocus;
end;

procedure TForm1.SendCompMessage(aMsg : cardinal);
begin
  case aMsg of
    CEF_AFTERCREATED : Application.QueueAsyncCall(BrowserCreatedMsg, 0);
    CEF_BEFORECLOSE  : Application.QueueAsyncCall(BrowserCloseFormMsg, 0);
    CEF_SETFOCUS     : Application.QueueAsyncCall(BrowserSetFocusMsg, 0);
  end;
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

procedure TForm1.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.CanEmulateBtnClick(Sender: TObject);
begin
  FPendingMsgID := DEVTOOLS_CANEMULATE_MSGID;
  Chromium1.ExecuteDevToolsMethod(0, 'Emulation.canEmulate', nil);
end;

procedure TForm1.Chromium1DevToolsMethodResult(Sender: TObject;
  const browser: ICefBrowser; message_id: Integer; success: Boolean;
  const result: ICefValue);
begin
  case FPendingMsgID of
    DEVTOOLS_SETUSERAGENTOVERRIDE_MSGID       : HandleSetUserAgentResult(success, result);
    DEVTOOLS_SETTOUCHEMULATIONENABLED_MSGID   : HandleSetTouchEmulationEnabledResult(success, result);
    DEVTOOLS_CANEMULATE_MSGID                 : HandleCanEmulateResult(success, result);
    DEVTOOLS_CLEARDEVICEMETRICSOVERRIDE_MSGID : HandleClearDeviceMetricsOverrideResult(success, result);
    DEVTOOLS_SETDEVICEMETRICSOVERRIDE_MSGID   : HandleSetDeviceMetricsOverrideResult(success, result);
  end;
end;

procedure TForm1.ClearDeviceMetricsOverrideBtnClick(Sender: TObject);
begin
  FPendingMsgID := DEVTOOLS_CLEARDEVICEMETRICSOVERRIDE_MSGID;
  Chromium1.ExecuteDevToolsMethod(0, 'Emulation.clearDeviceMetricsOverride', nil);
end;

procedure TForm1.EmulateTouchChkClick(Sender: TObject);
var
  TempParams : ICefDictionaryValue;
begin
  try
    TempParams := TCefDictionaryValueRef.New;
    TempParams.SetBool('enabled', EmulateTouchChk.Checked);

    if EmulateTouchChk.Checked then
      TempParams.SetInt('maxTouchPoints', 2);

    FPendingMsgID := DEVTOOLS_SETTOUCHEMULATIONENABLED_MSGID;
    Chromium1.ExecuteDevToolsMethod(0, 'Emulation.setTouchEmulationEnabled', TempParams);
  finally
    TempParams := nil;
  end;
end;

procedure TForm1.OverrideDeviceMetricsBtnClick(Sender: TObject);
var
  TempParams, TempDict : ICefDictionaryValue;
  TempFormatSettings : TFormatSettings;
  TempOrientation : string;
begin
  try
    TempParams := TCefDictionaryValueRef.New;
    TempParams.SetInt('width',  WidthEdt.Value);
    TempParams.SetInt('height', HeightEdt.Value);

    TempFormatSettings.DecimalSeparator := '.';
    TempParams.SetDouble('deviceScaleFactor', StrToFloat(ScaleEdt.Text, TempFormatSettings));

    TempParams.SetBool('mobile', True);

    case OrientationCb.ItemIndex of
      0 :  TempOrientation := 'portraitPrimary';
      1 :  TempOrientation := 'portraitSecondary';
      2 :  TempOrientation := 'landscapePrimary';
      else TempOrientation := 'landscapeSecondary';
    end;

    TempDict := TCefDictionaryValueRef.New;
    TempDict.SetString('type', TempOrientation);
    TempDict.SetInt('angle', AngleEdt.Value);
    TempParams.SetDictionary('screenOrientation', TempDict);

    FPendingMsgID := DEVTOOLS_SETDEVICEMETRICSOVERRIDE_MSGID;
    Chromium1.ExecuteDevToolsMethod(0, 'Emulation.setDeviceMetricsOverride', TempParams);
  finally
    TempDict := nil;
    TempParams := nil;
  end;
end;

procedure TForm1.OverrideUserAgentBtnClick(Sender: TObject);
var
  TempParams : ICefDictionaryValue;
begin
  try
    TempParams := TCefDictionaryValueRef.New;
    TempParams.SetString('userAgent', UserAgentCb.Text);

    FPendingMsgID := DEVTOOLS_SETUSERAGENTOVERRIDE_MSGID;
    Chromium1.ExecuteDevToolsMethod(0, 'Emulation.setUserAgentOverride', TempParams);
  finally
    TempParams := nil;
  end;
end;

procedure TForm1.HandleSetUserAgentResultMsg(Data: PtrInt);
begin
  if (Data <> 0) then
    LogMem.Lines.Add('Successful SetUserAgentOverride')
   else
    LogMem.Lines.Add('Unsuccessful SetUserAgentOverride');
end;

procedure TForm1.HandleSetTouchEmulationEnabledResultMsg(Data: PtrInt);
begin
  if (Data <> 0) then
    LogMem.Lines.Add('Successful SetTouchEmulationEnabled')
   else
    LogMem.Lines.Add('Unsuccessful SetTouchEmulationEnabled');
end;

procedure TForm1.HandleCanEmulateResultMsg(Data: PtrInt);      
begin
  case Data of
    CANEMULATE_RESULT_FAIL :
      LogMem.Lines.Add('Unsuccessful CanEmulate');

    CANEMULATE_RESULT_SUCCESS_UNSUPPORTED :
      LogMem.Lines.Add('Successful CanEmulate. Emulation is not supported.');

    CANEMULATE_RESULT_SUCCESS_SUPPORTED :
      LogMem.Lines.Add('Successful CanEmulate. Emulation is supported.');
  end;
end;

procedure TForm1.HandleClearDeviceMetricsOverrideResultMsg(Data: PtrInt);   
begin
  if (Data <> 0) then
    LogMem.Lines.Add('Successful ClearDeviceMetricsOverride')
   else
    LogMem.Lines.Add('Unsuccessful ClearDeviceMetricsOverride');
end;

procedure TForm1.HandleSetDeviceMetricsOverrideResultMsg(Data: PtrInt);  
begin
  if (Data <> 0) then
    LogMem.Lines.Add('Successful SetDeviceMetricsOverride')
   else
    LogMem.Lines.Add('Unsuccessful SetDeviceMetricsOverride');
end;

procedure TForm1.HandleSetUserAgentResult(aSuccess : boolean; const aResult: ICefValue);
begin
  Application.QueueAsyncCall(HandleSetUserAgentResultMsg, ord(aSuccess and (aResult <> nil)));
end;

procedure TForm1.HandleSetTouchEmulationEnabledResult(aSuccess : boolean; const aResult: ICefValue);
begin                
  Application.QueueAsyncCall(HandleSetTouchEmulationEnabledResultMsg, ord(aSuccess and (aResult <> nil)));
end;

procedure TForm1.HandleCanEmulateResult(aSuccess : boolean; const aResult: ICefValue);
var
  TempRsltDict : ICefDictionaryValue;
  TempResult : boolean;
  TempData : PtrInt;
begin
  if aSuccess and (aResult <> nil) then
    begin
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadBoolean(TempRsltDict, 'result', TempResult) and TempResult then
        TempData := CANEMULATE_RESULT_SUCCESS_SUPPORTED
       else
        TempData := CANEMULATE_RESULT_SUCCESS_UNSUPPORTED;
    end
   else
    TempData := CANEMULATE_RESULT_FAIL;

  Application.QueueAsyncCall(HandleCanEmulateResultMsg, TempData);
end;

procedure TForm1.HandleClearDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
begin                            
  Application.QueueAsyncCall(HandleClearDeviceMetricsOverrideResultMsg, ord(aSuccess and (aResult <> nil)));
end;

procedure TForm1.HandleSetDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
begin                      
  Application.QueueAsyncCall(HandleSetDeviceMetricsOverrideResultMsg, ord(aSuccess and (aResult <> nil)));
end;

end.
