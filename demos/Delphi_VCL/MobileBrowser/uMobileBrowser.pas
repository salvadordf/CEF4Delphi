unit uMobileBrowser;

{$I ..\..\..\source\cef.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask, Vcl.Samples.Spin,
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes, uCEFJson,
  uCEFWinControl, uCEFChromiumCore, uCEFDictionaryValue;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Chromium1: TChromium;
    Panel1: TPanel;
    Panel2: TPanel;
    AddressPnl: TPanel;
    GoBtn: TButton;
    CEFWindowParent1: TCEFWindowParent;
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
    AddressCb: TComboBox;

    procedure GoBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CanEmulateBtnClick(Sender: TObject);
    procedure OverrideUserAgentBtnClick(Sender: TObject);
    procedure EmulateTouchChkClick(Sender: TObject);
    procedure ClearDeviceMetricsOverrideBtnClick(Sender: TObject);
    procedure OverrideDeviceMetricsBtnClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
    procedure Chromium1DevToolsMethodResult(Sender: TObject; const browser: ICefBrowser; message_id: Integer; success: Boolean; const result: ICefValue);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FPendingMsgID : integer;

    // You have to handle this two messages to call NotifyMoveOrResizeStarted or some page elements will be misaligned.
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    // You also have to handle these two messages to set GlobalCEFApp.OsmodalLoop
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;

    procedure HandleSetUserAgentResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleSetTouchEmulationEnabledResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleCanEmulateResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleClearDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
    procedure HandleSetDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication, uCefMiscFunctions, uCEFListValue;

// This demo allows you to emulate a mobile browser using the "Emulation" namespace of the DevTools.
// It's necesary to reload the browser after using the controls in the right panel.

// This demo uses a TChromium and a TCEFWindowParent

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

const
  DEVTOOLS_SETUSERAGENTOVERRIDE_MSGID       = 1;
  DEVTOOLS_SETTOUCHEMULATIONENABLED_MSGID   = 2;
  DEVTOOLS_CANEMULATE_MSGID                 = 3;
  DEVTOOLS_CLEARDEVICEMETRICSOVERRIDE_MSGID = 4;
  DEVTOOLS_SETDEVICEMETRICSOVERRIDE_MSGID   = 5;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.cache                      := 'cache';
  GlobalCEFApp.EnablePrintPreview         := True;
  GlobalCEFApp.EnableGPU                  := True;
  // Enable this line to set the user agent. This is needed for https://abrahamjuliot.github.io/creepjs/
  //GlobalCEFApp.UserAgent                  := 'Mozilla/5.0 (Linux; Android 11; M2102K1G) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.101 Mobile Safari/537.36';
  {$IFDEF DEBUG}
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  {$ENDIF}
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCanClose            := False;
  FClosing             := False;
  FPendingMsgID        := 0;
  Chromium1.DefaultURL := AddressCb.Text;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then Timer1.Enabled := True;
end;

procedure TForm1.CanEmulateBtnClick(Sender: TObject);
begin
  FPendingMsgID := DEVTOOLS_CANEMULATE_MSGID;
  Chromium1.ExecuteDevToolsMethod(0, 'Emulation.canEmulate', nil);
end;

procedure TForm1.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TForm1.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TForm1.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
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

procedure TForm1.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
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

procedure TForm1.BrowserCreatedMsg(var aMessage : TMessage);
begin
  Caption            := 'Mobile Browser';
  AddressPnl.Enabled := True;
end;

procedure TForm1.GoBtnClick(Sender: TObject);
begin
  // This will load the URL in the edit box
  Chromium1.LoadURL(AddressCb.Text);
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

    TempFormatSettings := TFormatSettings.Create;
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
  TempMetadataDict, TempBrandDict, TempFullVersionDict: ICefDictionaryValue;
  TempBrandsArray, TempFullVersionListArray: ICefListValue;
begin
  try
    TempParams := TCefDictionaryValueRef.New;

    TempMetadataDict := TCefDictionaryValueRef.New;
    TempBrandsArray := TCefListValueRef.New;
    TempFullVersionListArray := TCefListValueRef.New;

    TempBrandsArray.SetSize(1);
    TempBrandDict := TCefDictionaryValueRef.New;
    TempBrandDict.SetString('brand', 'Chromium');
    TempBrandDict.SetString('version', '91');
    TempBrandsArray.SetDictionary(0, TempBrandDict);


    TempFullVersionListArray.SetSize(1);
    TempFullVersionDict := TCefDictionaryValueRef.New;
    TempFullVersionDict.SetString('brand', 'Chromium'); //Not:A
    TempFullVersionDict.SetString('version', '91.0.4472.114');
    TempFullVersionListArray.SetDictionary(0, TempFullVersionDict);

    TempMetadataDict.SetList('brands', TempBrandsArray);
    TempMetadataDict.SetList('fullVersionList', TempFullVersionListArray);
    TempMetadataDict.SetString('fullVersion', '91.0.4472.114'); // Deprecated
    TempMetadataDict.SetString('platform', 'Android'); //or Windows
    TempMetadataDict.SetString('platformVersion', '12');
    TempMetadataDict.SetString('architecture', 'arm');
    TempMetadataDict.SetString('model', 'SM-F916N');
    TempMetadataDict.SetBool('mobile', true);
    TempMetadataDict.SetString('bitness', '32');

    TempParams.SetString('userAgent', UserAgentCb.Text);
    // Setting the userAgentMetadata value is optional and can be omited.
    // All the values in TempMetadataDict are just an example and they should be customized for each use case.
    TempParams.SetDictionary('userAgentMetadata', TempMetadataDict);

    FPendingMsgID := DEVTOOLS_SETUSERAGENTOVERRIDE_MSGID;
    Chromium1.ExecuteDevToolsMethod(0, 'Emulation.setUserAgentOverride', TempParams);
  finally
    TempParams := nil;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TForm1.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TForm1.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
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

procedure TForm1.HandleSetUserAgentResult(aSuccess : boolean; const aResult: ICefValue);
begin
  if aSuccess and (aResult <> nil) then
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Successful SetUserAgentOverride');
      end)
   else
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Unsuccessful SetUserAgentOverride');
      end);
end;

procedure TForm1.HandleSetTouchEmulationEnabledResult(aSuccess : boolean; const aResult: ICefValue);
begin
  if aSuccess and (aResult <> nil) then
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Successful SetTouchEmulationEnabled');
      end)
   else
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Unsuccessful SetTouchEmulationEnabled');
      end);
end;

procedure TForm1.HandleCanEmulateResult(aSuccess : boolean; const aResult: ICefValue);
var
  TempRsltDict : ICefDictionaryValue;
  TempResult : boolean;
begin
  if aSuccess and (aResult <> nil) then
    begin
      TempRsltDict := aResult.GetDictionary;

      if TCEFJson.ReadBoolean(TempRsltDict, 'result', TempResult) and TempResult then
        TThread.ForceQueue(nil,
          procedure
          begin
            LogMem.Lines.Add('Successful CanEmulate. Emulation is supported.');
          end)
       else
        TThread.ForceQueue(nil,
          procedure
          begin
            LogMem.Lines.Add('Successful CanEmulate. Emulation is not supported.');
          end);
    end
   else
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Unsuccessful CanEmulate');
      end);
end;

procedure TForm1.HandleClearDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
begin
  if aSuccess and (aResult <> nil) then
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Successful ClearDeviceMetricsOverride');
      end)
   else
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Unsuccessful ClearDeviceMetricsOverride');
      end);
end;

procedure TForm1.HandleSetDeviceMetricsOverrideResult(aSuccess : boolean; const aResult: ICefValue);
begin
  if aSuccess and (aResult <> nil) then
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Successful SetDeviceMetricsOverride');
      end)
   else
    TThread.ForceQueue(nil,
      procedure
      begin
        LogMem.Lines.Add('Unsuccessful SetDeviceMetricsOverride');
      end);
end;

end.
