unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, SyncObjs, Dialogs, ExtCtrls,
  LMessages, StdCtrls,
  uCEFChromium, uCEFLinkedWindowParent, uCEFInterfaces, uCEFChromiumEvents,
  uCEFTypes;
            
const
  CEF_SETFOCUS = 1;

type
  { TMainForm }

  TMainForm = class(TForm)
    GoBtn: TButton;
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    Chromium1: TChromium;
    AddressCb: TComboBox;
    AddressPnl: TPanel;

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);

    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

    procedure GoBtnClick(Sender: TObject);        
    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);
  private   

  protected
    // Variables to control when can we destroy the form safely
    FCanClose  : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing   : boolean;  // Set to True in the CloseQuery event.

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure SendCompMessage(aMsg : cardinal; aData: PtrInt = 0);

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserSetFocusMsg(Data: PtrInt);

  public

  end;

var
  MainForm: TMainForm;
                           
procedure CreateGlobalCEFApp;
function StartMainProcess: boolean;

implementation

{$R *.lfm}

// This is a demo with the simplest web browser you can build using CEF4Delphi and
// it doesn't show any sign of progress like other web browsers do.

// Remember that it may take a few seconds to load if Windows update, your antivirus or
// any other windows service is using your hard drive.

// Depending on your internet connection it may take longer than expected.

// Please check that your firewall or antivirus are not blocking this application
// or the domain "google.com". If you don't live in the US, you'll be redirected to
// another domain which will take a little time too.

// This demo uses a TChromium and a TCEFLinkedWindowParent

// We need to use TCEFLinkedWindowParent in Linux to update the browser
// visibility and size automatically.

// Most of the TChromium events are executed in a CEF thread and this causes
// issues with most QT API functions. If you need to update the GUI, store the
// TChromium event parameters and use SendCompMessage (Application.QueueAsyncCall)
// to do it in the main application thread.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFLinkedWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends CEF_BEFORECLOSE to close the form.

uses
  Math,  
  uCEFMiscFunctions, uCEFApplication, uCEFLinuxFunctions, uCEFConstants;

var
  MainAppEvent : TEventObject;

{GlobalCEFApp functions}
{%Region}
procedure GlobalCEFApp_OnContextInitialized();
begin
  MainAppEvent.SetEvent;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;  
  GlobalCEFApp.LogFile                    := 'debug.log';
  GlobalCEFApp.LogSeverity                := LOGSEVERITY_INFO;
  GlobalCEFApp.RootCache                  := 'RootCache';
  GlobalCEFApp.Cache                      := IncludeTrailingPathDelimiter(GlobalCEFApp.RootCache) + 'cache';
  GlobalCEFApp.SetCurrentDir              := True;                             
  GlobalCEFApp.DisableZygote              := True;
  GlobalCEFApp.EnableGPU                  := True;  
  GlobalCEFApp.EnablePrintPreview         := True;
  GlobalCEFApp.OnContextInitialized       := @GlobalCEFApp_OnContextInitialized;
end;

function StartMainProcess: boolean;
begin
  Result := False;

  if GlobalCEFApp.StartMainProcess then
    begin
      // Wait until the context is initialized before initializing GTK.
      if (MainAppEvent.WaitFor(10000) = wrTimeout) then
        CefDebugLog('CEF initialization failure!')
       else
        Result := True;
    end;
end;    
{%Endregion}

{TForm events}
{%Region}
procedure TMainForm.FormCreate(Sender: TObject);
begin                             
  FCanClose   := False;
  FClosing    := False;

  // CEF requires a native widget
  CEFLinkedWindowParent1.SetQTWidgetAsNative;

  Chromium1.DefaultURL := UTF8Decode(AddressCb.Text);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  TempRect : TRect;
begin
  TempRect := Rect(0, 0, CEFLinkedWindowParent1.Width, CEFLinkedWindowParent1.Height);
  Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, TempRect);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not Chromium1.Initialized then
    begin
      FCanClose := True;
      FClosing  := True;
    end;

  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      Chromium1.CloseBrowser(True);
      FreeAndNil(CEFLinkedWindowParent1);
    end;
end;
{%Endregion}

{TCEFLinkedWindowParent events}
{%Region}
procedure TMainForm.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SetFocus(True);
end;

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use CEFLinkedWindowParent1.OnEnter, CEFLinkedWindowParent1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// CEFLinkedWindowParent1.TabStop must be TRUE.
procedure TMainForm.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;
{%Endregion}

{Message handlers}
{%Region}
procedure TMainForm.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption            := 'Simple Browser';
  AddressPnl.Enabled := True;
  Chromium1.UpdateXWindowVisibility(True);
  CEFLinkedWindowParent1.UpdateSize;
  CEFLinkedWindowParent1.InvalidateChildren;
end;

procedure TMainForm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TMainForm.BrowserSetFocusMsg(Data: PtrInt);
begin
  if assigned(CEFLinkedWindowParent1) then
    CEFLinkedWindowParent1.SetFocus;
end;

procedure TMainForm.WMMove(var Message: TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMSize(var Message: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;
{%Endregion}  

{Misc functions}
{%Region}
procedure TMainForm.SendCompMessage(aMsg : cardinal; aData: PtrInt);
begin
  case aMsg of
    CEF_AFTERCREATED : Application.QueueAsyncCall(@BrowserCreatedMsg, 0);
    CEF_BEFORECLOSE  : Application.QueueAsyncCall(@BrowserCloseFormMsg, 0);
    CEF_SETFOCUS     : Application.QueueAsyncCall(@BrowserSetFocusMsg, 0);
  end;
end;    

procedure TMainForm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(UTF8Decode(AddressCb.Text));
end;
{%Endregion}

{TChromium events}
{%Region}
procedure TMainForm.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can initialize the UI.
  SendCompMessage(CEF_AFTERCREATED);
end;

procedure TMainForm.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  if (Chromium1.BrowserId = 0) then
    begin
      // We must wait until all browsers trigger the TChromium.OnBeforeClose event
      // in order to close the application safely or we will have shutdown issues.
      FCanClose := True;
      SendCompMessage(CEF_BEFORECLOSE);
    end;
end;

procedure TMainForm.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; popup_id: Integer;
  const targetUrl, targetFrameName: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo;
  var client: ICefClient; var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;

procedure TMainForm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin   
  SendCompMessage(CEF_SETFOCUS);
end;

procedure TMainForm.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [CEF_WOD_NEW_FOREGROUND_TAB, CEF_WOD_NEW_BACKGROUND_TAB, CEF_WOD_NEW_POPUP, CEF_WOD_NEW_WINDOW]);
end;      
{%Endregion}

initialization
  MainAppEvent := TEventObject.Create(nil, True, False, 'MainAppEvent');

finalization
  if assigned(MainAppEvent) then
    FreeAndNil(MainAppEvent);

end.

