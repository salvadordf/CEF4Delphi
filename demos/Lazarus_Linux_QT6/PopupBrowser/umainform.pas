unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, SyncObjs, Dialogs, ExtCtrls,
  LMessages, StdCtrls,
  uCEFChromium, uCEFLinkedWindowParent, uCEFInterfaces, uCEFChromiumEvents,
  uCEFTypes, uchildform;
            
const
  CEF_CREATENEXTCHILD  = $A50;
  CEF_CHILDDESTROYED   = $A51;
  CEF_CLOSECHILD       = $A52;
  CEF_SETFOCUS         = $A53;
  CEF_TITLECHANGE      = $A54;

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
    FChildForm       : TChildForm;
    FCriticalSection : TCriticalSection;
    FCanClose        : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosingMainForm : boolean;  // Set to True in the CloseQuery event.
    FClosingChildren : boolean;  // Set to True in the CloseQuery event.
                                                      
    function  GetPopupChildCount : integer;

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure ClosePopupChildren;
    procedure CreateHiddenChildForm;

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCreateNextChildMsg(Data: PtrInt);
    procedure BrowserChildDestroyedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserSetFocusMsg(Data: PtrInt);

    property  PopupChildCount : integer  read  GetPopupChildCount;

  public       
    function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;
    procedure SendCompMessage(aMsg : cardinal; aData: PtrInt = 0);
  end;

var
  MainForm: TMainForm;
                           
procedure CreateGlobalCEFApp;
function StartMainProcess: boolean;

implementation

{$R *.lfm}

// This is demo shows how to create popup windows in CEF.

// You need to understand the SimpleBrowser demo completely before trying to understand this demo.

// When TChromium needs to show a new popup window it executes TChromium.OnBeforePopup.

// LCL components *MUST* be created and destroyed in the main thread but CEF executes the
// TChromium.OnBeforePopup in a different thread.

// For this reason this demo creates a hidden popup form (TChildForm) in case CEF needs to show a popup window.
// TChromium.OnBeforePopup calls TChildForm.CreateClientHandler to initialize some parameters and create the new ICefClient.
// After that, it sends a CEF_CREATENEXTCHILD message to show the popup form and create a new one.

// All the child forms must be correctly destroyed before closing the main form. Read the code comments in uChildForm.pas
// to know how the popup windows are destroyed.

// The main form close all active popup forms and waits until all of them have sent a CEF_CHILDDESTROYED message.

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE and it closes all child forms.
// 2. When all the child forms are closed then FormCloseQuery is triggered again, sets CanClose to FALSE, destroys CEFLinkedWindowParent1 and calls TChromium.CloseBrowser which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sets FCanClose := True and sends a CEF_BEFORECLOSE message to close the form in the main thread.

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
  FClosingChildren := False;
  FClosingMainForm := False;
  FCanClose        := False;
  FCriticalSection := TCriticalSection.Create;

  // CEF requires a native widget
  CEFLinkedWindowParent1.SetQTWidgetAsNative;

  // CEF can't find the HTML if we load file:///filename.html in Linux so we
  // add the full path manually.
  // The "Click me to open a file" button in PopupBrowser.html will not work
  // because of this limitation.
  AddressCb.Text       := 'file://' + IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'PopupBrowser.html';
  Chromium1.DefaultURL := UTF8Decode(AddressCb.Text);

  CreateHiddenChildForm;
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
  FClosingChildren := True;
  Visible          := False;

  if (PopupChildCount > 0) then
    begin
      ClosePopupChildren;
      CanClose := False;
    end
   else
    begin
      CanClose := FCanClose;

      if not(FClosingMainForm) then
        begin
          FClosingMainForm := True;
          Chromium1.CloseBrowser(True);
          FreeAndNil(CEFLinkedWindowParent1);
        end;
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
  Caption            := 'PopupBrowser';
  AddressPnl.Enabled := True;
  Chromium1.UpdateXWindowVisibility(True);
  CEFLinkedWindowParent1.UpdateSize;
  CEFLinkedWindowParent1.InvalidateChildren;
end;

procedure TMainForm.BrowserCreateNextChildMsg(Data: PtrInt);
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm <> nil) then
      begin
        FChildForm.ApplyPopupFeatures;
        FChildForm.Show;
      end;

    CreateHiddenChildForm;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TMainForm.BrowserChildDestroyedMsg(Data: PtrInt);
begin
  if FClosingChildren and (PopupChildCount = 0) then Close;
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
function TMainForm.GetPopupChildCount : integer;
var
  i        : integer;
  TempForm : TCustomForm;
begin
  Result := 0;
  i      := pred(screen.CustomFormCount);

  while (i >= 0) do
    begin
      TempForm := screen.CustomForms[i];

      if (TempForm is TChildForm) and
         TChildForm(TempForm).ClientInitialized then
        inc(Result);

      dec(i);
    end;
end;

procedure TMainForm.ClosePopupChildren;
var
  i        : integer;
  TempForm : TCustomForm;
begin
  i := pred(screen.CustomFormCount);

  while (i >= 0) do
    begin
      TempForm := screen.CustomForms[i];

      if (TempForm is TChildForm) and
         TChildForm(TempForm).ClientInitialized and
         not(TChildForm(TempForm).Closing) then
        TempForm.Close;

      dec(i);
    end;
end;

procedure TMainForm.CreateHiddenChildForm;
var
  TempSize : TCefSize;
begin
  // Linux requires a fully formed window in order to add a Chromium browser so
  // we show the next popup window outside the visible screen space and then we
  // hide it.
  FChildForm               := TChildForm.Create(self);
  TempSize.width           := FChildForm.Width;
  TempSize.height          := FChildForm.Height;
  FChildForm.Width         := 0;
  FChildForm.Height        := 0;
  FChildForm.Show;
  FChildForm.Hide;
  FChildForm.Width         := TempSize.width;
  FChildForm.Height        := TempSize.height;
  // Center the child form on the screen by default
  FChildForm.Top           := (screen.Height - FChildForm.Height) div 2;
  FChildForm.Left          := (screen.Width  - FChildForm.Width)  div 2;
end;

function TMainForm.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                       var   client          : ICefClient;
                                       const targetFrameName : string;
                                       const popupFeatures   : TCefPopupFeatures) : boolean;
begin
  try
    FCriticalSection.Acquire;

    if (FChildForm <> nil) and
       FChildForm.CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures) then
      begin
        SendCompMessage(CEF_CREATENEXTCHILD);
        Result := True;
      end;
  finally
    FCriticalSection.Release;
  end;
end;

procedure TMainForm.SendCompMessage(aMsg : cardinal; aData: PtrInt);
begin
  case aMsg of
    CEF_AFTERCREATED    : Application.QueueAsyncCall(@BrowserCreatedMsg, aData);
    CEF_CREATENEXTCHILD : Application.QueueAsyncCall(@BrowserCreateNextChildMsg, aData);
    CEF_CHILDDESTROYED  : Application.QueueAsyncCall(@BrowserChildDestroyedMsg, aData);
    CEF_BEFORECLOSE     : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
    CEF_SETFOCUS        : Application.QueueAsyncCall(@BrowserSetFocusMsg, aData);
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
  if Chromium1.IsSameBrowser(browser) then
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
  case targetDisposition of
    CEF_WOD_NEW_FOREGROUND_TAB,
    CEF_WOD_NEW_BACKGROUND_TAB,
    CEF_WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    CEF_WOD_NEW_POPUP  : Result := not(CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

procedure TMainForm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  if Chromium1.IsSameBrowser(browser) then
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

