unit uMiniBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, SyncObjs, Dialogs, ExtCtrls,
  LMessages, StdCtrls, Types, ComCtrls, Menus,
  uCEFChromium, uCEFLinkedWindowParent, uCEFInterfaces, uCEFChromiumEvents,
  uCEFTypes;

type

  { TMiniBrowserFrm }

  TMiniBrowserFrm = class(TForm)
    CEFLinkedWindowParent1: TCEFLinkedWindowParent;
    NavControlPnl: TPanel;
    NavButtonPnl: TPanel;
    URLEditPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    Chromium1: TChromium;
    StopBtn: TButton;
    StatusBar1: TStatusBar;
    URLCbx: TComboBox;
    ConfigPnl: TPanel;
    ConfigBtn: TButton;
    PopupMenu1: TPopupMenu;
    GoBtn: TButton;
    Print1: TMenuItem;
    N3: TMenuItem;
    Zoom1: TMenuItem;
    Inczoom1: TMenuItem;
    Deczoom1: TMenuItem;
    Resetzoom1: TMenuItem;
                                                               
    procedure CEFLinkedWindowParent1Enter(Sender: TObject);
    procedure CEFLinkedWindowParent1Exit(Sender: TObject);
    procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);

    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
                                                                                      
    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium1BeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure Chromium1ContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean);
    procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
    procedure Chromium1StatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);
    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);     
    procedure Chromium1ChromeCommand(Sender: TObject; const browser: ICefBrowser; command_id: Integer; disposition: TCefWindowOpenDisposition; var aResult: Boolean);

    procedure BackBtnClick(Sender: TObject);    
    procedure ConfigBtnClick(Sender: TObject);  
    procedure Deczoom1Click(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);    
    procedure GoBtnClick(Sender: TObject);       
    procedure Inczoom1Click(Sender: TObject);  
    procedure Print1Click(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure Resetzoom1Click(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose  : boolean;  // Set to True in TChromium.OnBeforeClose
    FClosing   : boolean;  // Set to True in the CloseQuery event.
    FStatus    : ustring;
    FCaption   : ustring;
    FBrowserCS : TCriticalSection;

    procedure AddURL(const aURL : string);      
    procedure SendCompMessage(aMsg : cardinal; aData: PtrInt = 0);  

    // CEF needs to handle these messages to call TChromium.NotifyMoveOrResizeStarted
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    procedure BrowserCreatedMsg(Data: PtrInt);
    procedure BrowserCloseFormMsg(Data: PtrInt);
    procedure BrowserSetFocusMsg(Data: PtrInt);
    procedure BrowserTitleChangedMsg(Data: PtrInt);     
    procedure BrowserStatusChangedMsg(Data: PtrInt);

  public

  end;

var
  MiniBrowserFrm : TMiniBrowserFrm;

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
// 1. FormCloseQuery sets CanClose to FALSE, destroys CEFLinkedWindowParent1 and calls TChromium.CloseAllBrowsers which triggers the TChromium.OnBeforeClose event.
// 2. TChromium.OnBeforeClose sets FCanClose := True and sends CEF_BEFORECLOSE to close the form.

uses
  uCEFMiscFunctions, uCEFApplication, uCEFConstants;

const
  MINIBROWSER_CONTEXTMENU_MUTEAUDIO       = MENU_ID_USER_FIRST + 1;
  MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO     = MENU_ID_USER_FIRST + 2;
  MINIBROWSER_CONTEXTMENU_INCZOOM         = MENU_ID_USER_FIRST + 3;
  MINIBROWSER_CONTEXTMENU_DECZOOM         = MENU_ID_USER_FIRST + 4;
  MINIBROWSER_CONTEXTMENU_RESETZOOM       = MENU_ID_USER_FIRST + 5;

  CEF_TITLECHANGE      = $B01;
  CEF_STATUSCHANGE     = $B02;   
  CEF_SETFOCUS         = $B03;

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
procedure TMiniBrowserFrm.FormActivate(Sender: TObject);
var
  TempRect : TRect;
begin
  TempRect := Rect(0, 0, CEFLinkedWindowParent1.Width, CEFLinkedWindowParent1.Height);
  Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, TempRect);
end;

procedure TMiniBrowserFrm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FreeAndNil(FBrowserCS);
end;

procedure TMiniBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
      Chromium1.CloseAllBrowsers;
      FreeAndNil(CEFLinkedWindowParent1);
    end;
end;

procedure TMiniBrowserFrm.FormCreate(Sender: TObject);
begin
  FCanClose            := False;
  FClosing             := False; 
  FBrowserCS           := TCriticalSection.Create;

  // The MultiBrowserMode store all the browser references in TChromium.
  // The first browser reference is the browser in the main form.
  // When MiniBrowser allows CEF to create child popup browsers it will also
  // store their reference inside TChromium and you can use all the TChromium's
  // methods and properties to manipulate those browsers.
  // To do that call TChromium.SelectBrowser with the browser ID that will be
  // used when you call any method or property in TChromium.
  Chromium1.MultiBrowserMode := True;
  Chromium1.DefaultURL       := UTF8Decode(URLCbx.Text);

  // CEF requires a native widget
  CEFLinkedWindowParent1.SetQTWidgetAsNative;
end;       
{%Endregion}
          
{TCEFLinkedWindowParent events}
{%Region}
procedure TMiniBrowserFrm.CEFLinkedWindowParent1Enter(Sender: TObject);
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
procedure TMiniBrowserFrm.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;
{%Endregion}

{Message handlers}
{%Region}
procedure TMiniBrowserFrm.BrowserCreatedMsg(Data: PtrInt);
begin
  Caption               := 'MiniBrowser';
  NavControlPnl.Enabled := True;
  Chromium1.UpdateXWindowVisibility(True);
  CEFLinkedWindowParent1.UpdateSize;
  CEFLinkedWindowParent1.InvalidateChildren;
end;

procedure TMiniBrowserFrm.BrowserCloseFormMsg(Data: PtrInt);
begin
  Close;
end;

procedure TMiniBrowserFrm.BrowserSetFocusMsg(Data: PtrInt);
begin
  if assigned(CEFLinkedWindowParent1) then
    CEFLinkedWindowParent1.SetFocus;
end;

procedure TMiniBrowserFrm.BrowserTitleChangedMsg(Data: PtrInt);
begin                 
  FBrowserCS.Acquire;
  Caption := UTF8Encode(FCaption);      
  FBrowserCS.Release;
end;

procedure TMiniBrowserFrm.BrowserStatusChangedMsg(Data: PtrInt);
begin
  FBrowserCS.Acquire;
  StatusBar1.Panels[1].Text := UTF8Encode(FStatus);
  FBrowserCS.Release;
end;              

procedure TMiniBrowserFrm.WMMove(var Message: TLMMove);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMSize(var Message: TLMSize);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TMiniBrowserFrm.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  inherited;
  Chromium1.NotifyMoveOrResizeStarted;
end;       
{%Endregion}

{Misc functions}
{%Region}
procedure TMiniBrowserFrm.AddURL(const aURL : string);
begin
  if (URLCbx.Items.IndexOf(aURL) < 0) then
    URLCbx.Items.Add(aURL);

  URLCbx.Text := aURL;
end;

procedure TMiniBrowserFrm.SendCompMessage(aMsg : cardinal; aData: PtrInt);
begin
  case aMsg of
    CEF_AFTERCREATED : Application.QueueAsyncCall(@BrowserCreatedMsg, aData);
    CEF_BEFORECLOSE  : Application.QueueAsyncCall(@BrowserCloseFormMsg, aData);
    CEF_SETFOCUS     : Application.QueueAsyncCall(@BrowserSetFocusMsg, aData);
    CEF_TITLECHANGE  : Application.QueueAsyncCall(@BrowserTitleChangedMsg, aData);
    CEF_STATUSCHANGE : Application.QueueAsyncCall(@BrowserStatusChangedMsg, aData);
  end;
end;
{%Endregion}

{TChromium events}
{%Region}
procedure TMiniBrowserFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  if Chromium1.IsSameBrowser(browser) then
    SendCompMessage(CEF_AFTERCREATED);
end;

procedure TMiniBrowserFrm.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  // The main browser is being destroyed
  if (Chromium1.BrowserId = 0) then
    begin
      FCanClose := True;
      SendCompMessage(CEF_BEFORECLOSE);
    end;
end;

procedure TMiniBrowserFrm.Chromium1BeforeContextMenu(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  model.AddSeparator;
  if Chromium1.AudioMuted then
    model.AddItem(MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO, 'Unmute audio')
   else
    model.AddItem(MINIBROWSER_CONTEXTMENU_MUTEAUDIO,   'Mute audio');

  model.AddSeparator;
  if Chromium1.CanIncZoom then
    model.AddItem(MINIBROWSER_CONTEXTMENU_INCZOOM,   'Increment zoom');

  if Chromium1.CanDecZoom then
    model.AddItem(MINIBROWSER_CONTEXTMENU_DECZOOM,   'Decrement zoom');

  if Chromium1.CanResetZoom then
    model.AddItem(MINIBROWSER_CONTEXTMENU_RESETZOOM, 'Reset zoom');
end;

procedure TMiniBrowserFrm.Chromium1ContextMenuCommand(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer;
  eventFlags: TCefEventFlags; out Result: Boolean);
begin
  Result := False;

  case commandId of
    MINIBROWSER_CONTEXTMENU_UNMUTEAUDIO :
      Chromium1.AudioMuted := False;

    MINIBROWSER_CONTEXTMENU_MUTEAUDIO :
      Chromium1.AudioMuted := True;

    MINIBROWSER_CONTEXTMENU_INCZOOM :
      Chromium1.IncZoomCommand;

    MINIBROWSER_CONTEXTMENU_DECZOOM :
      Chromium1.DecZoomCommand;

    MINIBROWSER_CONTEXTMENU_RESETZOOM :
      Chromium1.ResetZoomCommand;
  end;
end;           

procedure TMiniBrowserFrm.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin                           
  if Chromium1.IsSameBrowser(browser) then
    SendCompMessage(CEF_SETFOCUS);
end;

procedure TMiniBrowserFrm.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode;
  const errorText, failedUrl: ustring);
var
  TempString : string;
begin
  if not(Chromium1.IsSameBrowser(browser)) or (errorCode = ERR_ABORTED) then
     exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  Chromium1.LoadString(TempString, frame);
end;

procedure TMiniBrowserFrm.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;

  FBrowserCS.Acquire;
  FStatus := value;
  FBrowserCS.Release;
  SendCompMessage(CEF_STATUSCHANGE);
end;

procedure TMiniBrowserFrm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if not(Chromium1.IsSameBrowser(browser)) then exit;
                       
  FBrowserCS.Acquire;
  FCaption := title;   
  FBrowserCS.Release;
  SendCompMessage(CEF_TITLECHANGE);
end;

procedure TMiniBrowserFrm.Chromium1ChromeCommand(Sender: TObject;
  const browser: ICefBrowser; command_id: Integer;
  disposition: TCefWindowOpenDisposition; var aResult: Boolean);
begin
  aResult := (command_id = IDC_HELP_PAGE_VIA_KEYBOARD) or // Block the new Chromium window created when the user presses F1 for help.
             (command_id = IDC_FULLSCREEN);               // Block the "switch to full screen" command when the user presses F11.
end;
{%Endregion}

{Address bar events}
{%Region}
procedure TMiniBrowserFrm.BackBtnClick(Sender: TObject);
begin
  Chromium1.GoBack;
end;

procedure TMiniBrowserFrm.ForwardBtnClick(Sender: TObject);
begin
  Chromium1.GoForward;
end;

procedure TMiniBrowserFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(UTF8Decode(URLCbx.Text));
end;

procedure TMiniBrowserFrm.ReloadBtnClick(Sender: TObject);
begin
  Chromium1.Reload;
end;

procedure TMiniBrowserFrm.Resetzoom1Click(Sender: TObject);
begin
  Chromium1.ResetZoomStep;
end; 

procedure TMiniBrowserFrm.StopBtnClick(Sender: TObject);
begin
  Chromium1.StopLoad;
end;            

procedure TMiniBrowserFrm.Inczoom1Click(Sender: TObject);
begin
  Chromium1.IncZoomStep;
end;           

procedure TMiniBrowserFrm.Deczoom1Click(Sender: TObject);
begin
  Chromium1.DecZoomStep;
end;

procedure TMiniBrowserFrm.Print1Click(Sender: TObject);
begin
  Chromium1.Print;
end;

procedure TMiniBrowserFrm.ConfigBtnClick(Sender: TObject);
var
  TempPoint : TPoint;
begin
  TempPoint.x := ConfigBtn.left;
  TempPoint.y := ConfigBtn.top + ConfigBtn.Height;
  TempPoint   := ConfigPnl.ClientToScreen(TempPoint);

  PopupMenu1.Popup(TempPoint.x, TempPoint.y);
end;          
{%Endregion}


initialization
  MainAppEvent := TEventObject.Create(nil, True, False, 'MainAppEvent');

finalization
  if assigned(MainAppEvent) then
    FreeAndNil(MainAppEvent);

end.
