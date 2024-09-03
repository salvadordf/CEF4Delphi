unit uBrowserFrame;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, System.SyncObjs,
  {$ELSE}
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, SyncObjs,
  {$ENDIF}
  uCEFWinControl, uCEFWindowParent, uCEFChromiumCore, uCEFChromium,
  uCEFInterfaces, uCEFTypes, uCEFConstants;

const
  CEF_UPDATECAPTION    = WM_APP + $A55;
  CEF_UPDATEADDRESS    = WM_APP + $A56;
  CEF_UPDATESTATE      = WM_APP + $A57;
  CEF_UPDATESTATUSTEXT = WM_APP + $A58;


type
  TBrowserTitleEvent = procedure(Sender: TObject; const aTitle : string) of object;

  TBrowserFrame = class(TFrame)
      NavControlPnl: TPanel;
      NavButtonPnl: TPanel;
      BackBtn: TButton;
      ForwardBtn: TButton;
      ReloadBtn: TButton;
      StopBtn: TButton;
      URLEditPnl: TPanel;
      URLCbx: TComboBox;
      ConfigPnl: TPanel;
      GoBtn: TButton;
      StatusBar1: TStatusBar;
      Chromium1: TChromium;
      CEFWindowParent1: TCEFWindowParent;

      procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure Chromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
      procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
      procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: TCefErrorCode; const errorText, failedUrl: ustring);
      procedure Chromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
      procedure Chromium1StatusMessage(Sender: TObject; const browser: ICefBrowser; const value: ustring);
      procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
      procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
      procedure Chromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);

      procedure BackBtnClick(Sender: TObject);
      procedure ForwardBtnClick(Sender: TObject);
      procedure ReloadBtnClick(Sender: TObject);
      procedure StopBtnClick(Sender: TObject);
      procedure GoBtnClick(Sender: TObject);

    protected
      FCriticalSection      : TCriticalSection;
      FClosing              : boolean;   // Indicates that this frame is destroying the browser
      FHomepage             : string;
      FPendingAddress       : string;
      FPendingTitle         : string;
      FPendingStatus        : string;
      FPendingIsLoading     : boolean;
      FPendingCanGoBack     : boolean;
      FPendingCanGoForward  : boolean;
      FOnBrowserDestroyed   : TNotifyEvent;
      FOnBrowserTitleChange : TBrowserTitleEvent;

      function  GetInitialized : boolean;
      function  GetPendingAddress : string;
      function  GetPendingTitle : string;
      function  GetPendingStatus : string;
      function  GetPendingIsLoading : boolean;
      function  GetPendingCanGoBack : boolean;
      function  GetPendingCanGoForward : boolean;

      procedure SetPendingAddress(const aValue : string);
      procedure SetPendingTitle(const aValue : string);
      procedure SetPendingStatus(const aValue : string);
      procedure SetPendingIsLoading(aValue : boolean);
      procedure SetPendingCanGoBack(aValue : boolean);
      procedure SetPendingCanGoForward(aValue : boolean);

      procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
      procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;
      procedure BrowserUpdateCaptionMsg(var aMessage : TMessage); message CEF_UPDATECAPTION;
      procedure BrowserUpdateAddressMsg(var aMessage : TMessage); message CEF_UPDATEADDRESS;
      procedure BrowserUpdateStateMsg(var aMessage : TMessage); message CEF_UPDATESTATE;
      procedure BrowserUpdateStatusTextMsg(var aMessage : TMessage); message CEF_UPDATESTATUSTEXT;

      property PendingAddress       : string    read GetPendingAddress       write SetPendingAddress;
      property PendingTitle         : string    read GetPendingTitle         write SetPendingTitle;
      property PendingStatus        : string    read GetPendingStatus        write SetPendingStatus;
      property PendingIsLoading     : boolean   read GetPendingIsLoading     write SetPendingIsLoading;
      property PendingCanGoBack     : boolean   read GetPendingCanGoBack     write SetPendingCanGoBack;
      property PendingCanGoForward  : boolean   read GetPendingCanGoForward  write SetPendingCanGoForward;

    public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateAllHandles;
      procedure   CreateBrowser;
      procedure   CloseBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      function    CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;

      property    Initialized          : boolean             read GetInitialized;
      property    Closing              : boolean             read FClosing;
      property    Homepage             : string              read FHomepage              write FHomepage;
      property    OnBrowserDestroyed   : TNotifyEvent        read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property    OnBrowserTitleChange : TBrowserTitleEvent  read FOnBrowserTitleChange  write FOnBrowserTitleChange;
  end;

implementation

{$R *.dfm}

uses
  uCEFApplication, uCEFMiscFunctions, uBrowserTab, uCEFWindowInfoWrapper;

// The TChromium events are executed in a CEF thread and we should only update the
// GUI controls in the main application thread.

// This demo saves all the information in those events using a synchronization
// object and sends a custom message to update the GUI in the main application thread.

// Destruction steps
// =================
// 1. TBrowserFrame.CloseBrowser sets CanClose to FALSE calls TChromium.CloseBrowser
//    which triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1
//    in the main thread, which triggers the TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose triggers the TBrowserFrame.OnBrowserDestroyed event
//    which sends a CEF_DESTROYTAB message with the TabID to the main form.

constructor TBrowserFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FCriticalSection       := TCriticalSection.Create;
  FClosing               := False;
  FHomepage              := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;
end;

destructor TBrowserFrame.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
end;

procedure TBrowserFrame.CreateAllHandles;
begin
  CreateHandle;

  CEFWindowParent1.CreateHandle;
end;

function TBrowserFrame.GetInitialized : boolean;
begin
  Result := Chromium1.Initialized;
end;

function TBrowserFrame.GetPendingAddress : string;
begin
  FCriticalSection.Acquire;
  Result := FPendingAddress;
  FCriticalSection.Release;
end;

function TBrowserFrame.GetPendingTitle : string;
begin
  FCriticalSection.Acquire;
  Result := FPendingTitle;
  FCriticalSection.Release;
end;

function TBrowserFrame.GetPendingStatus : string;
begin
  FCriticalSection.Acquire;
  Result := FPendingStatus;
  FCriticalSection.Release;
end;

function TBrowserFrame.GetPendingIsLoading : boolean;
begin
  FCriticalSection.Acquire;
  Result := FPendingIsLoading;
  FCriticalSection.Release;
end;

function TBrowserFrame.GetPendingCanGoBack : boolean;
begin
  FCriticalSection.Acquire;
  Result := FPendingCanGoBack;
  FCriticalSection.Release;
end;

function TBrowserFrame.GetPendingCanGoForward : boolean;
begin
  FCriticalSection.Acquire;
  Result := FPendingCanGoForward;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.SetPendingAddress(const aValue : string);
begin
  FCriticalSection.Acquire;
  FPendingAddress := aValue;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.SetPendingTitle(const aValue : string);
begin
  FCriticalSection.Acquire;
  FPendingTitle := aValue;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.SetPendingStatus(const aValue : string);
begin
  FCriticalSection.Acquire;
  FPendingStatus := aValue;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.SetPendingIsLoading(aValue : boolean);
begin
  FCriticalSection.Acquire;
  FPendingIsLoading := aValue;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.SetPendingCanGoBack(aValue : boolean);
begin
  FCriticalSection.Acquire;
  FPendingCanGoBack := aValue;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.SetPendingCanGoForward(aValue : boolean);
begin
  FCriticalSection.Acquire;
  FPendingCanGoForward := aValue;
  FCriticalSection.Release;
end;

procedure TBrowserFrame.NotifyMoveOrResizeStarted;
begin
  Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TBrowserFrame.ReloadBtnClick(Sender: TObject);
begin
  Chromium1.Reload;
end;

procedure TBrowserFrame.StopBtnClick(Sender: TObject);
begin
  Chromium1.StopLoad;
end;

procedure TBrowserFrame.CreateBrowser;
begin
  Chromium1.DefaultURL   := FHomepage;
  Chromium1.RuntimeStyle := CEF_RUNTIME_STYLE_ALLOY;
  Chromium1.CreateBrowser(CEFWindowParent1);
end;

procedure TBrowserFrame.CloseBrowser;
begin
  if not(FClosing) then
    begin
      FClosing              := True;
      NavControlPnl.Enabled := False;
      Chromium1.CloseBrowser(True);
    end;
end;

procedure TBrowserFrame.ShowBrowser;
begin
  Chromium1.WasHidden(False);
  Chromium1.SetFocus(True);
  Chromium1.AudioMuted := False;
end;

procedure TBrowserFrame.HideBrowser;
begin
  Chromium1.SetFocus(False);
  Chromium1.WasHidden(True);
  Chromium1.AudioMuted := True;
end;

procedure TBrowserFrame.ForwardBtnClick(Sender: TObject);
begin
  Chromium1.GoForward;
end;

procedure TBrowserFrame.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(URLCbx.Text);
end;

procedure TBrowserFrame.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TBrowserFrame.BackBtnClick(Sender: TObject);
begin
  Chromium1.GoBack;
end;

procedure TBrowserFrame.Chromium1AddressChange(      Sender  : TObject;
                                               const browser : ICefBrowser;
                                               const frame   : ICefFrame;
                                               const url     : ustring);
begin
  PendingAddress := url;
  PostMessage(Handle, CEF_UPDATEADDRESS, 0, 0);
end;

procedure TBrowserFrame.Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then FOnBrowserDestroyed(self);
end;

procedure TBrowserFrame.Chromium1BeforePopup(      Sender             : TObject;
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
  Result := not(assigned(Parent) and
                (Parent is TBrowserTab) and
                TBrowserTab(Parent).DoOnBeforePopup(windowInfo, client, targetFrameName, popupFeatures, targetDisposition));
end;

procedure TBrowserFrame.Chromium1OpenUrlFromTab(      Sender            : TObject;
                                                const browser           : ICefBrowser;
                                                const frame             : ICefFrame;
                                                const targetUrl         : ustring;
                                                      targetDisposition : TCefWindowOpenDisposition;
                                                      userGesture       : Boolean;
                                                out   Result            : Boolean);
begin
  Result := assigned(Parent) and
            (Parent is TBrowserTab) and
            TBrowserTab(Parent).DoOpenUrlFromTab(targetUrl, targetDisposition);
end;

procedure TBrowserFrame.Chromium1Close(      Sender  : TObject;
                                       const browser : ICefBrowser;
                                       var   aAction : TCefCloseBrowserAction);
begin
  PostMessage(Handle, CEF_DESTROY, 0, 0);
  aAction := cbaDelay;
end;

procedure TBrowserFrame.Chromium1LoadError(      Sender    : TObject;
                                           const browser   : ICefBrowser;
                                           const frame     : ICefFrame;
                                                 errorCode : TCefErrorCode;
                                           const errorText : ustring;
                                           const failedUrl : ustring);
var
  TempString : string;
begin
  if (errorCode = ERR_ABORTED) then exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  Chromium1.LoadString(TempString, frame);
end;

procedure TBrowserFrame.Chromium1LoadingStateChange(      Sender       : TObject;
                                                    const browser      : ICefBrowser;
                                                          isLoading    : Boolean;
                                                          canGoBack    : Boolean;
                                                          canGoForward : Boolean);
begin
  PendingIsLoading    := isLoading;
  PendingCanGoBack    := canGoBack;
  PendingCanGoForward := canGoForward;

  PostMessage(Handle, CEF_UPDATESTATE, 0, 0);
end;

procedure TBrowserFrame.Chromium1StatusMessage(      Sender  : TObject;
                                               const browser : ICefBrowser;
                                               const value   : ustring);
begin
  PendingStatus := value;

  PostMessage(Handle, CEF_UPDATESTATUSTEXT, 0, 0);
end;

procedure TBrowserFrame.Chromium1TitleChange(      Sender  : TObject;
                                             const browser : ICefBrowser;
                                             const title   : ustring);
begin
  if (length(title) > 0) then
    PendingTitle := title
   else
    PendingTitle := Chromium1.DocumentURL;

  PostMessage(Handle, CEF_UPDATECAPTION, 0, 0);
end;

procedure TBrowserFrame.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
end;

procedure TBrowserFrame.BrowserDestroyMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.Free;
end;

procedure TBrowserFrame.BrowserUpdateCaptionMsg(var aMessage : TMessage);
begin
  if assigned(FOnBrowserTitleChange) then
    FOnBrowserTitleChange(self, PendingTitle);
end;

procedure TBrowserFrame.BrowserUpdateAddressMsg(var aMessage : TMessage);
var
  TempAddress : string;
begin
  TempAddress := PendingAddress;

  if (URLCbx.Items.IndexOf(TempAddress) < 0) then
    URLCbx.Items.Add(TempAddress);

  URLCbx.Text := TempAddress;
end;

procedure TBrowserFrame.BrowserUpdateStateMsg(var aMessage : TMessage);
begin
  BackBtn.Enabled    := PendingCanGoBack;
  ForwardBtn.Enabled := PendingCanGoForward;

  if PendingIsLoading then
    begin
      ReloadBtn.Enabled := False;
      StopBtn.Enabled   := True;
    end
   else
    begin
      ReloadBtn.Enabled := True;
      StopBtn.Enabled   := False;
    end;
end;

procedure TBrowserFrame.BrowserUpdateStatusTextMsg(var aMessage : TMessage);
begin
  StatusBar1.Panels[0].Text := PendingStatus;
end;

function TBrowserFrame.CreateClientHandler(var   windowInfo        : TCefWindowInfo;
                                           var   client            : ICefClient;
                                           const targetFrameName   : string;
                                           const popupFeatures     : TCefPopupFeatures) : boolean;
var
  TempRect : TRect;
begin
  if CEFWindowParent1.HandleAllocated and
     Chromium1.CreateClientHandler(client, False) then
    begin
      Result   := True;
      TempRect := CEFWindowParent1.ClientRect;

      TCEFWindowInfoWrapper.AsChild(windowInfo, CEFWindowParent1.Handle, TempRect);
    end
   else
    Result := False;
end;

end.


