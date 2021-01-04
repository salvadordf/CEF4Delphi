// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uBrowserFrame;

{$mode objfpc}{$H+}

{$I cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, SyncObjs, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls,
  uCEFWinControl, uCEFWindowParent, uCEFChromiumCore, uCEFChromium, uCEFTypes,
  uCEFInterfaces, uCEFConstants, uCEFLinkedWindowParent, uCEFChromiumEvents;

type
  TBrowserTitleEvent = procedure(Sender: TObject; const aTitle : string) of object;

  { TBrowserFrame }

  TBrowserFrame = class(TFrame)
      CEFLinkedWindowParent1: TCEFLinkedWindowParent;
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

      procedure CEFLinkedWindowParent1Enter(Sender: TObject);
      procedure CEFLinkedWindowParent1Exit(Sender: TObject);

      procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure Chromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
      procedure Chromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure Chromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
      procedure Chromium1GotFocus(Sender: TObject; const browser: ICefBrowser);
      procedure Chromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
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
      FClosing              : boolean;   // Indicates that this frame is destroying the browser
      FHomepage             : string;
      FOnBrowserDestroyed   : TNotifyEvent;
      FOnBrowserTitleChange : TBrowserTitleEvent;
      FBrowserCS            : TCriticalSection;
      FBrowserAddress       : string;
      FBrowserIsLoading     : boolean;
      FBrowserCanGoBack     : boolean;
      FBrowserCanGoForward  : boolean;
      FBrowserStatusText    : string;       
      FBrowserTitle         : string;

      procedure SetBrowserAddress(const aValue : string);     
      procedure SetBrowserIsLoading(aValue : boolean);
      procedure SetBrowserCanGoBack(aValue : boolean);
      procedure SetBrowserCanGoForward(aValue : boolean);  
      procedure SetBrowserStatusText(const aValue : string);
      procedure SetBrowserTitle(const aValue : string);

      function  GetBrowserAddress : string;       
      function  GetBrowserIsLoading : boolean;
      function  GetBrowserCanGoBack : boolean;
      function  GetBrowserCanGoForward : boolean; 
      function  GetBrowserStatusText : string;
      function  GetBrowserTitle : string;

      procedure BrowserCreatedMsg(Data: PtrInt);
      procedure BrowserUpdateAddressMsg(Data: PtrInt);
      procedure BrowserUpdateLoadingStateMsg(Data: PtrInt);
      procedure BrowserUpdateStatusTextMsg(Data: PtrInt);
      procedure BrowserUpdateTitleMsg(Data: PtrInt);

      procedure SendCompMessage(aMsg : cardinal);

      property  BrowserAddress       : string              read GetBrowserAddress      write SetBrowserAddress;
      property  BrowserIsLoading     : boolean             read GetBrowserIsLoading    write SetBrowserIsLoading;
      property  BrowserCanGoBack     : boolean             read GetBrowserCanGoBack    write SetBrowserCanGoBack;
      property  BrowserCanGoForward  : boolean             read GetBrowserCanGoForward write SetBrowserCanGoForward;
      property  BrowserStatusText    : string              read GetBrowserStatusText   write SetBrowserStatusText;
      property  BrowserTitle         : string              read GetBrowserTitle        write SetBrowserTitle;

    public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser;
      procedure   CloseBrowser;

      property  Closing              : boolean             read FClosing;
      property  Homepage             : string              read FHomepage              write FHomepage;
      property  OnBrowserDestroyed   : TNotifyEvent        read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property  OnBrowserTitleChange : TBrowserTitleEvent  read FOnBrowserTitleChange  write FOnBrowserTitleChange;
  end;

implementation

{$R *.lfm}

const
  CEF_UPDATEADDRESS      = 301;
  CEF_UPDATELOADINGSTATE = 302;
  CEF_UPDATESTATUSTEXT   = 303;    
  CEF_UPDATETITLE        = 304;

constructor TBrowserFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FClosing               := False;
  FHomepage              := '';
  FBrowserAddress        := '';
  FBrowserIsLoading      := False;
  FBrowserCanGoBack      := False;
  FBrowserCanGoForward   := False;
  FBrowserStatusText     := '';
  FBrowserTitle          := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;
  FBrowserCS             := TCriticalSection.Create;
end;

destructor TBrowserFrame.Destroy;
begin
  FBrowserCS.Free;
  inherited Destroy;
end;

procedure TBrowserFrame.SetBrowserAddress(const aValue : string);
begin
  FBrowserCS.Acquire;
  FBrowserAddress := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserIsLoading(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserIsLoading := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserCanGoBack(aValue : boolean);  
begin
  FBrowserCS.Acquire;
  FBrowserCanGoBack := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserCanGoForward(aValue : boolean);
begin
  FBrowserCS.Acquire;
  FBrowserCanGoForward := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserStatusText(const aValue : string);
begin
  FBrowserCS.Acquire;
  FBrowserStatusText := aValue;
  FBrowserCS.Release;
end;

procedure TBrowserFrame.SetBrowserTitle(const aValue : string);
begin
  FBrowserCS.Acquire;
  FBrowserTitle := aValue;
  FBrowserCS.Release;
end;

function TBrowserFrame.GetBrowserAddress : string;
begin
  FBrowserCS.Acquire;
  Result := FBrowserAddress;
  FBrowserCS.Release;
end;

function TBrowserFrame.GetBrowserIsLoading : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserIsLoading;
  FBrowserCS.Release;
end;

function TBrowserFrame.GetBrowserCanGoBack : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserCanGoBack;
  FBrowserCS.Release;
end;

function TBrowserFrame.GetBrowserCanGoForward : boolean;
begin
  FBrowserCS.Acquire;
  Result := FBrowserCanGoForward;
  FBrowserCS.Release;
end;

function TBrowserFrame.GetBrowserStatusText : string;
begin
  FBrowserCS.Acquire;
  Result := FBrowserStatusText;
  FBrowserCS.Release;
end;

function TBrowserFrame.GetBrowserTitle : string;
begin
  FBrowserCS.Acquire;
  Result := FBrowserTitle;
  FBrowserCS.Release;
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
  Chromium1.DefaultURL := FHomepage;
  Chromium1.CreateBrowser(CEFLinkedWindowParent1.Handle, CEFLinkedWindowParent1.BoundsRect);
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

procedure TBrowserFrame.ForwardBtnClick(Sender: TObject);
begin
  Chromium1.GoForward;
end;

procedure TBrowserFrame.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(UTF8Decode(URLCbx.Text));
end;

procedure TBrowserFrame.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  SendCompMessage(CEF_AFTERCREATED);
end;

// This is a workaround for the CEF issue #2026
// https://bitbucket.org/chromiumembedded/cef/issues/2026/multiple-major-keyboard-focus-issues-on
// We use CEFLinkedWindowParent1.OnEnter, CEFLinkedWindowParent1.OnExit and
// TChromium.OnGotFocus to avoid most of the focus issues.
// CEFLinkedWindowParent1.TabStop must be TRUE.
procedure TBrowserFrame.CEFLinkedWindowParent1Exit(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    Chromium1.SendCaptureLostEvent;
end;    

procedure TBrowserFrame.CEFLinkedWindowParent1Enter(Sender: TObject);
begin
  if not(csDesigning in ComponentState) and
     Chromium1.Initialized and
     not(Chromium1.FrameIsFocused) then
    Chromium1.SendFocusEvent(True);
end;   

procedure TBrowserFrame.Chromium1GotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  CEFLinkedWindowParent1.SetFocus;
end;

procedure TBrowserFrame.BackBtnClick(Sender: TObject);
begin
  Chromium1.GoBack;
end;

procedure TBrowserFrame.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  BrowserAddress := url;
  SendCompMessage(CEF_UPDATEADDRESS);
end;

procedure TBrowserFrame.Chromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then FOnBrowserDestroyed(self);
end;

procedure TBrowserFrame.Chromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue;
  var noJavascriptAccess, Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TBrowserFrame.Chromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  // continue closing the browser
  aAction := cbaClose;
end;

procedure TBrowserFrame.Chromium1LoadError(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
var
  TempString : string;
begin
  if (errorCode = ERR_ABORTED) then exit;

  TempString := '<html><body bgcolor="white">' +
                '<h2>Failed to load URL ' + failedUrl +
                ' with error ' + errorText +
                ' (' + inttostr(errorCode) + ').</h2></body></html>';

  Chromium1.LoadString(UTF8Decode(TempString), frame);
end;

procedure TBrowserFrame.Chromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  BrowserIsLoading    := isLoading;
  BrowserCanGoBack    := canGoBack;
  BrowserCanGoForward := canGoForward;

  SendCompMessage(CEF_UPDATELOADINGSTATE);
end;

procedure TBrowserFrame.Chromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TBrowserFrame.Chromium1StatusMessage(Sender: TObject;
  const browser: ICefBrowser; const value: ustring);
begin
  BrowserStatusText := value;
  SendCompMessage(CEF_UPDATESTATUSTEXT);
end;

procedure TBrowserFrame.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if (length(title) > 0) then
    BrowserTitle := title
   else
    BrowserTitle := Chromium1.DocumentURL;

  SendCompMessage(CEF_UPDATETITLE);
end;

procedure TBrowserFrame.BrowserCreatedMsg(Data: PtrInt);
begin
  CEFLinkedWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
end;

procedure TBrowserFrame.BrowserUpdateAddressMsg(Data: PtrInt);
var
  TempURL : string;
begin
  TempURL := BrowserAddress;

  if (URLCbx.Items.IndexOf(TempURL) < 0) then
     URLCbx.Items.Add(TempURL);

  URLCbx.Text := TempURL;
end;

procedure TBrowserFrame.BrowserUpdateLoadingStateMsg(Data: PtrInt);
begin
  BackBtn.Enabled    := BrowserCanGoBack;
  ForwardBtn.Enabled := BrowserCanGoForward;

  if BrowserIsLoading then
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

procedure TBrowserFrame.BrowserUpdateStatusTextMsg(Data: PtrInt);
begin
  StatusBar1.Panels[0].Text := BrowserStatusText;
end;

procedure TBrowserFrame.BrowserUpdateTitleMsg(Data: PtrInt);
begin
  if assigned(FOnBrowserTitleChange) then
    FOnBrowserTitleChange(self, BrowserTitle);
end;

procedure TBrowserFrame.SendCompMessage(aMsg : cardinal);
begin
  case aMsg of
    CEF_AFTERCREATED       : Application.QueueAsyncCall(@BrowserCreatedMsg, 0);
    CEF_UPDATEADDRESS      : Application.QueueAsyncCall(@BrowserUpdateAddressMsg, 0);
    CEF_UPDATELOADINGSTATE : Application.QueueAsyncCall(@BrowserUpdateLoadingStateMsg, 0);
    CEF_UPDATESTATUSTEXT   : Application.QueueAsyncCall(@BrowserUpdateStatusTextMsg, 0);
    CEF_UPDATETITLE        : Application.QueueAsyncCall(@BrowserUpdateTitleMsg, 0);
  end;
end;

end.


