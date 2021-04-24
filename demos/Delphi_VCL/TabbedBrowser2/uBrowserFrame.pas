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

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls,
  {$ENDIF}
  uCEFWinControl, uCEFWindowParent, uCEFChromiumCore, uCEFChromium,
  uCEFInterfaces, uCEFTypes, uCEFConstants;

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

      function  CreateClientHandler(var windowInfo : TCefWindowInfo; var client : ICefClient; const targetFrameName : string; const popupFeatures : TCefPopupFeatures) : boolean;

      procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
      procedure BrowserDestroyMsg(var aMessage : TMessage); message CEF_DESTROY;

    public
      constructor Create(AOwner : TComponent); override;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser;
      procedure   CloseBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;

      property    Closing              : boolean             read FClosing;
      property    Homepage             : string              read FHomepage              write FHomepage;
      property    OnBrowserDestroyed   : TNotifyEvent        read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property    OnBrowserTitleChange : TBrowserTitleEvent  read FOnBrowserTitleChange  write FOnBrowserTitleChange;
  end;

implementation

{$R *.dfm}

uses
  uBrowserTab;

constructor TBrowserFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FClosing               := False;
  FHomepage              := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;
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
  Chromium1.SendFocusEvent(True);
  Chromium1.AudioMuted := False;
end;

procedure TBrowserFrame.HideBrowser;
begin
  Chromium1.SendFocusEvent(False);
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
  if (URLCbx.Items.IndexOf(url) < 0) then URLCbx.Items.Add(url);

  URLCbx.Text := url;
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
  case targetDisposition of
    WOD_NEW_FOREGROUND_TAB,
    WOD_NEW_BACKGROUND_TAB,
    WOD_NEW_WINDOW : Result := True;  // For simplicity, this demo blocks new tabs and new windows.

    WOD_NEW_POPUP  : Result := not(CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures));

    else Result := False;
  end;
end;

procedure TBrowserFrame.Chromium1OpenUrlFromTab(      Sender            : TObject;
                                                const browser           : ICefBrowser;
                                                const frame             : ICefFrame;
                                                const targetUrl         : ustring;
                                                      targetDisposition : TCefWindowOpenDisposition;
                                                      userGesture       : Boolean;
                                                out   Result            : Boolean);
begin
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
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
                                                 errorCode : Integer;
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
  BackBtn.Enabled    := canGoBack;
  ForwardBtn.Enabled := canGoForward;

  if isLoading then
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

procedure TBrowserFrame.Chromium1StatusMessage(      Sender  : TObject;
                                               const browser : ICefBrowser;
                                               const value   : ustring);
begin
  StatusBar1.Panels[0].Text := value;
end;

procedure TBrowserFrame.Chromium1TitleChange(      Sender  : TObject;
                                             const browser : ICefBrowser;
                                             const title   : ustring);
begin
  if not(assigned(FOnBrowserTitleChange)) then exit;

  if (length(title) > 0) then
    FOnBrowserTitleChange(self, title)
   else
    FOnBrowserTitleChange(self, Chromium1.DocumentURL);
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

function TBrowserFrame.CreateClientHandler(var   windowInfo      : TCefWindowInfo;
                                           var   client          : ICefClient;
                                           const targetFrameName : string;
                                           const popupFeatures   : TCefPopupFeatures) : boolean;
begin
  Result := assigned(Parent) and
            (Parent is TBrowserTab) and
            TBrowserTab(Parent).CreateClientHandler(windowInfo, client, targetFrameName, popupFeatures);
end;

end.


