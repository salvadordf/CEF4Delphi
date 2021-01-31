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
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation, FMX.TabControl,
  uCEFChromiumCore, uCEFFMXChromium, uCEFFMXWindowParent, uCEFInterfaces,
  uCEFTypes, uCEFConstants;

type
  TBrowserTitleEvent = procedure(Sender: TObject; const aTitle : string) of object;

  TBrowserFrame = class(TFrame)
      FMXChromium1: TFMXChromium;
      AddressLay: TLayout;
      GoBtn: TSpeedButton;
      NavButtonLay: TLayout;
      BackBtn: TSpeedButton;
      ForwardBtn: TSpeedButton;
      ReloadBtn: TSpeedButton;
      StopBtn: TSpeedButton;
      URLEdt: TEdit;
      WindowParentLay: TLayout;

      procedure BackBtnClick(Sender: TObject);
      procedure ForwardBtnClick(Sender: TObject);
      procedure ReloadBtnClick(Sender: TObject);
      procedure StopBtnClick(Sender: TObject);
      procedure GoBtnClick(Sender: TObject);
      procedure WindowParentLayResize(Sender: TObject);

      procedure FMXChromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure FMXChromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
      procedure FMXChromium1OpenUrlFromTab(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; out Result: Boolean);
      procedure FMXChromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
      procedure FMXChromium1AddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
      procedure FMXChromium1LoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer; const errorText, failedUrl: ustring);
      procedure FMXChromium1LoadingStateChange(Sender: TObject; const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
      procedure FMXChromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);

    protected
      FClosing              : boolean;   // Indicates that this frame is destroying the browser
      FHomepage             : string;    // Used to set the TChromium.DefaultURL property
      FOnBrowserDestroyed   : TNotifyEvent;
      FOnBrowserClosing     : TNotifyEvent;
      FOnBrowserTitleChange : TBrowserTitleEvent;
      FMXWindowParent       : TFMXWindowParent;   // TFMXWindowParent has to be created at runtime. See the SimpleFMXBrowser demo for more details.

      function  GetParentForm : TCustomForm;
      function  GetParentTab : TTabItem;
      function  GetFMXWindowParentRect : System.Types.TRect;
      procedure CreateFMXWindowParent;

    public
      constructor Create(AOwner : TComponent); override;
      procedure   NotifyMoveOrResizeStarted;
      procedure   CreateBrowser;
      procedure   CloseBrowser;
      procedure   ResizeBrowser;
      procedure   ShowBrowser;
      procedure   HideBrowser;
      procedure   DestroyWindowParent;

      property    ParentForm           : TCustomForm         read GetParentForm;
      property    ParentTab            : TTabItem            read GetParentTab;
      property    Closing              : boolean             read FClosing;
      property    Homepage             : string              read FHomepage              write FHomepage;
      property    OnBrowserDestroyed   : TNotifyEvent        read FOnBrowserDestroyed    write FOnBrowserDestroyed;
      property    OnBrowserTitleChange : TBrowserTitleEvent  read FOnBrowserTitleChange  write FOnBrowserTitleChange;
      property    OnBrowserClosing     : TNotifyEvent        read FOnBrowserClosing      write FOnBrowserClosing;
  end;

implementation

{$R *.fmx}

uses
  FMX.Platform, {$IFDEF MSWINDOWS}FMX.Platform.Win,{$ENDIF}
  uCEFMiscFunctions, uCEFApplication, uBrowserTab, uMainForm;

procedure TBrowserFrame.BackBtnClick(Sender: TObject);
begin
  FMXChromium1.GoBack;
end;

constructor TBrowserFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FClosing               := False;
  FHomepage              := '';
  FOnBrowserDestroyed    := nil;
  FOnBrowserTitleChange  := nil;
  FOnBrowserClosing      := nil;
  FMXWindowParent        := nil;
end;

function TBrowserFrame.GetFMXWindowParentRect : System.Types.TRect;
var
  TempRect : TRectF;
  TempScale : single;
begin
  TempScale       := FMXChromium1.ScreenScale;
  TempRect        := WindowParentLay.AbsoluteRect;
  Result.Left     := round(TempRect.Left   * TempScale);
  Result.Top      := round(TempRect.Top    * TempScale);
  Result.Right    := round(TempRect.Right  * TempScale) - 1;
  Result.Bottom   := round(TempREct.Bottom * TempScale) - 1;
end;

procedure TBrowserFrame.ReloadBtnClick(Sender: TObject);
begin
  FMXChromium1.Reload;
end;

procedure TBrowserFrame.ResizeBrowser;
begin
  if (FMXWindowParent <> nil) then
    FMXWindowParent.SetBounds(GetFMXWindowParentRect);
end;

procedure TBrowserFrame.ShowBrowser;
begin
  if (FMXWindowParent <> nil) then
    begin
      FMXWindowParent.WindowState := TWindowState.wsNormal;
      ResizeBrowser;
      FMXWindowParent.Show;
    end;
end;

procedure TBrowserFrame.HideBrowser;
begin
  if (FMXWindowParent <> nil) then FMXWindowParent.Hide;
end;

procedure TBrowserFrame.DestroyWindowParent;
begin
  if (FMXWindowParent <> nil) then FreeAndNil(FMXWindowParent);
end;

procedure TBrowserFrame.NotifyMoveOrResizeStarted;
begin
  FMXChromium1.NotifyMoveOrResizeStarted;
end;

procedure TBrowserFrame.StopBtnClick(Sender: TObject);
begin
  FMXChromium1.StopLoad;
end;

procedure TBrowserFrame.WindowParentLayResize(Sender: TObject);
begin
  ResizeBrowser;
end;

procedure TBrowserFrame.CreateFMXWindowParent;
begin
  if (FMXWindowParent = nil) then
    begin
      FMXWindowParent := TFMXWindowParent.CreateNew(nil);
      FMXWindowParent.Reparent(ParentForm.Handle);
      ShowBrowser;
    end;
end;

procedure TBrowserFrame.FMXChromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  TThread.Queue(nil, procedure
                     begin
                       URLEdt.Text := url;
                     end);
end;

procedure TBrowserFrame.FMXChromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  TThread.Queue(nil, procedure
                     begin
                       AddressLay.Enabled := True;
                     end);
end;

procedure TBrowserFrame.FMXChromium1BeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  if assigned(FOnBrowserDestroyed) then FOnBrowserDestroyed(Sender);
end;

procedure TBrowserFrame.FMXChromium1BeforePopup(Sender: TObject;
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

procedure TBrowserFrame.FMXChromium1Close(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  aAction := cbaDelay;
  if assigned(FOnBrowserClosing) then FOnBrowserClosing(self);
end;

procedure TBrowserFrame.FMXChromium1LoadError(Sender: TObject;
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

  FMXChromium1.LoadString(TempString, frame);
end;

procedure TBrowserFrame.FMXChromium1LoadingStateChange(Sender: TObject;
  const browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  TThread.Queue(nil, procedure
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
                     end);
end;

procedure TBrowserFrame.FMXChromium1OpenUrlFromTab(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl: ustring;
  targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
  out Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TBrowserFrame.FMXChromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  if assigned(FOnBrowserTitleChange) then FOnBrowserTitleChange(Sender, title);
end;

procedure TBrowserFrame.ForwardBtnClick(Sender: TObject);
begin
  FMXChromium1.GoForward;
end;

function TBrowserFrame.GetParentForm : TCustomForm;
var
  TempParent : TTabItem;
begin
  Result     := nil;
  TempParent := ParentTab;

  if (TempParent <> nil) and (TempParent is TBrowserTab) then
    Result := TBrowserTab(TempParent).ParentForm;
end;

function TBrowserFrame.GetParentTab : TTabItem;
var
  TempParent : TFMXObject;
begin
  Result     := nil;
  TempParent := Parent;

  while (TempParent <> nil) and not(TempParent is TTabItem) do
    TempParent := TempParent.Parent;

  if (TempParent <> nil) and (TempParent is TTabItem) then
    Result := TTabItem(TempParent);
end;

procedure TBrowserFrame.GoBtnClick(Sender: TObject);
begin
  FMXChromium1.LoadURL(URLEdt.Text);
end;

procedure TBrowserFrame.CreateBrowser;
var
  {$IFDEF MSWINDOWS}
  TempHandle : HWND;
  TempRect   : System.Types.TRect;
  TempClientRect : TRectF;
  {$ENDIF}
begin
  CreateFMXWindowParent;

  if not(FMXChromium1.Initialized) then
    begin
      {$IFDEF MSWINDOWS}
      TempHandle      := FmxHandleToHWND(FMXWindowParent.Handle);
      TempClientRect  := FMXWindowParent.ClientRect;
      TempRect.Left   := round(TempClientRect.Left);
      TempRect.Top    := round(TempClientRect.Top);
      TempRect.Right  := round(TempClientRect.Right);
      TempRect.Bottom := round(TempClientRect.Bottom);

      FMXChromium1.DefaultUrl := FHomepage;
      FMXChromium1.CreateBrowser(TempHandle, TempRect);
      {$ENDIF}
    end;
end;

procedure TBrowserFrame.CloseBrowser;
begin
  if not(FClosing) then
    begin
      FClosing           := True;
      AddressLay.Enabled := False;
      FMXChromium1.CloseBrowser(True);
    end;
end;

end.
