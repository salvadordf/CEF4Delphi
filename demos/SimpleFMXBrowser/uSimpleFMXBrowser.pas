// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

unit uSimpleFMXBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, uFMXWindowParent, uFMXChromium,
  uCEFInterfaces, uCEFConstants, uCEFTypes;

type
  TSimpleFMXBrowserFrm = class(TForm)
    AddressPnl: TPanel;
    AddressEdt: TEdit;
    GoBtn: TButton;
    FMXChromium1: TFMXChromium;
    Timer1: TTimer;
    procedure GoBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FMXChromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure FMXChromium1Close(Sender: TObject;
      const browser: ICefBrowser; out Result: Boolean);
    procedure FMXChromium1BeforeClose(Sender: TObject;
      const browser: ICefBrowser);
    procedure FMXChromium1BeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring;
      targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean;
      const popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient;
      var settings: TCefBrowserSettings; var noJavascriptAccess,
      Result: Boolean);
    procedure FormResize(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True in TFMXChromium.OnBeforeClose
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    FMXWindowParent : TFMXWindowParent;

    procedure LoadURL;
    procedure ResizeChild;
    procedure CreateFMXWindowParent;
    function  PostCustomMessage(aMessage : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;

  public
    procedure DoBrowserCreated;
    procedure DoDestroyParent;
    procedure NotifyMoveOrResizeStarted;
  end;

var
  SimpleFMXBrowserFrm: TSimpleFMXBrowserFrm;

implementation

{$R *.fmx}

// This is a demo with the simplest web browser you can build using CEF4Delphi
// with FMX components and it doesn't show any sign of progress like other web browsers do.

// Remember that it may take a few seconds to load if Windows update, your antivirus or
// any other windows service is using your hard drive.

// Depending on your internet connection it may take longer than expected.

// Please check that your firewall or antivirus are not blocking this application
// or the domain "google.com". If you don't live in the US, you'll be redirected to
// another domain which will take a little time too.

// This demo uses a TFMXChromium and a TFMXWindowParent.
// TFMXApplicationService is used to handle custom Windows messages

// Destruction steps
// =================
// 1. FormCloseQuery sets CanClose to FALSE calls TFMXChromium.CloseBrowser which triggers the TFMXChromium.OnClose event.
// 2. TFMXChromium.OnClose sends a CEFBROWSER_DESTROY message to destroy CEFWindowParent1 in the main thread, which triggers the TFMXChromium.OnBeforeClose event.
// 3. TFMXChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.

uses
  FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uCEFApplication, uFMXApplicationService;

procedure TSimpleFMXBrowserFrm.FMXChromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostCustomMessage(CEF_AFTERCREATED);
end;

procedure TSimpleFMXBrowserFrm.FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostCustomMessage(WM_CLOSE);
end;

procedure TSimpleFMXBrowserFrm.FMXChromium1BeforePopup(      Sender             : TObject;
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
                                                       var   noJavascriptAccess : boolean;
                                                       var   Result             : boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TSimpleFMXBrowserFrm.FMXChromium1Close(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  PostCustomMessage(CEF_DESTROY);
  Result := True;
end;

function TSimpleFMXBrowserFrm.PostCustomMessage(aMessage, wParam : cardinal; lParam : integer) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHWND : HWND;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TempHWND := FmxHandleToHWND(Handle);
  Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMessage, wParam, lParam);
  {$ELSE}
  Result   := False;
  {$ENDIF}
end;

procedure TSimpleFMXBrowserFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      Visible  := False;
      FMXChromium1.CloseBrowser(True);
    end;
end;

procedure TSimpleFMXBrowserFrm.FormCreate(Sender: TObject);
begin
  // TFMXApplicationService is used to handle custom Windows messages
  TFMXApplicationService.AddPlatformService;

  FCanClose       := False;
  FClosing        := False;
  FMXWindowParent := nil;
end;

procedure TSimpleFMXBrowserFrm.FormResize(Sender: TObject);
begin
  // TFMXWindowParent has to be resized at runtime
  ResizeChild;
end;

procedure TSimpleFMXBrowserFrm.ResizeChild;
var
  TempRect : System.Types.TRect;
begin
  if (FMXWindowParent <> nil) then
    begin
      TempRect.Top    := round(AddressPnl.Height);
      TempRect.Left   := 0;
      TempRect.Right  := ClientWidth  - 1;
      TempRect.Bottom := ClientHeight - 1;

      FMXWindowParent.SetBounds(TempRect);
    end;
end;

procedure TSimpleFMXBrowserFrm.CreateFMXWindowParent;
begin
  if (FMXWindowParent = nil) then
    begin
      FMXWindowParent := TFMXWindowParent.CreateNew(nil);
      FMXWindowParent.Reparent(Handle);
      ResizeChild;
      FMXWindowParent.Show;
    end;
end;

procedure TSimpleFMXBrowserFrm.FormShow(Sender: TObject);
var
  TempHandle : HWND;
  TempRect   : System.Types.TRect;
  TempClientRect : TRectF;
begin
  // TFMXWindowParent has to be created at runtime
  CreateFMXWindowParent;

  // You *MUST* call CreateBrowser to create and initialize the browser.
  // This will trigger the AfterCreated event when the browser is fully
  // initialized and ready to receive commands.

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(FMXChromium1.Initialized) then
    begin
      TempHandle      := FmxHandleToHWND(FMXWindowParent.Handle);
      TempClientRect  := FMXWindowParent.ClientRect;
      TempRect.Left   := round(TempClientRect.Left);
      TempRect.Top    := round(TempClientRect.Top);
      TempRect.Right  := round(TempClientRect.Right);
      TempRect.Bottom := round(TempClientRect.Bottom);

      if not(FMXChromium1.CreateBrowser(TempHandle, TempRect)) then Timer1.Enabled := True;
    end;
end;

procedure TSimpleFMXBrowserFrm.GoBtnClick(Sender: TObject);
begin
  LoadURL;
end;

procedure TSimpleFMXBrowserFrm.NotifyMoveOrResizeStarted;
begin
  if (FMXChromium1 <> nil) then FMXChromium1.NotifyMoveOrResizeStarted;
end;

procedure TSimpleFMXBrowserFrm.Timer1Timer(Sender: TObject);
var
  TempHandle : HWND;
  TempRect   : System.Types.TRect;
  TempClientRect : TRectF;
begin
  Timer1.Enabled  := False;
  TempHandle      := FmxHandleToHWND(FMXWindowParent.Handle);
  TempClientRect  := FMXWindowParent.ClientRect;
  TempRect.Left   := round(TempClientRect.Left);
  TempRect.Top    := round(TempClientRect.Top);
  TempRect.Right  := round(TempClientRect.Right);
  TempRect.Bottom := round(TempClientRect.Bottom);

  if not(FMXChromium1.CreateBrowser(TempHandle, TempRect)) and not(FMXChromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TSimpleFMXBrowserFrm.DoBrowserCreated;
begin
  // Now the browser is fully initialized
  Caption            := 'Simple FMX Browser';
  AddressPnl.Enabled := True;
  LoadURL;
end;

procedure TSimpleFMXBrowserFrm.DoDestroyParent;
begin
  if (FMXWindowParent <> nil) then FMXWindowParent.Free;
end;

procedure TSimpleFMXBrowserFrm.LoadURL;
begin
  FMXChromium1.LoadURL(AddressEdt.Text);
end;

end.
