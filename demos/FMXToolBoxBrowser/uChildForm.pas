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
//        Copyright © 2018 Salvador Diaz Fau. All rights reserved.
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

unit uChildForm;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uFMXChromium, uFMXWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes;

type
  TChildForm = class(TForm)
    FMXChromium1: TFMXChromium;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);

    procedure FMXChromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var noJavascriptAccess, Result: Boolean);
    procedure FMXChromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure FMXChromium1Close(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose       : boolean;  // Set to True in TFMXChromium.OnBeforeClose
    FClosing        : boolean;  // Set to True in the CloseQuery event.
    FMXWindowParent : TFMXWindowParent;
    FHomepage       : string;

    function  GetBrowserID : integer;

    procedure ResizeChild;
    procedure CreateFMXWindowParent;
    function  PostCustomMessage(aMessage : cardinal; wParam : cardinal = 0; lParam : integer = 0) : boolean;

  public
    procedure NotifyMoveOrResizeStarted;
    procedure DoBrowserCreated;
    procedure DoDestroyParent;
    procedure SendCloseMsg;

    property Closing   : boolean    read FClosing;
    property Homepage  : string     read FHomepage     write FHomepage;
    property BrowserID : integer    read GetBrowserID;
  end;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uCEFApplication, uFMXApplicationService, uMainForm;

// Child destruction steps
// =======================
// 1. FormCloseQuery calls TFMXChromium.CloseBrowser
// 2. TFMXChromium.OnClose sends a CEF_DESTROY message to destroy FMXWindowParent in the main thread.
// 3. TFMXChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.


function TChildForm.PostCustomMessage(aMessage, wParam : cardinal; lParam : integer) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHWND : HWND;
{$ENDIF}
begin
  Result := False;

  {$IFDEF MSWINDOWS}
  if (Handle <> nil) then
    begin
      TempHWND := FmxHandleToHWND(Handle);
      Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMessage, wParam, lParam);
    end;
  {$ENDIF}
end;

function TChildForm.GetBrowserID : integer;
begin
  Result := FMXChromium1.BrowserID;
end;

procedure TChildForm.CreateFMXWindowParent;
begin
  if (FMXWindowParent = nil) then
    begin
      FMXWindowParent := TFMXWindowParent.CreateNew(nil);
      FMXWindowParent.Reparent(Handle);
      ResizeChild;
      FMXWindowParent.Show;
    end;
end;

procedure TChildForm.ResizeChild;
var
  TempRect : System.Types.TRect;
begin
  if (FMXWindowParent <> nil) then
    begin
      TempRect.Top    := 0;
      TempRect.Left   := 0;
      TempRect.Right  := ClientWidth  - 1;
      TempRect.Bottom := ClientHeight - 1;

      FMXWindowParent.SetBounds(TempRect);
    end;
end;

procedure TChildForm.FMXChromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  // Now the browser is fully initialized we can send a message to the main form to load the initial web page.
  PostCustomMessage(CEF_AFTERCREATED, 0, BrowserID);
end;

procedure TChildForm.FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  PostCustomMessage(WM_CLOSE);
end;

procedure TChildForm.FMXChromium1BeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess,
  Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TChildForm.FMXChromium1Close(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  PostCustomMessage(CEF_DESTROY, 0, BrowserID);
  Result := True;
end;

procedure TChildForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TChildForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing := True;
      FMXChromium1.CloseBrowser(True);
    end;
end;

procedure TChildForm.FormCreate(Sender: TObject);
begin
  FCanClose       := False;
  FClosing        := False;
  FMXWindowParent := nil;
  FHomepage       := '';
end;

procedure TChildForm.FormDestroy(Sender: TObject);
begin
  // Tell the main form that a child has been destroyed.
  // The main form will check if this was the last child to close itself
  if (MainForm <> nil) then MainForm.SendChildDestroyedMsg;
end;

procedure TChildForm.FormResize(Sender: TObject);
begin
  // TFMXWindowParent has to be resized at runtime
  ResizeChild;
end;

procedure TChildForm.FormShow(Sender: TObject);
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

  if not(FMXChromium1.Initialized) then
    begin
      TempHandle      := FmxHandleToHWND(FMXWindowParent.Handle);
      TempClientRect  := FMXWindowParent.ClientRect;
      TempRect.Left   := round(TempClientRect.Left);
      TempRect.Top    := round(TempClientRect.Top);
      TempRect.Right  := round(TempClientRect.Right);
      TempRect.Bottom := round(TempClientRect.Bottom);

      FMXChromium1.CreateBrowser(TempHandle, TempRect);
    end;
end;

procedure TChildForm.NotifyMoveOrResizeStarted;
begin
  // This is needed to display some HTML elements correctly
  if (FMXChromium1 <> nil) then FMXChromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.DoBrowserCreated;
begin
  // Load the homepage after the browser is fully initialized
  if (length(FHomepage) > 0) then FMXChromium1.LoadURL(FHomepage);
end;

procedure TChildForm.DoDestroyParent;
begin
  // We destroy FMXWindowParent safely in the main thread and this will trigger the TFMXChromium.OnBeforeClose event.
  if (FMXWindowParent <> nil) then FreeAndNil(FMXWindowParent);
end;

procedure TChildForm.SendCloseMsg;
begin
  PostCustomMessage(WM_CLOSE);
end;

end.
