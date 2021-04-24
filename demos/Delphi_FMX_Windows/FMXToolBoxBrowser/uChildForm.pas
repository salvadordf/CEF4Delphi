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

unit uChildForm;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uCEFFMXChromium, uCEFFMXWindowParent, uCEFInterfaces, uCEFConstants, uCEFTypes,
  uCEFChromiumCore;

type
  TChildForm = class(TForm)
    FMXChromium1: TFMXChromium;

    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);

    procedure FMXChromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
    procedure FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure FMXChromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose       : boolean;  // Set to True in TFMXChromium.OnBeforeClose
    FClosing        : boolean;  // Set to True in the CloseQuery event.
    FMXWindowParent : TFMXWindowParent;
    FHomepage       : string;

    {$IFDEF MSWINDOWS}
    // This is a workaround for the issue #253
    // https://github.com/salvadordf/CEF4Delphi/issues/253
    FCustomWindowState      : TWindowState;
    FOldWndPrc              : TFNWndProc;
    FFormStub               : Pointer;
    {$ENDIF}

    function  GetBrowserID : integer;

    procedure ResizeChild;
    procedure CreateFMXWindowParent;
    function  GetFMXWindowParentRect : System.Types.TRect;
    function  PostCustomMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    procedure NotifyMoveOrResizeStarted;

    {$IFDEF MSWINDOWS}
    function  GetCurrentWindowState : TWindowState;
    procedure UpdateCustomWindowState;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure CustomWndProc(var aMessage: TMessage);
    {$ENDIF}

  public
    procedure SendCloseMsg;
    procedure SendShowBrowserMsg;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    property Closing   : boolean    read FClosing;
    property Homepage  : string     read FHomepage     write FHomepage;
    property BrowserID : integer    read GetBrowserID;
  end;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uCEFApplication, uMainForm;

// Child destruction steps
// =======================
// 1. FormCloseQuery calls TFMXChromium.CloseBrowser
// 2. TFMXChromium.OnClose sends a CEF_DESTROY message to destroy FMXWindowParent in the main thread.
// 3. TFMXChromium.OnBeforeClose sets FCanClose := True and sends WM_CLOSE to the form.


function TChildForm.PostCustomMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
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
      Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMsg, aWParam, aLParam);
    end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TChildForm.CreateHandle;
begin
  inherited CreateHandle;

  FFormStub  := MakeObjectInstance(CustomWndProc);
  FOldWndPrc := TFNWndProc(SetWindowLongPtr(FmxHandleToHWND(Handle), GWLP_WNDPROC, NativeInt(FFormStub)));
end;

procedure TChildForm.DestroyHandle;
begin
  SetWindowLongPtr(FmxHandleToHWND(Handle), GWLP_WNDPROC, NativeInt(FOldWndPrc));
  FreeObjectInstance(FFormStub);

  inherited DestroyHandle;
end;

procedure TChildForm.CustomWndProc(var aMessage: TMessage);
const
  SWP_STATECHANGED = $8000;  // Undocumented
var
  TempWindowPos : PWindowPos;
begin
  try
    case aMessage.Msg of
      WM_ENTERMENULOOP :
        if (aMessage.wParam = 0) and
           (GlobalCEFApp <> nil) then
          GlobalCEFApp.OsmodalLoop := True;

      WM_EXITMENULOOP :
        if (aMessage.wParam = 0) and
           (GlobalCEFApp <> nil) then
          GlobalCEFApp.OsmodalLoop := False;

      WM_MOVE,
      WM_MOVING : NotifyMoveOrResizeStarted;

      WM_SIZE :
        if (aMessage.wParam = SIZE_RESTORED) then
          UpdateCustomWindowState;

      WM_WINDOWPOSCHANGING :
        begin
          TempWindowPos := TWMWindowPosChanging(aMessage).WindowPos;
          if ((TempWindowPos.Flags and SWP_STATECHANGED) <> 0) then
            UpdateCustomWindowState;
        end;

      WM_SHOWWINDOW :
        if (aMessage.wParam <> 0) and (aMessage.lParam = SW_PARENTOPENING) then
          PostCustomMessage(CEF_SHOWBROWSER);

      CEF_DESTROY :
        if (FMXWindowParent <> nil) then
          FreeAndNil(FMXWindowParent);

      CEF_SHOWBROWSER :
        if (FMXWindowParent <> nil) then
          begin
            FMXWindowParent.WindowState := TWindowState.wsNormal;
            FMXWindowParent.Show;
            FMXWindowParent.SetBounds(GetFMXWindowParentRect);
          end;
    end;

    aMessage.Result := CallWindowProc(FOldWndPrc, FmxHandleToHWND(Handle), aMessage.Msg, aMessage.wParam, aMessage.lParam);
  except
    on e : exception do
      if CustomExceptionHandler('TChildForm.CustomWndProc', e) then raise;
  end;
end;

procedure TChildForm.UpdateCustomWindowState;
var
  TempNewState : TWindowState;
begin
  TempNewState := GetCurrentWindowState;

  if (FCustomWindowState <> TempNewState) then
    begin
      // This is a workaround for the issue #253
      // https://github.com/salvadordf/CEF4Delphi/issues/253
      if (FCustomWindowState = TWindowState.wsMinimized) then
        SendShowBrowserMsg;

      FCustomWindowState := TempNewState;
    end;
end;

function TChildForm.GetCurrentWindowState : TWindowState;
var
  TempPlacement : TWindowPlacement;
  TempHWND      : HWND;
begin
  // TForm.WindowState is not updated correctly in FMX forms.
  // We have to call the GetWindowPlacement function in order to read the window state correctly.

  Result   := TWindowState.wsNormal;
  TempHWND := FmxHandleToHWND(Handle);

  ZeroMemory(@TempPlacement, SizeOf(TWindowPlacement));
  TempPlacement.Length := SizeOf(TWindowPlacement);

  if GetWindowPlacement(TempHWND, @TempPlacement) then
    case TempPlacement.showCmd of
      SW_SHOWMAXIMIZED : Result := TWindowState.wsMaximized;
      SW_SHOWMINIMIZED : Result := TWindowState.wsMinimized;
    end;
end;
{$ENDIF}

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

function TChildForm.GetFMXWindowParentRect : System.Types.TRect;
var
  TempScale : single;
begin
  TempScale     := FMXChromium1.ScreenScale;
  Result.Left   := 0;
  Result.Top    := 0;
  Result.Right  := round(ClientWidth  * TempScale) - 1;
  Result.Bottom := round(ClientHeight * TempScale) - 1;
end;

procedure TChildForm.ResizeChild;
begin
  if (FMXWindowParent <> nil) then
    FMXWindowParent.SetBounds(GetFMXWindowParentRect);
end;

procedure TChildForm.FMXChromium1BeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  FCanClose := True;
  SendCloseMsg;
end;

procedure TChildForm.FMXChromium1BeforePopup(Sender: TObject;
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

procedure TChildForm.FMXChromium1Close(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
begin
  PostCustomMessage(CEF_DESTROY, 0, BrowserID);
  aAction := cbaDelay;
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

  {$IFDEF MSWINDOWS}
  FCustomWindowState := WindowState;
  {$ENDIF}
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

      FMXChromium1.DefaultUrl := FHomepage;
      FMXChromium1.CreateBrowser(TempHandle, TempRect);
    end;
end;

procedure TChildForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  PositionChanged: Boolean;
begin
  PositionChanged := (ALeft <> Left) or (ATop <> Top);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if PositionChanged then
    NotifyMoveOrResizeStarted;
end;

procedure TChildForm.NotifyMoveOrResizeStarted;
begin
  // This is needed to display some HTML elements correctly
  if (FMXChromium1 <> nil) then FMXChromium1.NotifyMoveOrResizeStarted;
end;

procedure TChildForm.SendCloseMsg;
begin
  PostCustomMessage(WM_CLOSE);
end;

procedure TChildForm.SendShowBrowserMsg;
begin
  PostCustomMessage(CEF_SHOWBROWSER);
end;

end.
