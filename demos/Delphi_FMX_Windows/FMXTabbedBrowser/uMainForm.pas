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

unit uMainForm;

{$I cef.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, System.Actions,
  FMX.ActnList, System.Math;


const
  CEF_INITIALIZED      = WM_APP + $100;
  CEF_DESTROYTAB       = WM_APP + $101;
  CEF_SHOWBROWSER      = WM_APP + $102;
  CEF_DESTROYWINPARENT = WM_APP + $103;

  HOMEPAGE_URL        = 'https://www.google.com';
  DEFAULT_TAB_CAPTION = 'New tab';

type
  TMainForm = class(TForm)
    ButtonLay: TLayout;
    AddTabBtn: TSpeedButton;
    RemoveTabBtn: TSpeedButton;
    BrowserTabCtrl: TTabControl;
    ActionList1: TActionList;
    AddTabAction: TAction;
    RemoveTabAction: TAction;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure AddTabActionExecute(Sender: TObject);
    procedure RemoveTabActionExecute(Sender: TObject);
    procedure BrowserTabCtrlChange(Sender: TObject);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose   : boolean;
    FClosing    : boolean;

    FLastTabID  : cardinal; // Used by NextTabID to generate unique tab IDs

    {$IFDEF MSWINDOWS}
    // This is a workaround for the issue #253
    // https://github.com/salvadordf/CEF4Delphi/issues/253
    FCustomWindowState      : TWindowState;
    FOldWndPrc              : TFNWndProc;
    FFormStub               : Pointer;

    function  GetCurrentWindowState : TWindowState;
    procedure UpdateCustomWindowState;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure CustomWndProc(var aMessage: TMessage);
    {$ENDIF}
    function  GetNextTabID : cardinal;
    procedure EnableButtonLay;
    procedure ShowSelectedBrowser;
    procedure DestroyWindowParent(aTabID : cardinal);
    procedure DestroyTab(aTabID : cardinal);
    function  CloseAllTabs : boolean;
    procedure CloseSelectedTab;

    property  NextTabID : cardinal   read GetNextTabID;
  public
    function  PostCustomMessage(aMsg : cardinal; aWParam : WPARAM = 0; aLParam : LPARAM = 0) : boolean;
    procedure NotifyMoveOrResizeStarted;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.fmx}

uses
  FMX.Platform, FMX.Platform.Win,
  uCEFMiscFunctions, uCEFApplication, uCEFConstants, uBrowserTab;

// This demo shows how to use a TTabControl with TFrames that include
// CEF4Delphi browsers.

// Instead of a regular TTabItem we use a custom TBrowserTab class that
// inherits from TTabItem and instead of a regular TFrame we use a custom
// TBrowserFrame class that inherits from TFrame.

// To create a new tab you need to call TBrowserTab.CreateBrowser in the last
// step to create all the browser components and initialize the browser.

// To close a tab you have to call TBrowserTab.CloseBrowser and wait for a
// CEF_DESTROYTAB message that includes TBrowserTab.TabID in TMessage.wParam.
// Then you find the tab with that unique TabID and you free it.

// TBrowserFrame has all the usual code to close CEF4Delphi browsers following
// a similar destruction sequence than the SimpleFMXBrowser demo :
//
// 1. TBrowserTab.CloseBrowser calls TChromium.CloseBrowser which triggers the
//    TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEF_DESTROYWINPARENT message to destroy
//    CEFWindowParent1 in the main thread, which triggers the
//    TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose executes the TBrowserFrame.OnBrowserDestroyed
//    event which will be used in TBrowserTab to send a CEF_DESTROYTAB message
//    to the main form to free the tab.

// To close safely this demo you must close all the browser tabs first following
// this steps :
//
// 1. FormCloseQuery sets CanClose to FALSE and calls CloseAllTabs and FClosing
//    is set to TRUE.
// 2. Each tab will send a CEF_DESTROYTAB message to free that tab.
// 3. When TTabControl has no tabs then we can set FCanClose to TRUE and close
//    the main form.

procedure GlobalCEFApp_OnContextInitialized;
begin
  if (MainForm <> nil) then MainForm.PostCustomMessage(CEF_INITIALIZED);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.cache                := 'cache';
  GlobalCEFApp.OnContextInitialized := GlobalCEFApp_OnContextInitialized;
end;

procedure TMainForm.NotifyMoveOrResizeStarted;
var
  i : integer;
begin
  if (BrowserTabCtrl = nil) then exit;

  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserTabCtrl.Tabs[i]).NotifyMoveOrResizeStarted;
      dec(i);
    end;
end;

procedure TMainForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  PositionChanged: Boolean;
begin
  PositionChanged := (ALeft <> Left) or (ATop <> Top);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if PositionChanged then
    NotifyMoveOrResizeStarted;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing          := True;
      ButtonLay.Enabled := False;

      if not(CloseAllTabs) then CanClose := True;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanClose   := False;
  FClosing    := False;
  FLastTabID  := 0;
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  i : integer;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserTabCtrl.Tabs[i]).ResizeBrowser;
      dec(i);
    end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then
    EnableButtonLay;
end;

procedure TMainForm.EnableButtonLay;
begin
  if not(ButtonLay.Enabled) then
    begin
      ButtonLay.Enabled := True;
      Caption           := 'FMX Tabbed Browser';
      cursor            := crDefault;
      if (BrowserTabCtrl.TabCount <= 0) then AddTabAction.Execute;
    end;
end;

function TMainForm.GetNextTabID : cardinal;
begin
  inc(FLastTabID);
  Result := FLastTabID;
end;

procedure TMainForm.ShowSelectedBrowser;
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).ShowBrowser;
end;

procedure TMainForm.AddTabActionExecute(Sender: TObject);
var
  TempNewTab : TBrowserTab;
begin
  TempNewTab        := TBrowserTab.Create(BrowserTabCtrl, NextTabID, DEFAULT_TAB_CAPTION);
  TempNewTab.Parent := BrowserTabCtrl;

  BrowserTabCtrl.TabIndex := pred(BrowserTabCtrl.TabCount);

  TempNewTab.CreateBrowser(HOMEPAGE_URL);
end;

procedure TMainForm.RemoveTabActionExecute(Sender: TObject);
begin
  CloseSelectedTab;
end;

{$IFDEF MSWINDOWS}
procedure TMainForm.CreateHandle;
begin
  inherited CreateHandle;

  FFormStub  := MakeObjectInstance(CustomWndProc);
  FOldWndPrc := TFNWndProc(SetWindowLongPtr(FmxHandleToHWND(Handle), GWLP_WNDPROC, NativeInt(FFormStub)));
end;

procedure TMainForm.DestroyHandle;
begin
  SetWindowLongPtr(FmxHandleToHWND(Handle), GWLP_WNDPROC, NativeInt(FOldWndPrc));
  FreeObjectInstance(FFormStub);

  inherited DestroyHandle;
end;

procedure TMainForm.CustomWndProc(var aMessage: TMessage);
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

      CEF_INITIALIZED       : EnableButtonLay;
      CEF_DESTROYWINPARENT  : DestroyWindowParent(aMessage.wParam);
      CEF_DESTROYTAB        : DestroyTab(aMessage.wParam);
      CEF_SHOWBROWSER       : ShowSelectedBrowser;
    end;

    aMessage.Result := CallWindowProc(FOldWndPrc, FmxHandleToHWND(Handle), aMessage.Msg, aMessage.wParam, aMessage.lParam);
  except
    on e : exception do
      if CustomExceptionHandler('TMainForm.CustomWndProc', e) then raise;
  end;
end;

procedure TMainForm.UpdateCustomWindowState;
var
  TempNewState : TWindowState;
begin
  TempNewState := GetCurrentWindowState;

  if (FCustomWindowState <> TempNewState) then
    begin
      // This is a workaround for the issue #253
      // https://github.com/salvadordf/CEF4Delphi/issues/253
      if (FCustomWindowState = TWindowState.wsMinimized) then
        PostCustomMessage(CEF_SHOWBROWSER);

      FCustomWindowState := TempNewState;
    end;
end;

function TMainForm.GetCurrentWindowState : TWindowState;
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

  if IsIconic(TempHWND) then Result := TWindowState.wsMinimized;
end;
{$ENDIF}

procedure TMainForm.DestroyWindowParent(aTabID : cardinal);
var
  i : integer;
  TempTab : TBrowserTab;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TempTab := TBrowserTab(BrowserTabCtrl.Tabs[i]);

      if (TempTab.TabID = aTabID) then
        begin
          TempTab.DestroyWindowParent;
          break;
        end
       else
        dec(i);
    end;
end;

procedure TMainForm.DestroyTab(aTabID : cardinal);
var
  i : integer;
  TempTab : TBrowserTab;
  TempText : string;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TempTab := TBrowserTab(BrowserTabCtrl.Tabs[i]);

      if (TempTab.TabID = aTabID) then
        begin
          BrowserTabCtrl.Delete(i);
          break;
        end
       else
        dec(i);
    end;

  if FClosing then
    begin
      if (BrowserTabCtrl.TabCount <= 0) then
        begin
          FCanClose := True;
          Close;
        end;
    end
   else
    begin
      ShowSelectedBrowser;

      // Sometimes TTabControl doesn't draw the new selected tab correctly.
      // Changing TTabItem.Text forces the component to redraw all the tabs.
      // A nicer solution would be to use a custom ttabcontrol that publishes
      // the TTabControl.RealignTabs procedure.
      if (BrowserTabCtrl.ActiveTab <> nil) then
        begin
          TempText := BrowserTabCtrl.ActiveTab.Text;
          BrowserTabCtrl.ActiveTab.Text := TempText + '-';
          BrowserTabCtrl.ActiveTab.Text := TempText;
        end;
    end;
end;

function TMainForm.PostCustomMessage(aMsg : cardinal; aWParam : WPARAM; aLParam : LPARAM) : boolean;
{$IFDEF MSWINDOWS}
var
  TempHWND : HWND;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TempHWND := FmxHandleToHWND(Handle);
  Result   := (TempHWND <> 0) and WinApi.Windows.PostMessage(TempHWND, aMsg, aWParam, aLParam);
  {$ELSE}
  Result   := False;
  {$ENDIF}
end;

procedure TMainForm.BrowserTabCtrlChange(Sender: TObject);
var
  i : integer;
begin
  i := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      if (BrowserTabCtrl.TabIndex = i) then
        TBrowserTab(BrowserTabCtrl.Tabs[i]).ShowBrowser
       else
        TBrowserTab(BrowserTabCtrl.Tabs[i]).HideBrowser;

      dec(i);
    end;
end;

function TMainForm.CloseAllTabs : boolean;
var
  i : integer;
begin
  Result := False;
  i      := pred(BrowserTabCtrl.TabCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserTabCtrl.Tabs[i]).CloseBrowser;
      Result := True;
      dec(i);
    end;
end;

procedure TMainForm.CloseSelectedTab;
begin
  if (BrowserTabCtrl.ActiveTab <> nil) then
    TBrowserTab(BrowserTabCtrl.ActiveTab).CloseBrowser;
end;

end.
