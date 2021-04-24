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
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, Buttons, ExtCtrls, StdCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFSentinel;

const
  CEFBROWSER_DESTROYWNDPARENT = WM_APP + $100;
  CEFBROWSER_DESTROYTAB       = WM_APP + $101;
  CEFBROWSER_INITIALIZED      = WM_APP + $102;
  CEFBROWSER_CHECKTAGGEDTABS  = WM_APP + $103;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    ButtonPnl: TPanel;
    NavButtonPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    StopBtn: TButton;
    ConfigPnl: TPanel;
    GoBtn: TButton;
    URLEditPnl: TPanel;
    URLCbx: TComboBox;
    AddTabBtn: TButton;
    RemoveTabBtn: TButton;
    procedure AddTabBtnClick(Sender: TObject);
    procedure RemoveTabBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BackBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  protected
    FClosingTab : boolean;
    FCanClose   : boolean;
    FClosing    : boolean;

    procedure Chromium_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium_OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
    procedure Chromium_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
    procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess: Boolean; var Result: Boolean);

    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure BrowserDestroyWindowParentMsg(var aMessage : TMessage); message CEFBROWSER_DESTROYWNDPARENT;
    procedure BrowserDestroyTabMsg(var aMessage : TMessage); message CEFBROWSER_DESTROYTAB;
    procedure BrowserCheckTaggedTabsMsg(var aMessage : TMessage); message CEFBROWSER_CHECKTAGGEDTABS;
    procedure CEFInitializedMsg(var aMessage : TMessage); message CEFBROWSER_INITIALIZED;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

    function  AllTabSheetsAreTagged : boolean;
    procedure CloseAllBrowsers;
    function  GetPageIndex(const aSender : TObject; var aPageIndex : integer) : boolean;
    procedure NotifyMoveOrResizeStarted;
    function  SearchChromium(aPageIndex : integer; var aChromium : TChromium) : boolean;
    function  SearchWindowParent(aPageIndex : integer; var aWindowParent : TCEFWindowParent) : boolean;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

// This is just a simplified demo with tab handling.
// It's not meant to be a complete browser or the best way to implement a tabbed browser.

// In this demo all browsers share the buttons and URL combobox.
// All TChromium components share the same functions for their events sending the
// PageIndex of the Tab where they are included in the Message.lParam parameter if necessary.

// For simplicity the Button panel and the PageControl are disabled while adding or removing tab sheets.
// The Form can't be closed if it's destroying a tab.

// This is the destruction sequence when a user closes a tab sheet:
// 1. RemoveTabBtnClick calls TChromium.CloseBrowser of the selected tab which triggers a TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROYWNDPARENT message to destroy TCEFWindowParent in the main thread which triggers a TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sends a CEFBROWSER_DESTROYTAB message to destroy the tab in the main thread.

// This is the destruction sequence when the user closes the main form
// 1. FormCloseQuery hides the form and calls CloseAllBrowsers which calls TChromium.CloseBrowser in all tabs and triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROYWNDPARENT message to destroy TCEFWindowParent in the main thread which triggers a TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sends a CEFBROWSER_CHECKTAGGEDTABS message to set the TAG property to 1 in the TabSheet containing the TChromium.
//    When all tabsheets have a TAG = 1 it calls TCEFSentinel.Start, which will trigger TCEFSentinel.OnClose when the renderer processes are closed.
// 4. TCEFSentinel.OnClose sends WM_CLOSE to the form.

procedure GlobalCEFApp_OnContextInitialized;
begin
  if (MainForm <> nil) and MainForm.HandleAllocated then
    PostMessage(MainForm.Handle, CEFBROWSER_INITIALIZED, 0, 0);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.OnContextInitialized := GlobalCEFApp_OnContextInitialized;
end;

procedure TMainForm.AddTabBtnClick(Sender: TObject);
var
  TempSheet        : TTabSheet;
  TempWindowParent : TCEFWindowParent;
  TempChromium     : TChromium;
begin
  ButtonPnl.Enabled    := False;
  PageControl1.Enabled := False;

  TempSheet             := TTabSheet.Create(PageControl1);
  TempSheet.Caption     := 'New Tab';
  TempSheet.PageControl := PageControl1;

  TempWindowParent        := TCEFWindowParent.Create(TempSheet);
  TempWindowParent.Parent := TempSheet;
  TempWindowParent.Color  := clWhite;
  TempWindowParent.Align  := alClient;

  TempChromium                 := TChromium.Create(TempSheet);
  TempChromium.OnAfterCreated  := Chromium_OnAfterCreated;
  TempChromium.OnAddressChange := Chromium_OnAddressChange;
  TempChromium.OnTitleChange   := Chromium_OnTitleChange;
  TempChromium.OnClose         := Chromium_OnClose;
  TempChromium.OnBeforeClose   := Chromium_OnBeforeClose;
  TempChromium.OnBeforePopup   := Chromium_OnBeforePopup;

  TempChromium.CreateBrowser(TempWindowParent, '');
end;

procedure TMainForm.RemoveTabBtnClick(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if SearchChromium(PageControl1.TabIndex, TempChromium) then
    begin
      FClosingTab          := True;
      ButtonPnl.Enabled    := False;
      PageControl1.Enabled := False;
      TempChromium.CloseBrowser(True);
    end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FClosingTab then
    CanClose := False
   else
    if (PageControl1.PageCount = 0) then
      CanClose := True
     else
      begin
        CanClose := FCanClose;

        if not(FClosing) then
          begin
            FClosing := True;
            Visible  := False;

            CloseAllBrowsers;
          end;
      end;
end;

procedure TMainForm.CloseAllBrowsers;
var
  i, j, k : integer;
  TempComponent : TComponent;
  TempSheet : TTabSheet;
  TempCtnue : boolean;
begin
  k := pred(PageControl1.PageCount);

  while (k >= 0) do
    begin
      TempSheet := PageControl1.Pages[k];
      TempCtnue := True;
      i         := 0;
      j         := TempSheet.ComponentCount;

      while (i < j) and TempCtnue do
        begin
          TempComponent := TempSheet.Components[i];

          if (TempComponent <> nil) and (TempComponent is TChromium) then
            begin
              TChromium(TempComponent).CloseBrowser(True);
              TempCtnue := False;
            end
           else
            inc(i);
        end;

      dec(k);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FClosingTab := False;
  FCanClose   := False;
  FClosing    := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and
     GlobalCEFApp.GlobalContextInitialized and
     not(ButtonPnl.Enabled) then
    begin
      ButtonPnl.Enabled := True;
      Caption           := 'Tab Browser';
      cursor            := crDefault;
      if (PageControl1.PageCount = 0) then AddTabBtn.Click;
    end;
end;

procedure TMainForm.ForwardBtnClick(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if SearchChromium(PageControl1.TabIndex, TempChromium) then TempChromium.GoForward;
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if SearchChromium(PageControl1.TabIndex, TempChromium) then TempChromium.LoadURL(URLCbx.Text);
end;

procedure TMainForm.ReloadBtnClick(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if SearchChromium(PageControl1.TabIndex, TempChromium) then TempChromium.Reload;
end;

procedure TMainForm.BackBtnClick(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if SearchChromium(PageControl1.TabIndex, TempChromium) then TempChromium.GoBack;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if SearchChromium(PageControl1.TabIndex, TempChromium) then TempChromium.StopLoad;
end;

procedure TMainForm.BrowserCreatedMsg(var aMessage : TMessage);
var
  TempWindowParent : TCEFWindowParent;
  TempChromium     : TChromium;
begin
  ButtonPnl.Enabled    := True;
  PageControl1.Enabled := True;

  if SearchWindowParent(aMessage.lParam, TempWindowParent) then
    TempWindowParent.UpdateSize;

  if SearchChromium(aMessage.lParam, TempChromium) then
    TempChromium.LoadURL(URLCbx.Items[0]);
end;

procedure TMainForm.BrowserDestroyWindowParentMsg(var aMessage : TMessage);
var
  TempWindowParent : TCEFWindowParent;
begin
  if SearchWindowParent(aMessage.lParam, TempWindowParent) then TempWindowParent.Free;
end;

procedure TMainForm.BrowserDestroyTabMsg(var aMessage : TMessage);
begin
  if (aMessage.lParam >= 0) and
     (aMessage.lParam < PageControl1.PageCount) then
    PageControl1.Pages[aMessage.lParam].Free;

  FClosingTab          := False;
  ButtonPnl.Enabled    := True;
  PageControl1.Enabled := True;
end;

procedure TMainForm.BrowserCheckTaggedTabsMsg(var aMessage : TMessage);
begin
  if (aMessage.lParam >= 0) and
     (aMessage.lParam < PageControl1.PageCount) then
    begin
      PageControl1.Pages[aMessage.lParam].Tag := 1;

      if AllTabSheetsAreTagged then
        begin
          FCanClose := True;
          PostMessage(Handle, WM_CLOSE, 0, 0);
        end;
    end;
end;

function TMainForm.AllTabSheetsAreTagged : boolean;
var
  i : integer;
begin
  Result := True;
  i      := pred(PageControl1.PageCount);

  while (i >= 0) and Result do
    if (PageControl1.Pages[i].Tag <> 1) then
      Result := False
     else
      dec(i);
end;

procedure TMainForm.Chromium_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
var
  TempPageIndex : integer;
begin
  if GetPageIndex(Sender, TempPageIndex) then
    PostMessage(Handle, CEF_AFTERCREATED, 0, TempPageIndex);
end;

procedure TMainForm.Chromium_OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
  TempPageIndex : integer;
begin
  if not(FClosing) and
     (PageControl1.TabIndex >= 0) and
     GetPageIndex(Sender, TempPageIndex) and
     (PageControl1.TabIndex = TempPageIndex) then
    URLCbx.Text := url;
end;

function TMainForm.GetPageIndex(const aSender : TObject; var aPageIndex : integer) : boolean;
begin
  Result     := False;
  aPageIndex := -1;

  if (aSender <> nil) and
     (aSender is TComponent) and
     (TComponent(aSender).Owner <> nil) and
     (TComponent(aSender).Owner is TTabSheet) then
    begin
      aPageIndex := TTabSheet(TComponent(aSender).Owner).PageIndex;
      Result     := True;
    end;
end;

procedure TMainForm.Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
var
  TempPageIndex : integer;
begin
  if not(FClosing) and GetPageIndex(Sender, TempPageIndex) then
    PageControl1.Pages[TempPageIndex].Caption := title;
end;

procedure TMainForm.Chromium_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction);
var
  TempPageIndex : integer;
begin
  if GetPageIndex(Sender, TempPageIndex) then
    PostMessage(Handle, CEFBROWSER_DESTROYWNDPARENT, 0, TempPageIndex);
end;

procedure TMainForm.Chromium_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
var
  TempPageIndex : integer;
begin
  if GetPageIndex(Sender, TempPageIndex) then
    begin
      if FClosing then
        PostMessage(Handle, CEFBROWSER_CHECKTAGGEDTABS, 0, TempPageIndex)
       else
        PostMessage(Handle, CEFBROWSER_DESTROYTAB, 0, TempPageIndex);
    end;
end;

procedure TMainForm.Chromium_OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

function TMainForm.SearchChromium(aPageIndex : integer; var aChromium : TChromium) : boolean;
var
  i, j : integer;
  TempComponent : TComponent;
  TempSheet : TTabSheet;
begin
  Result    := False;
  aChromium := nil;

  if (aPageIndex >= 0) and (aPageIndex < PageControl1.PageCount) then
    begin
      TempSheet := PageControl1.Pages[aPageIndex];
      i         := 0;
      j         := TempSheet.ComponentCount;

      while (i < j) and not(Result) do
        begin
          TempComponent := TempSheet.Components[i];

          if (TempComponent <> nil) and (TempComponent is TChromium) then
            begin
              aChromium := TChromium(TempComponent);
              Result    := True;
            end
           else
            inc(i);
        end;
    end;
end;

function TMainForm.SearchWindowParent(aPageIndex : integer; var aWindowParent : TCEFWindowParent) : boolean;
var
  i, j : integer;
  TempControl : TControl;
  TempSheet : TTabSheet;
begin
  Result        := False;
  aWindowParent := nil;

  if (aPageIndex >= 0) and (aPageIndex < PageControl1.PageCount) then
    begin
      TempSheet := PageControl1.Pages[aPageIndex];
      i         := 0;
      j         := TempSheet.ControlCount;

      while (i < j) and not(Result) do
        begin
          TempControl := TempSheet.Controls[i];

          if (TempControl <> nil) and (TempControl is TCEFWindowParent) then
            begin
              aWindowParent := TCEFWindowParent(TempControl);
              Result        := True;
            end
           else
            inc(i);
        end;
    end;
end;

procedure TMainForm.NotifyMoveOrResizeStarted;
var
  i, j : integer;
  TempChromium : TChromium;
begin
  if not(showing) or (PageControl1 = nil) or FClosing then exit;

  i := 0;
  j := PageControl1.PageCount;

  while (i < j) do
    begin
      if SearchChromium(i, TempChromium) then TempChromium.NotifyMoveOrResizeStarted;

      inc(i);
    end;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if not(FClosing) and (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if not(FClosing) and (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if showing and SearchChromium(PageControl1.TabIndex, TempChromium) then
    URLCbx.Text := TempChromium.DocumentURL;
end;

procedure TMainForm.CEFInitializedMsg(var aMessage : TMessage);
begin
  if not(ButtonPnl.Enabled) then
    begin
      ButtonPnl.Enabled := True;
      Caption           := 'Tab Browser';
      cursor            := crDefault;
      if (PageControl1.PageCount = 0) then AddTabBtn.Click;
    end;
end;

end.
