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

{$mode objfpc}{$H+}

{$I cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ToolWin, Buttons, ExtCtrls,
  uCEFApplication, uCEFTypes, uCEFConstants;

const
  CEF_INITIALIZED     = $100;
  CEF_DESTROYTAB      = $101;

  HOMEPAGE_URL        = 'https://www.google.com';
  DEFAULT_TAB_CAPTION = 'New tab';

type

  { TMainForm }

  TMainForm = class(TForm)
    BrowserPageCtrl: TPageControl;
    ButtonPnl: TPanel;
    AddTabBtn: TSpeedButton;
    RemoveTabBtn: TSpeedButton;

    procedure AddTabBtnClick(Sender: TObject);
    procedure RemoveTabBtnClick(Sender: TObject);

    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  protected
    // Variables to control when can we destroy the form safely
    FCanClose   : boolean;
    FClosing    : boolean;
    FLastTabID  : cardinal; // Used by NextTabID to generate unique tab IDs

    function  GetNextTabID : cardinal;

    procedure EnableButtonPnl;
    function  CloseAllTabs : boolean;
    procedure CloseTab(aIndex : integer);
    procedure NotifyMoveOrResizeStarted;
    procedure BrowserInitializedMsg(Data: PtrInt);
    procedure BrowserDestroyedTabMsg(Data: PtrInt);

    procedure WMMove(var aMessage: TLMMove); message LM_MOVE;
    procedure WMSize(var aMessage: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var aMessage: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;

    property  NextTabID : cardinal   read GetNextTabID;

  public
    procedure SendCompMessage(aMsg : cardinal; aData : PtrInt = 0);
  end;

var
  MainForm: TMainForm = nil;

procedure CreateGlobalCEFApp;

implementation

{$R *.lfm}

uses
  uBrowserTab;       

// This demo shows how to use a TPageControl with TFrames that include
// CEF4Delphi browsers.

// Instead of a regular TTabSheet we use a custom TBrowserTab class that
// inherits from TTabSheet and instead of a regular TFrame we use a custom
// TBrowserFrame class that inherits from TFrame.

// To create a new tab you need to call TBrowserTab.CreateBrowser in the last
// step to create all the browser components and initialize the browser.

// To close a tab you have to call TBrowserTab.CloseBrowser and wait for a
// CEF_DESTROYTAB message with the TBrowserTab.TabID value.
// Then you find the tab with that unique TabID and free it.

// Most of the TChromium events store the parameter values in a set of class
// fields which are protected by a critial section and then they call
// TBrowserFrame.SendCompMessage to execute the GUI code in the main thread.

// TBrowserFrame has all the usual code to close CEF4Delphi browsers following
// a similar destruction sequence than the MiniBrowser demo :
//
// 1. TBrowserTab.CloseBrowser calls TChromium.CloseBrowser which triggers the
//    TChromium.OnClose event.
// 2. TChromium.OnClose sets aAction to cbaClose which trigges the
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
// 3. When TPageControl has no tabs then we can set FCanClose to TRUE and
//    close the main form.

procedure GlobalCEFApp_OnContextInitialized;
begin
  if (MainForm <> nil) and MainForm.Showing and MainForm.Focused then
    MainForm.SendCompMessage(CEF_INITIALIZED);
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.OnContextInitialized := @GlobalCEFApp_OnContextInitialized;
end;

procedure TMainForm.EnableButtonPnl;
begin
  if not(ButtonPnl.Enabled) then
    begin
      ButtonPnl.Enabled := True;
      Caption           := 'Tabbed Browser 2';
      cursor            := crDefault;
      if (BrowserPageCtrl.PageCount = 0) then AddTabBtn.Click;
    end;
end;

function TMainForm.GetNextTabID : cardinal;
begin
  inc(FLastTabID);
  Result := FLastTabID;
end;

procedure TMainForm.AddTabBtnClick(Sender: TObject);
var
  TempNewTab : TBrowserTab;
begin
  TempNewTab             := TBrowserTab.Create(self, NextTabID, DEFAULT_TAB_CAPTION);
  TempNewTab.PageControl := BrowserPageCtrl;

  BrowserPageCtrl.ActivePageIndex := pred(BrowserPageCtrl.PageCount);

  TempNewTab.CreateBrowser(HOMEPAGE_URL);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then
    EnableButtonPnl;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FCanClose;

  if not(FClosing) then
    begin
      FClosing          := True;
      ButtonPnl.Enabled := False;

      if not(CloseAllTabs) then
        begin
          FCanClose := True;
          CanClose  := True;
        end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanClose   := False;
  FClosing    := False;
  FLastTabID  := 0;
end;

procedure TMainForm.RemoveTabBtnClick(Sender: TObject);
begin
  CloseTab(BrowserPageCtrl.ActivePageIndex);
end;

function TMainForm.CloseAllTabs : boolean;
var
  i : integer;
begin
  Result := False;
  i      := pred(BrowserPageCtrl.PageCount);

  while (i >= 0) do
    begin
      TBrowserTab(BrowserPageCtrl.Pages[i]).CloseBrowser;
      Result := True;
      dec(i);
    end;
end;

procedure TMainForm.CloseTab(aIndex : integer);
begin
  if (aIndex >= 0) and (aIndex < BrowserPageCtrl.PageCount) then
    TBrowserTab(BrowserPageCtrl.Pages[aIndex]).CloseBrowser;
end;

procedure TMainForm.NotifyMoveOrResizeStarted;
var
  i : integer;
begin
  i := 0;
  while (i < BrowserPageCtrl.PageCount) do
    begin
      TBrowserTab(BrowserPageCtrl.Pages[i]).NotifyMoveOrResizeStarted;
      inc(i);
    end;
end;

procedure TMainForm.WMMove(var aMessage : TLMMove);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMSize(var aMessage: TLMSize);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMWindowPosChanged(var aMessage: TLMWindowPosChanged);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

procedure TMainForm.BrowserInitializedMsg(Data: PtrInt);
begin
  EnableButtonPnl;
end;          

procedure TMainForm.BrowserDestroyedTabMsg(Data: PtrInt);
var
  i : integer;
  TempTab : TBrowserTab;
begin
  i := 0;
  while (i < BrowserPageCtrl.PageCount) do
    begin
      TempTab := TBrowserTab(BrowserPageCtrl.Pages[i]);

      if (TempTab.TabID = cardinal(Data)) then
        begin
          TempTab.Free;
          break;
        end
       else
        inc(i);
    end;

  if FClosing and (BrowserPageCtrl.PageCount = 0) then
    begin
      FCanClose := True;
      close;
    end;
end;

procedure TMainForm.SendCompMessage(aMsg : cardinal; aData : PtrInt);
begin
  case aMsg of
    CEF_INITIALIZED : Application.QueueAsyncCall(@BrowserInitializedMsg, aData);
    CEF_DESTROYTAB  : Application.QueueAsyncCall(@BrowserDestroyedTabMsg, aData);
  end;
end;

end.
