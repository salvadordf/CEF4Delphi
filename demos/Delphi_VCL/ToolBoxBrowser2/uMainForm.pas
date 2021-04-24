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

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  uCEFInterfaces, uCEFTypes, uCEFConstants, uCEFViewComponent,
  uCEFPanelComponent, uCEFWindowComponent,
  uCEFBrowserViewComponent, uCEFChromiumCore, uCEFChromium;

const
  CEFBROWSER_INITIALIZED     = WM_APP + $100;

  DEFAULT_WINDOW_VIEW_WIDTH  = 800;
  DEFAULT_WINDOW_VIEW_HEIGHT = 600;

type
  TMainForm = class(TForm)
    ButtonPnl: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    CEFWindowComponent1: TCEFWindowComponent;
    CEFBrowserViewComponent1: TCEFBrowserViewComponent;
    Chromium1: TChromium;

    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);

    procedure CEFWindowComponent1GetPreferredSize(const Sender: TObject; const view: ICefView; var aResult: TCefSize);
    procedure CEFWindowComponent1WindowCreated(const Sender: TObject; const window: ICefWindow);
    procedure CEFWindowComponent1WindowDestroyed(const Sender: TObject; const window: ICefWindow);
    procedure CEFWindowComponent1CanClose(const Sender: TObject; const window: ICefWindow; var aResult: Boolean);

    procedure Chromium1TitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium1BeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);

  protected
    procedure CEFInitializedMsg(var aMessage : TMessage); message CEFBROWSER_INITIALIZED;

    procedure EnableInterface;
  end;

var
  MainForm: TMainForm;

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication;

// This demo uses the Views Framework in CEF to create a child window with a
// web browser.

// CEF4Delphi has several components to facilitate the creation of several views
// and this demo uses these :

// TCEFWindowComponent : Has a ICefWindow and a ICefWindowDelegate to call
// the methods of a "CefWindow" and the events from the "CefWindowDelegate".
// Read the linked documents bellow for more details about them.

// TCEFBrowserViewComponent : Has a ICefBrowserView and a ICefBrowserViewDelegate
// to call the methods of a "CefBrowserView" and the events from the
// "CefBrowserViewDelegate".

// TChromium : Is used to create the browser and to handle all its events.

// All the views have a hierarchy and they inherit methods or events from their
// parents. See the diagram in the uCEFTypes.pas file (line 2900 aprox.)

// This demo creates a window when the user clicks on the "Open" button.
// Most of the methods in the Views Framework must be used in the CEF UI thread
// but most of the procedures used to create the CEF4Delphi components create a
// task if they are called in a different thread. The rest of the methods *MUST*
// be called in the CEF UI thread or they won't work.

// TCEFWindowComponent.CreateTopLevelWindow triggers the
// TCEFWindowComponent.OnWindowCreated event which is executed in the CEF UI
// thread and you can use it to create the child views in the window, in this
// case a browser view.

// TCEFWindowComponent.OnGetPreferredSize will alse be triggered when you create
// the window to get the window size in DIPs.

// REFERENCES :
// ------------
// https://bitbucket.org/chromiumembedded/cef/issues/1749
// https://www.chromium.org/developers/design-documents/chromeviews
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefView.html
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefViewDelegate.html
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefWindow.html
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefWindowDelegate.html
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefBrowserView.html
// https://magpcss.org/ceforum/apidocs3/projects/(default)/CefBrowserViewDelegate.html

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

procedure TMainForm.Button1Click(Sender: TObject);
begin
  CEFWindowComponent1.CreateTopLevelWindow;
  ButtonPnl.Enabled := False;
end;

procedure TMainForm.CEFInitializedMsg(var aMessage : TMessage);
begin
  EnableInterface;
end;

procedure TMainForm.CEFWindowComponent1CanClose(const Sender: TObject;
  const window: ICefWindow; var aResult: Boolean);
begin
  aResult := Chromium1.TryCloseBrowser;
end;

procedure TMainForm.CEFWindowComponent1GetPreferredSize(const Sender: TObject;
  const view: ICefView; var aResult: TCefSize);
begin
  // This is the initial window size
  aResult.width  := DEFAULT_WINDOW_VIEW_WIDTH;
  aResult.height := DEFAULT_WINDOW_VIEW_HEIGHT;
end;

procedure TMainForm.CEFWindowComponent1WindowCreated(const Sender: TObject;
  const window: ICefWindow);
var
  TempURL      : ustring;
  TempWorkArea : TCefRect;
  TempPosition : TCefPoint;
  TempDisplay  : ICefDisplay;
begin
  TempURL := trim(Edit1.Text);
  if (length(TempURL) = 0) then TempURL := 'about:blank';

  // This event is executed in the CEF UI thread and we can call all these other
  // functions on this thread. In fact, all of these functions only work when
  // you call them on this thread.

  if Chromium1.CreateBrowser(TempURL, CEFBrowserViewComponent1) then
    begin
      CEFWindowComponent1.AddChildView(CEFBrowserViewComponent1.BrowserView);
      CEFWindowComponent1.Show;

      // This centers the window on the screen
      TempDisplay := CEFWindowComponent1.Display;
      if (TempDisplay <> nil) then
        begin
          TempWorkArea   := TempDisplay.WorkArea;
          TempPosition.x := ((TempWorkArea.width  - DEFAULT_WINDOW_VIEW_WIDTH)  div 2) + TempWorkArea.x;
          TempPosition.y := ((TempWorkArea.height - DEFAULT_WINDOW_VIEW_HEIGHT) div 2) + TempWorkArea.y;

          CEFWindowComponent1.Position := TempPosition;
        end;

      CEFBrowserViewComponent1.RequestFocus;
    end;
end;

procedure TMainForm.CEFWindowComponent1WindowDestroyed(const Sender: TObject;
  const window: ICefWindow);
begin
  ButtonPnl.Enabled := True;
end;

procedure TMainForm.Chromium1BeforePopup(Sender: TObject;
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

procedure TMainForm.Chromium1TitleChange(Sender: TObject;
  const browser: ICefBrowser; const title: ustring);
begin
  CEFWindowComponent1.Title := title;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then
    EnableInterface;
end;

procedure TMainForm.EnableInterface;
begin
  Caption           := 'ToolBox Browser 2';
  ButtonPnl.Enabled := True;
  cursor            := crDefault;
end;

end.

