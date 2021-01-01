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

unit uTinyBrowser;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFApplication, uCEFChromium,
  uCEFWindowComponent, uCEFBrowserViewComponent;

procedure CreateGlobalCEFApp;
procedure DestroyTinyBrowser;

implementation

const
  DEFAULT_WINDOW_VIEW_WIDTH  = 1024;
  DEFAULT_WINDOW_VIEW_HEIGHT = 768;

type
  TTinyBrowser = class(TComponent)
    protected
      FChromium                : TChromium;
      FCEFWindowComponent      : TCEFWindowComponent;
      FCEFBrowserViewComponent : TCEFBrowserViewComponent;
      FHomepage                : string;

      procedure Chromium_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
      procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition; userGesture: Boolean; const popupFeatures: TCefPopupFeatures; var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings; var extra_info: ICefDictionaryValue; var noJavascriptAccess, Result: Boolean);
      procedure Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);

      procedure CEFWindowComponent_OnWindowCreated(const Sender : TObject; const window : ICefWindow);
      procedure CEFWindowComponent_OnCanClose(const Sender : TObject; const window : ICefWindow; var aResult : Boolean);
      procedure CEFWindowComponent_OnGetPreferredSize(const Sender : TObject; const view : ICefView; var aResult : TCefSize);

    public
      constructor Create(AOwner : TComponent); override;
      procedure   AfterConstruction; override;
      procedure   CreateTopLevelWindow;

      property Homepage    : string     read FHomepage     write FHomepage;
  end;

var
  TinyBrowser : TTinyBrowser = nil;

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

// This demo only creates a browser window without VCL or FMX.
// It doesn't use the default GlobalCEFApp.MultiThreadedMessageLoop and
// GlobalCEFApp.ExternalMessagePump is also disabled. For this reason we have to
// call GlobalCEFApp.RunMessageLoop to start the message loop and also call
// GlobalCEFApp.QuitMessageLoop when the browser is closed to stop the message
// loop.

// The TinyBrowser instance is created in GlobalCEFApp.OnContextInitialized and
// it's destroyed before destroying GlobalCEFApp (see the DPR file).

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

constructor TTinyBrowser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FHomepage                := 'about:blank';
  FChromium                := nil;
  FCEFWindowComponent      := nil;
  FCEFBrowserViewComponent := nil;
end;

procedure TTinyBrowser.AfterConstruction;
begin
  inherited AfterConstruction;

  FChromium               := TChromium.Create(self);
  FChromium.OnBeforeClose := Chromium_OnBeforeClose;
  FChromium.OnBeforePopup := Chromium_OnBeforePopup;
  FChromium.OnTitleChange := Chromium_OnTitleChange;

  FCEFBrowserViewComponent := TCEFBrowserViewComponent.Create(self);

  FCEFWindowComponent                    := TCEFWindowComponent.Create(self);
  FCEFWindowComponent.OnWindowCreated    := CEFWindowComponent_OnWindowCreated;
  FCEFWindowComponent.OnCanClose         := CEFWindowComponent_OnCanClose;
  FCEFWindowComponent.OnGetPreferredSize := CEFWindowComponent_OnGetPreferredSize;
end;

procedure TTinyBrowser.CreateTopLevelWindow;
begin
  if (FCEFWindowComponent <> nil) then
    FCEFWindowComponent.CreateTopLevelWindow;
end;

procedure TTinyBrowser.Chromium_OnBeforeClose(Sender: TObject; const browser: ICefBrowser);
begin
  GlobalCEFApp.QuitMessageLoop;
end;

procedure TTinyBrowser.Chromium_OnBeforePopup(      Sender             : TObject;
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
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;

procedure TTinyBrowser.Chromium_OnTitleChange(      Sender  : TObject;
                                              const browser : ICefBrowser;
                                              const title   : ustring);
begin
  FCEFWindowComponent.Title := title;
end;

procedure TTinyBrowser.CEFWindowComponent_OnWindowCreated(const Sender : TObject;
                                                          const window : ICefWindow);
var
  TempWorkArea : TCefRect;
  TempPosition : TCefPoint;
  TempDisplay  : ICefDisplay;
begin
  if FChromium.CreateBrowser(FHomepage, FCEFBrowserViewComponent) then
    begin
      FCEFWindowComponent.AddChildView(FCEFBrowserViewComponent.BrowserView);
      FCEFWindowComponent.Show;

      // This centers the window on the screen
      TempDisplay := FCEFWindowComponent.Display;
      if (TempDisplay <> nil) then
        begin
          TempWorkArea   := TempDisplay.WorkArea;
          TempPosition.x := ((TempWorkArea.width  - DEFAULT_WINDOW_VIEW_WIDTH)  div 2) + TempWorkArea.x;
          TempPosition.y := ((TempWorkArea.height - DEFAULT_WINDOW_VIEW_HEIGHT) div 2) + TempWorkArea.y;

          FCEFWindowComponent.Position := TempPosition;
        end;

      FCEFBrowserViewComponent.RequestFocus;
    end;
end;

procedure TTinyBrowser.CEFWindowComponent_OnCanClose(const Sender  : TObject;
                                                     const window  : ICefWindow;
                                                     var   aResult : Boolean);
begin
  aResult := FChromium.TryCloseBrowser;
end;

procedure TTinyBrowser.CEFWindowComponent_OnGetPreferredSize(const Sender  : TObject;
                                                             const view    : ICefView;
                                                             var   aResult : TCefSize);
begin
  // This is the initial window size
  aResult.width  := DEFAULT_WINDOW_VIEW_WIDTH;
  aResult.height := DEFAULT_WINDOW_VIEW_HEIGHT;
end;

procedure GlobalCEFApp_OnContextInitialized;
begin
  TinyBrowser          := TTinyBrowser.Create(nil);
  TinyBrowser.Homepage := 'https://www.briskbard.com';
  TinyBrowser.CreateTopLevelWindow;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                          := TCefApplication.Create;
  GlobalCEFApp.MultiThreadedMessageLoop := False;
  GlobalCEFApp.ExternalMessagePump      := False;
  GlobalCEFApp.OnContextInitialized     := GlobalCEFApp_OnContextInitialized;
end;

procedure DestroyTinyBrowser;
begin
  if (TinyBrowser <> nil) then
    TinyBrowser.Free;
end;

end.
