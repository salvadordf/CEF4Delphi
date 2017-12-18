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
//        Copyright © 2017 Salvador Díaz Fau. All rights reserved.
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

unit uGeolocation;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  MINIBROWSER_NEWLOCATION = WM_APP + $100;

type
  TGeolocationFrm = class(TForm)
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    NavControlPnl: TPanel;
    Edit1: TEdit;
    GoBtn: TButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;

    procedure Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure GoBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure BrowserCreatedMsg(var aMessage : TMessage); message CEF_AFTERCREATED;
    procedure NewLocationMsg(var aMessage : TMessage); message MINIBROWSER_NEWLOCATION;
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
  end;

var
  GeolocationFrm : TGeolocationFrm;
  GlobalPosition : TCefGeoposition;

implementation

{$R *.dfm}

uses
  uCEFMiscFunctions;

procedure GeoLocationUpdate(const position: PCefGeoposition);
begin
  GlobalPosition.latitude          := position.latitude;
  GlobalPosition.longitude         := position.longitude;
  GlobalPosition.altitude          := position.altitude;
  GlobalPosition.accuracy          := position.accuracy;
  GlobalPosition.altitude_accuracy := position.altitude_accuracy;
  GlobalPosition.heading           := position.heading;
  GlobalPosition.speed             := position.speed;
  GlobalPosition.timestamp         := position.timestamp;
  GlobalPosition.error_code        := position.error_code;
  GlobalPosition.error_message     := position.error_message;

  PostMessage(GeolocationFrm.Handle, MINIBROWSER_NEWLOCATION, 0, 0);
end;

procedure TGeolocationFrm.Chromium1AfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  PostMessage(Handle, CEF_AFTERCREATED, 0, 0);
end;

procedure TGeolocationFrm.FormCreate(Sender: TObject);
begin
  CefGetGeolocation(GeoLocationUpdate);
end;

procedure TGeolocationFrm.FormShow(Sender: TObject);
begin
  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) then Timer1.Enabled := True;
end;

procedure TGeolocationFrm.GoBtnClick(Sender: TObject);
begin
  Chromium1.LoadURL(Edit1.Text);
end;

procedure TGeolocationFrm.BrowserCreatedMsg(var aMessage : TMessage);
begin
  CEFWindowParent1.UpdateSize;
  NavControlPnl.Enabled := True;
  GoBtn.Click;
end;

procedure TGeolocationFrm.NewLocationMsg(var aMessage : TMessage);
begin
  StatusBar1.Panels[0].Text := 'lat : ' + floattostr(GlobalPosition.latitude);
  StatusBar1.Panels[1].Text := 'lon : ' + floattostr(GlobalPosition.longitude);
  StatusBar1.Panels[2].Text := 'alt : ' + floattostr(GlobalPosition.altitude);
end;

procedure TGeolocationFrm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(Chromium1.CreateBrowser(CEFWindowParent1, '')) and not(Chromium1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TGeolocationFrm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TGeolocationFrm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (Chromium1 <> nil) then Chromium1.NotifyMoveOrResizeStarted;
end;

procedure TGeolocationFrm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TGeolocationFrm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
