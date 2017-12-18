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

unit uMainForm;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFInterfaces, uCustomResourceHandler, uCEFConstants;

type
  TMainForm = class(TForm)
    ChromiumWindow1: TChromiumWindow;
    AddressBarPnl: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Timer1: TTimer;

    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  private
    procedure WMMove(var aMessage : TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;

  protected
    procedure Chromium_OnAfterCreated(Sender: TObject);
    procedure Chromium_OnGetResourceHandler(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest; out Result: ICefResourceHandler);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uCEFMiscFunctions, uCEFApplication;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ChromiumWindow1.LoadURL(Edit1.Text);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ChromiumWindow1.OnAfterCreated                       := Chromium_OnAfterCreated;
  ChromiumWindow1.ChromiumBrowser.OnGetResourceHandler := Chromium_OnGetResourceHandler;

  // GlobalCEFApp.GlobalContextInitialized has to be TRUE before creating any browser
  // If it's not initialized yet, we use a simple timer to create the browser later.
  if not(ChromiumWindow1.CreateBrowser) then Timer1.Enabled := True;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if not(ChromiumWindow1.CreateBrowser) and not(ChromiumWindow1.Initialized) then
    Timer1.Enabled := True;
end;

procedure TMainForm.Chromium_OnAfterCreated(Sender: TObject);
begin
  ChromiumWindow1.UpdateSize;
  AddressBarPnl.Enabled := True;
end;

procedure TMainForm.Chromium_OnGetResourceHandler(Sender : TObject;
                                                  const browser : ICefBrowser;
                                                  const frame   : ICefFrame;
                                                  const request : ICefRequest;
                                                  out   Result  : ICefResourceHandler);
var
  TempStream : TStringStream;
begin
  // This event is called from the IO thread. Use mutexes if necessary.
  TempStream := nil;
  Result     := nil;

  try
    try
      TempStream := TStringStream.Create('<!DOCTYPE html><html><body><p>test</p></body></html>', TEncoding.UTF8, false);
      Result     := TCustomResourceHandler.Create(browser, frame, '', request, TStream(TempStream), CefGetMimeType('html'));
    except
      on e : exception do
        if CustomExceptionHandler('TMainForm.Chromium_OnGetResourceHandler', e) then raise;
    end;
  finally
    if (TempStream <> nil) then FreeAndNil(TempStream);
  end;
end;

procedure TMainForm.WMMove(var aMessage : TWMMove);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMMoving(var aMessage : TMessage);
begin
  inherited;

  if (ChromiumWindow1 <> nil) then ChromiumWindow1.NotifyMoveOrResizeStarted;
end;

procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);
begin
  inherited;

  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then GlobalCEFApp.OsmodalLoop := False;
end;

end.
