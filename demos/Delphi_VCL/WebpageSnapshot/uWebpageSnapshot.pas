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

unit uWebpageSnapshot;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  {$ENDIF}
  uCEFBrowserThread;

type
  TWebpageSnapshotFrm = class(TForm)
    StatusBar1: TStatusBar;
    Image1: TImage;
    NavigationPnl: TPanel;
    GoBtn: TButton;
    AddressEdt: TEdit;
    procedure GoBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  private
    FThread : TCEFBrowserThread;

    procedure Thread_OnError(Sender: TObject);
    procedure Thread_OnSnapshotAvailable(Sender: TObject);
  end;

var
  WebpageSnapshotFrm: TWebpageSnapshotFrm;

// This demo shows how to encapsulate a browser without user interface in a thread.

// The thread in the uCEFBrowserThread unit has a browser in "off-screen" mode
// and it takes a snapshot when the browser has loaded a web page.

// The thread triggers the TCEFBrowserThread.OnSnapshotAvailable when the main thread
// can copy the snapshot in a bitmap.

// If there's an error loading the page then TCEFBrowserThread.OnError will be
// triggered and the error information will be available in the
// TCEFBrowserThread.ErrorCode, TCEFBrowserThread.ErrorText and
// TCEFBrowserThread.FailedUrl properties.

// The TCEFBrowserThread.Create constructor has the default URL, virtual screen size,
// virtual screen scale and a delay as parameters. The delay is applied after the browser
// has finished loading the main frame and before taking the snapshot.

procedure CreateGlobalCEFApp;

implementation

{$R *.dfm}

uses
  uCEFApplication;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
end;

procedure TWebpageSnapshotFrm.GoBtnClick(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Loading...';
  screen.cursor := crAppStart;

  if (FThread = nil) then
    begin
      FThread                     := TCEFBrowserThread.Create(AddressEdt.Text, 1024, 768);
      FThread.OnError             := Thread_OnError;
      FThread.OnSnapshotAvailable := Thread_OnSnapshotAvailable;
      FThread.SyncEvents          := True;
      FThread.Start;
    end
   else
    FThread.LoadUrl(AddressEdt.Text);
end;

procedure TWebpageSnapshotFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (FThread <> nil) then
    begin
      if FThread.TerminateBrowserThread then
        FThread.WaitFor;

      FreeAndNil(FThread);
    end;

  CanClose := True;
end;

procedure TWebpageSnapshotFrm.FormCreate(Sender: TObject);
begin
  FThread := nil;
end;

procedure TWebpageSnapshotFrm.Thread_OnError(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'Error ' + inttostr(FThread.ErrorCode) + ' : ' + FThread.ErrorText + ' - ' + FThread.FailedUrl;
  screen.cursor := crDefault;
end;

procedure TWebpageSnapshotFrm.Thread_OnSnapshotAvailable(Sender: TObject);
var
  TempBitmap : TBitmap;
begin
  TempBitmap    := nil;
  screen.cursor := crDefault;

  if (FThread <> nil) and FThread.CopySnapshot(TempBitmap) then
    begin
      Image1.Picture.Assign(TempBitmap);
      StatusBar1.Panels[0].Text := 'Snapshot copied successfully';
      TempBitmap.Free;
    end
   else
    StatusBar1.Panels[0].Text := 'There was an error copying the snapshot';
end;

end.
