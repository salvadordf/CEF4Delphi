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
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs, Vcl.Buttons, Winapi.Messages,
  Vcl.ExtCtrls, Vcl.ComCtrls;
  {$ELSE}
  Windows, SysUtils, Classes, Graphics, Forms,
  Controls, StdCtrls, Dialogs, Buttons, Messages,
  ExtCtrls, ComCtrls;
  {$ENDIF}

const
  CEFBROWSER_CREATED          = WM_APP + $100;
  CEFBROWSER_CHILDDESTROYED   = WM_APP + $101;

type
  TMainForm = class(TForm)
    Timer1: TTimer;
    ButtonPnl: TPanel;
    NewBtn: TSpeedButton;
    ExitBtn: TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
  private
    // Variables to control when can we destroy the form safely
    FCanClose : boolean;  // Set to True when the final timer is triggered
    FClosing  : boolean;  // Set to True in the CloseQuery event.

    procedure CreateMDIChild(const Name: string);
    procedure CloseAllChildForms;

  protected
    procedure ChildDestroyedMsg(var aMessage : TMessage); message CEFBROWSER_CHILDDESTROYED;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uChildForm;

// Destruction steps
// =================
// 1. Destroy all child forms
// 2. Enable a Timer and wait for 1 second
// 3. Close and destroy the main form

procedure TMainForm.CreateMDIChild(const Name: string);
var
  TempChild : TChildForm;
begin
  TempChild         := TChildForm.Create(Application);
  TempChild.Caption := Name;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FClosing then
    CanClose := FCanClose
   else
    begin
      FClosing := True;

      if (MDIChildCount = 0) then
        CanClose := True
       else
        begin
          CanClose := False;
          CloseAllChildForms;
        end;
    end;
end;

procedure TMainForm.CloseAllChildForms;
var
  i : integer;
begin
  i := pred(MDIChildCount);

  while (i >= 0) do
    begin
      if not(TChildForm(MDIChildren[i]).Closing) then
        PostMessage(MDIChildren[i].Handle, WM_CLOSE, 0, 0);

      dec(i);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCanClose := False;
  FClosing  := False;
end;

procedure TMainForm.NewBtnClick(Sender: TObject);
begin
  CreateMDIChild('ChildForm' + IntToStr(MDIChildCount + 1));
end;

procedure TMainForm.ExitBtnClick(Sender: TObject);
begin
  ButtonPnl.Enabled := False;

  if (MDIChildCount = 0) then
    Close
   else
    CloseAllChildForms;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if not(FCanClose) then
    begin
      FCanClose := True;
      PostMessage(self.Handle, WM_CLOSE, 0, 0);
    end;
end;

procedure TMainForm.ChildDestroyedMsg(var aMessage : TMessage);
begin
  // If there are no more child forms we can destroy the main form
  if (MDIChildCount = 0) then
    begin
      ButtonPnl.Enabled := False;
      Timer1.Enabled    := True;
    end;
end;

end.
