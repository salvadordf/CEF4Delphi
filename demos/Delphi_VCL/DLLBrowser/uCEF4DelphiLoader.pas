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

unit uCEF4DelphiLoader;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;
  {$ENDIF}

type
  TForm1 = class(TForm)
    InitializeBtn: TButton;
    ShowBtn: TButton;
    FinalizeBtn: TButton;
    CloseBtn: TButton;
    procedure InitializeBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
    procedure FinalizeBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FInitialized : boolean;
  public
    { Public declarations }
  end;

  procedure InitializeCEF4Delphi; stdcall; external 'DLLBrowser.dll';
  procedure FinalizeCEF4Delphi; stdcall; external 'DLLBrowser.dll';
  procedure ShowBrowser; stdcall; external 'DLLBrowser.dll';

var
  Form1: TForm1;

implementation

{$R *.dfm}

// ****************************************
// READ THE CODE COMMENTS IN DLLBROWSER.DPR
// ****************************************

// To test this demo you need to build the CEF4DelphiLoader, DLLBrowser and SubProcess projects found in this directory.

procedure TForm1.CloseBtnClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.FinalizeBtnClick(Sender: TObject);
begin
  FinalizeCEF4Delphi;

  ShowBtn.Enabled       := False;
  FinalizeBtn.Enabled   := False;
  CloseBtn.Enabled      := True;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FInitialized then FinalizeCEF4Delphi;

  CanClose := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FInitialized := False;
end;

procedure TForm1.InitializeBtnClick(Sender: TObject);
begin
  InitializeCEF4Delphi;

  FInitialized := True;

  InitializeBtn.Enabled := False;
  ShowBtn.Enabled       := True;
  FinalizeBtn.Enabled   := True;
end;

procedure TForm1.ShowBtnClick(Sender: TObject);
begin
  ShowBrowser;

  ShowBtn.Enabled       := False;
end;

end.


