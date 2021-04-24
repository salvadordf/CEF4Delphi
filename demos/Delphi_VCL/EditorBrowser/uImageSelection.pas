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

unit uImageSelection;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TImageSelectionFrm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    RemoteRb: TRadioButton;
    LocalRb: TRadioButton;
    URLLbl: TLabel;
    URLEdt: TEdit;
    FileLbl: TLabel;
    FileEdt: TEdit;
    FileBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    procedure FileBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageSelectionFrm: TImageSelectionFrm;

implementation

{$R *.dfm}

procedure TImageSelectionFrm.FileBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'PNG Files (*.png)|*.PNG';

  if OpenDialog1.Execute then FileEdt.Text := OpenDialog1.FileName;
end;

end.
