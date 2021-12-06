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

unit uDirectorySelector;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl,
  Vcl.ExtCtrls;
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  FileCtrl, ExtCtrls;
  {$ENDIF}

type
  TDirectorySelectorFrm = class(TForm)
    Panel1: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    DriveComboBox1: TDriveComboBox;
    Panel3: TPanel;
    DirectoryListBox1: TDirectoryListBox;
  private
    procedure SetSelectedDir(const aValue : string);
    function  GetSelectedDir : string;

  public
    property SelectedDir : string read GetSelectedDir write SetSelectedDir;
  end;

implementation

{$R *.dfm}

procedure TDirectorySelectorFrm.SetSelectedDir(const aValue : string);
begin
  DirectoryListBox1.Directory := aValue;
end;

function TDirectorySelectorFrm.GetSelectedDir : string;
begin
  Result := DirectoryListBox1.Directory;
end;

end.
