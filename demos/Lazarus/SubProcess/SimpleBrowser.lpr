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
//        Copyright © 2018 Salvador Díaz Fau. All rights reserved.
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

program SimpleBrowser;

{$I cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uSimpleBrowser in 'uSimpleBrowser.pas' {Form1},
  uCEFLoader in 'uCEFLoader.pas';

//{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
// The IMAGE_FILE_LARGE_ADDRESS_AWARE constant is declared in WinApi.Windows. If you don't want to add
// WinApi.Windows to the 'uses' section in this file just replace the following line by
// {$SetPEFlags $20}
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  // This demo has the GlobalCEFApp creation, initialization and destruction in uCEFLoader.pas
  // Read the code comments in uCEFLoader.pas for more details.

  Application.Initialize;
  {$IFDEF DELPHI11_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
