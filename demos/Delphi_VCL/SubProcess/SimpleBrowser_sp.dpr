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
//        Copyright © 2021 Salvador Díaz Fau. All rights reserved.
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
program SimpleBrowser_sp;
{$I cef.inc}
uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
  uCEFLoader in 'uCEFLoader.pas';

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes
// to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
begin
  // This SubProcess project is only used for the CEF subprocesses and it needs
  // to declare "CEFSUBPROCESS" conditional define. Follow these steps to add it:
  // 1. Open the project options in the Project->Options menu option and select
  // "Building->Delphi Compiler" on the left.
  // 2. Select "All configurations - All platforms" option as the "Target" on
  // the right section of that window.
  // 3. Add "CEFSUBPROCESS" (without quotes) in the "Conditional defines" box.
  // uCEFLoader will call CreateGlobalCEFApp and DestroyGlobalCEFApp in the
  // initialization and finalization sections of that unit.
end.
