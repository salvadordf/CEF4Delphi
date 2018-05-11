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
//        Copyright � 2018 Salvador D�az Fau. All rights reserved.
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

program JSSimpleWindowBinding;

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Forms,
  Windows,
  {$ENDIF }
  uCEFApplication,
  uJSSimpleWindowBinding in 'uJSSimpleWindowBinding.pas' {JSSimpleWindowBindingFrm};

{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  GlobalCEFApp := TCefApplication.Create;

  // This is the same demo than the JSSimpleWindowBinding but using a different executable for the subprocesses.
  // Notice that GlobalCEFApp.OnContextCreated is now defined in the SubProcess.

  // Follow these steps to test this demo :
  // 1. Build the SubProcess project in this directory.
  // 2. Copy the CEF binaries to the BIN directory in CEF4Delphi.
  // 3. Build this project : JSSimpleWindowBinding
  // 4. Run this demo : JSSimpleWindowBinding

  GlobalCEFApp.BrowserSubprocessPath := 'SubProcess.exe';

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TJSSimpleWindowBindingFrm, JSSimpleWindowBindingFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
