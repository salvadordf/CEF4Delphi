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
//        Copyright © 2023 Salvador Diaz Fau. All rights reserved.
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

unit uCEFLoader;

interface

implementation

uses
  VirtualUI_SDK,
  uCEFApplication, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFMiscFunctions,
  uVirtualUIBrowserConstants;

procedure CreateGlobalCEFApp;
begin
  // Start VirtualUI and allow the execution of CEF subprocesses with VirtualUIBrowser_sp.exe
  VirtualUI.Start;
  VirtualUI.AllowExecute('VirtualUIBrowser_sp.exe');

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.BrowserSubprocessPath      := 'VirtualUIBrowser_sp.exe';
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.TouchEvents                := STATE_ENABLED;
  GlobalCEFApp.EnableGPU                  := True;
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_VERBOSE;

  // If you need transparency leave the GlobalCEFApp.BackgroundColor property
  // with the default value or set the alpha channel to 0
  if TRANSPARENT_BROWSER then
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($00, $00, $00, $00)
   else
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

  // This demo uses a different EXE for the subprocesses.
  // With this configuration it's not necessary to have the
  // GlobalCEFApp.StartMainProcess call in a if..then clause.
  GlobalCEFApp.StartMainProcess;
end;

initialization
  CreateGlobalCEFApp;

finalization
  DestroyGlobalCEFApp;

end.
