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


 (*
 * Include the following files
 * BrowserWindowEx.app/Contents/Frameworks/ExternalPumpBrowser Helper.app/
 *   files from the demos/Lazarus_Mac/AppHelper project
 *   use create_mac_helper.sh
 *
 * BrowserWindowEx.app/Contents/Frameworks/Chromium Embedded Framework.framework
 *   files from Release folder in cef download
 *
 *)


program BrowserWindowEx;

{$mode objfpc}{$H+}
{$I cef.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF LINUX}
  InitSubProcess, // On Linux this unit must be used *before* the "interfaces" unit.
  {$ENDIF}
  Interfaces,
  Forms,
  uBrowserWindowEx, GlobalCefApplication
  { you can add units after this }
  ;

{$R *.res}

{$IFDEF WIN32}
  // CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

