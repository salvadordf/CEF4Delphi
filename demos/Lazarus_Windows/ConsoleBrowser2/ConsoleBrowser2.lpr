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

program ConsoleBrowser2;

{$MODE Delphi}

{$APPTYPE CONSOLE}

{.$R *.res}

uses
  SysUtils, Interfaces,
  uCEFApplication,
  uEncapsulatedBrowser in 'uEncapsulatedBrowser.pas',
  uCEFBrowserThread in 'uCEFBrowserThread.pas';

{$IFDEF WIN32}
  // CEF3 needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

// This demo navigates to a webpage, captures the browser screen and saves it as a bitmap file called "snapshot.bmp"

// ConsoleBrowser2 is a console application without a user interface. For this reason the browser is in
// "off-screen mode" (OSR mode), it's encamsulated in a custom thread and it uses a different EXE for the
// CEF subprocesses (ConsoleBrowser2_sp.exe).

// In order to test this demo you need to build the "ConsoleBrowser2_sp.lpr" project too !!!

// While the custom browser thread is loading the webpage and capturing the screen, the console application is
// waiting for an Event.

// ConsoleBrowser2 reads the "/url" parameter and uses that URL to navigate and capture the screen.
// For example : ConsoleBrowser2.exe /url=https://www.briskbard.com
// If you need to debug this demo in Lazarus using http://www.example.com click on the "Run->Run Parameters" menu option,
// and type "/url=http://www.example.com" (without quotes) in the "Command line parameters" box.

// By default the browser uses a virtual screen size of 1024x728 with 96 DPI (screen scale = 1)
// If you need a different resolution or scale just edit those values in TEncapsulatedBrowser.Create
// or add new command line switches with the new information.

// The browser captures the screen when the main frame is loaded but some web pages need some extra time to finish.
// This demo uses a 500 ms delay to avoid this problem and you can modify this value in TEncapsulatedBrowser.Create

// CEF is configured in this demo to use "in memory" cache. As a result of this, the browser will always download
// all the web page resources. Remember that if you switch to a local directory for the cache you will have problems
// when you run several instances of this demo at the same time because Chromium doesn't allow sharing the same cache
// by different processes.

// This demo is configured to load the CEF binaries in the same directory were ConsoleBrowser2.exe is located but you
// can set a different directory for the binaries by setting the GlobalCEFApp.FrameworkDirPath,
// GlobalCEFApp.ResourcesDirPath and GlobalCEFApp.LocalesDirPath properties inside CreateGlobalCEFApp.
// See the SimpleBrowser2 demo for more details.

// CEF is configured to use ConsoleBrowser2_sp.exe for the subprocesses and it tries to find it in the same directory as
// ConsoleBrowser2.exe but it's possible to use a different location for that EXE if you set a custom path in
// GlobalCEFApp.BrowserSubprocessPath.

// Most of the GlobalCEFApp properties must be the same in the main EXE and the EXE for the subprocesses. If you modify
// them in CreateGlobalCEFApp then you'll also have to copy those property values in ConsoleBrowser2_sp.dpr
// See the "SubProcess" demo for more details.

begin
  try
    try
      CreateGlobalCEFApp;
      if WaitForMainAppEvent then
        WriteResult;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    DestroyGlobalCEFApp;
  end;
end.
