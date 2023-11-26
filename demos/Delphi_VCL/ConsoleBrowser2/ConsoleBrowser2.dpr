program ConsoleBrowser2;

{$I ..\..\..\source\cef.inc}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uCEFApplication,
  uEncapsulatedBrowser in 'uEncapsulatedBrowser.pas',
  uCEFBrowserThread in 'uCEFBrowserThread.pas';

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

// This demo navigates to a webpage, captures the browser screen and saves it as a bitmap file called "snapshot.bmp"

// ConsoleBrowser2 is a console application without a user interface. For this reason the browser is in
// "off-screen mode" (OSR mode), it's encamsulated in a custom thread and it uses a different EXE for the
// CEF subprocesses (ConsoleBrowser2_sp.exe).

// While the custom browser thread is loading the webpage and capturing the screen, the console application is
// waiting for an Event.

// ConsoleBrowser2 reads the "/url" parameter and uses that URL to navigate and capture the screen.
// For example : ConsoleBrowser2.exe /url=https://www.briskbard.com
// If you need to debug this demo in Delphi using http://www.example.com click on the "Run->Parameters" menu option,
// select the right target (Windows 32 bits / Windows 64 bits) and type "/url=http://www.example.com" (without quotes)
// in the "Parameters" box.

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
