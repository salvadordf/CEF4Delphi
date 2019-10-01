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

library OSRDLLBrowser;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF }
  uCEFApplication,
  uCEFConstants,
  uWebBrowser in 'uWebBrowser.pas' {WebBrowserFrm};

{$R *.res}


// This is the simplest way to create a DLL with all that it's necessary to show a
// Chromium based browser using CEF4Delphi

// This demo uses the OSR mode and disables the multithreaded mode. For this reason, the ShowBrowser
// functions calls GlobalCEFApp.RunMessageLoop and the browser form calls GlobalCEFApp.QuitMessageLoop as the last step in
// the form destruction.

// To test this demo you need to build the ConsoleLoader, OSRDLLBrowser and OSRSubProcess projects found in this directory.

// CEF3 needs to be initialized and finalized outside the DLL's initialization and
// finalization sections. For this reason, you need to call InitializeCEF4Delphi
// after you load this DLL and you also need to call FinalizeCEF4Delphi before
// unloading this DLL.

// CEF3 can only be initialized once per process and this means that :
// 1. You can only call InitializeCEF4Delphi and FinalizeCEF4Delphi once.
// 2. If you use a DLL like this as a plugin and there's another loaded plugin using
//    CEF you will have problems.

// When you use CEF in a DLL you must use a different EXE for the subprocesses and that EXE
// must configure GlobalCEFApp with the same properties.

// ***************************
// This demo is incomplete!!!!
// ***************************
// As all other demos, you need to close all web browsers before calling FinalizeCEF4Delphi.
// All the browsers must be closed following the destruction sequence described in uWebBrowser.pas.

procedure InitializeCEF4Delphi; stdcall;
begin
  GlobalCEFApp := TCefApplication.Create;

  // In case you want to use custom directories for the CEF3 binaries, cache and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
  // The cache, cookies and user data directories must be writable.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
  GlobalCEFApp.LogFile              := 'debug.log';
  GlobalCEFApp.LogSeverity          := LOGSEVERITY_INFO;
}

  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport       := True;
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.BrowserSubprocessPath      := 'OSRSubProcess.exe';
  GlobalCEFApp.ExternalMessagePump        := False;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;

  // This demo uses a different EXE for the subprocesses.
  // With this configuration it's not necessary to have the
  // GlobalCEFApp.StartMainProcess call in a if..then clause.

  GlobalCEFApp.StartMainProcess;
end;

procedure FinalizeCEF4Delphi; stdcall;
begin
  DestroyGlobalCEFApp;
end;

procedure ShowBrowser; stdcall;
begin
  WebBrowserFrm := TWebBrowserFrm.Create(nil);
  WebBrowserFrm.Show;
  GlobalCEFApp.RunMessageLoop;
end;

exports
  InitializeCEF4Delphi,
  FinalizeCEF4Delphi,
  ShowBrowser;

begin
  //
end.
