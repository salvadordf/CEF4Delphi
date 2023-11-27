 (*
 * Include the following files
 * BrowserWindow.app/Contents/Frameworks/ExternalPumpBrowser Helper.app/
 *   files from the demos/Lazarus_Mac/AppHelper project
 *   use create_mac_helper.sh
 *
 * BrowserWindow.app/Contents/Frameworks/Chromium Embedded Framework.framework
 *   files from Release folder in cef download
 *
 *)


program BrowserWindowDom;

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}{$I ..\..\..\source\cef.inc}{$ELSE}{$I ../../../source/cef.inc}{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF LINUX}
  InitSubProcess, // On Linux this unit must be used *before* the "interfaces" unit.
  {$ENDIF}
  Interfaces,
  Forms,
  uBrowserWindowDom, GlobalCefApplication,
  uHelperProcessDom  // Used here, for any OS where the main exe server as AppHelper too.
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

