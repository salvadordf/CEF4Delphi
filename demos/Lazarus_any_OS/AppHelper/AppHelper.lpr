program AppHelper;

(*
 * The compiled exe should be copied into
 * SimpleBrowser.app/Content/Frameworks/SimpleBrowser2 Helper.app/Content/MacOS/SimpleBrowser2 Helper
 * including app bundle in SimpleBrowser2.app/Content/Frameworks/SimpleBrowser2 Helper.app
 *)

{$mode objfpc}{$H+}

{$IFDEF MSWINDOWS}{$I ..\..\..\source\cef.inc}{$ELSE}{$I ../../../source/cef.inc}{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  uCEFApplicationCore, uCEFTypes, uCEFConstants, LazFileUtils, sysutils;

begin
  GlobalCEFApp                  := TCefApplicationCore.Create;

  // The main process and the subprocess *MUST* have the same GlobalCEFApp
  // properties and events, specially FrameworkDirPath, ResourcesDirPath,
  // LocalesDirPath, cache and UserDataPath paths.
  {$IFDEF MACOSX}
  GlobalCEFApp.InitLibLocationFromArgs;
  {$ENDIF}

  GlobalCEFApp.UseMockKeyChain := True;
  //GlobalCEFApp.EnableGPU := False;
  //GlobalCEFApp.LogFile     := 'debug.log';
  //GlobalCEFApp.LogSeverity := LOGSEVERITY_VERBOSE;

  GlobalCEFApp.StartSubProcess;
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.

