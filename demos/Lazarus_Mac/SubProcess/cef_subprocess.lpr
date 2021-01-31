program cef_subprocess;

(*
 * The compiled exe should be copied into
 * SimpleBrowser.app/Content/Frameworks/SimpleBrowser2 Helper.app/Content/MacOS/SimpleBrowser2 Helper
 * including app bundle in SimpleBrowser2.app/Content/Frameworks/SimpleBrowser2 Helper.app
 *)

{$mode objfpc}{$H+}

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


  GlobalCEFApp.MultiThreadedMessageLoop:=false;
  GlobalCEFApp.StartSubProcess;
  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.

