program SubProcess_sp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  uCEFApplicationCore;

begin
  GlobalCEFApp := TCefApplicationCore.Create;

  // The main process and the subprocess *MUST* have the same GlobalCEFApp
  // properties and events, specially FrameworkDirPath, ResourcesDirPath,
  // LocalesDirPath, cache and UserDataPath paths.

  // The demos are compiled into the BIN directory. Make sure SubProcess.exe
  // and SubProcess_sp.exe are in that directory or this demo won't work.

  // In case you want to use custom directories for the CEF3 binaries, cache
  // and user data.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  GlobalCEFApp.StartSubProcess;
  DestroyGlobalCEFApp;
end.

