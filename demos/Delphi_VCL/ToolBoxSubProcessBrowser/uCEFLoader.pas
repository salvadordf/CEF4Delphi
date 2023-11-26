unit uCEFLoader;

interface

implementation

uses
  uCEFApplication;

  // Follow these steps to test this demo :
  // 1. Build the ToolBoxSubProcessBrowser_sp project in this directory.
  // 2. Copy the CEF binaries to the BIN directory in CEF4Delphi.
  // 3. Build this project : ToolBoxSubProcessBrowser
  // 4. Run this demo : ToolBoxSubProcessBrowser

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp := TCefApplication.Create;

  // In case you want to use custom directories for the CEF binaries, cache and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
  // The cache and user data directories must be writable.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  GlobalCEFApp.BrowserSubprocessPath := 'ToolBoxSubProcessBrowser_sp.exe';

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
