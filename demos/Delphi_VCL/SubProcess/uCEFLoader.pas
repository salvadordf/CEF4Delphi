unit uCEFLoader;

interface

implementation

uses
  uCEFApplication;

procedure CreateGlobalCEFApp;
begin
  // In case you prefer to call CreateGlobalCEFApp and DestroyGlobalCEFApp manually
  // you have to remember that GlobalCEFApp can only be initialized *ONCE* per process.
  // This is a CEF requirement and there's no workaround.
  if (GlobalCEFApp <> nil) then
    exit;

  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.BrowserSubprocessPath := 'SimpleBrowser_sp.exe';

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
