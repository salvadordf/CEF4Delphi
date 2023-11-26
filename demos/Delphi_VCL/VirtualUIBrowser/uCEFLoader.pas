unit uCEFLoader;

interface

implementation

uses
  VirtualUI_SDK,
  uCEFApplication, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFMiscFunctions,
  uVirtualUIBrowserConstants;

procedure CreateGlobalCEFApp;
begin
  // Start VirtualUI and allow the execution of CEF subprocesses with VirtualUIBrowser_sp.exe
  VirtualUI.Start;
  VirtualUI.AllowExecute('VirtualUIBrowser_sp.exe');

  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.BrowserSubprocessPath      := 'VirtualUIBrowser_sp.exe';
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.TouchEvents                := STATE_ENABLED;
  GlobalCEFApp.EnableGPU                  := True;
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_VERBOSE;

  // If you need transparency leave the GlobalCEFApp.BackgroundColor property
  // with the default value or set the alpha channel to 0
  if TRANSPARENT_BROWSER then
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($00, $00, $00, $00)
   else
    GlobalCEFApp.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);

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
