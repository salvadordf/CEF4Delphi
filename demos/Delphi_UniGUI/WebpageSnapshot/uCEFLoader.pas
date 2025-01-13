unit uCEFLoader;

interface

uses
  uCEFApplication;

implementation

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp                            := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ShowMessageDlg             := False;
  GlobalCEFApp.BrowserSubprocessPath      := 'WebpageSnapshotUniGUI_sp.exe'; // This is the other EXE for the CEF subprocesses. It's on the same directory as this app.
  GlobalCEFApp.BlinkSettings              := 'hideScrollbars';               // This setting removes all scrollbars to capture a cleaner snapshot
  GlobalCEFApp.StartMainProcess;
end;

initialization
  CreateGlobalCEFApp;

finalization
  DestroyGlobalCEFApp;

end.
