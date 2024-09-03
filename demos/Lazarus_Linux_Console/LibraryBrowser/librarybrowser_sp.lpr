program librarybrowser_sp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Classes, SysUtils, uCEFApplicationCore;

begin
  GlobalCEFApp                            := TCefApplicationCore.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ShowMessageDlg             := False;
  GlobalCEFApp.BlinkSettings              := 'hideScrollbars';     
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.StartSubProcess;
  DestroyGlobalCEFApp;
end.

