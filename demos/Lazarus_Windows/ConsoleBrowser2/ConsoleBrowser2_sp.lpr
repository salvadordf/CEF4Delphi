program ConsoleBrowser2_sp;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  uCEFApplicationCore;

{$IFDEF WIN32}
  // CEF3 needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

begin
  GlobalCEFApp                            := TCefApplicationCore.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ShowMessageDlg             := False;
  GlobalCEFApp.BlinkSettings              := 'hideScrollbars';     
  GlobalCEFApp.SetCurrentDir              := True;
  GlobalCEFApp.StartSubProcess;
  DestroyGlobalCEFApp;
end.

