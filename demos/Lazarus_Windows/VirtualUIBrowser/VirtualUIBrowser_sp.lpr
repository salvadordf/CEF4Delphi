program VirtualUIBrowser_sp;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  LCLIntf, LCLType, LMessages, Forms, Interfaces,
  uCEFApplicationCore;

{$IFDEF MSWINDOWS}
  // CEF needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

{$R *.res}

begin
  GlobalCEFApp                            := TCefApplicationCore.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableGPU                  := True;     
  GlobalCEFApp.SetCurrentDir              := True;
  //GlobalCEFApp.LogFile                    := 'debug.log';
  //GlobalCEFApp.LogSeverity                := LOGSEVERITY_VERBOSE;  

  GlobalCEFApp.StartSubProcess;
  DestroyGlobalCEFApp;
end.

