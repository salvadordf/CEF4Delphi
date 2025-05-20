program VirtualUIBrowser_sp;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  LCLIntf, LCLType, LMessages, Forms, Interfaces,
  uCEFApplicationCore;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

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

