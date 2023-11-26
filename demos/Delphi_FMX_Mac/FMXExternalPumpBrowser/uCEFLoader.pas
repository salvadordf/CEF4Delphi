unit uCEFLoader;

{$I ..\..\..\source\cef.inc}

interface

uses
  uCEFApplicationCore;

implementation

procedure StartCEFSubprocess;
begin
  GlobalCEFApp                            := TCefApplicationCore.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.ExternalMessagePump        := True;
  GlobalCEFApp.MultiThreadedMessageLoop   := False;
  GlobalCEFApp.InitLibLocationFromArgs;
  GlobalCEFApp.StartSubProcess;
end;

initialization
  StartCEFSubprocess;

finalization
  DestroyGlobalCEFApp;

end.
