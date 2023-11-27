unit InitSubProcess;

{$mode ObjFPC}{$H+}
{$IFDEF MSWINDOWS}{$I ..\..\..\source\cef.inc}{$ELSE}{$I ../../../source/cef.inc}{$ENDIF}

interface

uses
  GlobalCefApplication, uCEFApplication, uCEFWorkScheduler;

implementation

initialization
  CreateGlobalCEFApp;
  if not GlobalCEFApp.StartMainProcess then begin
    if GlobalCEFWorkScheduler <> nil then
      GlobalCEFWorkScheduler.StopScheduler;
    DestroyGlobalCEFApp;
    DestroyGlobalCEFWorkScheduler;
    halt(0); // exit the subprocess
  end;

end.

