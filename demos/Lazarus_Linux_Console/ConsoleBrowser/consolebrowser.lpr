program consolebrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  // "Interfaces" is a custom unit used to initialize the LCL WidgetSet
  // We keep the same name to avoid a Lazarus warning.
  Interfaces, // this includes the LCL widgetset
  Classes, SysUtils, uCEFApplication, uencapsulatedbrowser;

begin
  try
    try
      CreateGlobalCEFApp;

      // The LCL Widgetset must be initialized after the CEF initialization
      CustomWidgetSetInitialization;

      if WaitForMainAppEvent then
        WriteResult;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    CustomWidgetSetFinalization;
    DestroyGlobalCEFApp;
  end;
end.

