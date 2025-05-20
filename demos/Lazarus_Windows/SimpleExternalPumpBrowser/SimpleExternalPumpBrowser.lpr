program SimpleExternalPumpBrowser;

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uCEFWorkScheduler,
  uSimpleExternalPumpBrowser in 'uSimpleExternalPumpBrowser.pas' {SimpleExternalPumpBrowserFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

{$R *.res}

begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TSimpleExternalPumpBrowserFrm, SimpleExternalPumpBrowserFrm);
      Application.Run;

      // The form needs to be destroyed *BEFORE* stopping the scheduler.
      SimpleExternalPumpBrowserFrm.Free;

      GlobalCEFWorkScheduler.StopScheduler;
    end;

  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;
end.
