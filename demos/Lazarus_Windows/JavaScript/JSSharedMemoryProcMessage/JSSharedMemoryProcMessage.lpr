program JSSharedMemoryProcMessage;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

uses
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm};

{.$R *.res}

// CEF needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags $20}

{$R *.res}

begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      //ReportMemoryLeaksOnShutdown := True;

      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;

  // This is not really necessary to fix the bug #89 but if you free GlobalCEFApp in a different unit
  // then you can call 'FreeAndNil' without adding SysUtils to this DPR.
  DestroyGlobalCEFApp;
end.
