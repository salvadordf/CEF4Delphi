program SimpleOSRBrowser2;

{$MODE OBJFPC}{$H+}

uses
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,    
  uCEFWorkScheduler,
  usimplelazosrbrowser2 in 'uSimpleOSRBrowser2.pas' {Form1};

{$IFDEF MSWINDOWS}
  // CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;    

      // The form needs to be destroyed *BEFORE* stopping the scheduler.
      Form1.Free;

      GlobalCEFWorkScheduler.StopScheduler;
    end;

  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;
end.
