program MediaRouter;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Windows,
  Forms,
  Interfaces,
  uCEFApplication,
  uMediaRouterFrm in 'uMediaRouterFrm.pas' {MediaRouterFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TMediaRouterFrm, MediaRouterFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
