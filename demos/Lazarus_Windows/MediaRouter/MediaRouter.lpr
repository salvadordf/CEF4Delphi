program MediaRouter;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Windows,
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uMediaRouterFrm in 'uMediaRouterFrm.pas' {MediaRouterFrm};

{.$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

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
