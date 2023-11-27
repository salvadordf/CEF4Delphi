program OAuth2Tester;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows,
  Vcl.Forms,
  {$ELSE}
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uCEFConstants,
  uOAuth2TesterFrm in 'uOAuth2TesterFrm.pas' {OAuth2TesterFrm};

{.$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TOAuth2TesterFrm, OAuth2TesterFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
