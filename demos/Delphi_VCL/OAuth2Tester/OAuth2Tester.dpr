program OAuth2Tester;

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  {$ELSE}
  Forms,
  {$ENDIF }
  uCEFApplication,
  uCEFConstants,
  uOAuth2TesterFrm in 'uOAuth2TesterFrm.pas' {OAuth2TesterFrm};

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TOAuth2TesterFrm, OAuth2TesterFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
