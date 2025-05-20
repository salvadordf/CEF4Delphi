program OAuth2Tester;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  Windows,
  Interfaces,
  uCEFApplication,
  uOAuth2TesterFrm in 'uOAuth2TesterFrm.pas' {OAuth2TesterFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;   
  GlobalCEFApp.SetCurrentDir       := True;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TOAuth2TesterFrm, OAuth2TesterFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
