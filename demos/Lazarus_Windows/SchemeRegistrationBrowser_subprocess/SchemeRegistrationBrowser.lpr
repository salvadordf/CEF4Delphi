program SchemeRegistrationBrowser;

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uSchemeRegistrationBrowser in 'uSchemeRegistrationBrowser.pas' {SchemeRegistrationBrowserFrm},
  uHelloScheme in 'uHelloScheme.pas';

//{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

{$R *.res}

begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TSchemeRegistrationBrowserFrm, SchemeRegistrationBrowserFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
