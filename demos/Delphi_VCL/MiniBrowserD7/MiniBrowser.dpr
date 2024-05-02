program MiniBrowser;

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  uCEFApplication,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm};

{$R *.res}
{$R 'Win7UAC.res'}
// Manifest made by Wellington Torrejais da Silva
// https://gist.github.com/hotsoft-desenv2/5f722f1c44ed3abfc4f3d5d62ed644eb

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
