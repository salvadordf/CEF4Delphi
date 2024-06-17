program SimpleBrowser2_D7;

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  uCEFApplication,
  uSimpleBrowser2 in 'uSimpleBrowser2.pas' {Form1};


{$R *.res}
{$R 'Win7UAC.res'}
// Manifest made by Wellington Torrejais da Silva
// https://gist.github.com/hotsoft-desenv2/5f722f1c44ed3abfc4f3d5d62ed644eb

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;
	
  CreateGlobalCEFApp;
end.
