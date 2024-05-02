program SimpleBrowser_D7;

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  uCEFApplication,
  uSimpleBrowser in 'uSimpleBrowser.pas' {Form1};


{$R *.res}
{$R 'Win7UAC.res'}
// Manifest made by Wellington Torrejais da Silva
// https://gist.github.com/hotsoft-desenv2/5f722f1c44ed3abfc4f3d5d62ed644eb

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags $20}

begin
  GlobalCEFApp := TCefApplication.Create;

  // In case you want to use custom directories for the CEF3 binaries, cache and user data.

{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
