program NetworkTrackerBrowser;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  uCEFApplication,
  uCEFConstants,
  uMainForm in 'uMainForm.pas' {MainForm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}
const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;

  // In case you want to use custom directories for the CEF3 binaries, cache and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
{
  GlobalCEFApp.FrameworkDirPath     := 'c:\cef';
  GlobalCEFApp.ResourcesDirPath     := 'c:\cef';
  GlobalCEFApp.LocalesDirPath       := 'c:\cef\locales';
  GlobalCEFApp.EnableGPU            := True;      // Enable hardware acceleration
  GlobalCEFApp.cache                := 'c:\cef\cache';
  GlobalCEFApp.UserDataPath         := 'c:\cef\User Data';
}

  // You *MUST* call GlobalCEFApp.StartMainProcess in a if..then clause
  // with the Application initialization inside the begin..end.
  // Read this https://www.briskbard.com/index.php?lang=en&pageid=cef
  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
