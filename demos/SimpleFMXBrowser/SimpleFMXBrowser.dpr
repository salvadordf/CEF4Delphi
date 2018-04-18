program SimpleFMXBrowser;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF }
  uCEFApplication,
  uSimpleFMXBrowser in 'uSimpleFMXBrowser.pas' {SimpleFMXBrowserFrm},
  uFMXApplicationService in 'uFMXApplicationService.pas';

{$R *.res}

{$IFDEF MSWINDOWS}
// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

begin
  GlobalCEFApp := TCefApplication.Create;

  // In case you want to use custom directories for the CEF3 binaries, cache, cookies and user data.
  // If you don't set a cache directory the browser will use in-memory cache.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.EnableGPU            := True;      // Enable hardware acceleration
  GlobalCEFApp.DisableGPUCache      := True;      // Disable the creation of a 'GPUCache' directory in the hard drive.
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.cookies              := 'cef\cookies';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  // You *MUST* call GlobalCEFApp.StartMainProcess in a if..then clause
  // with the Application initialization inside the begin..end.
  // Read this https://www.briskbard.com/index.php?lang=en&pageid=cef
  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TSimpleFMXBrowserFrm, SimpleFMXBrowserFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
