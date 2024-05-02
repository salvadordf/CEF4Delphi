program SimpleBrowser;

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  uCEFApplication,
  uSimpleBrowser in 'uSimpleBrowser.pas' {Form1};

{$IFDEF WIN32}
  // CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  // If you don't add this flag the rederer process will crash when you try to load large images.
  {$SetPEFlags $20}
{$ENDIF}

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;  
  GlobalCEFApp.SetCurrentDir       := True;

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
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;           

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
