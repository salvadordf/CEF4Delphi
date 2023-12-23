program SimpleBrowser2;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  { you can add units after this }
  uCEFApplication,
  uSimpleBrowser2 in 'uSimpleBrowser2.pas' {Form1};

{$R *.res}

{$IFDEF MSWINDOWS}
  // CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags $20}
{$ENDIF}

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
      RequireDerivedFormResource := True;
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.

