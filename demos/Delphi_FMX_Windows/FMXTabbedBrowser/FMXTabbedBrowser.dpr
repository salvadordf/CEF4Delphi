program FMXTabbedBrowser;

uses
  System.StartUpCopy,
  FMX.Forms,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uBrowserTab in 'uBrowserTab.pas',
  uBrowserFrame in 'uBrowserFrame.pas' {BrowserFrame: TFrame};

{$R *.res}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;

      MainForm.Free;
    end;

  DestroyGlobalCEFApp;
end.
