program FMXTabbedBrowser;

uses
  System.StartUpCopy,
  FMX.Forms,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uBrowserTab in 'uBrowserTab.pas',
  uBrowserFrame in 'uBrowserFrame.pas' {BrowserFrame: TFrame};

{$R *.res}

{$IFDEF MSWINDOWS}
// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags $20}
{$ENDIF}

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
