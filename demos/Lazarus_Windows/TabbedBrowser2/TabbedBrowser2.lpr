program TabbedBrowser2;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uBrowserFrame in 'uBrowserFrame.pas' {BrowserFrame: TFrame},
  uBrowserTab in 'uBrowserTab.pas',
  uChildForm in 'uChildForm.pas' {ChildForm};

{.$R *.res}

{$IFDEF WIN32}
  // CEF needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  // If you don't add this flag the rederer process will crash when you try to load large images.
  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
