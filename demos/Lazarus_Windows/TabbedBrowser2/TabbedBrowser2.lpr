program TabbedBrowser2;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uBrowserFrame in 'uBrowserFrame.pas' {BrowserFrame: TFrame},
  uBrowserTab in 'uBrowserTab.pas',
  uChildForm in 'uChildForm.pas' {ChildForm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
