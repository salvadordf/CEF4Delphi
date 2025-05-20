program ToolBoxBrowser2;

{$MODE Delphi}

uses
  Windows,
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
