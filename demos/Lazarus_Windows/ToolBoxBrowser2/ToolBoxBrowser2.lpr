program ToolBoxBrowser2;

{$MODE Delphi}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Windows,
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm};

{.$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

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
