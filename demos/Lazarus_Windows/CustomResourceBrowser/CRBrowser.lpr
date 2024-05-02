program CRBrowser;

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Forms, Interfaces,
  Windows,
  {$ENDIF }
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uCustomResourceHandler in 'uCustomResourceHandler.pas';

//{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

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
