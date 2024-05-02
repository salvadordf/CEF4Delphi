program PostInspectorBrowser;

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
  uPostInspectorBrowser in 'uPostInspectorBrowser.pas' {Form1};

//{$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
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
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
