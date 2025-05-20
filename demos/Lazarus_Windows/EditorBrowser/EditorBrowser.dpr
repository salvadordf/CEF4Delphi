program EditorBrowser;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  Windows, 
  Interfaces,
  uCEFApplication,
  uEditorBrowser in 'uEditorBrowser.pas' {Form1};

{.$R *.res}

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
