program ResponseFilterBrowser;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uResponseFilterBrowser in 'uResponseFilterBrowser.pas' {ResponseFilterBrowserFrm};

{.$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
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
      Application.CreateForm(TResponseFilterBrowserFrm, ResponseFilterBrowserFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
