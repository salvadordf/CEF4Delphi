program ResponseFilterBrowser;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  Interfaces,
  Windows,
  uCEFApplication,
  uResponseFilterBrowser in 'uResponseFilterBrowser.pas' {ResponseFilterBrowserFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TResponseFilterBrowserFrm, ResponseFilterBrowserFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
