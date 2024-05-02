program URLRequest;

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
  uURLRequest in 'uURLRequest.pas' {URLRequestFrm};

{.$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
// If you don't add this flag the rederer process will crash when you try to load large images.
{$SetPEFlags $20}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TURLRequestFrm, URLRequestFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
