program URLRequest;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uURLRequest in 'uURLRequest.pas' {URLRequestFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TURLRequestFrm, URLRequestFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
