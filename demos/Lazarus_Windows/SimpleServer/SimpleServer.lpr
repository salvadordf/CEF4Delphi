program SimpleServer;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  uCEFApplication,
  uSimpleServer in 'uSimpleServer.pas' {SimpleServerFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;    
  GlobalCEFApp.SetCurrentDir       := True;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TSimpleServerFrm, SimpleServerFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
