program SimpleServer;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  {$ELSE}
  Forms, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uSimpleServer in 'uSimpleServer.pas' {SimpleServerFrm};

{.$R *.res}

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;    
  GlobalCEFApp.SetCurrentDir       := True;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TSimpleServerFrm, SimpleServerFrm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
  GlobalCEFApp := nil;
end.
