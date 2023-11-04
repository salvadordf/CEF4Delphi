program MiniBrowser;

{$I cef.inc}

uses
  Forms,
  uCEFApplication,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm};

{$R *.res}

{$SetPEFlags $20}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
