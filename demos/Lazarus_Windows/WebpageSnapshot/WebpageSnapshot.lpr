program WebpageSnapshot;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  uCEFApplication,
  uCEFBrowserThread in 'uCEFBrowserThread.pas',
  uWebpageSnapshot in 'uWebpageSnapshot.pas' {WebpageSnapshotFrm};

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags $20}{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TWebpageSnapshotFrm, WebpageSnapshotFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
