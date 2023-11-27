program TinyBrowser;

{$MODE Delphi}

{$I ../../../source/cef.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  //LCLIntf, LCLType, LMessages,
  uTinyBrowser in 'uTinyBrowser.pas',
  uCEFApplicationCore;

{.$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      GlobalCEFApp.RunMessageLoop;
      DestroyTinyBrowser;
    end;

  DestroyGlobalCEFApp;
end.
