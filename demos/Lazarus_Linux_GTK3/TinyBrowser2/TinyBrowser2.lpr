program TinyBrowser2;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  uCEFApplication,
  uTinyBrowser2 in 'uTinyBrowser2.pas';

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      GlobalCEFApp.RunMessageLoop;
      DestroyTinyBrowser;
    end;

  DestroyGlobalCEFApp;
end.
