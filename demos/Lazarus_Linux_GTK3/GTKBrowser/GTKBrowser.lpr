program GTKBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  uCEFApplication, umainwindow;

begin
  CreateGlobalCEFApp;

  if StartMainProcess then
    begin
      MainWindow := TMainWindow.Create;
      MainWindow.Show;
      MainWindow.Run;
      MainWindow.Free;
    end;

  DestroyGlobalCEFApp;
end.

