program librarybrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  // "Interfaces" is a custom unit used to initialize the LCL WidgetSet
  // We keep the same name to avoid a Lazarus warning.
  Interfaces, // this includes the LCL widgetset
  Classes, SysUtils, ucustombrowserloader;

  // This demo shows how to use a CEF browser in a Linux library.

  // CEF is initalized using a different executable for the subprocesses called
  // "librarybrowser_sp".

  // The CEF browser uses the off-screen rendering mode and this demo only takes
  // a snapshot when it finishes loading the default URL.
  // It creates a "snapshot.png" file in the same directory or shows an error message
  // in the console.

  // It's necessary to build librarybrowser_sp.lpr and custombrowser.lpr before
  // executing this project.

begin
  try
    try
      // This demo uses TCustomBrowserLoader to load "libcustombrowser.so"
      // dynamically
      GlobalCustomBrowseLoader := TCustomBrowserLoader.Create;

      // The LCL Widgetset must be initialized after the CEF initialization
      CustomWidgetSetInitialization;

      GlobalCustomBrowseLoader.TakeSnapshot;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    CustomWidgetSetFinalization;

    if assigned(GlobalCustomBrowseLoader) then
      FreeAndNil(GlobalCustomBrowseLoader);
  end;
end.

