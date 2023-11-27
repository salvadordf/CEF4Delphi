program MiniBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, uMiniBrowser
  { you can add units after this },
  uCEFApplication;

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      // The LCL Widgetset must be initialized after the CEF initialization and
      // only in the browser process.
      CustomWidgetSetInitialization;
      RequireDerivedFormResource:=True;
      Application.Scaled:=True;
      Application.Initialize;
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.Run;
      CustomWidgetSetFinalization;
    end;

  DestroyGlobalCEFApp;
end.

