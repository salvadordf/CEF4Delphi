program PopupBrowser2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  // "Interfaces" is a custom unit used to initialize the LCL WidgetSet
  // We keep the same name to avoid a Lazarus warning.
  Interfaces, // this includes the LCL widgetset
  Forms,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uChildForm in 'uChildForm.pas' {ChildForm};

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
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
      CustomWidgetSetFinalization;
    end;

  DestroyGlobalCEFApp;
end.
