program SimpleBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm
  { you can add units after this },
  uCEFApplication;

{$R *.res}

begin
  CreateGlobalCEFApp;

  if StartMainProcess then
    begin
      // The LCL Widgetset must be initialized after the CEF initialization and
      // only in the browser process.
      CustomWidgetSetInitialization;

      RequireDerivedFormResource:=True;
      Application.Scaled:=True;
      {$PUSH}{$WARN 5044 OFF}
      Application.MainFormOnTaskbar:=True;
      {$POP}
      Application.Initialize;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;

      CustomWidgetSetFinalization;
    end;

  DestroyGlobalCEFApp;
end.

