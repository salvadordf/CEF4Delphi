program MiniBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  uCEFApplication,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm};

{$R *.res}

begin
  CreateGlobalCEFApp;

  if StartMainProcess then
    begin
      // The LCL Widgetset must be initialized after the CEF initialization and
      // only in the browser process.
      CustomWidgetSetInitialization;

      RequireDerivedFormResource:=True;
      {$PUSH}{$WARN 5044 OFF}
      Application.MainFormOnTaskbar:=True;
      {$POP}
      Application.Initialize;
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.Run;

      CustomWidgetSetFinalization;
    end;

  DestroyGlobalCEFApp;

end.
