 (*
 * Include the following files
 * ExternalPumpBrowser.app/Contents/Frameworks/ExternalPumpBrowser Helper.app/
 *   files from the demos/Lazarus_Mac/AppHelper project
 *   use create_mac_helper.sh
 *
 * ExternalPumpBrowser.app/Contents/Frameworks/Chromium Embedded Framework.framework
 *   files from Release folder in cef download
 *
 *)


program ExternalPumpBrowser;

{$mode objfpc}{$H+}
{$I ../../../source/cef.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  uExternalPumpBrowser, GlobalCefApplication, uCEFApplication, uCEFWorkScheduler
  { you can add units after this }
  ;

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      RequireDerivedFormResource := True;
      Application.Title  := 'External Pump Browser';
      Application.Scaled := True;
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;

      // The form needs to be destroyed *BEFORE* stopping the scheduler.	
      Form1.Free;

      GlobalCEFWorkScheduler.StopScheduler;
    end;

  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;
end.

