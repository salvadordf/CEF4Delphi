program MiniBrowser;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  WinApi.Windows,
  {$ELSE}
  Forms, Windows,
  LCLIntf, LCLType, LMessages, Interfaces,
  {$ENDIF }
  uCEFApplication,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm},
  uPreferences in 'uPreferences.pas' {PreferencesFrm},
  uSimpleTextViewer in 'uSimpleTextViewer.pas' {SimpleTextViewerFrm};

{.$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TMiniBrowserFrm, MiniBrowserFrm);
      Application.CreateForm(TPreferencesFrm, PreferencesFrm);
      Application.CreateForm(TSimpleTextViewerFrm, SimpleTextViewerFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
