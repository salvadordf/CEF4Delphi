program MiniBrowser;

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows,
  Vcl.Forms,
  {$ELSE}
  Forms,
  Windows,
  {$ENDIF }
  uCEFApplication,
  uMiniBrowser in 'uMiniBrowser.pas' {MiniBrowserFrm},
  uPreferences in 'uPreferences.pas' {PreferencesFrm},
  uSimpleTextViewer in 'uSimpleTextViewer.pas' {SimpleTextViewerFrm},
  uFindFrm in 'uFindFrm.pas' {FindFrm},
  uDirectorySelector in 'uDirectorySelector.pas' {DirectorySelectorFrm},
  uSelectCertForm in 'uSelectCertForm.pas' {SelectCertForm};

{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

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
      Application.CreateForm(TFindFrm, FindFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
