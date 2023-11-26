program EditorBrowser;

{$I ..\..\..\source\cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  Vcl.Forms,
  {$ELSE}
  Forms,
  {$ENDIF }
  uCEFApplication,
  uEditorBrowser in 'uEditorBrowser.pas' {EditorBrowserFrm},
  uImageSelection in 'uImageSelection.pas' {ImageSelectionFrm};

{$R *.res}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

// CEF needs to set the LARGEADDRESSAWARE ($20) flag which allows 32-bit processes to use up to 3GB of RAM.
{$IFDEF WIN32}{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}{$ENDIF}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TEditorBrowserFrm, EditorBrowserFrm);
      Application.CreateForm(TImageSelectionFrm, ImageSelectionFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
