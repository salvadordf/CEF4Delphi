program JSEval;

{$MODE Delphi}

{$I ..\..\..\..\source\cef.inc}

uses
  Forms, Windows,
  LCLIntf, LCLType, LMessages, Interfaces,
  uCEFApplication,
  uJSEval in 'uJSEval.pas' {JSEvalFrm},
  uSimpleTextViewer in 'uSimpleTextViewer.pas' {SimpleTextViewerFrm};

{.$R *.res}

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

{$R *.res}

begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      {$IFDEF DELPHI11_UP}
      Application.MainFormOnTaskbar := True;
      {$ENDIF}
      Application.CreateForm(TJSEvalFrm, JSEvalFrm);
      Application.CreateForm(TSimpleTextViewerFrm, SimpleTextViewerFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
