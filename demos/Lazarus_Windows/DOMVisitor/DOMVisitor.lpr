program DOMVisitor;

{$I ..\..\..\source\cef.inc}

uses
  Forms, Interfaces,
  Windows,
  uCEFApplication,
  uDOMVisitor in 'uDOMVisitor.pas' {DOMVisitorFrm};

//{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

{$R *.res}

begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      //ReportMemoryLeaksOnShutdown := True;

      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TDOMVisitorFrm, DOMVisitorFrm);
      Application.Run;
    end;

  // This is not really necessary to fix the bug #89 but if you free GlobalCEFApp in a different unit
  // then you can call 'FreeAndNil' without adding SysUtils to this DPR.
  DestroyGlobalCEFApp;
end.
