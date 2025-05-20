program CookieVisitor;

{$I ..\..\..\source\cef.inc}

uses
  Forms,
  Interfaces,
  Windows,
  uCEFApplication,
  uCookieVisitor in 'uCookieVisitor.pas' {CookieVisitorFrm},
  uSimpleTextViewer in 'uSimpleTextViewer.pas' {SimpleTextViewerFrm};

{$IFDEF WIN32}
  // CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
  {$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}
{$ENDIF}

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TCookieVisitorFrm, CookieVisitorFrm);
      Application.CreateForm(TSimpleTextViewerFrm, SimpleTextViewerFrm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
