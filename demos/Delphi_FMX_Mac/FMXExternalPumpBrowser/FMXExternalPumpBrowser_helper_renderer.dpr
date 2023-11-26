program FMXExternalPumpBrowser_helper_renderer;

{$I cef.inc}

uses
  {$IFDEF DELPHI17_UP}
  System.StartUpCopy,
  {$ENDIF}
  FMX.Forms,
  uCEFLoader in 'uCEFLoader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.

