library custombrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Classes,
  { you can add units after this }
  uencapsulatedbrowser;

procedure InitializeCEF4Delphi; cdecl;
begin
  InitializeEncapsulatedBrowser;
end;

procedure FinalizeCEF4Delphi; cdecl;
begin
  FinalizeEncapsulatedBrowser;
end;

procedure TakeSnapshot; cdecl;
begin
  CaptureScreenshot('https://www.google.com');
end;

exports
  InitializeCEF4Delphi,
  FinalizeCEF4Delphi,
  TakeSnapshot;

begin
  //
end.

