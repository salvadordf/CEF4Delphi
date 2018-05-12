program SimpleLazarusBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uSimpleLazarusBrowser
  { you can add units after this }
  ,uCEFApplication;

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;

  if GlobalCEFApp.StartMainProcess then
    begin
      RequireDerivedFormResource:=True;
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.

