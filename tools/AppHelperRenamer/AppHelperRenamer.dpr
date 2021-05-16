program AppHelperRenamer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uAppHelperRenamer in 'uAppHelperRenamer.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
