program CEF4DelphiLoader;

uses
  Vcl.Forms,
  uCEF4DelphiLoader in 'uCEF4DelphiLoader.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
