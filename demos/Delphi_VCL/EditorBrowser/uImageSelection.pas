unit uImageSelection;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TImageSelectionFrm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    RemoteRb: TRadioButton;
    LocalRb: TRadioButton;
    URLLbl: TLabel;
    URLEdt: TEdit;
    FileLbl: TLabel;
    FileEdt: TEdit;
    FileBtn: TSpeedButton;
    OpenDialog1: TOpenDialog;
    procedure FileBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageSelectionFrm: TImageSelectionFrm;

implementation

{$R *.dfm}

procedure TImageSelectionFrm.FileBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'PNG Files (*.png)|*.PNG';

  if OpenDialog1.Execute then FileEdt.Text := OpenDialog1.FileName;
end;

end.
