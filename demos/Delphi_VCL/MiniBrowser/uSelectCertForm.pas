unit uSelectCertForm;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, Vcl.ExtCtrls;
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  FileCtrl, ExtCtrls;
  {$ENDIF}

type
  TSelectCertForm = class(TForm)
    ButtonPnl: TPanel;
    SelectBtn: TButton;
    CancelBtn: TButton;
    CertificatesPnl: TPanel;
    CertificatesLb: TListBox;

    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure SelectBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CertificatesLbClick(Sender: TObject);

  private
    FCertificates : TStringList;
    FSelected     : integer;

  public
    property Certificates : TStringList read FCertificates;
    property Selected     : integer     read FSelected;
  end;

implementation

{$R *.dfm}

procedure TSelectCertForm.FormCreate(Sender: TObject);
begin
  FCertificates := TStringList.Create;
  FSelected     := -1;
end;

procedure TSelectCertForm.CertificatesLbClick(Sender: TObject);
begin
  SelectBtn.Enabled := (CertificatesLb.ItemIndex >= 0);
end;

procedure TSelectCertForm.FormDestroy(Sender: TObject);
begin
  if assigned(FCertificates) then
    FreeAndNil(FCertificates);
end;

procedure TSelectCertForm.FormShow(Sender: TObject);
begin
  CertificatesLb.Items.Clear;

  if assigned(FCertificates) and (FCertificates.Count > 0) then
    CertificatesLb.Items.AddStrings(FCertificates);
end;

procedure TSelectCertForm.CancelBtnClick(Sender: TObject);
begin
  FSelected := -1;
  close;
end;

procedure TSelectCertForm.SelectBtnClick(Sender: TObject);
begin
  FSelected := CertificatesLb.ItemIndex;
  close;
end;

end.
