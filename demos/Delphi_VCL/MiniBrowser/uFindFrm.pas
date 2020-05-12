unit uFindFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls;

type
  TFindFrm = class(TForm)
    FindTextEdt: TEdit;
    FindPrevBtn: TSpeedButton;
    FindNextBtn: TSpeedButton;
    procedure FindTextEdtChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FindPrevBtnClick(Sender: TObject);
    procedure FindNextBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSearchID : integer;
    FFirstNext : boolean;
  end;

var
  FindFrm: TFindFrm;

implementation

{$R *.dfm}

uses
  uMiniBrowser;


procedure TFindFrm.FindNextBtnClick(Sender: TObject);
begin
  MiniBrowserFrm.Chromium1.Find(FSearchID, FindTextEdt.Text, True, False, FFirstNext);
  FFirstNext := True;
end;

procedure TFindFrm.FindPrevBtnClick(Sender: TObject);
begin
  MiniBrowserFrm.Chromium1.Find(FSearchID, FindTextEdt.Text, False, False, FFirstNext);
  FFirstNext := True;
end;

procedure TFindFrm.FindTextEdtChange(Sender: TObject);
begin
  FindPrevBtn.Enabled := (length(FindTextEdt.Text) > 0);
  FindNextBtn.Enabled := FindPrevBtn.Enabled;
end;

procedure TFindFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MiniBrowserFrm.Chromium1.StopFinding(True);
end;

procedure TFindFrm.FormShow(Sender: TObject);
begin
  FindTextEdt.Text := '';
  FFirstNext := False;
  inc(FSearchID);
end;

end.
