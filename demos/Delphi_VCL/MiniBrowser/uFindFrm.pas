unit uFindFrm;

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, Vcl.ExtCtrls, Vcl.Buttons;
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  FileCtrl, ExtCtrls, Vcl.Buttons, Buttons;
  {$ENDIF}

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
  MiniBrowserFrm.Chromium1.Find(FindTextEdt.Text, True, False, FFirstNext);
  FFirstNext := True;
end;

procedure TFindFrm.FindPrevBtnClick(Sender: TObject);
begin
  MiniBrowserFrm.Chromium1.Find(FindTextEdt.Text, False, False, FFirstNext);
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
end;

end.
