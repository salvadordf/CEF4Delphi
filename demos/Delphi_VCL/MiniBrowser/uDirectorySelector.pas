unit uDirectorySelector;

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
  TDirectorySelectorFrm = class(TForm)
    Panel1: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    DriveComboBox1: TDriveComboBox;
    Panel3: TPanel;
    DirectoryListBox1: TDirectoryListBox;
  private
    procedure SetSelectedDir(const aValue : string);
    function  GetSelectedDir : string;

  public
    property SelectedDir : string read GetSelectedDir write SetSelectedDir;
  end;

implementation

{$R *.dfm}

procedure TDirectorySelectorFrm.SetSelectedDir(const aValue : string);
begin
  DirectoryListBox1.Directory := aValue;
end;

function TDirectorySelectorFrm.GetSelectedDir : string;
begin
  Result := DirectoryListBox1.Directory;
end;

end.
