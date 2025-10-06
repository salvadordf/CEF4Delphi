unit uSimpleTextViewer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TSimpleTextViewerFrm = class(TForm)
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SimpleTextViewerFrm: TSimpleTextViewerFrm;

implementation

{$R *.lfm}

end.
