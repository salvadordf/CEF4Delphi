unit uSimpleTextViewer;

{$MODE Delphi}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
  {$ELSE}
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;
  {$ENDIF}


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
