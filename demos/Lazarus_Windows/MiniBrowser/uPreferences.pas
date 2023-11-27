unit uPreferences;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
  {$ELSE}
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Spin;
  {$ENDIF}

type

  { TPreferencesFrm }

  TPreferencesFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    MaxConnectionsPerProxyLbl: TLabel;
    ProxyTypeCbx: TComboBox;
    ProxyTypeLbl: TLabel;
    ProxyServerLbl: TLabel;
    ProxyServerEdt: TEdit;
    ProxyPortLbl: TLabel;
    ProxyPortEdt: TEdit;
    ProxyUsernameLbl: TLabel;
    ProxyUsernameEdt: TEdit;
    ProxyPasswordLbl: TLabel;
    ProxyPasswordEdt: TEdit;
    ProxyScriptURLEdt: TEdit;
    ProxyScriptURLLbl: TLabel;
    ProxyByPassListEdt: TEdit;
    ProxyByPassListLbl: TLabel;
    GroupBox2: TGroupBox;
    HeaderNameEdt: TEdit;
    HeaderNameLbl: TLabel;
    HeaderValueEdt: TEdit;
    HeaderValueLbl: TLabel;
    ProxySchemeCb: TComboBox;
    MaxConnectionsPerProxyEdt: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PreferencesFrm: TPreferencesFrm;

implementation

{$R *.lfm}

end.
