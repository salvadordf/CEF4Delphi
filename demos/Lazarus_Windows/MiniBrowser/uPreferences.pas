unit uPreferences;

{$MODE Delphi}

{$I ..\..\..\source\cef.inc}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Spin;

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
