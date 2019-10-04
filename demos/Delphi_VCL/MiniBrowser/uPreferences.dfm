object PreferencesFrm: TPreferencesFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Preferences'
  ClientHeight = 388
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  Padding.Bottom = 10
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 10
    Top = 10
    Width = 408
    Height = 250
    Align = alTop
    Caption = ' Proxy '
    TabOrder = 0
    object ProxyTypeLbl: TLabel
      Left = 12
      Top = 27
      Width = 24
      Height = 13
      Caption = 'Type'
    end
    object ProxyServerLbl: TLabel
      Left = 12
      Top = 56
      Width = 32
      Height = 13
      Caption = 'Server'
    end
    object ProxyPortLbl: TLabel
      Left = 12
      Top = 83
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object ProxyUsernameLbl: TLabel
      Left = 12
      Top = 110
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object ProxyPasswordLbl: TLabel
      Left = 12
      Top = 137
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object ProxyScriptURLLbl: TLabel
      Left = 12
      Top = 164
      Width = 49
      Height = 13
      Caption = 'Script URL'
    end
    object ProxyByPassListLbl: TLabel
      Left = 12
      Top = 191
      Width = 50
      Height = 13
      Caption = 'ByPass list'
    end
    object MaxConnectionsPerProxyLbl: TLabel
      Left = 12
      Top = 218
      Width = 154
      Height = 13
      Caption = 'Maximum connections per proxy'
    end
    object ProxyTypeCbx: TComboBox
      Left = 108
      Top = 24
      Width = 292
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Direct'
      Items.Strings = (
        'Direct'
        'Autodetect'
        'System'
        'Fixed servers'
        'PAC script')
    end
    object ProxyServerEdt: TEdit
      Left = 184
      Top = 53
      Width = 216
      Height = 21
      TabOrder = 2
    end
    object ProxyPortEdt: TEdit
      Left = 108
      Top = 80
      Width = 292
      Height = 21
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 3
      Text = '80'
    end
    object ProxyUsernameEdt: TEdit
      Left = 108
      Top = 107
      Width = 292
      Height = 21
      TabOrder = 4
    end
    object ProxyPasswordEdt: TEdit
      Left = 108
      Top = 134
      Width = 292
      Height = 21
      PasswordChar = '*'
      TabOrder = 5
    end
    object ProxyScriptURLEdt: TEdit
      Left = 108
      Top = 161
      Width = 292
      Height = 21
      TabOrder = 6
    end
    object ProxyByPassListEdt: TEdit
      Left = 108
      Top = 188
      Width = 292
      Height = 21
      TabOrder = 7
    end
    object ProxySchemeCb: TComboBox
      Left = 108
      Top = 53
      Width = 70
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'HTTP'
      Items.Strings = (
        'HTTP'
        'SOCKS4'
        'SOCKS5')
    end
    object MaxConnectionsPerProxyEdt: TSpinEdit
      Left = 184
      Top = 215
      Width = 216
      Height = 22
      MaxValue = 99
      MinValue = 7
      TabOrder = 8
      Value = 32
    end
  end
  object GroupBox2: TGroupBox
    Left = 10
    Top = 260
    Width = 408
    Height = 84
    Align = alTop
    Caption = ' Custom header '
    TabOrder = 1
    object HeaderNameLbl: TLabel
      Left = 12
      Top = 26
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object HeaderValueLbl: TLabel
      Left = 12
      Top = 53
      Width = 26
      Height = 13
      Caption = 'Value'
    end
    object HeaderNameEdt: TEdit
      Left = 108
      Top = 23
      Width = 292
      Height = 21
      TabOrder = 0
    end
    object HeaderValueEdt: TEdit
      Left = 108
      Top = 50
      Width = 292
      Height = 21
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 10
    Top = 353
    Width = 408
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 30
    Padding.Right = 30
    TabOrder = 2
    ExplicitTop = 373
    object Button1: TButton
      Left = 30
      Top = 0
      Width = 120
      Height = 25
      Align = alLeft
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 258
      Top = 0
      Width = 120
      Height = 25
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 278
    end
  end
end
