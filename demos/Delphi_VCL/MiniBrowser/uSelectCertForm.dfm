object SelectCertForm: TSelectCertForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select a certificate'
  ClientHeight = 441
  ClientWidth = 612
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object ButtonPnl: TPanel
    Left = 0
    Top = 405
    Width = 612
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 50
    Padding.Right = 50
    Padding.Bottom = 10
    TabOrder = 0
    object SelectBtn: TButton
      Left = 50
      Top = 0
      Width = 150
      Height = 26
      Align = alLeft
      Caption = 'Select'
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = SelectBtnClick
    end
    object CancelBtn: TButton
      Left = 412
      Top = 0
      Width = 150
      Height = 26
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
  object CertificatesPnl: TPanel
    Left = 0
    Top = 0
    Width = 612
    Height = 405
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
    object CertificatesLb: TListBox
      Left = 10
      Top = 10
      Width = 592
      Height = 385
      Align = alClient
      ItemHeight = 15
      TabOrder = 0
      OnClick = CertificatesLbClick
    end
  end
end
