object GeolocationFrm: TGeolocationFrm
  Left = 0
  Top = 0
  Caption = 'Geolocation'
  ClientHeight = 691
  ClientWidth = 978
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 978
    Height = 642
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 395
  end
  object NavControlPnl: TPanel
    Left = 0
    Top = 0
    Width = 978
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 1
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 937
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.google.com'
      ExplicitHeight = 21
    end
    object GoBtn: TButton
      Left = 942
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 1
      OnClick = GoBtnClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 672
    Width = 978
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 200
      end
      item
        Width = 200
      end>
    ExplicitLeft = 584
    ExplicitTop = 592
    ExplicitWidth = 0
  end
  object Chromium1: TChromium
    OnAfterCreated = Chromium1AfterCreated
    Left = 32
    Top = 224
  end
end
