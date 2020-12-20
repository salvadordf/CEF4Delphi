object WebpageSnapshotFrm: TWebpageSnapshotFrm
  Left = 0
  Top = 0
  Caption = 'Web page snapshot'
  ClientHeight = 737
  ClientWidth = 1032
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 25
    Width = 1032
    Height = 693
    Align = alClient
    AutoSize = True
    Center = True
    Proportional = True
    ExplicitLeft = 104
    ExplicitTop = 112
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 718
    Width = 1032
    Height = 19
    Panels = <
      item
        Width = 1000
      end>
  end
  object NavigationPnl: TPanel
    Left = 0
    Top = 0
    Width = 1032
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    TabOrder = 1
    object GoBtn: TButton
      Left = 955
      Top = 2
      Width = 75
      Height = 21
      Align = alRight
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoBtnClick
    end
    object AddressEdt: TEdit
      Left = 2
      Top = 2
      Width = 953
      Height = 21
      Align = alClient
      TabOrder = 1
      Text = 'https://www.google.com'
    end
  end
end
