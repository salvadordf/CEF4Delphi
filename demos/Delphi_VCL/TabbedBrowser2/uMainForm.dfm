object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Initializing. Please, wait...'
  ClientHeight = 703
  ClientWidth = 991
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object BrowserPageCtrl: TPageControl
    Left = 32
    Top = 0
    Width = 959
    Height = 703
    Align = alClient
    TabOrder = 0
    TabWidth = 150
  end
  object ButtonPnl: TPanel
    Left = 0
    Top = 0
    Width = 32
    Height = 703
    Align = alLeft
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 3
    Padding.Top = 3
    Padding.Right = 3
    Padding.Bottom = 3
    TabOrder = 1
    object AddTabBtn: TSpeedButton
      Left = 3
      Top = 3
      Width = 26
      Height = 26
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      OnClick = AddTabBtnClick
    end
    object RemoveTabBtn: TSpeedButton
      Left = 3
      Top = 32
      Width = 26
      Height = 26
      Caption = #8722
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Arial Black'
      Font.Style = []
      ParentFont = False
      OnClick = RemoveTabBtnClick
    end
  end
end
