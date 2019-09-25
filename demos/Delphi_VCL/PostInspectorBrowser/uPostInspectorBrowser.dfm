object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Initializing browser. Please wait...'
  ClientHeight = 699
  ClientWidth = 1038
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 469
    Width = 1038
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 30
    ExplicitWidth = 554
  end
  object AddressPnl: TPanel
    Left = 0
    Top = 0
    Width = 1038
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    object GoBtn: TButton
      Left = 1002
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoBtnClick
    end
    object AddressCb: TComboBox
      Left = 5
      Top = 5
      Width = 997
      Height = 21
      Align = alClient
      ItemIndex = 0
      TabOrder = 1
      Text = 'https://tryphp.w3schools.com/showphp.php?filename=demo_form_post'
      Items.Strings = (
        'https://tryphp.w3schools.com/showphp.php?filename=demo_form_post')
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 1038
    Height = 439
    Align = alClient
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 0
    Top = 472
    Width = 1038
    Height = 227
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 56
    Top = 88
  end
  object Chromium1: TChromium
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    OnBeforeResourceLoad = Chromium1BeforeResourceLoad
    Left = 56
    Top = 152
  end
end
