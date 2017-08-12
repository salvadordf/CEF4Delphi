object SchemeRegistrationBrowserFrm: TSchemeRegistrationBrowserFrm
  Left = 0
  Top = 0
  Caption = 'SchemeRegistrationBrowser'
  ClientHeight = 652
  ClientWidth = 980
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
  object AddressBarPnl: TPanel
    Left = 0
    Top = 0
    Width = 980
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    DoubleBuffered = True
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ParentDoubleBuffered = False
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 865
    object GoBtn: TButton
      Left = 944
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoBtnClick
      ExplicitLeft = 829
    end
    object AddressCbx: TComboBox
      Left = 5
      Top = 5
      Width = 939
      Height = 21
      Align = alClient
      ItemIndex = 1
      TabOrder = 1
      Text = 'hello://world'
      Items.Strings = (
        'https://www.google.com'
        'hello://world')
      ExplicitWidth = 824
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 980
    Height = 622
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 865
    ExplicitHeight = 528
  end
  object Chromium1: TChromium
    OnBeforeContextMenu = Chromium1BeforeContextMenu
    OnContextMenuCommand = Chromium1ContextMenuCommand
    OnAfterCreated = Chromium1AfterCreated
    Left = 16
    Top = 40
  end
end
