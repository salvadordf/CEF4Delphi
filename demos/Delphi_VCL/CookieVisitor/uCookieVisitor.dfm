object CookieVisitorFrm: TCookieVisitorFrm
  Left = 0
  Top = 0
  Caption = 'Cookie Visitor'
  ClientHeight = 762
  ClientWidth = 884
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AddressBarPnl: TPanel
    Left = 0
    Top = 0
    Width = 884
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
    TabOrder = 0
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 843
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'https://www.google.com'
      ExplicitHeight = 21
    end
    object GoBtn: TButton
      Left = 848
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
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 884
    Height = 732
    Align = alClient
    TabOrder = 1
  end
  object Chromium1: TChromium
    OnCookiesDeleted = Chromium1CookiesDeleted
    OnCookiesVisited = Chromium1CookiesVisited
    OnCookieVisitorDestroyed = Chromium1CookieVisitorDestroyed
    OnCookieSet = Chromium1CookieSet
    OnBeforeContextMenu = Chromium1BeforeContextMenu
    OnContextMenuCommand = Chromium1ContextMenuCommand
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    OnCanSaveCookie = Chromium1CanSaveCookie
    Left = 32
    Top = 224
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 32
    Top = 280
  end
end
