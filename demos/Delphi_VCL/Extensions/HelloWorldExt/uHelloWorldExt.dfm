object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Initializing browser. Please wait...'
  ClientHeight = 624
  ClientWidth = 1088
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
  object AddressPnl: TPanel
    Left = 0
    Top = 0
    Width = 1088
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    object AddressEdt: TEdit
      Left = 5
      Top = 5
      Width = 1047
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.google.com'
      ExplicitHeight = 21
    end
    object GoBtn: TButton
      Left = 1052
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
    Width = 824
    Height = 594
    Align = alClient
    TabOrder = 1
  end
  object ExtensionPnl: TPanel
    Left = 824
    Top = 30
    Width = 264
    Height = 594
    Align = alRight
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 2
    object ExtensionMem: TMemo
      Left = 5
      Top = 97
      Width = 254
      Height = 222
      Align = alTop
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 5
      Top = 5
      Width = 254
      Height = 92
      Align = alTop
      BevelOuter = bvNone
      Padding.Bottom = 5
      TabOrder = 1
      object LoadExtensionBtn: TButton
        Left = 0
        Top = 0
        Width = 254
        Height = 25
        Align = alTop
        Caption = '1. Load extension'
        TabOrder = 0
        OnClick = LoadExtensionBtnClick
      end
      object UnloadExtensionBtn: TButton
        Left = 0
        Top = 62
        Width = 254
        Height = 25
        Align = alBottom
        Caption = '3. Unload extension'
        Enabled = False
        TabOrder = 2
        OnClick = UnloadExtensionBtnClick
      end
      object LoadPopupPageBtn: TButton
        Left = 0
        Top = 31
        Width = 254
        Height = 25
        Caption = '2. Load popup page'
        TabOrder = 1
        OnClick = LoadPopupPageBtnClick
      end
    end
    object CEFWindowParent2: TCEFWindowParent
      Left = 5
      Top = 319
      Width = 254
      Height = 270
      Align = alClient
      TabOrder = 2
    end
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
    OnOpenUrlFromTab = Chromium1OpenUrlFromTab
    Left = 56
    Top = 152
  end
  object ExtensionChr: TChromium
    OnLoadEnd = ExtensionChrLoadEnd
    OnLoadError = ExtensionChrLoadError
    OnBeforeClose = ExtensionChrBeforeClose
    OnClose = ExtensionChrClose
    OnExtensionLoadFailed = ExtensionChrExtensionLoadFailed
    OnExtensionLoaded = ExtensionChrExtensionLoaded
    OnExtensionUnloaded = ExtensionChrExtensionUnloaded
    Left = 944
    Top = 392
  end
end
