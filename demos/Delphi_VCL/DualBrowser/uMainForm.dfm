object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Dual Browser'
  ClientHeight = 662
  ClientWidth = 1075
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 5
  Padding.Right = 5
  Padding.Bottom = 5
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 542
    Top = 0
    Width = 5
    Height = 657
    ExplicitLeft = 537
    ExplicitHeight = 662
  end
  object CEFPnl: TPanel
    Left = 5
    Top = 0
    Width = 537
    Height = 657
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object CEFAddressPnl: TPanel
      Left = 0
      Top = 0
      Width = 537
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      Padding.Top = 5
      Padding.Bottom = 5
      TabOrder = 0
      object CEFAddressEdt: TEdit
        Left = 0
        Top = 5
        Width = 506
        Height = 20
        Margins.Right = 5
        Align = alClient
        TabOrder = 0
        Text = 'http://www.google.com'
        ExplicitHeight = 21
      end
      object CEFGoBtn: TButton
        Left = 506
        Top = 5
        Width = 31
        Height = 20
        Margins.Left = 5
        Align = alRight
        Caption = 'Go'
        TabOrder = 1
        OnClick = CEFGoBtnClick
      end
    end
    object CEFWindowParent1: TCEFWindowParent
      Left = 0
      Top = 30
      Width = 537
      Height = 627
      Align = alClient
      Color = clWhite
      TabOrder = 1
    end
  end
  object WVPnl: TPanel
    Left = 547
    Top = 0
    Width = 523
    Height = 657
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object WVAddressPnl: TPanel
      Left = 0
      Top = 0
      Width = 523
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      Padding.Top = 5
      Padding.Bottom = 5
      TabOrder = 0
      object WVAddressEdt: TEdit
        Left = 0
        Top = 5
        Width = 492
        Height = 20
        Margins.Right = 5
        Align = alClient
        TabOrder = 0
        Text = 'http://www.bing.com'
        ExplicitHeight = 21
      end
      object WVGoBtn: TButton
        Left = 492
        Top = 5
        Width = 31
        Height = 20
        Margins.Left = 5
        Align = alRight
        Caption = 'Go'
        TabOrder = 1
        OnClick = WVGoBtnClick
      end
    end
    object WVWindowParent1: TWVWindowParent
      Left = 0
      Top = 30
      Width = 523
      Height = 627
      Align = alClient
      Color = clWhite
      TabStop = True
      TabOrder = 1
      Browser = WVBrowser1
    end
  end
  object Chromium1: TChromium
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnOpenUrlFromTab = Chromium1OpenUrlFromTab
    Left = 224
    Top = 256
  end
  object WVBrowser1: TWVBrowser
    TargetCompatibleBrowserVersion = '95.0.1020.44'
    AllowSingleSignOnUsingOSPrimaryAccount = False
    OnAfterCreated = WVBrowser1AfterCreated
    Left = 808
    Top = 264
  end
end
