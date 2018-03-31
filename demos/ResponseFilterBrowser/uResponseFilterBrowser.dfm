object ResponseFilterBrowserFrm: TResponseFilterBrowserFrm
  Left = 0
  Top = 0
  Caption = 'Initializing browser. Please wait...'
  ClientHeight = 624
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
    Top = 532
    Width = 1038
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 30
    ExplicitWidth = 505
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
    ShowCaption = False
    TabOrder = 0
    object AddressEdt: TEdit
      Left = 5
      Top = 5
      Width = 755
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'https://www.wikipedia.org'
      ExplicitHeight = 21
    end
    object Panel1: TPanel
      Left = 760
      Top = 5
      Width = 273
      Height = 20
      Align = alRight
      BevelOuter = bvNone
      Padding.Left = 5
      ShowCaption = False
      TabOrder = 1
      object Label1: TLabel
        Left = 54
        Top = 3
        Width = 84
        Height = 13
        Caption = 'Resource name : '
      end
      object GoBtn: TButton
        Left = 5
        Top = 0
        Width = 31
        Height = 20
        Margins.Left = 5
        Align = alLeft
        Caption = 'Go'
        TabOrder = 0
        OnClick = GoBtnClick
      end
      object RscNameEdt: TEdit
        Left = 144
        Top = 0
        Width = 129
        Height = 20
        Align = alRight
        TabOrder = 1
        Text = 'index-47f5f07682.js'
        ExplicitHeight = 21
      end
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 1038
    Height = 502
    Align = alClient
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 0
    Top = 535
    Width = 1038
    Height = 89
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
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
    OnGetResourceResponseFilter = Chromium1GetResourceResponseFilter
    OnResourceLoadComplete = Chromium1ResourceLoadComplete
    Left = 56
    Top = 160
  end
end
