object ResponseFilterBrowserFrm: TResponseFilterBrowserFrm
  Left = 0
  Top = 0
  Caption = 'Initializing browser. Please wait...'
  ClientHeight = 710
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
    Top = 477
    Width = 1038
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 30
    ExplicitWidth = 450
  end
  object AddressPnl: TPanel
    Left = 0
    Top = 0
    Width = 1038
    Height = 49
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
      Width = 724
      Height = 39
      Margins.Right = 5
      Align = alClient
      ReadOnly = True
      TabOrder = 0
      Text = 'https://www.briskbard.com/'
      ExplicitHeight = 21
    end
    object Panel1: TPanel
      Left = 729
      Top = 5
      Width = 304
      Height = 39
      Align = alRight
      BevelOuter = bvNone
      Padding.Left = 5
      TabOrder = 1
      object GoBtn: TButton
        Left = 5
        Top = 0
        Width = 63
        Height = 39
        Margins.Left = 5
        Align = alLeft
        Caption = 'Go'
        TabOrder = 0
        OnClick = GoBtnClick
      end
      object RscNameEdt: TEdit
        Left = 176
        Top = 0
        Width = 129
        Height = 21
        TabOrder = 1
        Text = 'script.js'
      end
      object CopyScriptBtn: TRadioButton
        Left = 74
        Top = 2
        Width = 96
        Height = 17
        Caption = 'Copy script :'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object ReplaceLogoBtn: TRadioButton
        Left = 74
        Top = 21
        Width = 96
        Height = 17
        Caption = 'Replace logo'
        TabOrder = 3
      end
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 49
    Width = 1038
    Height = 428
    Align = alClient
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 480
    Width = 1038
    Height = 230
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 1038
      Height = 211
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object StatusBar1: TStatusBar
      Left = 0
      Top = 211
      Width = 1038
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
        end
        item
          Width = 200
        end>
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
    OnLoadStart = Chromium1LoadStart
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    OnBeforeResourceLoad = Chromium1BeforeResourceLoad
    OnResourceResponse = Chromium1ResourceResponse
    OnGetResourceResponseFilter = Chromium1GetResourceResponseFilter
    OnResourceLoadComplete = Chromium1ResourceLoadComplete
    Left = 56
    Top = 160
  end
end
