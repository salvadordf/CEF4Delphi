object MiniBrowserFrm: TMiniBrowserFrm
  Left = 0
  Top = 0
  Caption = 'MiniBrowser'
  ClientHeight = 716
  ClientWidth = 1089
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
  object Splitter1: TSplitter
    Left = 1084
    Top = 41
    Width = 5
    Height = 656
    Align = alRight
    Visible = False
    ExplicitLeft = 0
    ExplicitTop = 657
    ExplicitHeight = 909
  end
  object NavControlPnl: TPanel
    Left = 0
    Top = 0
    Width = 1089
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    ShowCaption = False
    TabOrder = 0
    object NavButtonPnl: TPanel
      Left = 0
      Top = 0
      Width = 133
      Height = 41
      Align = alLeft
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 0
      object BackBtn: TButton
        Left = 8
        Top = 8
        Width = 25
        Height = 25
        Caption = '3'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = BackBtnClick
      end
      object ForwardBtn: TButton
        Left = 39
        Top = 8
        Width = 25
        Height = 25
        Caption = '4'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = ForwardBtnClick
      end
      object ReloadBtn: TButton
        Left = 70
        Top = 8
        Width = 25
        Height = 25
        Caption = 'q'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = ReloadBtnClick
      end
      object StopBtn: TButton
        Left = 101
        Top = 8
        Width = 25
        Height = 25
        Caption = '='
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = ReloadBtnClick
      end
    end
    object URLEditPnl: TPanel
      Left = 133
      Top = 0
      Width = 883
      Height = 41
      Align = alClient
      BevelOuter = bvNone
      Padding.Top = 9
      Padding.Bottom = 8
      ShowCaption = False
      TabOrder = 1
      object URLCbx: TComboBox
        Left = 0
        Top = 9
        Width = 883
        Height = 21
        Align = alClient
        TabOrder = 0
        Text = 'https://www.google.com'
        Items.Strings = (
          'https://www.google.com'
          'hello://world/'
          
            'https://www.whatismybrowser.com/detect/what-http-headers-is-my-b' +
            'rowser-sending'
          'https://www.w3schools.com/js/tryit.asp?filename=tryjs_win_close'
          'https://www.w3schools.com/html/html5_video.asp'
          'http://www.adobe.com/software/flash/about/'
          'chrome://version/'
          'http://html5test.com/'
          'https://www.w3schools.com/')
      end
    end
    object ConfigPnl: TPanel
      Left = 1016
      Top = 0
      Width = 73
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object ConfigBtn: TButton
        Left = 40
        Top = 8
        Width = 25
        Height = 25
        Caption = #8801
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = ConfigBtnClick
      end
      object GoBtn: TButton
        Left = 8
        Top = 8
        Width = 25
        Height = 25
        Caption = #9658
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = GoBtnClick
      end
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 41
    Width = 1084
    Height = 656
    Align = alClient
    TabOrder = 1
  end
  object DevTools: TCEFWindowParent
    Left = 1089
    Top = 41
    Width = 0
    Height = 656
    Align = alRight
    TabOrder = 2
    Visible = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 697
    Width = 1089
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Chromium1: TChromium
    OnTextResultAvailable = Chromium1TextResultAvailable
    OnProcessMessageReceived = Chromium1ProcessMessageReceived
    OnLoadingStateChange = Chromium1LoadingStateChange
    OnBeforeContextMenu = Chromium1BeforeContextMenu
    OnContextMenuCommand = Chromium1ContextMenuCommand
    OnAddressChange = Chromium1AddressChange
    OnTitleChange = Chromium1TitleChange
    OnStatusMessage = Chromium1StatusMessage
    OnAfterCreated = Chromium1AfterCreated
    Left = 424
    Top = 352
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 664
    Top = 104
    object DevTools1: TMenuItem
      Caption = 'DevTools'
      OnClick = DevTools1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Print1: TMenuItem
      Caption = 'Print'
      OnClick = Print1Click
    end
    object PrintinPDF1: TMenuItem
      Caption = 'Print to PDF'
      OnClick = PrintinPDF1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Zoom1: TMenuItem
      Caption = 'Zoom'
      object Inczoom1: TMenuItem
        Caption = 'Inc zoom'
        OnClick = Inczoom1Click
      end
      object Deczoom1: TMenuItem
        Caption = 'Dec zoom'
        OnClick = Deczoom1Click
      end
      object Resetzoom1: TMenuItem
        Caption = 'Reset zoom'
        OnClick = Resetzoom1Click
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Preferences1: TMenuItem
      Caption = 'Preferences...'
      OnClick = Preferences1Click
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 488
    Top = 232
  end
end
