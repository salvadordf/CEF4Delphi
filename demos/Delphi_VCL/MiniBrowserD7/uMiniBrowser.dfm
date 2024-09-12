object MiniBrowserFrm: TMiniBrowserFrm
  Left = 528
  Top = 154
  Width = 1196
  Height = 750
  Caption = 'MiniBrowser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object NavControlPnl: TPanel
    Left = 0
    Top = 0
    Width = 1180
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    TabOrder = 1
    object NavButtonPnl: TPanel
      Left = 0
      Top = 0
      Width = 133
      Height = 41
      Align = alLeft
      BevelOuter = bvNone
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
        OnClick = StopBtnClick
      end
    end
    object URLEditPnl: TPanel
      Left = 133
      Top = 0
      Width = 974
      Height = 41
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object URLCbx: TComboBox
        Left = 0
        Top = 9
        Width = 974
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'https://www.google.com'
        Items.Strings = (
          'https://www.google.com'
          
            'https://www.whatismybrowser.com/detect/what-http-headers-is-my-b' +
            'rowser-sending'
          'https://www.w3schools.com/js/tryit.asp?filename=tryjs_win_close'
          'https://www.w3schools.com/js/tryit.asp?filename=tryjs_alert'
          'https://www.w3schools.com/js/tryit.asp?filename=tryjs_loc_assign'
          
            'https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_styl' +
            'e_backgroundcolor'
          
            'https://www.w3schools.com/Tags/tryit.asp?filename=tryhtml_iframe' +
            '_name'
          
            'https://www.w3schools.com/tags/tryit.asp?filename=tryhtml5_input' +
            '_type_file'
          
            'https://www.w3schools.com/jsref/tryit.asp?filename=tryjsref_stat' +
            'e_throw_error'
          'https://www.htmlquick.com/es/reference/tags/input-file.html'
          
            'https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/' +
            'file'
          
            'https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElemen' +
            't/webkitdirectory'
          'https://www.w3schools.com/html/html5_video.asp'
          'http://html5test.com/'
          
            'https://webrtc.github.io/samples/src/content/devices/input-outpu' +
            't/'
          'https://test.webrtc.org/'
          'https://www.browserleaks.com/webrtc'
          'https://shaka-player-demo.appspot.com/demo/'
          'http://webglsamples.org/'
          'https://get.webgl.org/'
          'https://www.briskbard.com'
          'https://www.youtube.com'
          'https://html5demos.com/drag/'
          'https://frames-per-second.appspot.com/'
          
            'https://www.sede.fnmt.gob.es/certificados/persona-fisica/verific' +
            'ar-estado'
          'https://www.kirupa.com/html5/accessing_your_webcam_in_html5.htm'
          'https://www.xdumaine.com/enumerateDevices/test/'
          
            'https://dagrs.berkeley.edu/sites/default/files/2020-01/sample.pd' +
            'f'
          'chrome://version/'
          'chrome://net-internals/'
          'chrome://tracing/'
          'chrome://appcache-internals/'
          'chrome://blob-internals/'
          'chrome://view-http-cache/'
          'chrome://credits/'
          'chrome://histograms/'
          'chrome://media-internals/'
          'chrome://kill'
          'chrome://crash'
          'chrome://hang'
          'chrome://shorthang'
          'chrome://gpuclean'
          'chrome://gpucrash'
          'chrome://gpuhang'
          'chrome://extensions-support'
          'chrome://process-internals')
      end
    end
    object ConfigPnl: TPanel
      Left = 1107
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
        Caption = '='
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
        Caption = '>'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Arial Black'
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
    Width = 1180
    Height = 651
    Align = alClient
    TabStop = True
    TabOrder = 0
    DoubleBuffered = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 692
    Width = 1180
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 500
      end
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object Chromium1: TChromium
    OnLoadEnd = Chromium1LoadEnd
    OnLoadError = Chromium1LoadError
    OnBeforeContextMenu = Chromium1BeforeContextMenu
    OnContextMenuCommand = Chromium1ContextMenuCommand
    OnAddressChange = Chromium1AddressChange
    OnTitleChange = Chromium1TitleChange
    OnStatusMessage = Chromium1StatusMessage
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    OnChromeCommand = Chromium1ChromeCommand
    Left = 32
    Top = 224
  end
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 168
    object Print1: TMenuItem
      Caption = 'Print'
      OnClick = Print1Click
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
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 32
    Top = 104
  end
end
