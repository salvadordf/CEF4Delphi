object BrowserFrame: TBrowserFrame
  Left = 0
  Top = 0
  Width = 932
  Height = 670
  TabOrder = 0
  object NavControlPnl: TPanel
    Left = 0
    Top = 0
    Width = 932
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    TabOrder = 0
    object NavButtonPnl: TPanel
      Left = 0
      Top = 0
      Width = 123
      Height = 35
      Align = alLeft
      BevelOuter = bvNone
      Padding.Left = 5
      Padding.Top = 5
      Padding.Right = 5
      Padding.Bottom = 5
      TabOrder = 0
      object BackBtn: TButton
        Left = 5
        Top = 5
        Width = 25
        Height = 25
        Align = alLeft
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
        Left = 35
        Top = 5
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
        Left = 64
        Top = 5
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
        Left = 93
        Top = 5
        Width = 25
        Height = 25
        Align = alRight
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
      Left = 123
      Top = 0
      Width = 774
      Height = 35
      Align = alClient
      BevelOuter = bvNone
      Padding.Top = 7
      Padding.Bottom = 10
      TabOrder = 1
      object URLCbx: TComboBox
        Left = 0
        Top = 7
        Width = 774
        Height = 21
        Align = alClient
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
          'https://www.w3schools.com/html/html5_video.asp'
          'http://www.adobe.com/software/flash/about/'
          'http://isflashinstalled.com/'
          'https://helpx.adobe.com/flash-player.html'
          'https://www.ultrasounds.com/'
          'https://www.whatismybrowser.com/detect/is-flash-installed'
          'http://html5test.com/'
          
            'https://webrtc.github.io/samples/src/content/devices/input-outpu' +
            't/'
          'https://test.webrtc.org/'
          'https://www.w3schools.com/'
          'http://webglsamples.org/'
          'https://get.webgl.org/'
          'https://www.briskbard.com'
          'https://www.youtube.com'
          'https://html5demos.com/drag/'
          
            'https://developers.google.com/maps/documentation/javascript/exam' +
            'ples/streetview-embed?hl=fr'
          
            'https://www.w3schools.com/Tags/tryit.asp?filename=tryhtml_iframe' +
            '_name'
          
            'http://www-db.deis.unibo.it/courses/TW/DOCS/w3schools/html/tryit' +
            '.asp-filename=tryhtml5_html_manifest.html'
          'https://www.browserleaks.com/webrtc'
          'https://frames-per-second.appspot.com/'
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
      Left = 897
      Top = 0
      Width = 35
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      Padding.Left = 5
      Padding.Top = 5
      Padding.Right = 5
      Padding.Bottom = 5
      TabOrder = 2
      object GoBtn: TButton
        Left = 5
        Top = 5
        Width = 25
        Height = 25
        Align = alClient
        Caption = #9658
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = GoBtnClick
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 651
    Width = 932
    Height = 19
    Panels = <
      item
        Width = 500
      end>
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 35
    Width = 932
    Height = 616
    Align = alClient
    TabOrder = 2
  end
  object Chromium1: TChromium
    OnLoadError = Chromium1LoadError
    OnLoadingStateChange = Chromium1LoadingStateChange
    OnAddressChange = Chromium1AddressChange
    OnTitleChange = Chromium1TitleChange
    OnStatusMessage = Chromium1StatusMessage
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    OnOpenUrlFromTab = Chromium1OpenUrlFromTab
    Left = 40
    Top = 72
  end
end
