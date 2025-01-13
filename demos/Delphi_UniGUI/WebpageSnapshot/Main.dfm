object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 645
  ClientWidth = 800
  Caption = 'Webpage Snapshot UniGUI'
  OnShow = UniFormShow
  BorderStyle = bsSingle
  OldCreateOrder = False
  OnClose = UniFormClose
  BorderIcons = [biSystemMenu]
  MonitoredKeys.Keys = <>
  OnCreate = UniFormCreate
  TextHeight = 15
  object UniPanel1: TUniPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 23
    Hint = ''
    Align = alTop
    TabOrder = 0
    BorderStyle = ubsNone
    Caption = ''
    object AddressCb: TUniComboBox
      Left = 0
      Top = 0
      Width = 725
      Height = 23
      Hint = ''
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
        'https://codepen.io/udaymanvar/pen/MWaePBY'
        
          'https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/acc' +
          'ept'
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
      ItemIndex = 0
      Align = alClient
      TabOrder = 1
      IconItems = <>
    end
    object GoBtn: TUniButton
      Left = 725
      Top = 0
      Width = 75
      Height = 23
      Hint = ''
      Caption = 'Go'
      Align = alRight
      TabOrder = 2
      OnClick = GoBtnClick
    end
  end
  object UniStatusBar1: TUniStatusBar
    Left = 0
    Top = 623
    Width = 800
    Hint = ''
    Panels = <
      item
        Width = 600
      end>
    SizeGrip = False
    Align = alBottom
    ParentColor = False
  end
  object UniMemo1: TUniMemo
    Left = 0
    Top = 534
    Width = 800
    Height = 89
    Hint = ''
    Align = alBottom
    ReadOnly = True
    TabOrder = 2
  end
  object UniCanvas1: TUniCanvas
    Left = 0
    Top = 23
    Width = 800
    Height = 511
    Hint = ''
    Align = alClient
    ExplicitLeft = 200
    ExplicitTop = 104
    ExplicitWidth = 320
    ExplicitHeight = 320
  end
  object UniTimer1: TUniTimer
    Interval = 100
    ClientEvent.Strings = (
      'function(sender)'
      '{'
      ' '
      '}')
    OnTimer = UniTimer1Timer
    Left = 496
    Top = 376
  end
end
