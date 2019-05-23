object SimpleExternalPumpBrowserFrm: TSimpleExternalPumpBrowserFrm
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
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
    TabOrder = 0
    object GoBtn: TButton
      Left = 1002
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoBtnClick
    end
    object URLCbx: TComboBox
      Left = 5
      Top = 5
      Width = 997
      Height = 21
      Align = alClient
      ItemIndex = 0
      TabOrder = 1
      Text = 'https://www.google.com'
      Items.Strings = (
        'https://www.google.com'
        
          'https://www.whatismybrowser.com/detect/what-http-headers-is-my-b' +
          'rowser-sending'
        'https://www.w3schools.com/js/tryit.asp?filename=tryjs_win_close'
        'https://www.w3schools.com/html/html5_video.asp'
        'http://www.adobe.com/software/flash/about/'
        'http://isflashinstalled.com/'
        'chrome://version/'
        'http://html5test.com/'
        'https://www.w3schools.com/'
        'http://webglsamples.org/'
        'https://get.webgl.org/'
        'https://www.youtube.com'
        'https://html5demos.com/drag/'
        
          'https://developers.google.com/maps/documentation/javascript/exam' +
          'ples/streetview-embed?hl=fr'
        
          'https://www.w3schools.com/Tags/tryit.asp?filename=tryhtml_iframe' +
          '_name'
        'https://www.browserleaks.com/webrtc'
        'https://frames-per-second.appspot.com/')
    end
  end
  object ChromiumWindow1: TChromiumWindow
    Left = 0
    Top = 30
    Width = 1038
    Height = 594
    Align = alClient
    TabOrder = 1
    OnClose = ChromiumWindow1Close
    OnBeforeClose = ChromiumWindow1BeforeClose
    OnAfterCreated = ChromiumWindow1AfterCreated
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 56
    Top = 88
  end
end
