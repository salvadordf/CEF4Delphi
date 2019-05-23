object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Initializing browser. Please wait...'
  ClientHeight = 573
  ClientWidth = 897
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
  object ButtonPnl: TPanel
    Left = 0
    Top = 0
    Width = 897
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Caption = 'ButtonPnl'
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    object NavButtonPnl: TPanel
      Left = 5
      Top = 5
      Width = 183
      Height = 25
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object BackBtn: TButton
        Left = 63
        Top = 0
        Width = 25
        Height = 25
        Caption = '3'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = BackBtnClick
      end
      object ForwardBtn: TButton
        Left = 93
        Top = 0
        Width = 25
        Height = 25
        Caption = '4'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = ForwardBtnClick
      end
      object ReloadBtn: TButton
        Left = 123
        Top = 0
        Width = 25
        Height = 25
        Caption = 'q'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = ReloadBtnClick
      end
      object StopBtn: TButton
        Left = 153
        Top = 0
        Width = 25
        Height = 25
        Caption = '='
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Webdings'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = StopBtnClick
      end
      object AddTabBtn: TButton
        Left = 1
        Top = 0
        Width = 25
        Height = 25
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = AddTabBtnClick
      end
      object RemoveTabBtn: TButton
        Left = 32
        Top = 0
        Width = 25
        Height = 25
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = RemoveTabBtnClick
      end
    end
    object ConfigPnl: TPanel
      Left = 860
      Top = 5
      Width = 32
      Height = 25
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object GoBtn: TButton
        Left = 6
        Top = 0
        Width = 25
        Height = 25
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
    object URLEditPnl: TPanel
      Left = 188
      Top = 5
      Width = 672
      Height = 25
      Align = alClient
      BevelOuter = bvNone
      Padding.Top = 2
      TabOrder = 0
      object URLCbx: TComboBox
        Left = 0
        Top = 2
        Width = 672
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
          'https://www.w3schools.com/html/html5_video.asp'
          'http://www.adobe.com/software/flash/about/'
          'http://isflashinstalled.com/'
          'chrome://version/'
          'http://html5test.com/'
          'https://www.w3schools.com/'
          'http://webglsamples.org/'
          'https://www.youtube.com'
          'https://html5demos.com/drag/'
          
            'https://developers.google.com/maps/documentation/javascript/exam' +
            'ples/streetview-embed?hl=fr')
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 35
    Width = 897
    Height = 538
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
  end
end
