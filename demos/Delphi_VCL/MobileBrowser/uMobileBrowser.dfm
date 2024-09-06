object Form1: TForm1
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
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 741
    Top = 0
    Height = 624
    Align = alRight
    MinSize = 220
    ExplicitLeft = 728
    ExplicitTop = 336
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 744
    Top = 0
    Width = 294
    Height = 624
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object LogMem: TMemo
      Left = 0
      Top = 406
      Width = 294
      Height = 218
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 3
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 294
      Height = 80
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 0
      object CanEmulateBtn: TButton
        Left = 10
        Top = 10
        Width = 274
        Height = 25
        Align = alTop
        Caption = 'Can emulate'
        TabOrder = 0
        OnClick = CanEmulateBtnClick
      end
      object ClearDeviceMetricsOverrideBtn: TButton
        Left = 10
        Top = 45
        Width = 274
        Height = 25
        Align = alBottom
        Caption = 'Clear device metrics override'
        TabOrder = 1
        OnClick = ClearDeviceMetricsOverrideBtnClick
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 80
      Width = 294
      Height = 121
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 1
      object GroupBox1: TGroupBox
        Left = 10
        Top = 0
        Width = 274
        Height = 86
        Align = alTop
        Caption = ' User agent '
        Padding.Left = 10
        Padding.Top = 5
        Padding.Right = 10
        Padding.Bottom = 10
        TabOrder = 0
        object UserAgentCb: TComboBox
          Left = 12
          Top = 20
          Width = 250
          Height = 21
          Align = alTop
          ItemIndex = 0
          TabOrder = 0
          Text = 
            'Mozilla/5.0 (Linux; Android 11; M2102K1G) AppleWebKit/537.36 (KH' +
            'TML, like Gecko) Chrome/91.0.4472.101 Mobile Safari/537.36'
          Items.Strings = (
            
              'Mozilla/5.0 (Linux; Android 11; M2102K1G) AppleWebKit/537.36 (KH' +
              'TML, like Gecko) Chrome/91.0.4472.101 Mobile Safari/537.36'
            
              'Mozilla/5.0 (iPhone; CPU iPhone OS 10_3_1 like Mac OS X) AppleWe' +
              'bKit/603.1.30 (KHTML, like Gecko) Version/10.0 Mobile/14E304 Saf' +
              'ari/602.1'
            
              'Mozilla/5.0 (Linux; U; Android 4.4.2; en-us; SCH-I535 Build/KOT4' +
              '9H) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Mobile Sa' +
              'fari/534.30'
            
              'Mozilla/5.0 (Linux; Android 7.0; SM-G930V Build/NRD90M) AppleWeb' +
              'Kit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Mobile Safar' +
              'i/537.36'
            
              'Mozilla/5.0 (Linux; Android 7.0; SM-A310F Build/NRD90M) AppleWeb' +
              'Kit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.91 Mobile Safari' +
              '/537.36 OPR/42.7.2246.114996'
            
              'Opera/9.80 (Android 4.1.2; Linux; Opera Mobi/ADR-1305251841) Pre' +
              'sto/2.11.355 Version/12.10'
            
              'Opera/9.80 (J2ME/MIDP; Opera Mini/5.1.21214/28.2725; U; ru) Pres' +
              'to/2.8.119 Version/11.10'
            
              'Mozilla/5.0 (iPhone; CPU iPhone OS 7_1_2 like Mac OS X) AppleWeb' +
              'Kit/537.51.2 (KHTML, like Gecko) OPiOS/10.2.0.93022 Mobile/11D25' +
              '7 Safari/9537.53'
            
              'Mozilla/5.0 (Android 7.0; Mobile; rv:54.0) Gecko/54.0 Firefox/54' +
              '.0'
            
              'Mozilla/5.0 (iPhone; CPU iPhone OS 10_3_2 like Mac OS X) AppleWe' +
              'bKit/603.2.4 (KHTML, like Gecko) FxiOS/7.5b3349 Mobile/14F89 Saf' +
              'ari/603.2.4'
            
              'Mozilla/5.0 (Linux; U; Android 7.0; en-US; SM-G935F Build/NRD90M' +
              ') AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 UCBrowser/1' +
              '1.3.8.976 U3/0.8.0 Mobile Safari/534.30'
            
              'Mozilla/5.0 (Linux; Android 6.0.1; SM-G920V Build/MMB29K) AppleW' +
              'ebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.98 Mobile Safa' +
              'ri/537.36'
            
              'Mozilla/5.0 (Linux; Android 5.1.1; SM-N750K Build/LMY47X; ko-kr)' +
              ' AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Mob' +
              'ile Safari/537.36 Puffin/6.0.8.15804AP'
            
              'Mozilla/5.0 (Linux; Android 5.1.1; SM-N750K Build/LMY47X; ko-kr)' +
              ' AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Mob' +
              'ile Safari/537.36 Puffin/6.0.8.15804AP'
            
              'Mozilla/5.0 (Linux; Android 7.0; SAMSUNG SM-G955U Build/NRD90M) ' +
              'AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/5.4 Chrome' +
              '/51.0.2704.106 Mobile Safari/537.36'
            
              'Mozilla/5.0 (Linux; Android 6.0; Lenovo K50a40 Build/MRA58K) App' +
              'leWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.137 YaBrows' +
              'er/17.4.1.352.00 Mobile Safari/537.36'
            
              'Mozilla/5.0 (Linux; U; Android 7.0; en-us; MI 5 Build/NRD90M) Ap' +
              'pleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/53.0.278' +
              '5.146 Mobile Safari/537.36 XiaoMi/MiuiBrowser/9.0.3'
            
              'Mozilla/5.0 (compatible; MSIE 10.0; Windows Phone 8.0; Trident/6' +
              '.0; IEMobile/10.0; ARM; Touch; Microsoft; Lumia 950)'
            
              'Mozilla/5.0 (Windows Phone 10.0; Android 6.0.1; Microsoft; Lumia' +
              ' 950) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.11' +
              '6 Mobile Safari/537.36 Edge/15.14977'
            
              'Mozilla/5.0 (BB10; Kbd) AppleWebKit/537.35+ (KHTML, like Gecko) ' +
              'Version/10.3.3.2205 Mobile Safari/537.35+')
        end
        object OverrideUserAgentBtn: TButton
          Left = 12
          Top = 49
          Width = 250
          Height = 25
          Align = alBottom
          Caption = 'Override user agent'
          TabOrder = 1
          OnClick = OverrideUserAgentBtnClick
        end
      end
      object EmulateTouchChk: TCheckBox
        Left = 10
        Top = 94
        Width = 274
        Height = 17
        Align = alBottom
        Caption = 'Emulate Touch'
        TabOrder = 1
        OnClick = EmulateTouchChkClick
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 201
      Width = 294
      Height = 205
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 2
      object GroupBox2: TGroupBox
        Left = 10
        Top = 0
        Width = 274
        Height = 195
        Align = alClient
        Caption = ' Emulate device metrics '
        Padding.Left = 10
        Padding.Top = 5
        Padding.Right = 10
        Padding.Bottom = 10
        TabOrder = 0
        object Panel6: TPanel
          Left = 12
          Top = 47
          Width = 250
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          Padding.Bottom = 5
          TabOrder = 1
          object Label1: TLabel
            Left = 0
            Top = 0
            Width = 75
            Height = 22
            Align = alLeft
            AutoSize = False
            Caption = 'Height'
            Layout = tlCenter
          end
          object HeightEdt: TSpinEdit
            Left = 75
            Top = 0
            Width = 175
            Height = 22
            Align = alClient
            MaxValue = 10000000
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
        end
        object Panel7: TPanel
          Left = 12
          Top = 20
          Width = 250
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          Padding.Bottom = 5
          TabOrder = 0
          object Label2: TLabel
            Left = 0
            Top = 0
            Width = 75
            Height = 22
            Align = alLeft
            AutoSize = False
            Caption = 'Width'
            Layout = tlCenter
          end
          object WidthEdt: TSpinEdit
            Left = 75
            Top = 0
            Width = 175
            Height = 22
            Align = alClient
            MaxValue = 10000000
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
        end
        object OverrideDeviceMetricsBtn: TButton
          Left = 12
          Top = 158
          Width = 250
          Height = 25
          Align = alBottom
          Caption = 'Override device metrics'
          TabOrder = 5
          OnClick = OverrideDeviceMetricsBtnClick
        end
        object Panel8: TPanel
          Left = 12
          Top = 74
          Width = 250
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          Padding.Bottom = 5
          TabOrder = 2
          object Label3: TLabel
            Left = 0
            Top = 0
            Width = 75
            Height = 22
            Align = alLeft
            AutoSize = False
            Caption = 'Scale'
            Layout = tlCenter
          end
          object ScaleEdt: TMaskEdit
            Left = 75
            Top = 0
            Width = 175
            Height = 22
            Align = alClient
            EditMask = '#.##;1;0'
            MaxLength = 4
            TabOrder = 0
            Text = '1.  '
            ExplicitHeight = 21
          end
        end
        object Panel9: TPanel
          Left = 12
          Top = 101
          Width = 250
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          Padding.Bottom = 5
          TabOrder = 3
          object Label4: TLabel
            Left = 0
            Top = 0
            Width = 75
            Height = 22
            Align = alLeft
            AutoSize = False
            Caption = 'Orientation'
            Layout = tlCenter
          end
          object OrientationCb: TComboBox
            Left = 75
            Top = 0
            Width = 175
            Height = 21
            Align = alClient
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = 'Portrait Primary'
            Items.Strings = (
              'Portrait Primary'
              'Portrait Secondary'
              'Landscape Primary'
              'Landscape Secondary')
          end
        end
        object Panel10: TPanel
          Left = 12
          Top = 128
          Width = 250
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          Padding.Bottom = 5
          TabOrder = 4
          object Label5: TLabel
            Left = 0
            Top = 0
            Width = 75
            Height = 22
            Align = alLeft
            AutoSize = False
            Caption = 'Angle'
            Layout = tlCenter
            ExplicitHeight = 36
          end
          object AngleEdt: TSpinEdit
            Left = 75
            Top = 0
            Width = 175
            Height = 22
            Align = alClient
            MaxValue = 360
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 741
    Height = 624
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object AddressPnl: TPanel
      Left = 0
      Top = 0
      Width = 741
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
        Left = 705
        Top = 5
        Width = 31
        Height = 20
        Margins.Left = 5
        Align = alRight
        Caption = 'Go'
        TabOrder = 0
        OnClick = GoBtnClick
      end
      object AddressCb: TComboBox
        Left = 5
        Top = 5
        Width = 700
        Height = 21
        Align = alClient
        ItemIndex = 0
        TabOrder = 1
        Text = 'https://www.google.com'
        Items.Strings = (
          'https://www.google.com'
          'https://browserleaks.com'
          'https://www.leaktest.io/'
          'https://coveryourtracks.eff.org/'
          'https://ipleak.com/full-report/'
          'https://xsinator.com/testing.html'
          'https://abrahamjuliot.github.io/creepjs/')
        ExplicitLeft = -3
        ExplicitTop = 3
      end
    end
    object CEFWindowParent1: TCEFWindowParent
      Left = 0
      Top = 30
      Width = 741
      Height = 594
      Align = alClient
      TabOrder = 1
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
    OnOpenUrlFromTab = Chromium1OpenUrlFromTab
    OnDevToolsMethodResult = Chromium1DevToolsMethodResult
    Left = 56
    Top = 152
  end
end
