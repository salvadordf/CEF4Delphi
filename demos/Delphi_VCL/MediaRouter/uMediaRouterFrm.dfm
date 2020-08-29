object MediaRouterFrm: TMediaRouterFrm
  Left = 0
  Top = 0
  Caption = 'Media Router'
  ClientHeight = 757
  ClientWidth = 1029
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
  object CEFWindowParent1: TCEFWindowParent
    Left = 184
    Top = 56
    Width = 0
    Height = 0
    TabOrder = 0
    Visible = False
    Enabled = False
  end
  object MainPnl: TPanel
    Left = 0
    Top = 0
    Width = 1029
    Height = 757
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 15
    Padding.Top = 15
    Padding.Right = 15
    Padding.Bottom = 15
    TabOrder = 1
    object SinksGbx: TGroupBox
      Left = 15
      Top = 15
      Width = 250
      Height = 727
      Align = alLeft
      Caption = ' Sinks on this network '
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 0
      object SinksLbx: TListBox
        Left = 12
        Top = 25
        Width = 226
        Height = 579
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = SinksLbxClick
      end
      object SinksButtonsPnl: TPanel
        Left = 12
        Top = 604
        Width = 226
        Height = 111
        Align = alBottom
        BevelOuter = bvNone
        Padding.Top = 10
        TabOrder = 1
        object GetDeviceInfoBtn: TButton
          Left = 0
          Top = 47
          Width = 226
          Height = 27
          Caption = 'Get device info'
          Enabled = False
          TabOrder = 0
          OnClick = GetDeviceInfoBtnClick
        end
        object NotifySinksBtn: TButton
          Left = 0
          Top = 84
          Width = 226
          Height = 27
          Align = alBottom
          Caption = 'Notify sinks'
          TabOrder = 1
          OnClick = NotifySinksBtnClick
        end
        object CreateRouteBtn: TButton
          Left = 0
          Top = 10
          Width = 226
          Height = 27
          Align = alTop
          Caption = 'Create route'
          Enabled = False
          TabOrder = 2
          OnClick = CreateRouteBtnClick
        end
      end
    end
    object CentralPnl: TPanel
      Left = 265
      Top = 15
      Width = 499
      Height = 727
      Align = alClient
      BevelOuter = bvNone
      Padding.Left = 20
      Padding.Right = 20
      TabOrder = 1
      object SourcePnl: TPanel
        Left = 20
        Top = 0
        Width = 459
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Padding.Top = 10
        Padding.Right = 2
        Padding.Bottom = 10
        TabOrder = 0
        object SourceLblPnl: TPanel
          Left = 0
          Top = 10
          Width = 65
          Height = 21
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object SourceURNLbl: TLabel
            Left = 0
            Top = 0
            Width = 65
            Height = 21
            Align = alClient
            Caption = 'Source URN'
            Layout = tlCenter
            ExplicitWidth = 57
            ExplicitHeight = 13
          end
        end
        object SourceURNCbx: TComboBox
          Left = 65
          Top = 10
          Width = 392
          Height = 21
          Align = alClient
          ItemIndex = 1
          TabOrder = 1
          Text = 'cast:CC1AD845?clientId=123456789'
          Items.Strings = (
            'cast:<appId>?clientId=<clientId>'
            'cast:CC1AD845?clientId=123456789'
            'cast:4F8B3483?clientId=123456789')
        end
      end
      object MessageGbx: TGroupBox
        Left = 20
        Top = 352
        Width = 459
        Height = 375
        Align = alClient
        Caption = ' Message '
        Padding.Left = 10
        Padding.Top = 10
        Padding.Right = 10
        Padding.Bottom = 10
        TabOrder = 1
        object SendMessagePnl: TPanel
          Left = 12
          Top = 326
          Width = 435
          Height = 37
          Align = alBottom
          BevelOuter = bvNone
          Padding.Top = 10
          TabOrder = 0
          object SendMsgBtn: TButton
            Left = 0
            Top = 10
            Width = 435
            Height = 27
            Align = alClient
            Caption = 'Send message'
            TabOrder = 0
            OnClick = SendMsgBtnClick
          end
        end
        object MessageMem: TMemo
          Left = 12
          Top = 25
          Width = 435
          Height = 301
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 1
          OnChange = MessageMemChange
        end
      end
      object LogGbx: TGroupBox
        Left = 20
        Top = 41
        Width = 459
        Height = 296
        Align = alTop
        Caption = ' Log '
        Padding.Left = 10
        Padding.Top = 10
        Padding.Right = 10
        Padding.Bottom = 10
        TabOrder = 2
        object LogMem: TMemo
          Left = 12
          Top = 25
          Width = 435
          Height = 222
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
        end
        object ClearLogPnl: TPanel
          Left = 12
          Top = 247
          Width = 435
          Height = 37
          Align = alBottom
          BevelOuter = bvNone
          Padding.Top = 10
          TabOrder = 1
          object ClearLogBtn: TButton
            Left = 0
            Top = 10
            Width = 435
            Height = 27
            Align = alClient
            Caption = 'Clear log'
            TabOrder = 0
            OnClick = ClearLogBtnClick
          end
        end
      end
      object SpacerPnl: TPanel
        Left = 20
        Top = 337
        Width = 459
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
      end
    end
    object RoutesGbx: TGroupBox
      Left = 764
      Top = 15
      Width = 250
      Height = 727
      Align = alRight
      Caption = ' Established routes '
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 10
      TabOrder = 2
      object RoutesLbx: TListBox
        Left = 12
        Top = 25
        Width = 226
        Height = 616
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = RoutesLbxClick
      end
      object RoutesButtonPnl: TPanel
        Left = 12
        Top = 641
        Width = 226
        Height = 74
        Align = alBottom
        BevelOuter = bvNone
        Padding.Top = 10
        TabOrder = 1
        object TerminateRouteBtn: TButton
          Left = 0
          Top = 10
          Width = 226
          Height = 27
          Align = alTop
          Caption = 'Terminate route'
          Enabled = False
          TabOrder = 0
          OnClick = TerminateRouteBtnClick
        end
        object NotifyRoutesBtn: TButton
          Left = 0
          Top = 47
          Width = 226
          Height = 27
          Align = alBottom
          Caption = 'Notify routes'
          TabOrder = 1
          OnClick = NotifyRoutesBtnClick
        end
      end
    end
  end
  object Chromium1: TChromium
    OnMediaRouteCreateFinished = Chromium1MediaRouteCreateFinished
    OnMediaSinkDeviceInfo = Chromium1MediaSinkDeviceInfo
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    OnSinks = Chromium1Sinks
    OnRoutes = Chromium1Routes
    OnRouteStateChanged = Chromium1RouteStateChanged
    OnRouteMessageReceived = Chromium1RouteMessageReceived
    Left = 72
    Top = 56
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 72
    Top = 136
  end
end
