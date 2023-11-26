object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'JSSharedMemoryProcMessage'
  ClientHeight = 579
  ClientWidth = 878
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 878
    Height = 549
    Align = alClient
    TabOrder = 0
  end
  object AddressBarPnl: TPanel
    Left = 0
    Top = 0
    Width = 878
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    DoubleBuffered = True
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ParentDoubleBuffered = False
    TabOrder = 1
    object AddressEdt: TEdit
      Left = 5
      Top = 5
      Width = 740
      Height = 20
      Align = alClient
      TabOrder = 0
      Text = 'https://www.google.com/'
      ExplicitHeight = 21
    end
    object Panel1: TPanel
      Left = 745
      Top = 5
      Width = 128
      Height = 20
      Align = alRight
      BevelOuter = bvNone
      Padding.Left = 5
      TabOrder = 1
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
      object SendMessageBtn: TButton
        Left = 40
        Top = 0
        Width = 88
        Height = 20
        Align = alRight
        Caption = 'Send message'
        TabOrder = 1
        OnClick = SendMessageBtnClick
      end
    end
  end
  object Chromium1: TChromium
    OnProcessMessageReceived = Chromium1ProcessMessageReceived
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    Left = 16
    Top = 40
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 16
    Top = 96
  end
end
