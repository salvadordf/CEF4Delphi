object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Custom Resource Browser'
  ClientHeight = 658
  ClientWidth = 781
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
  object ChromiumWindow1: TChromiumWindow
    Left = 0
    Top = 30
    Width = 781
    Height = 628
    Align = alClient
    TabOrder = 0
    OnClose = ChromiumWindow1Close
    OnBeforeClose = ChromiumWindow1BeforeClose
  end
  object AddressBarPnl: TPanel
    Left = 0
    Top = 0
    Width = 781
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
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 740
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.example.com'
      ExplicitHeight = 21
    end
    object Button1: TButton
      Left = 745
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 48
    Top = 240
  end
end
