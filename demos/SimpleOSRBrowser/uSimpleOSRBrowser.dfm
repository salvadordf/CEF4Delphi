object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple OSR Browser'
  ClientHeight = 716
  ClientWidth = 992
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object NavControlPnl: TPanel
    Left = 0
    Top = 0
    Width = 992
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 1
    object GoBtn: TButton
      Left = 956
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 1
      OnClick = GoBtnClick
    end
    object ComboBox1: TComboBox
      Left = 5
      Top = 5
      Width = 951
      Height = 21
      Align = alClient
      ItemIndex = 0
      TabOrder = 0
      Text = 'https://www.google.com'
      Items.Strings = (
        'https://www.google.com'
        'https://html5demos.com/drag')
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 30
    Width = 992
    Height = 686
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    TabStop = True
    OnEnter = Panel1Enter
    OnExit = Panel1Exit
    object PaintBox: TPaintBox32
      Left = 0
      Top = 0
      Width = 992
      Height = 686
      Align = alClient
      TabOrder = 0
      OnClick = PaintBoxClick
      OnMouseDown = PaintBoxMouseDown
      OnMouseMove = PaintBoxMouseMove
      OnMouseUp = PaintBoxMouseUp
      OnMouseWheel = PaintBoxMouseWheel
      OnResize = PaintBoxResize
    end
  end
  object chrmosr: TChromium
    OnAfterCreated = chrmosrAfterCreated
    OnGetRootScreenRect = chrmosrGetRootScreenRect
    OnGetViewRect = chrmosrGetViewRect
    OnGetScreenPoint = chrmosrGetScreenPoint
    OnPopupShow = chrmosrPopupShow
    OnPopupSize = chrmosrPopupSize
    OnPaint = chrmosrPaint
    OnCursorChange = chrmosrCursorChange
    Left = 24
    Top = 56
  end
  object AppEvents: TApplicationEvents
    OnMessage = AppEventsMessage
    Left = 24
    Top = 128
  end
end
