object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple OSR Browser'
  ClientHeight = 578
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 677
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 0
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 636
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.google.com'
      ExplicitHeight = 21
    end
    object Button1: TButton
      Left = 641
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
  object PaintBox: TPaintBox32
    Left = 0
    Top = 30
    Width = 677
    Height = 548
    Align = alClient
    TabOrder = 1
    OnMouseDown = PaintBoxMouseDown
    OnMouseMove = PaintBoxMouseMove
    OnMouseUp = PaintBoxMouseUp
    OnMouseWheel = PaintBoxMouseWheel
    OnResize = PaintBoxResize
  end
  object chrmosr: TChromium
    OnGetRootScreenRect = chrmosrGetRootScreenRect
    OnGetViewRect = chrmosrGetViewRect
    OnGetScreenPoint = chrmosrGetScreenPoint
    OnPaint = chrmosrPaint
    OnCursorChange = chrmosrCursorChange
    Left = 304
    Top = 304
  end
  object AppEvents: TApplicationEvents
    OnMessage = AppEventsMessage
    Left = 504
    Top = 240
  end
end
