object ChildForm: TChildForm
  Left = 197
  Top = 117
  Caption = 'MDI Child'
  ClientHeight = 451
  ClientWidth = 708
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 708
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 684
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 667
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.google.com'
      ExplicitWidth = 643
      ExplicitHeight = 21
    end
    object Button1: TButton
      Left = 672
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 1
      OnClick = Button1Click
      ExplicitLeft = 648
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 708
    Height = 421
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 336
    ExplicitTop = 216
    ExplicitWidth = 100
    ExplicitHeight = 41
  end
  object Chromium1: TChromium
    OnLoadEnd = Chromium1LoadEnd
    OnAfterCreated = Chromium1AfterCreated
    OnClose = Chromium1Close
    Left = 592
    Top = 288
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 608
    Top = 344
  end
end
