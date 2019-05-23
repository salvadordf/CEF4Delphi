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
    TabOrder = 0
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 667
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.google.com'
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
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 708
    Height = 402
    Align = alClient
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 432
    Width = 708
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 500
      end>
  end
  object Chromium1: TChromium
    OnLoadingStateChange = Chromium1LoadingStateChange
    OnStatusMessage = Chromium1StatusMessage
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    Left = 592
    Top = 288
  end
end
