object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ToolBox Browser 2'
  ClientHeight = 37
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPnl: TPanel
    Left = 0
    Top = 0
    Width = 357
    Height = 37
    Align = alClient
    BevelOuter = bvNone
    Enabled = False
    TabOrder = 0
    object Edit1: TEdit
      Left = 8
      Top = 8
      Width = 286
      Height = 21
      TabOrder = 0
      Text = 'https://www.google.com'
    end
    object Button1: TButton
      Left = 300
      Top = 6
      Width = 51
      Height = 25
      Caption = 'Open'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object CEFWindowComponent1: TCEFWindowComponent
    OnGetPreferredSize = CEFWindowComponent1GetPreferredSize
    OnWindowCreated = CEFWindowComponent1WindowCreated
    OnWindowDestroyed = CEFWindowComponent1WindowDestroyed
    OnCanClose = CEFWindowComponent1CanClose
    Left = 48
  end
  object CEFBrowserViewComponent1: TCEFBrowserViewComponent
    Left = 152
    Top = 65528
  end
  object Chromium1: TChromium
    OnTitleChange = Chromium1TitleChange
    OnBeforePopup = Chromium1BeforePopup
    Left = 256
    Top = 8
  end
end
