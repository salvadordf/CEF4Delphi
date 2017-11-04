object PostDataInspectorFrm: TPostDataInspectorFrm
  Left = 0
  Top = 0
  Caption = 'PostData inspector'
  ClientHeight = 715
  ClientWidth = 1004
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 696
    Width = 1004
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 30
    Width = 1004
    Height = 666
    Align = alClient
    TabOrder = 1
  end
  object NavControlPnl: TPanel
    Left = 0
    Top = 0
    Width = 1004
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 2
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 963
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 
        'https://www.w3schools.com/php/showphp.asp?filename=demo_form_pos' +
        't'
      ExplicitHeight = 21
    end
    object GoBtn: TButton
      Left = 968
      Top = 5
      Width = 31
      Height = 20
      Margins.Left = 5
      Align = alRight
      Caption = 'Go'
      TabOrder = 1
      OnClick = GoBtnClick
    end
  end
  object Chromium1: TChromium
    OnProcessMessageReceived = Chromium1ProcessMessageReceived
    OnAfterCreated = Chromium1AfterCreated
    Left = 32
    Top = 224
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 32
    Top = 288
  end
end
