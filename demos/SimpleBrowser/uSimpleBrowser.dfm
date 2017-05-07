object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple Browser'
  ClientHeight = 624
  ClientWidth = 1038
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
  object ChromiumWindow1: TChromiumWindow
    Left = 0
    Top = 30
    Width = 1038
    Height = 594
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 684
    ExplicitHeight = 532
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1038
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    ShowCaption = False
    TabOrder = 1
    ExplicitWidth = 684
    object Edit1: TEdit
      Left = 5
      Top = 5
      Width = 997
      Height = 20
      Margins.Right = 5
      Align = alClient
      TabOrder = 0
      Text = 'http://www.google.com'
      ExplicitWidth = 643
      ExplicitHeight = 21
    end
    object Button1: TButton
      Left = 1002
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
end
