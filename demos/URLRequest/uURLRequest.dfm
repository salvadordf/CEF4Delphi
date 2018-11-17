object URLRequestFrm: TURLRequestFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'URL request'
  ClientHeight = 111
  ClientWidth = 494
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Edit1: TEdit
    Left = 48
    Top = 16
    Width = 433
    Height = 21
    TabOrder = 0
    Text = 
      'https://www.w3.org/WAI/ER/tests/xhtml/testfiles/resources/pdf/du' +
      'mmy.pdf'
  end
  object DownloadBtn: TButton
    Left = 16
    Top = 51
    Width = 465
    Height = 25
    Caption = 'Download'
    TabOrder = 1
    OnClick = DownloadBtnClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 92
    Width = 494
    Height = 19
    Panels = <
      item
        Width = 500
      end>
  end
  object SaveDialog1: TSaveDialog
    Left = 384
    Top = 64
  end
  object CEFUrlRequestClientComponent1: TCEFUrlRequestClientComponent
    OnRequestComplete = CEFUrlRequestClientComponent1RequestComplete
    OnDownloadProgress = CEFUrlRequestClientComponent1DownloadProgress
    OnDownloadData = CEFUrlRequestClientComponent1DownloadData
    OnCreateURLRequest = CEFUrlRequestClientComponent1CreateURLRequest
    Left = 80
    Top = 64
  end
end
