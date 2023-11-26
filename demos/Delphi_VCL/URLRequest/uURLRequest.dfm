object URLRequestFrm: TURLRequestFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'URL request'
  ClientHeight = 529
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Top = 5
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 510
    Width = 518
    Height = 19
    Panels = <
      item
        Width = 500
      end>
  end
  object GETGbx: TGroupBox
    Left = 10
    Top = 8
    Width = 494
    Height = 105
    Caption = ' GET example '
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 30
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object DownloadBtn: TButton
      Left = 13
      Top = 62
      Width = 465
      Height = 25
      Caption = 'Download'
      TabOrder = 0
      OnClick = DownloadBtnClick
    end
    object GetURLEdt: TEdit
      Left = 45
      Top = 27
      Width = 433
      Height = 21
      TabOrder = 1
      Text = 
        'https://www.w3.org/WAI/ER/tests/xhtml/testfiles/resources/pdf/du' +
        'mmy.pdf'
    end
  end
  object POSTGbx: TGroupBox
    Left = 10
    Top = 136
    Width = 494
    Height = 361
    Caption = ' POST example '
    TabOrder = 2
    object Label2: TLabel
      Left = 16
      Top = 29
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object PostURLEdt: TEdit
      Left = 45
      Top = 26
      Width = 433
      Height = 21
      TabOrder = 0
      Text = 'https://httpbin.org/post'
    end
    object SendPostReqBtn: TButton
      Left = 16
      Top = 193
      Width = 462
      Height = 25
      Caption = 'Send POST request'
      TabOrder = 1
      OnClick = SendPostReqBtnClick
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 56
      Width = 462
      Height = 57
      Caption = ' Parameter 1 '
      TabOrder = 2
      object Label3: TLabel
        Left = 16
        Top = 24
        Width = 34
        Height = 13
        Caption = 'Name :'
      end
      object Label4: TLabel
        Left = 264
        Top = 24
        Width = 33
        Height = 13
        Caption = 'Value :'
      end
      object PostParam1NameEdt: TEdit
        Left = 56
        Top = 21
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'name1'
      end
      object PostParam1ValueEdt: TEdit
        Left = 304
        Top = 21
        Width = 137
        Height = 21
        TabOrder = 1
        Text = 'value1'
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 123
      Width = 462
      Height = 57
      Caption = ' Parameter 2 '
      TabOrder = 3
      object Label5: TLabel
        Left = 16
        Top = 24
        Width = 34
        Height = 13
        Caption = 'Name :'
      end
      object Label6: TLabel
        Left = 264
        Top = 24
        Width = 33
        Height = 13
        Caption = 'Value :'
      end
      object PostParam2NameEdt: TEdit
        Left = 56
        Top = 21
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'name2'
      end
      object PostParam2ValueEdt: TEdit
        Left = 304
        Top = 21
        Width = 137
        Height = 21
        TabOrder = 1
        Text = 'value2'
      end
    end
    object Memo1: TMemo
      Left = 2
      Top = 232
      Width = 490
      Height = 127
      Align = alBottom
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 4
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 448
    Top = 104
  end
  object CEFUrlRequestClientComponent1: TCEFUrlRequestClientComponent
    OnRequestComplete = CEFUrlRequestClientComponent1RequestComplete
    OnDownloadProgress = CEFUrlRequestClientComponent1DownloadProgress
    OnDownloadData = CEFUrlRequestClientComponent1DownloadData
    OnCreateURLRequest = CEFUrlRequestClientComponent1CreateURLRequest
    Left = 304
    Top = 104
  end
end
