object Form1: TForm1
  Left = 0
  Top = 0
  BorderWidth = 8
  Caption = 'CEF Dlls Copy Tool'
  ClientHeight = 366
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 553
    Height = 366
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Setup'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 65
        Align = alTop
        TabOrder = 0
        DesignSize = (
          545
          65)
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 291
          Height = 13
          Caption = 'Root CEF Binaries folder (contains Release, Resources etc..)'
        end
        object edtRootFolder: TEdit
          Left = 8
          Top = 26
          Width = 529
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 297
        Width = 545
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnCopy: TButton
          Left = 16
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Copy'
          TabOrder = 0
          OnClick = btnCopyClick
        end
      end
      object MemoPanel: TPanel
        Left = 0
        Top = 65
        Width = 545
        Height = 232
        Align = alClient
        BorderWidth = 8
        TabOrder = 2
        object Label2: TLabel
          Left = 9
          Top = 9
          Width = 527
          Height = 13
          Align = alTop
          Caption = 'List of destination folders: (1 on each line)'
          ExplicitWidth = 202
        end
        object Memo1: TMemo
          Left = 9
          Top = 22
          Width = 527
          Height = 201
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel3: TPanel
        Left = 0
        Top = 297
        Width = 545
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object btnBack: TButton
          Left = 16
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Back'
          TabOrder = 0
          OnClick = btnBackClick
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 297
        Align = alClient
        BorderWidth = 8
        TabOrder = 1
        object Memo2: TMemo
          Left = 9
          Top = 9
          Width = 527
          Height = 279
          Align = alClient
          TabOrder = 0
        end
      end
    end
  end
end
