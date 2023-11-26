object DirectorySelectorFrm: TDirectorySelectorFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Select a directory'
  ClientHeight = 441
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 405
    Width = 317
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    object OkBtn: TButton
      Left = 5
      Top = 5
      Width = 120
      Height = 26
      Align = alLeft
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object CancelBtn: TButton
      Left = 192
      Top = 5
      Width = 120
      Height = 26
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 317
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 1
    object DriveComboBox1: TDriveComboBox
      Left = 5
      Top = 5
      Width = 307
      Height = 21
      Align = alClient
      DirList = DirectoryListBox1
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 31
    Width = 317
    Height = 374
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Right = 5
    TabOrder = 2
    object DirectoryListBox1: TDirectoryListBox
      Left = 5
      Top = 0
      Width = 307
      Height = 374
      Align = alClient
      TabOrder = 0
    end
  end
end
