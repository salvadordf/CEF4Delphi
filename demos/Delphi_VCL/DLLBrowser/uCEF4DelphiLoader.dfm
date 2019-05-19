object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CEF4Delphi loader'
  ClientHeight = 249
  ClientWidth = 274
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
  PixelsPerInch = 96
  TextHeight = 13
  object InitializeBtn: TButton
    Left = 16
    Top = 16
    Width = 241
    Height = 49
    Caption = '1. Initialize'
    TabOrder = 0
    OnClick = InitializeBtnClick
  end
  object ShowBtn: TButton
    Left = 16
    Top = 72
    Width = 241
    Height = 49
    Caption = '2. Show browser'
    Enabled = False
    TabOrder = 1
    OnClick = ShowBtnClick
  end
  object FinalizeBtn: TButton
    Left = 16
    Top = 128
    Width = 241
    Height = 49
    Caption = '3. Finalize'
    Enabled = False
    TabOrder = 2
    OnClick = FinalizeBtnClick
  end
  object CloseBtn: TButton
    Left = 16
    Top = 184
    Width = 241
    Height = 49
    Caption = '4. Close this form'
    Enabled = False
    TabOrder = 3
    OnClick = CloseBtnClick
  end
end
