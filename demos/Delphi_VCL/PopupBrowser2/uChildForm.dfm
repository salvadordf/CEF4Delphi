object ChildForm: TChildForm
  Left = 0
  Top = 0
  Caption = 'Popup'
  ClientHeight = 256
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 0
    Width = 352
    Height = 256
    Align = alClient
    TabOrder = 0
  end
  object Chromium1: TChromium
    OnTitleChange = Chromium1TitleChange
    OnBeforePopup = Chromium1BeforePopup
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    Left = 24
    Top = 56
  end
end
