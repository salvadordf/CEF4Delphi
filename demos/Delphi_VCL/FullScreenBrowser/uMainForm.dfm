object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'MainForm'
  ClientHeight = 288
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 0
    Width = 434
    Height = 288
    Align = alClient
    TabOrder = 0
  end
  object Chromium1: TChromium
    OnPreKeyEvent = Chromium1PreKeyEvent
    OnKeyEvent = Chromium1KeyEvent
    OnBeforePopup = Chromium1BeforePopup
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
    Left = 208
    Top = 120
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 272
    Top = 120
  end
end
