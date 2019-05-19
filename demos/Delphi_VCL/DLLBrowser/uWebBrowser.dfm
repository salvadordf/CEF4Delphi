object WebBrowserFrm: TWebBrowserFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Web Browser'
  ClientHeight = 600
  ClientWidth = 800
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ChromiumWindow1: TChromiumWindow
    Left = 0
    Top = 0
    Width = 800
    Height = 600
    Align = alClient
    TabOrder = 0
    OnClose = ChromiumWindow1Close
    OnBeforeClose = ChromiumWindow1BeforeClose
    OnAfterCreated = ChromiumWindow1AfterCreated
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 24
    Top = 24
  end
end
