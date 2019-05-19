object SimpleServerFrm: TSimpleServerFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple Server'
  ClientHeight = 436
  ClientWidth = 554
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
  object ButtonPnl: TPanel
    Left = 0
    Top = 0
    Width = 554
    Height = 94
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object AddressLbl: TLabel
      Left = 14
      Top = 11
      Width = 39
      Height = 13
      Caption = 'Address'
    end
    object PortLbl: TLabel
      Left = 14
      Top = 39
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object BacklogLbl: TLabel
      Left = 14
      Top = 68
      Width = 36
      Height = 13
      Caption = 'Backlog'
    end
    object AddressEdt: TEdit
      Left = 87
      Top = 8
      Width = 211
      Height = 21
      TabOrder = 0
      Text = '127.0.0.1'
      OnChange = AddressEdtChange
    end
    object PortEdt: TSpinEdit
      Left = 87
      Top = 36
      Width = 211
      Height = 22
      MaxValue = 65535
      MinValue = 1025
      TabOrder = 1
      Value = 8099
    end
    object BacklogEdt: TSpinEdit
      Left = 87
      Top = 65
      Width = 211
      Height = 22
      MaxValue = 512
      MinValue = 1
      TabOrder = 2
      Value = 10
    end
    object StartBtn: TButton
      Left = 312
      Top = 8
      Width = 105
      Height = 79
      Caption = 'Start'
      TabOrder = 3
      OnClick = StartBtnClick
    end
    object StopBtn: TButton
      Left = 434
      Top = 8
      Width = 105
      Height = 79
      Caption = 'Stop'
      Enabled = False
      TabOrder = 4
      OnClick = StopBtnClick
    end
  end
  object ConnectionLogMem: TMemo
    Left = 0
    Top = 94
    Width = 554
    Height = 342
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object CEFServerComponent1: TCEFServerComponent
    OnServerCreated = CEFServerComponent1ServerCreated
    OnServerDestroyed = CEFServerComponent1ServerDestroyed
    OnClientConnected = CEFServerComponent1ClientConnected
    OnClientDisconnected = CEFServerComponent1ClientDisconnected
    OnHttpRequest = CEFServerComponent1HttpRequest
    OnWebSocketRequest = CEFServerComponent1WebSocketRequest
    OnWebSocketConnected = CEFServerComponent1WebSocketConnected
    OnWebSocketMessage = CEFServerComponent1WebSocketMessage
    Left = 456
    Top = 304
  end
end
