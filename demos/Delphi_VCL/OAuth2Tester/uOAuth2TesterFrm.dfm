object OAuth2TesterFrm: TOAuth2TesterFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OAuth 2.0 Tester'
  ClientHeight = 686
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  Padding.Bottom = 10
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LoginGrp: TGroupBox
    Left = 10
    Top = 10
    Width = 600
    Height = 219
    Align = alTop
    Caption = 'Login information '
    TabOrder = 0
    object ClientIDLbl: TLabel
      Left = 12
      Top = 27
      Width = 41
      Height = 13
      Caption = 'Client ID'
      Layout = tlCenter
    end
    object ClientSecretLbl: TLabel
      Left = 12
      Top = 53
      Width = 60
      Height = 13
      Caption = 'Client secret'
      Layout = tlCenter
    end
    object ScopeLbl: TLabel
      Left = 12
      Top = 80
      Width = 29
      Height = 13
      Caption = 'Scope'
      Layout = tlCenter
    end
    object AccessTokenLbl: TLabel
      Left = 12
      Top = 107
      Width = 63
      Height = 13
      Caption = 'Access token'
      Layout = tlCenter
    end
    object RefreshTokenLbl: TLabel
      Left = 12
      Top = 134
      Width = 68
      Height = 13
      Caption = 'Refresh token'
      Layout = tlCenter
    end
    object ClientIDEdt: TEdit
      Left = 88
      Top = 24
      Width = 500
      Height = 21
      TabOrder = 0
    end
    object ClientSecretEdt: TEdit
      Left = 88
      Top = 50
      Width = 500
      Height = 21
      TabOrder = 1
    end
    object ScopeEdt: TEdit
      Left = 88
      Top = 77
      Width = 500
      Height = 21
      TabOrder = 2
      Text = 'openid'
    end
    object AccessTokenEdt: TEdit
      Left = 88
      Top = 104
      Width = 500
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object RefreshTokenEdt: TEdit
      Left = 88
      Top = 131
      Width = 500
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object LoginBtn: TButton
      Left = 88
      Top = 169
      Width = 196
      Height = 30
      Caption = 'User login'
      TabOrder = 5
      OnClick = LoginBtnClick
    end
    object RefreshBtn: TButton
      Left = 392
      Top = 169
      Width = 196
      Height = 30
      Caption = 'Request new token'
      TabOrder = 6
      OnClick = RefreshBtnClick
    end
  end
  object LogGrp: TGroupBox
    Left = 10
    Top = 312
    Width = 600
    Height = 364
    Align = alBottom
    Caption = ' Log '
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    TabOrder = 1
    object LogMem: TMemo
      Left = 12
      Top = 25
      Width = 576
      Height = 327
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object ApiPnl: TPanel
    Left = 10
    Top = 229
    Width = 600
    Height = 83
    Align = alClient
    BevelOuter = bvNone
    Padding.Top = 10
    Padding.Bottom = 10
    TabOrder = 2
    object ApiGrp: TGroupBox
      Left = 0
      Top = 10
      Width = 600
      Height = 63
      Align = alClient
      Caption = 'API information '
      TabOrder = 0
      object EndpointLbl: TLabel
        Left = 12
        Top = 27
        Width = 42
        Height = 13
        Caption = 'Endpoint'
      end
      object EndpointEdt: TEdit
        Left = 88
        Top = 24
        Width = 425
        Height = 21
        TabOrder = 0
        Text = 'https://www.googleapis.com/oauth2/v3/userinfo'
      end
      object RequestBtn: TButton
        Left = 519
        Top = 22
        Width = 69
        Height = 25
        Caption = 'Request'
        TabOrder = 1
        OnClick = RequestBtnClick
      end
    end
  end
  object CEFServerComponent1: TCEFServerComponent
    OnServerDestroyed = CEFServerComponent1ServerDestroyed
    OnHttpRequest = CEFServerComponent1HttpRequest
    Left = 383
    Top = 480
  end
  object CEFUrlRequestClientComponent1: TCEFUrlRequestClientComponent
    OnRequestComplete = CEFUrlRequestClientComponent1RequestComplete
    OnDownloadProgress = CEFUrlRequestClientComponent1DownloadProgress
    OnDownloadData = CEFUrlRequestClientComponent1DownloadData
    OnCreateURLRequest = CEFUrlRequestClientComponent1CreateURLRequest
    Left = 383
    Top = 544
  end
end
