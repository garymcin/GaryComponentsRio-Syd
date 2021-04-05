object Form22: TForm22
  Left = 0
  Top = 0
  Caption = 'Form22'
  ClientHeight = 411
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object tac: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 153
    Align = alTop
    TabOrder = 0
    ExplicitLeft = -8
    ExplicitTop = -6
    object lbl1: TLabel
      Left = 120
      Top = 23
      Width = 121
      Height = 20
      Caption = 'Traffic Server Host'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lbl2: TLabel
      Left = 120
      Top = 49
      Width = 116
      Height = 20
      Caption = 'Traffic Server Port'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object jvpnl_TrafficLights: TJvPanel
      Left = 16
      Top = 8
      Width = 74
      Height = 139
      Color = clBlack
      ParentBackground = False
      TabOrder = 0
      object jvshp_RedLight: TJvShape
        Left = 17
        Top = 2
        Width = 41
        Height = 41
        Brush.Color = clRed
        Shape = stCircle
      end
      object jvshp_YellowLight: TJvShape
        Left = 17
        Top = 49
        Width = 41
        Height = 41
        Brush.Color = clYellow
        Shape = stCircle
      end
      object jvshp_GreenLight: TJvShape
        Left = 17
        Top = 96
        Width = 41
        Height = 41
        Brush.Color = clGreen
        Shape = stCircle
      end
    end
    object TrafficServerHostEdit: TEdit
      Left = 256
      Top = 25
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'TrafficServerHostEdit'
    end
    object TrafficServerPortEdit: TEdit
      Left = 256
      Top = 52
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'TrafficServerHostEdit'
    end
    object btn1: TButton
      Left = 152
      Top = 88
      Width = 89
      Height = 25
      Caption = 'Connect'
      TabOrder = 3
      OnClick = btn1Click
    end
    object DisconnectButton: TButton
      Left = 280
      Top = 88
      Width = 97
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 4
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 153
    Width = 852
    Height = 258
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object WSocket1: TWSocket
    LineMode = True
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    OnDataAvailable = WSocket1DataAvailable
    OnSessionConnected = WSocket1SessionConnected
    SocketErrs = wsErrTech
    Left = 152
    Top = 288
  end
end
