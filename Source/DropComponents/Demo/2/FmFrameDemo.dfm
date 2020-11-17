object FmMain: TFmMain
  Left = 251
  Top = 114
  Caption = 'FmMain'
  ClientHeight = 532
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 260
    Width = 727
    Height = 241
    Align = alClient
    TabOrder = 0
    inline frmLeft: TFrame1
      Left = 1
      Top = 1
      Width = 443
      Height = 239
      Align = alLeft
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 443
      ExplicitHeight = 239
      inherited Label1: TLabel
        Width = 443
        Height = 18
        ExplicitWidth = 443
        ExplicitHeight = 18
      end
      inherited Memo1: TMemo
        Top = 18
        Width = 443
        Height = 180
        ExplicitLeft = 72
        ExplicitTop = 114
        ExplicitWidth = 443
        ExplicitHeight = 180
      end
      inherited Panel1: TPanel
        Top = 198
        Width = 443
        Height = 41
        ExplicitTop = 198
        ExplicitWidth = 443
        ExplicitHeight = 41
        inherited Label2: TLabel
          Left = 10
          Top = 12
          Width = 76
          Height = 17
          ExplicitLeft = 10
          ExplicitTop = 12
          ExplicitWidth = 76
          ExplicitHeight = 17
        end
        inherited Edit1: TEdit
          Left = 102
          Top = 7
          Width = 169
          Height = 25
          ExplicitLeft = 102
          ExplicitTop = 7
          ExplicitWidth = 169
          ExplicitHeight = 25
        end
      end
      inherited PJCtrlDropFiles1: TPJCtrlDropFiles
        ManagedControl = frmLeft
        Top = 48
      end
    end
    inline frmRight: TFrame1
      Left = 444
      Top = 1
      Width = 282
      Height = 239
      Align = alClient
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      ExplicitLeft = 444
      ExplicitTop = 1
      ExplicitWidth = 282
      ExplicitHeight = 239
      inherited Label1: TLabel
        Width = 282
        Height = 18
        ExplicitLeft = 6
        ExplicitTop = 4
        ExplicitWidth = 282
        ExplicitHeight = 18
      end
      inherited Memo1: TMemo
        Top = 18
        Width = 282
        Height = 180
        ExplicitLeft = 6
        ExplicitTop = -14
        ExplicitWidth = 282
        ExplicitHeight = 180
      end
      inherited Panel1: TPanel
        Top = 198
        Width = 282
        Height = 41
        ExplicitTop = 198
        ExplicitWidth = 282
        ExplicitHeight = 41
        inherited Label2: TLabel
          Left = 10
          Top = 12
          Width = 76
          Height = 17
          ExplicitLeft = 10
          ExplicitTop = 12
          ExplicitWidth = 76
          ExplicitHeight = 17
        end
        inherited Edit1: TEdit
          Left = 102
          Top = 7
          Width = 169
          Height = 25
          ExplicitLeft = 102
          ExplicitTop = 7
          ExplicitWidth = 169
          ExplicitHeight = 25
        end
      end
      inherited PJCtrlDropFiles1: TPJCtrlDropFiles
        ManagedControl = frmRight
        Top = 56
      end
      inherited PJExtFileFilter1: TPJExtFileFilter
        Top = 128
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 727
    Height = 260
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 725
      Height = 19
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Form Drop Area'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Memo1: TMemo
      Left = 1
      Top = 20
      Width = 725
      Height = 239
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 501
    Width = 727
    Height = 31
    Align = alBottom
    TabOrder = 2
  end
  object PJFormDropFiles1: TPJFormDropFiles
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = PJFormDropFiles1DropFiles
    Left = 88
    Top = 32
  end
end
