object DBGridEditForm: TDBGridEditForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DBGridEditForm'
  ClientHeight = 75
  ClientWidth = 141
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 141
    Height = 49
    Align = alTop
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 0
      Width = 124
      Height = 13
      Caption = 'Select Data Field Visability'
    end
  end
  object CloseBtn: TButton
    Left = 0
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = CloseBtnClick
  end
end
