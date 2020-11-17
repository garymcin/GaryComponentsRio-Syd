object GEMDBGridEditForm: TGEMDBGridEditForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Grid Column Visibility'
  ClientHeight = 95
  ClientWidth = 183
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 183
    Height = 66
    Align = alClient
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
    object CheckListBox1: TCheckListBox
      Left = 1
      Top = 1
      Width = 181
      Height = 64
      OnClickCheck = CheckListBox1ClickCheck
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object CloseBtn: TButton
    Left = 0
    Top = 66
    Width = 183
    Height = 29
    Align = alBottom
    Caption = 'Close'
    TabOrder = 1
    OnClick = CloseBtnClick
  end
end
