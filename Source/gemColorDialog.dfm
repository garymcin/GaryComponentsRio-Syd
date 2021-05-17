object ColorsDialog: TColorsDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ColorsDialog'
  ClientHeight = 317
  ClientWidth = 186
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object panel_Bottom: TJvPanel
    Left = 0
    Top = 260
    Width = 186
    Height = 57
    Align = alBottom
    TabOrder = 0
    object jvlbl_ColorLabel: TJvLabel
      Left = 12
      Top = 8
      Width = 27
      Height = 13
      Caption = 'None'
      Transparent = True
    end
    object BitBtn1: TBitBtn
      Left = 12
      Top = 27
      Width = 75
      Height = 25
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 0
    end
    object BitBtn2: TBitBtn
      Left = 93
      Top = 27
      Width = 75
      Height = 25
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object panel_BackPanel: TJvPanel
    Left = 0
    Top = 0
    Width = 186
    Height = 260
    Align = alClient
    TabOrder = 1
    object cncolorgrid_ColorSelection: TCnColorGrid
      Left = 1
      Top = 1
      Width = 173
      Height = 255
      Align = alClient
      ColCount = 10
      DefaultColWidth = 16
      DefaultRowHeight = 17
      FixedCols = 0
      RowCount = 14
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goFixedRowDefAlign]
      ScrollBars = ssNone
      TabOrder = 0
      OnMouseMove = cncolorgrid_ColorSelectionMouseMove
      OnSelectCell = cncolorgrid_ColorSelectionSelectCell
      ColorSet = csCustomColors
      CustomRowCount = 14
      CustomColCount = 10
      CustomColorType = ccDec
    end
  end
end
