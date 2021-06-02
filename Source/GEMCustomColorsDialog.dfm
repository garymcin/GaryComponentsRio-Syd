object GEMColorsDialog: TGEMColorsDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Color'
  ClientHeight = 308
  ClientWidth = 180
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object panel_Bottom: TJvPanel
    Left = 0
    Top = 251
    Width = 180
    Height = 57
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 449
    ExplicitWidth = 472
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
      Enabled = False
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
    Width = 180
    Height = 251
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 472
    ExplicitHeight = 449
    object cncolorgrid_ColorSelection: TCnColorGrid
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 163
      Height = 227
      Align = alClient
      ColCount = 10
      DefaultColWidth = 15
      DefaultRowHeight = 15
      DoubleBuffered = True
      FixedCols = 0
      RowCount = 14
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine]
      ParentDoubleBuffered = False
      ScrollBars = ssNone
      TabOrder = 0
      OnMouseMove = cncolorgrid_ColorSelectionMouseMove
      OnSelectCell = cncolorgrid_ColorSelectionSelectCell
      ColorSet = csGEMColors
      CustomColors.Strings = (
        '12695295'
        '3353215'
        '3937500'
        '16118015'
        '9662683'
        '11823615'
        '9639167'
        '8721863'
        '140535'
        '4204888'
        '14524637'
        '15631086'
        '16711935'
        '9109643'
        '8388736'
        '13850042'
        '13828244'
        '13382297'
        '8519755'
        '14822282'
        '14381203'
        '15624315'
        '13458026'
        '9125192'
        '16775416'
        '16443110'
        '16711680'
        '13434880'
        '9109504'
        '8388608'
        ' 7346457'
        '14772545'
        '15570276'
        '14599344'
        '10061943'
        '9470064'
        '16748574'
        ' 16775408'
        '11829830'
        '16436871'
        '15453831'
        '16760576'
        '15128749'
        '15130800'
        '10526303'
        '13749760'
        '16777200'
        '16777184'
        '15658671'
        '16776960'
        '9145088'
        '8421376'
        '5197615'
        '13422920'
        '11186720'
        '13688896'
        '13959039'
        '11193702'
        '10156544'
        '16449525'
        '8388352'
        '7451452'
        '5737262'
        '15794160'
        '9419919'
        '10025880'
        '9498256'
        '3329330'
        '65280'
        '2263842'
        '32768'
        '25600'
        '64636'
        '65407'
        '3145645'
        '3107669'
        '3329434'
        '2330219'
        '15794175'
        '14480885'
        '14745599'
        '13826810'
        '65535'
        '2896'
        ' 7059389'
        '11200750'
        '13499135'
        '9234160'
        '55295'
        '14481663'
        '2139610'
        '755384'
        '15792895'
        '15136253'
        '11788021'
        '42495'
        '11920639'
        '14020607'
        '13495295'
        '11394815'
        '1415065'
        '9221330'
        '8894686'
        '36095'
        '12903679'
        '15134970'
        '4163021'
        '12180223'
        '6333684'
        '1993170'
        '1262987'
        '15660543'
        '2970272'
        '8036607'
        '5275647'
        '17919'
        '8034025'
        '4678655'
        '7504122'
        '14804223'
        '8421616'
        '16448255'
        '9408444'
        '6053069'
        '255'
        '2763429'
        '223106'
        '139'
        '128'
        '16777215'
        '16119285'
        '14474460'
        '13882323'
        '12632256'
        '11119017'
        '8421504'
        '6908265'
        '0'
        '0'
        '0')
      CustomRowCount = 14
      CustomColCount = 10
      CustomColorType = ccHtml
    end
  end
end
