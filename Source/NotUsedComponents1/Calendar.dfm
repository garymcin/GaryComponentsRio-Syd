object CalendarForm: TCalendarForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Calendar'
  ClientHeight = 192
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 163
    Width = 238
    Height = 29
    Align = alBottom
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
    object OkButton: TButton
      Left = 4
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Ok'
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object CancelBtn: TButton
      Left = 82
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
  object DateFieldCalendar: TMonthCalendar
    Left = 5
    Top = 2
    Width = 225
    Height = 160
    Date = 42426.451394560190000000
    TabOrder = 1
  end
end
