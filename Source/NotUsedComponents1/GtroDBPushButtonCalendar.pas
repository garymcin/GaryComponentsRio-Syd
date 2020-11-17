{*******************************************************************************
                  Component: TGtroDBPushButtonCalendar
                          by Georges Trottier
                   Copyright(C) GTRO Informatique 2000
                           http://www.gtro.com
                            6 December 2000

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Code is GTRO Informatique (gtro@gtro.com)
Portions created by GTRO Informatique are Copyright (C) 2000 GTRO Informatique.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Name        : TGtroDBDBPushButtonCalendar, Version 1.0.0
Copyright   : Copyright(c)2000 GTRO Informatique
Date        : 28 November 2000
Author      : Georges Trottier
Description : TGtroDBPushButtonCalendar is a class (derived from TCustomGrid)
            : that can be used to implement a special purpose calendar whose
            : main action is to associate or disassociate a record in a dataset
            : with the date selected in the calendar.
            : This main action is executed either with a click or a double-click.
            : If an associate record does not exists when the calendar cell
            : is activated, one is created. If one exists already, the
            : associated record is deleted from the dataset. The rest of the
            : code is simply the internal mechanics of the calendar.

Public methods and properties

  	Create (constructor)
        Initializes the object.

    Destroy (destructor)
        Clean-up after execution

    NextMonth;
        Changes the calendar display to the next month

  	NextYear
        Changes the calendar display to the next year

    PrevMonth;
        Changes the calendar display to the previous month

    PrevYear;
        Changes the calendar display to the previous year

    Date (read-only property)
        Makes the date of the calendar available to the users

    OwnsRecord (read-only property)
        Inform the users if the selected cell has an associated record in the
        dataset. Its value is updated in methods ActionOnRecord and
        RowColToRecord.

Published methods and properties

    DataSource (property)
        Selects the dataset to be used.

    FieldName (property)
        Select the field of the dataset which will be used for the
        correspondance between the date and its associated record.

    MainActions (property)
      The following options are available:
        maClick    : the main action is executed when a cell is clicked
        maDblClick : the main action is executed when a cell is bouble-clicked.

    SelCellBkgColor (property)
        Selection of the color of the cells of the calendar which have an
        associate record.

    OnCalendarDateChange (event handler)
        Its value is updated in method UpdateCalendarData

Protected methods and properties (for developers wishing to use them in
derived classes):

		Click (overriden from ancestor)
      Action taken when a cell of the calendar is clicked.

    DblClick (overriden from ancestor)
      Action taken when a cell of the calendar is double-clicked.

    DataChange (overriden from ancestor)

    DrawCell (overriden from ancestor)

    WMSize (overriden from ancestor)
        Handles the WM_SIZE message.

    CalendarDate (property)

    Month (property)

		OnDrawCell (property)

    Year (property)

*******************************************************************************}

unit GtroDBPushButtonCalendar;

{$I GtroDBLib.INC}

interface

uses
	Windows, Messages, System.SysUtils, Classes, Graphics, Controls, Grids,
	DBCtrls, db;

type
  eMainActions = (maClick, maDblClick);
  TPBCalendarDateChangeEvent =
    procedure(Sender: TObject; OwnsRecord: Boolean) of Object;

{ Declaration of the component }

	TGtroDBPushButtonCalendar = class(TCustomGrid)
  private
    FDataLink: TFieldDataLink; // no need for a special datalink
    FMainActions: eMainActions;
  	FCells: array [0..6, 0..6] of string;
    FChangeAction: Boolean;
  	FDate: TDateTime;
    FDaysInMonth: Integer;
    FFirstOfMonth: TDateTime;
    FHasRecord: array[0..6, 0..6] of Boolean;
    FLastOfMonth: TDateTime;
    FLongFlag: Boolean;
    FOffSet: Integer;
    FOnCalendarDateChange: TPBCalendarDateChangeEvent;
    FOwnsRecord: Boolean;
    FSelCellBkgColor: TColor;
    FSelectedCell: TGridRect;
    FYear,
    FMonth,
    FDay: Word;
    procedure ActionOnRecord;
    function DaysPerMonth(AYear, AMonth: Integer): Integer;
    function GetDataSource: TDataSource;
    function GetFieldName: string;
    function IsLeapYear(AYear: Integer): Boolean;
  	function RowColToDate(ACol, ARow: Word): TDateTime;
    procedure RowColToRecord(Col, Row: Integer);
    procedure SetCalendarDate(Value: TDateTime);
    procedure SetChangeAction(Value: Boolean);
    procedure SetDataSource(DataSource: TDataSource);
    procedure SetDay(Value: Word);
    procedure SetMonth(Value: Word);
    procedure SetYear(Value: Word);
    procedure SetFieldName(FieldName: string);
    procedure TriggerOnCalendarDateChange;
    procedure UpdateCalendarData;  // sets internal data
  protected
		procedure Click; override;
    procedure DblClick; override;
    procedure DataChange(Sender: TObject);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NextMonth;
  	procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    property ChangeAction: Boolean read FChangeAction write SetChangeAction;
    property OwnsRecord: Boolean read FOwnsRecord;
    property Day: word read FDay write SetDay;
    property Month: Word read FMonth write SetMonth;
    property Year: Word read FYear write SetYear;
  published
    property Align;
    property Color;
    property Ctl3D;
    property CalendarDate: TDateTime read FDate write SetCalendarDate;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FieldName: string read GetFieldName write SetFieldName;
    property Font;
    property GridLineWidth;
    property MainActions: eMainActions read FMainActions write FMainActions;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SelCellBkgColor: TColor
      read FSelCellBkgColor write FSelCellBkgColor;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnCalendarDateChange: TPBCalendarDateChangeEvent
      read FOnCalendarDateChange write FOnCalendarDateChange;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF GTRO_D4}
      property Anchors;
      property BiDiMode;
      property BorderStyle;
      property BorderWidth;
      property Constraints;
      property DragKind;
      property ParentBiDiMode;
      property OnEndDock;
      property OnStartDock;
    {$ENDIF}
    {$IFDEF GTRO_D5}
      property OnContextPopup;
    {$ENDIF}
  end;

procedure Register;

implementation

{ TGtroDBPushButtonCalendar - Private Methods }

procedure TGtroDBPushButtonCalendar.ActionOnRecord;
// Produces the main action of the component (inserting or deleting records)
begin
  if Row > 0 then
  begin
    if FHasRecord[Col, Row] then
      FDataLink.DataSet.Delete
    else
    begin
      FDataLink.OnDataChange:= nil;
      try
        FDataLink.DataSet.Append;
        FDataLink.DataSet.FieldByName(FieldName).AsDateTime:= CalendarDate;
        FDataLink.DataSet.Post;
      finally
        FDataLink.OnDataChange:= DataChange;
        UpdateCalendarData;  // sets internal data
        Invalidate; // redraws the calendar
        FOwnsRecord:= FHasRecord[Col, Row];
      end; // try...finally
    end; // else
  end; // if Row ...
end;

function TGtroDBPushButtonCalendar.DaysPerMonth(AYear, AMonth: Integer): Integer;
// Given a month and a year, yields the number of days in the month
// Called by UpdateCalendarData, SetMonth and SetYear
const
  DaysInMonth: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function TGtroDBPushButtonCalendar.GetDataSource: TDataSource;
// Read method for the DataSource property
begin
	Result:= FDataLink.DataSource;
end;

function TGtroDBPushButtonCalendar.GetFieldName: string;
// Read method for the FieldName property
begin
	Result:= FDataLink.FieldName;
end;

function TGtroDBPushButtonCalendar.IsLeapYear(AYear: Integer): Boolean;
// Returns true if AYear is a leap year
// Called by DaysPerMonth, NextYear, PrevYear
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function TGtroDBPushButtonCalendar.RowColToDate(ACol, ARow: Word): TDateTime;
// Transforms Row and Col of Calendar data to DateTime
// Called by UpdateCalendarData, Click and RowColToRecord
var
	Number: Integer;
begin
	Number:= (ACol - FOffset - 1) + (ARow - 1)*7;
  Result:= FFirstOfMonth + Number;
end;

procedure TGtroDBPushButtonCalendar.RowColToRecord(Col, Row: Integer);
// Activates the record correspondind to Col, Row if it exists
// Called by UpdateCalendarData and Click
// when called, FDate has just been set
begin
  FOwnsRecord:= FHasRecord[Col, Row];  // resets HasDependentRecords
  if FOwnsRecord then
  begin
    FDataLink.DataSet.DisableControls;
    try
      // searches the dataset for a specified record and makes it the active record
      FDataLink.DataSet.Locate(FieldName, FDate, []);
    finally
      FDataLink.DataSet.EnableControls;
    end; // try...finally
  end; // ...if
end;

procedure TGtroDBPushButtonCalendar.SetCalendarDate(Value: TDateTime);
// Write method for the CalendarDate protected property
begin
  if Value <> FDate then
  begin
    FDate:= Value;
    DecodeDate(FDate, FYear, FMonth, FDay);
  end; // ...if
end;

procedure TGtroDBPushButtonCalendar.SetChangeAction(Value: Boolean);
begin
  if Value <> FChangeAction then
  begin
    FChangeAction:= Value;
    if FChangeAction then
      FDataLink.OnDataChange:= DataChange
    else
      FDataLink.OnDataChange:= nil;
  end; // ...if
end;

procedure TGtroDBPushButtonCalendar.SetDataSource(DataSource: TDataSource);
// Write method for the DataSource property
begin
	FDataLink.DataSource:= DataSource;
end;

procedure TGtroDBPushButtonCalendar.SetDay(Value: Word);
// Write method for the Day property
begin
  FDay:= Value;
  if FDay > DaysPerMonth(FYear, FMonth)
    then FDay:= DaysPerMonth(FYear, FMonth);
  FDate:= EncodeDate(FYear, FMonth, FDay);
  UpdateCalendarData;  // sets internal data
  Selection:= FSelectedCell;
  RowColToRecord(Col, Row);
  Invalidate; // redraws the calendar
 end;

procedure TGtroDBPushButtonCalendar.SetFieldName(FieldName: string);
// Write method for the FieldName property
begin
	FDataLink.FieldName:= FieldName;
end;

procedure TGtroDBPushButtonCalendar.SetMonth(Value: Word);
// Write method for the Month property
begin
	if (Value <> FMonth) then
  begin
    if Value > 12 then
    begin
      Value:= 1;
      FYear:= FYear + 1;
    end;
    if Value <= 0 then
    begin
      Value:= 12;
      FYear:= FYear - 1;
    end;
    FMonth:= Value;
    // before encoding the date, make sure the date is valid
    if FDay > DaysPerMonth(FYear, FMonth)
      then FDay:= DaysPerMonth(FYear, FMonth);
    FDate:= EncodeDate(FYear, FMonth, FDay);
    UpdateCalendarData;  // sets internal data
    Selection:= FSelectedCell;
    RowColToRecord(Col, Row);
    Invalidate; // redraws the calendar
  end;
end;

procedure TGtroDBPushButtonCalendar.SetYear(Value: Word);
// Write method for the Year property
begin
	if (Value <> FYear) then
  begin
  	FYear:= Value;
    // before encoding the date, make sure the date is valid
    if FDay > DaysPerMonth(FYear, FMonth)
      then FDay:= DaysPerMonth(FYear, FMonth);
  	FDate:= EncodeDate(FYear, FMonth, FDay);
  	UpdateCalendarData;  // sets internal data
    Selection:= FSelectedCell;
    RowColToRecord(Col, Row);
    Invalidate; // redraws the calendar
  end;
end;

procedure TGtroDBPushButtonCalendar.TriggerOnCalendarDateChange;
// Triggers OnCalendarChange event
// Called by UpdateCalendarData and Click
begin
  if Assigned(FOnCalendarDateChange) then
    FOnCalendarDateChange(Self, FOwnsRecord);
end;

procedure TGtroDBPushButtonCalendar.UpdateCalendarData;
// Sets the cell data
// Called by ActionOnRecord, DataChange, SetMonth, SetYear, Create
var
	i, j: Integer;
  Number: Integer;
  DateRecherche: TDateTime;
begin
  DecodeDate(FDate, FYear, FMonth, FDay);
  FFirstOfMonth:= EncodeDate(FYear, FMonth, 1);
  FOffSet:= DayOfWeek(FFirstOfMonth)-2;
  FDaysInMonth:= DaysPerMonth(FYear, FMonth);
  FLastOfMonth:= FFirstOfMonth + FDaysInMonth - 1;
  FDataLink.OnDataChange:= nil;  // no OnDataChange event handler
  try
    for i:= 0 to ColCount-1 do
      if FLongFlag then FCells[i, 0]:= FormatSettings.LongDayNames[i+1]
      else FCells[i, 0]:= FormatSettings.ShortDayNames[i+1];
    for i:= 0 to ColCount-1 do
      for j:= 1 to RowCount-1 do
      begin
        Number:= (i - FOffSet) + (j-1)*7;
        if Number = FDay then
        begin
          FSelectedCell.Left:= i;
          FSelectedCell.Right:= i;
          FSelectedCell.Top:= j;
          FSelectedCell.Bottom:= j;
        end;
        FHasRecord[i, j]:= False;
        if (Number <= 0) or (Number > FDaysInMonth) then FCells[i, j]:= ''
        else
        begin
          FCells[i, j]:= IntToStr(Number);
          if (FDataLink.DataSource <> nil) and (FDataLink.Field <> nil) then
          begin
            FDataLink.DataSet.DisableControls;
            try
              DateRecherche:= RowColToDate(i, j);
              if FDataLink.DataSource.DataSet.Locate(FieldName, DateRecherche, []) then
                FHasRecord[i, j]:= True;
              if Number = FDay then FOwnsRecord:= FHasRecord[i, j];
            finally
              FDataLink.DataSet.EnableControls;
            end; // try...finally
          end; // ...if
        end; // ...else
      end; // ....for, for
  finally
    if (FDataLink.DataSource <> nil) and (FDataLink.Field <> nil) then
      FDataLink.DataSource.DataSet.Locate(FieldName, FDate, []);
    FDataLink.OnDataChange:= DataChange;
    TriggerOnCalendarDateChange;
  end; // try...finally
end;

{ TGtroDBPushButtonCalendar - Protected Methods }

procedure TGtroDBPushButtonCalendar.Click;
// Overrides the ancertor's Click method
begin
	inherited Click;
  CalendarDate:= RowColToDate(Col, Row);
  RowColToRecord(Col, Row);
  TriggerOnCalendarDateChange;
  if MainActions = maClick then ActionOnRecord;
end;

procedure TGtroDBPushButtonCalendar.DataChange(Sender: TObject);
// Assigned as OnDataChange event handler of the datalink in the constructor
// Called by UpdateCalendarData, Create,
begin
  UpdateCalendarData;  // sets internal data
  Invalidate; // redraws the calendar
end;

procedure TGtroDBPushButtonCalendar.DblClick;
// Overrides the ancertor's DblClick method
begin
  if MainActions = maDblClick then ActionOnRecord;
  inherited;
end;

procedure TGtroDBPushButtonCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect;
	AState: TGridDrawState);
// OnDrawCell event handler (Overrides ancestor's method)
var
  TheText: string;
begin
  TheText:= FCells[ACol, ARow];
  with Canvas, ARect do
  begin
  	if gdFixed in AState then Brush.Color:= clBtnFace
    else
  		if FHasRecord[ACol, ARow] then // a record corresponds
      begin
      	Font.Color:= clWindow;
      	Font.Style:= Font.Style + [fsBold];
      	Brush.Color:= FSelCellBkgColor;
      end // ...if
    	else
      begin
      	Font.Color:= clWindowText;
      	Font.Style:= Font.Style - [fsBold];
      	Brush.Color:= clWindow;
      end; // ...else
    TextRect(ARect, Left + (Right - Left - TextWidth(TheText)) div 2,
      Top + (Bottom - Top - TextHeight(TheText)) div 2, TheText);
  end;
end;

procedure TGtroDBPushButtonCalendar.WMSize(var Message: TWMSize);
// OnResize event handler
var
  GridLines: Integer;
begin
	if Message.Width < 330 then FLongFlag:= False
  else FLongFlag:= True;
  GridLines := 7 * GridLineWidth;
  DefaultColWidth := (Message.Width - GridLines) div 7;
  DefaultRowHeight := (Message.Height - GridLines) div 7;
end;

{ TGtroDBPushButtonCalendar - Public Methods }

constructor TGtroDBPushButtonCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:= TFieldDataLink.Create;
  FDataLink.Control:= Self;
  FDataLink.OnDataChange:= DataChange;
  // Default values
  DefaultDrawing:= True;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 7;
	CalendarDate:= Date;
  UpdateCalendarData;  // sets internal data
  Selection:= FSelectedCell; // selects cell corresponding to to-day
  Invalidate; // draws the calendar
end;

destructor TGtroDBPushButtonCalendar.Destroy;
begin
	FDataLink.Free;
  FDataLink:= nil;
  inherited Destroy;
end;

procedure TGtroDBPushButtonCalendar.NextMonth;
// Changes the calendar display to next month
begin
  Month:= Month + 1;
end;

procedure TGtroDBPushButtonCalendar.NextYear;
// Changes the calendar display to next year
begin
  if IsLeapYear(FYear) and (FMonth = 2) and (FDay = 29) then FDay := 28;
  Year:= Year + 1;
end;

procedure TGtroDBPushButtonCalendar.PrevMonth;
// Changes the calendar display to previous month
begin
  Month:= Month - 1;
end;

procedure TGtroDBPushButtonCalendar.PrevYear;
// Changes the calendar display to previous year
begin
  if IsLeapYear(FYear) and (FMonth = 2) and (FDay = 29) then FDay := 28;
  Year := Year - 1;
end;

{ Global procedures }

procedure Register;
begin
	RegisterComponents('GTRO', [TGtroDBPushButtonCalendar]);
end;

end.
