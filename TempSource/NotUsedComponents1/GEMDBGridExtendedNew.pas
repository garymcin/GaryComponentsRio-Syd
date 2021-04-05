unit GEMDBGridExtended;
  {.$DEFINE USE_CODESITE}
  {.$DEFINE GRID_DEBUG}

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.UITypes, System.Types,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms, VCL.Grids, VCL.DBGrids,
  Vcl.Menus, Vcl.Buttons, Vcl.Dialogs,

  Data.DB,

  GEMCalendar, GEMDBGridEdit, GEMComponentsGlobal;//, CodeSiteLogging;

const
  KeyboardShiftStates = [ssShift, ssAlt, ssCtrl];

type
  TMyFieldModEvent = Procedure(AField:TField) of Object ;

  TDBGridCellHintPosition = (gchpDefault, gchpMouse);

  TTitleHintEvent = procedure(Sender: TObject; Field: TField;
                          var AHint: string; var ATimeOut: Integer) of object;
  //TCustomGridAccess = class(TCustomGrid);

  TCheckTitleBtnEvent = procedure(Sender: TObject; ACol: Longint;
           Field: TField; var Enabled: Boolean) of object;

  TSelectedRowHighLight = class(TPersistent)
    private
      fHighSelectedRow: boolean;
      fFontColor      : TColor;
      fBrushColor     : TColor;
    public
      procedure Assign(Source: TPersistent); override;
    published
      property HighLightSelectedRow: boolean read fHighSelectedRow write fHighSelectedRow;
      property FontColor: TColor read fFontColor write fFontColor;
      property BrushColor: TColor read fBrushColor write fBrushColor;
  end;

  TGEMDBGridExtended = class(TDBGrid)
  private
    fHighLightSelectedRow : TSelectedRowHighLight;
    fMouseOverRow         : integer;
    fGridInfoFile         : string;
    FAltRowColorUse       : Boolean;
    FDefDrw               : Boolean;
    FScrollBars           : System.UITypes.TScrollStyle;
    FAlwaysShowEditor     : Boolean;
    FDisableCount         : Boolean;
    FSaveGridLayout       : Boolean;
    //=========================
    // title arrow stuff
    FTitleArrow            : Boolean;
    FTitleArrowDown        : Boolean;
    FTitlePopup            : TPopupMenu;
    FOnTitleArrowMenuEvent : TNotifyEvent;

    // title arrow stuff
    //=========================
    FDatePullDownDateFields : Boolean;
    FInMouseClick           : boolean;
    //FMultiSelect            : Boolean;
    FClearSelection         : Boolean;
    FColFileName            : string;
    FBooleanFieldCheckBox   : Boolean;
    //FOnCheckButton          : TCheckTitleBtnEvent;
    FShowTitleHint          : Boolean;
    FTitleHint              : string;
    FOnShowTitleHint        : TTitleHintEvent;
    theTitleArrowGlyph      : TBitmap;
    FCellHintPosition       : TDBGridCellHintPosition;

    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure WmBeforeCreate(var Msg: TMessage); message WM_CREATE;
    procedure WmBeforeClose(var Msg: TMessage); message WM_DESTROY;

    function GetBtnRect(CellRect: TRect; complete: boolean): TRect;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    //procedure CheckTitleButton(ACol, ARow: Longint; var Enabled: Boolean);

    procedure SetFileName(const Value: string);
    procedure SetUseAltRowColor(const Value: Boolean);
    procedure SetCheckBoxes(Value: Boolean);
    procedure SetDatePullDownDateFields(Value: Boolean);
    procedure SetupCalendarForDateField(theField: TField; CLeft, CTop: integer);
    procedure SetMouseOverRow(const Value: integer);
    procedure SetHighLightSelectedRow(const Value: TSelectedRowHighLight);

    procedure OnCalendarOkBtnClick(Sender: TObject);
    procedure OnCalendarCancelBtnClick(Sender: TObject);
//    procedure SetHighLigthSelectedRow(const Value: boolean);
  protected
    //fAltRowColor: TColor;
    FAlternateRowColor     : TColor;
    FAlternateRowFontColor : TColor;
    fAltFontColor          : TColor;
    FSelecting             : Boolean;


    DateFieldCalendar      : TGEMCalendar;
    OriginalOptions        : TDBGridOptions;
    ColSelEditor           : TGEMDBGridEditForm;


     //=== text file stuff =============================
     {$IFDEF GRID_DEBUG}
     myFile : TextFile;
     DataStream: TFileStream;
     fDoFileCellTest: Boolean;
     procedure SetDoFileCellTest(const Value: Boolean);
     {$ENDIF}

    //=============================================================
    // overRide stuff =============================================

    procedure UpdateScrollBar; override;
    procedure Scroll(Distance: Integer); override;
    procedure ColEnter(); override;
    procedure ColExit(); override;
    procedure CellClick(Column: TColumn); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
                                    Column: TColumn; State: TGridDrawState); override;
    // overRide stuff =============================================
    //=============================================================

    function GetVersion: string;
    procedure SaveBoolean;
    function GetTitleOffset: Integer;
    procedure ShowSelectColumnClick; dynamic;
    procedure ShowColumnsDialog;


    procedure SetScrollBars(Value: System.UITypes.TScrollStyle);
    procedure SetAltFontColor(Value: TColor);
    procedure SetAlternateRowColor(const Value: TColor);
    procedure SetTitleArrow(const Value: Boolean);
    procedure AltColorsSet;

  public
    destructor Destroy; override;
    constructor Create(AOwner:TComponent); override;
    property VisibleRowCount;
    property VisibleColCount;
    property TitleOffset: Integer read GetTitleOffset;
    property Canvas;
    property Col;
    property InplaceEditor;
    property LeftCol;
    property SelectedRows;
    property MouseOverRow: integer read fMouseOverRow write SetMouseOverRow;
    {$IFDEF GRID_DEBUG}
    property DoFileCellTest: Boolean read fDoFileCellTest write SetDoFileCellTest default False;
    {$ENDIF}
  published
    property HighLightSelectedRow: TSelectedRowHighLight read fHighLightSelectedRow write SetHighLightSelectedRow;
    property CellHintPosition: TDBGridCellHintPosition read FCellHintPosition write FCellHintPosition default gchpDefault;
    property ShowTitleHint: Boolean read FShowTitleHint write FShowTitleHint default False;
    property BooleanFieldCheckBox: Boolean read FBooleanFieldCheckBox write SetCheckBoxes;
    property GridInfoFile : string read fGridInfoFile write fGridInfoFile;
    property AltFont: TColor read fAltFontColor write SetAltFontColor;
    property AltRowColor: TColor read FAlternateRowColor write SetAlternateRowColor;   //: the alternate color of a row.
    property AltRowColorUse: Boolean read FAltRowColorUse write SetUseAltRowColor default false;   //if the component "easy-read" feature is enabled.
    property FixedBackground: Boolean read FDefDrw write FDefDrw default false;  //selects two different kinds of scrolling the colored backgroun
    property ScrollBars: System.UITypes.TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property OnTitleArrowMenuEvent: TNotifyEvent read FOnTitleArrowMenuEvent write FOnTitleArrowMenuEvent;
    property OnMouseDown;
    property OnMouseUp;
    property ColSaveFileName: string read fColFileName write SetFileName;
    property TitlePopup: TPopupMenu read FTitlePopup write FTitlePopup;
    //property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ClearSelection: Boolean read FClearSelection write FClearSelection default True;
    property TitleArrow: Boolean read FTitleArrow write SetTitleArrow default False;
    property DatePullDownDateFields: Boolean read FDatePullDownDateFields write SetDatePullDownDateFields default False;
    property OnShowTitleHint: TTitleHintEvent read FOnShowTitleHint write FOnShowTitleHint;
    property TitleHint: string read FTitleHint write FTitleHint;
    property SaveGridLayout: Boolean read FSaveGridLayout write FSaveGridLayout default False;
    property Version: string read GetVersion;

  end;

//procedure Register;

implementation

{.$R Button.res}
uses
  Math;

const
  TXT_MARG: TPoint = (x: 8; y: 2);
  BTN_WIDTH = 12;

type
  TBookmarks = class(TBookmarkList);


//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMDBGridExtended]);
//end;



{ TGEMDBGridExtended }


constructor TGEMDBGridExtended.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fGridInfoFile := '';
  FScrollBars := ssBoth;
  FAlwaysShowEditor := True;
  FDisableCount := True;
  FClearSelection := True;
  fColFileName := '';
  FTitleArrowDown := False;
  theTitleArrowGlyph := TBitmap.Create;
  FInMouseClick := False;
  theTitleArrowGlyph.LoadFromResourceName(HInstance, 'ARROWDOWN');

  fHighLightSelectedRow := TSelectedRowHighLight.Create;
  fHighLightSelectedRow.HighLightSelectedRow := false;
  fHighLightSelectedRow.FontColor := clNavy;
  fHighLightSelectedRow.BrushColor := clSilver;
  {$IFDEF  GRID_DEBUG}
  fDoFileCellTest := false;
  {$ENDIF}
end;


destructor TGEMDBGridExtended.Destroy;
begin
  if Assigned(theTitleArrowGlyph) then begin
    theTitleArrowGlyph.Free;
  end;
  fHighLightSelectedRow.Free;

  if Assigned(DateFieldCalendar) then
    FreeAndNil(DateFieldCalendar);
  inherited;
end;


function TGEMDBGridExtended.GetVersion: string;
begin
  Result := '1.0';
end;



{$IFDEF GRID_DEBUG}
procedure TGEMDBGridExtended.SetDoFileCellTest(const Value: Boolean);
begin
  fDoFileCellTest := Value;
  if fDoFileCellTest then begin
    DataStream := TFileStream.Create('C:\Users\Gary\Downloads\DelphiXE2\GarysComponents\Source\no tneeded\CellSizeStream12.txt',  fmCreate);

    AssignFile(myFile, 'C:\Users\Gary\Downloads\DelphiXE2\GarysComponents\Source\no tneeded\CellSizeTest112.txt');
    ReWrite(myFile);
    Append(myFile);
  end
  else begin
    CloseFile(myFile);
    //DataStream.
    FreeAndNil(DataStream);
  end;
end;
{$ENDIF}


procedure TGEMDBGridExtended.CMHintShow(var Msg: TCMHintShow);
const
  C_TIMEOUT = 250;
var
  ACol, ARow, ATimeOut: Integer;
  AtCursorPosition: Boolean;
  InitialMousePos: TPoint;
begin
  try
    AtCursorPosition := True;
    with Msg.HintInfo^ do
    begin
      { Save the position of mouse cursor }
      InitialMousePos := Mouse.CursorPos;

      HintStr := GetShortHint(Hint);
      ATimeOut := HideTimeOut;
      MouseToCell(CursorPos.X, CursorPos.Y, ACol, ARow);

      if FShowTitleHint and (ACol = 0) and (ARow = 0) then
      begin
        //ShowMessage('in hint');
        AtCursorPosition := FCellHintPosition = gchpMouse;
        HintStr := FTitleHint;
        //HintStr := Columns[ACol].FieldName;
        ATimeOut := Max(ATimeOut, Length(HintStr) * C_TIMEOUT);
        if Assigned(FOnShowTitleHint) and DataLink.Active then
          FOnShowTitleHint(Self, Columns[ACol].Field, HintStr, ATimeOut);
        HideTimeOut := ATimeOut;
      end;


      if not AtCursorPosition and HintWindowClass.ClassNameIs('THintWindow') then
        HintPos := ClientToScreen(CursorRect.TopLeft)
      else
        HintPos := InitialMousePos;
    end;
    inherited;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.SetDatePullDownDateFields(Value: Boolean);
begin
  FDatePullDownDateFields := Value;
  Invalidate;
end;


procedure TGEMDBGridExtended.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;


procedure TGEMDBGridExtended.SetupCalendarForDateField(theField: TField; CLeft, CTop: integer);
begin
  DateFieldCalendar := TGEMCalendar.Create(nil);
  try
    DateFieldCalendar.Left := CLeft;
    DateFieldCalendar.Top := CTop;
    DateFieldCalendar.OnClick_OKButton     :=  OnCalendarOkBtnClick;
    DateFieldCalendar.OnClick_CancelButton := OnCalendarCancelBtnClick;


//    if DateFieldCalendar.ShowModal = mrOk then begin
//      theDate := DateFieldCalendar.Date;
//      Self.SelectedField.Dataset.Edit;
//      Self.SelectedField.AsDateTime := theDate;
//      Self.SelectedField.Dataset.Post;
//    end;
  finally
//    FreeAndNil(DateFieldCalendar);
  end;
end;


procedure TGEMDBGridExtended.CellClick(Column: TColumn);
var
  where, PlaceCalendar: TPoint;
  ACol, ARow: integer;
  btnRect: TRect;
begin
  try
    if Self.SelectedField.DataType = ftBoolean then begin
      SaveBoolean();
      Exit;
    end;


    if (Self.SelectedField.DataType = ftDate) and ( not FInMouseClick) then begin
      FInMouseClick := true;
      try
        //Get clicked coordinates and cell:
        where := Mouse.CursorPos;
        PlaceCalendar := where;
        where := ScreenToClient(where);
        MouseToCell(where.x, where.y, ACol, ARow);
        btnRect := Self.CellRect(ACol, ARow);
        btnRect := GetBtnRect(btnRect, false);
        InflateRect(btnrect, 2, 2);  //Allow 2px 'error-range'...
        if PtInRect(btnRect, where) then  begin
          SetupCalendarForDateField(Self.SelectedField, PlaceCalendar.x, PlaceCalendar.y);
        end;
      finally
        FInMouseClick := false;
      end;
    end
    else
      Exit;
    inherited;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
Const
  CtrlState : array[Boolean] of Integer =(DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
var
  txtRect: TRect;
  btnRect: TRect;
  CheckBoxRectangle : TRect;
  {$IFDEF  GRID_DEBUG}
    s: string;
  {$ENDIF}
begin
  try
    //inherited;
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(Rect);

    if AltRowColorUse then
      if (DataLink.ActiveRecord mod 2)=0 then
        Canvas.Brush.Color := FAlternateRowColor
      else
        Canvas.Brush.Color := Self.Color;
    DefaultDrawColumnCell(Rect, DataCol, Column, State);

    // Boolean Field check box
    if (Column.Field <> nil) and (FBooleanFieldCheckBox) and (Column.Field.DataType = ftBoolean) then begin
      //ShowMessage('DrawCheckBox');
      Canvas.FillRect(Rect);
      {$IFDEF  GRID_DEBUG}
      if fDoFileCellTest then begin
        s := '...Rect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, Rect.TopLeft.X, Rect.TopLeft.y, Rect.BottomRight.X, Rect.BottomRight.y]);
        DataStream.WriteBuffer(S[1], Length(S));

        Writeln(myFile, ' == Check box ====================================');
        Writeln(myFile, '...Rect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, Rect.TopLeft.X, Rect.TopLeft.y, Rect.BottomRight.X, Rect.BottomRight.y]));
      end;
      {$ENDIF}
      CheckBoxRectangle.Left := Rect.Left + 2;
      CheckBoxRectangle.Right := Rect.Right - 2;
      CheckBoxRectangle.Top := Rect.Top + 2;
      CheckBoxRectangle.Bottom := Rect.Bottom - 2;
      DrawFrameControl(Canvas.Handle, CheckBoxRectangle, DFC_BUTTON, CtrlState[Column.Field.AsBoolean]);
    end;

    //  Date field button
    if (Column.Field <> nil) and (FDatePullDownDateFields) and (Column.Field.DataType = ftDate) then begin
      txtRect := Rect;

      Canvas.Font.Name := Font.Name;
      Canvas.Font.Size := Font.Size;
      txtRect.Left := Rect.left + TXT_MARG.x;
      txtRect.Right := txtRect.Right - (Rect.Right - Rect.Left) - TXT_MARG.x;



      DrawText(canvas.Handle, PChar (Column.Field.AsString), length(Column.Field.AsString),
               txtRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER{ or DT_END_ELLIPSIS});

      // draw button
      btnRect := GetBtnRect(Rect, False);

      {$IFDEF  GRID_DEBUG}
      if fDoFileCellTest then begin
        Writeln(myFile, ' == Date Pull Down  ==============================');
        Writeln(myFile, '...Rect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, Rect.TopLeft.X, Rect.TopLeft.y, Rect.BottomRight.X, Rect.BottomRight.y]));
        Writeln(myFile, 'TxtRect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, txtRect.TopLeft.X, txtRect.TopLeft.y, txtRect.BottomRight.X, txtRect.BottomRight.y]));
        Writeln(myFile, 'BTNRect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, btnRect.TopLeft.X, btnRect.TopLeft.y, btnRect.BottomRight.X, btnRect.BottomRight.y]));
      end;
      {$ENDIF}
      Canvas.Brush.Color := clBtnFace;
      Canvas.Pen.Style := psClear;
      Canvas.Rectangle(btnRect);

      DrawEdge(canvas.Handle, btnRect, EDGE_RAISED, BF_FLAT or BF_RECT or BF_ADJUST);
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 8;
      Canvas.Font.Color := clBlack;
      //DrawText(canvas.Handle, '...', -1, btnRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
      DrawText(canvas.Handle, '...', length('...'), btnRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
    end;

    if fHighLightSelectedRow.HighLightSelectedRow then begin
      if Not ((gdFocused in State) or (gdSelected in State)) and
             (fMouseOverRow = 1 + DataLink.ActiveRecord) then
      begin
        Canvas.Brush.Color := fHighLightSelectedRow.BrushColor;
        Canvas.Font.Color  := fHighLightSelectedRow.FontColor;
      end;
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;


 end;


//Returns rectangle where button will be drawn:
function TGEMDBGridExtended.GetBtnRect(CellRect: TRect; complete: boolean): TRect;

  function MakeBtnRect(Alignment: TAlignment; cellrect: TRect; complete: boolean): TRect;
  var
    rowHeight: integer;
  begin
    result := cellrect;
    rowheight := cellrect.bottom - cellrect.top;

    case Alignment of
      taLeftJustify:
        begin
          result.Right := cellrect.left + BTN_WIDTH + TXT_MARG.x + (TXT_MARG.x div 2);
          if not complete then
          begin
            result.Top := cellrect.Top + ((RowHeight - BTN_WIDTH) div 2);
            result.Left := cellrect.Left + ((RowHeight - BTN_WIDTH) div 2);
            result.Bottom := result.Top + BTN_WIDTH;
            result.Right := result.Left + BTN_WIDTH;
          end;
        end;
      taRightJustify:
        begin
          result.Left := cellrect.Right - BTN_WIDTH - TXT_MARG.x - TXT_MARG.x;
          if result.left < cellrect.left then
            result.left := Cellrect.left;

          if not complete then
          begin
            result.top := cellrect.top + ((RowHeight - BTN_WIDTH) div 2);
            result.left := result.left + TXT_MARG.x;
            result.right := Result.left + BTN_WIDTH;
            result.Bottom := result.top + BTN_WIDTH;
          end;
        end;
      taCenter:
        begin
          result.left := result.left + ((cellrect.Right - cellrect.left) div 2) - (BTN_WIDTH div 2) - TXT_MARG.x;
          if result.left < cellrect.Left then
            result.left := cellrect.left;
          result.right := result.left + BTN_WIDTH + TXT_MARG.x + TXT_MARG.x;
          if not complete then
          begin
            result.Top := cellrect.Top + ((RowHeight - BTN_WIDTH) div 2);
            result.Left := result.Left + TXT_MARG.x;
            result.Bottom := result.Top + BTN_WIDTH;
            result.Right := result.Left + BTN_WIDTH;
          end;
        end;
    end;
  end;

begin
  result := Rect(0, 0, 0, 0);
  //Last visible row sometimes get truncated so we need to fix that
  if (cellrect.Bottom - cellrect.Top) < DefaultRowHeight then
    cellrect.Bottom := cellrect.top + DefaultRowheight;

  result := MakeBtnRect(taRightJustify, cellrect, complete);
end;


procedure TGEMDBGridExtended.ColEnter;
begin
  try
    if Self.SelectedField.DataType = ftBoolean then begin
      Self.OriginalOptions := Self.Options;
      Self.Options := Self.Options - [dgEditing];
    inherited;
  end;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.ColExit;
begin
  try
    if Self.SelectedField.DataType = ftBoolean then
      Self.Options := Self.OriginalOptions;
    inherited;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.AltColorsSet;
begin
//
end;


function TGEMDBGridExtended.GetTitleOffset: Integer;
var
  I, J: Integer;
begin
  Result := 0;
  if dgTitles in Options then
  begin
    Result := 1;
    if (DataLink <> nil) and (DataLink.DataSet <> nil) and DataLink.DataSet.ObjectView then
      for I := 0 to Columns.Count - 1 do
      begin
        if Columns[I].Showing then
        begin
          J := Columns[I].Depth;
          if J >= Result then
            Result := J + 1;
        end;
      end;
  end;
end;


procedure TGEMDBGridExtended.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
                               Y: Integer);
var
  Cell: TGridCoord;
  CursorPos: TPoint;
begin
  try
    Cell := MouseCoord(X, Y);

    if (Button = mbRight) and FTitleArrow and (Cell.X = 0) and (Cell.Y = 0)  then
          // Display TitlePopup if it exists
      if Assigned(FTitlePopup) then begin
        GetCursorPos(CursorPos);
        FTitlePopup.PopupComponent := Self;
        FTitlePopup.Popup(CursorPos.X, CursorPos.Y);
        Exit;
      end;


    if (Button = mbLeft) and FTitleArrow and (Cell.X = 0) and (Cell.Y = 0)  then
       FTitleArrowDown := True
    else
      inherited MouseDown(Button, Shift, X, Y);
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.MouseMove(Shift: TShiftState; x, y: integer);
var
  gc: TGridCoord;
begin
  inherited;
  gc := MouseCoord(x, y);
  fMouseOverRow := gc.Y;
end;


procedure TGEMDBGridExtended.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
                            Y: Integer);
var
  Cell: TGridCoord;
begin
  try
    Cell := MouseCoord(X, Y);

    if FTitleArrowDown and (Button = mbLeft) then begin
      FTitleArrowDown := False;
      if FTitleArrow and (Cell.X = 0) and (Cell.Y = 0) and (Columns.Count > 0) then
        ShowSelectColumnClick; // Selection of columns
    end
    else
      inherited MouseUp(Button, Shift, X, Y);
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.OnCalendarCancelBtnClick(Sender: TObject);
begin
  FreeAndNil(DateFieldCalendar);
end;


procedure TGEMDBGridExtended.OnCalendarOkBtnClick(Sender: TObject);
var
  theDate: TDate;
begin
  theDate := DateFieldCalendar.Date;
  Self.SelectedField.Dataset.Edit;
  Self.SelectedField.AsDateTime := theDate;
  Self.SelectedField.Dataset.Post;
  FreeAndNil(DateFieldCalendar);
end;


procedure TGEMDBGridExtended.SaveBoolean;
begin
  Self.SelectedField.Dataset.Edit;
  Self.SelectedField.AsBoolean := not Self.SelectedField.AsBoolean;
  //Self.SelectedField.Dataset.Post;
end;


procedure TGEMDBGridExtended.Scroll(Distance: Integer);
begin
  if AltRowColorUse and FixedBackground then
  begin
    inherited Scroll(0);     // In this case we have to repaint all the grid !!
    Invalidate;
  end
  else
    inherited Scroll(Distance);
end;


procedure TGEMDBGridExtended.SetAlternateRowColor(const Value: TColor);
begin
  if FAlternateRowColor <> Value then
  begin
    FAlternateRowColor := Value;
    Invalidate;
  end;
end;


procedure TGEMDBGridExtended.SetAltFontColor(Value: TColor);
begin
  if FAlternateRowFontColor <> Value then begin
    FAlternateRowFontColor := Value;
    Invalidate;
  end;
end;


procedure TGEMDBGridExtended.SetCheckBoxes(Value: Boolean);
begin
  FBooleanFieldCheckBox := Value;
  Invalidate;
end;


procedure TGEMDBGridExtended.SetFileName(const Value: string);
begin
  FColFileName := Value;
  FSaveGridLayout := FColFileName <> '';
end;


procedure TGEMDBGridExtended.SetHighLightSelectedRow(const Value: TSelectedRowHighLight);
begin
  fHighLightSelectedRow.assign(Value);
end;

//procedure TGemDBGrid.SetHighLigthSelectedRow(const Value: boolean);
//begin
//  fHighLightSelectedRow := Value;
//  repaint;
//end;
//
procedure TGEMDBGridExtended.SetMouseOverRow(const Value: integer);
begin
  if fMouseOverRow <> Value then begin
    fMouseOverRow := Value;
    Repaint;
  end;
end;

//procedure TGemDBGrid.SetMultiSelect(Value: Boolean);
//begin
////
//end;
//
//
procedure TGEMDBGridExtended.SetScrollBars(Value: System.UITypes.TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
    // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
    if Value in [ssVertical, ssBoth] then
      Value := ssHorizontal;

    if Value = inherited ScrollBars then
      RecreateWnd
    else
      inherited ScrollBars := Value;

    if (FScrollBars = ssVertical) and HandleAllocated then
      ShowScrollBar(Handle, SB_HORZ, False);
  end;
end;


procedure TGEMDBGridExtended.SetTitleArrow(const Value: Boolean);
begin
  if FTitleArrow <> Value then
  begin
    FTitleArrow := Value;
    Invalidate;
  end;
end;


procedure TGEMDBGridExtended.SetUseAltRowColor(const Value: Boolean);
begin
  FAltRowColorUse := Value;
  Invalidate;
end;


procedure TGEMDBGridExtended.ShowColumnsDialog;
var
  thePoint: TPoint;
begin
    ColSelEditor :=  TGEMDBGridEditForm.Create(nil);
    try
      thePoint.X := Self.Left;
      thePoint.Y := Self.Top;
      thePoint := Self.ClientToScreen(thePoint);

      ColSelEditor.theColumns        := Self.Columns;
      ColSelEditor.GridComponentName := Self.Name;

      ColSelEditor.left := thePoint.X - Self.Left;
      ColSelEditor.top := thePoint.Y - Self.Top;
      ColSelEditor.Height := Self.Height;

      ColSelEditor.CreateDataGridFieldVisible;

      ColSelEditor.ShowModal;
    finally
      ColSelEditor.free;
    end;
  Invalidate;
end;


procedure TGEMDBGridExtended.ShowSelectColumnClick;
begin
  ShowColumnsDialog;
end;


procedure TGEMDBGridExtended.UpdateScrollBar;
begin
  if HandleAllocated then
  begin
    // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
    if not (FScrollBars in [ssNone, ssHorizontal]) then
      inherited UpdateScrollBar;
    if FScrollBars = ssVertical then
      ShowScrollBar(Handle, SB_HORZ, False);

    // UpdateScrollBar is the only virtual method that is called from TDBGrid.DataChanged
    if FAlwaysShowEditor and ([dgRowSelect, dgEditing] * Options = [dgEditing]) and
       Focused then
    begin
      ShowEditor;
      InvalidateCol(Col);
    end;
  end;
end;


procedure TGEMDBGridExtended.WmBeforeClose(var Msg: TMessage);
begin
  try
    if FSaveGridLayout then begin
      if fColFileName <> '' then
        try
          Self.Columns.SaveToFile(fColFileName);
        except
          on e: exception do
            ShowMessage(E.Message);
        end;
  end;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.WmBeforeCreate(var Msg: TMessage);
begin
  try
    if FSaveGridLayout then begin
      if FileExists(fColFileName) then
        try
          Self.Columns.LoadFromFile(fColFileName);
        except
          on e: exception do
            ShowMessage(E.Message);
        end;
    end;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;


procedure TGEMDBGridExtended.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  try
    if (FTitleArrow) and (ARow = 0) and (ACol = 0) and (dgIndicator in Options) and (dgTitles in Options) then  begin
        Canvas.Draw(ARect.Right - 15, ARect.Bottom - 18, theTitleArrowGlyph);
    end
    else
      inherited;

  except
    on e: exception do
      ShowMessage(E.Message);
  end;
end;

{ TSelectedRowHighLight }

procedure TSelectedRowHighLight.Assign(Source: TPersistent);
begin
  if Source is TSelectedRowHighLight then  begin
    fFontColor := TSelectedRowHighLight(Source).fFontColor;
    fBrushColor := TSelectedRowHighLight(Source).fBrushColor;
    fHighSelectedRow := TSelectedRowHighLight(Source).fHighSelectedRow;
  end
  else
    inherited;
end;

end.
