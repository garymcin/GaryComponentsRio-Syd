unit GEMDBGridExtendedNew;
  {.$DEFINE USE_CODESITE}
  {.$DEFINE GRID_DEBUG}

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.UITypes, System.Types,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms, VCL.Grids, VCL.DBGrids,
  Vcl.Menus, Vcl.Buttons, Vcl.Dialogs,

  Data.DB,

  GEMCalendar, GEMDBGridEdit, GEMComponentsGlobal;

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

  TGEMDBExGrid = class(TDBGrid)
  private
//    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FMouseOver: Boolean;
//    FHintWindowClass: THintWindowClass;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT; overload;
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: TObject): LRESULT; overload;
    function BaseWndProcEx(Msg: Cardinal; WParam: WPARAM; var StructLParam): LRESULT;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure FocusChanged(AControl: TWinControl); dynamic;
//    procedure VisibleChanged; reintroduce; dynamic;
//    procedure EnabledChanged; reintroduce; dynamic;
//    procedure TextChanged; reintroduce; virtual;
//    procedure ColorChanged; reintroduce; dynamic;
//    procedure FontChanged; reintroduce; dynamic;
//    procedure ParentFontChanged; reintroduce; dynamic;
//    procedure ParentColorChanged; reintroduce; dynamic;
//    procedure ParentShowHintChanged; reintroduce; dynamic;
//    function WantKey(Key: Integer; Shift: TShiftState): Boolean; virtual;
//    function HintShow(var HintInfo: THintInfo): Boolean; reintroduce; dynamic;
//    function HitTest(X, Y: Integer): Boolean; reintroduce; virtual;
    procedure MouseEnter(AControl: TControl); reintroduce; dynamic;
    procedure MouseLeave(AControl: TControl); reintroduce; dynamic;
    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property HintColor: TColor read FHintColor write FHintColor default clDefault;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  private

    FHintWindowClass: THintWindowClass;  public
    constructor Create(AOwner: TComponent); override;
    property HintWindowClass: THintWindowClass read FHintWindowClass write FHintWindowClass;
  published
//    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  private
    FDotNetHighlighting: Boolean;
  protected
    procedure BoundsChanged; reintroduce; virtual;
    procedure CursorChanged; reintroduce; dynamic;
    procedure ShowingChanged; reintroduce; dynamic;
    procedure ShowHintChanged; reintroduce; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); reintroduce; dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); reintroduce; dynamic;
//    procedure GetDlgCode(var Code: TDlgCodes); virtual;
    procedure FocusSet(PrevWnd: THandle); virtual;
    procedure FocusKilled(NextWnd: THandle); virtual;
    function DoEraseBackground(Canvas: TCanvas; Param: LPARAM): Boolean; virtual;
//  {$IFDEF JVCLThemesEnabledD6}
//  private
//    function GetParentBackground: Boolean;
//  protected
//    procedure SetParentBackground(Value: Boolean); virtual;
//    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
//  {$ENDIF JVCLThemesEnabledD6}
  published
    property DotNetHighlighting: Boolean read FDotNetHighlighting write FDotNetHighlighting default False;
  end;

  TErrorEvent  = procedure(Sender: TObject; const aStrParam: string) of object;
  TStatusEvent = procedure(Sender: TObject; const aStrParam: string) of object;

  TGEMDBGridExtendedNew = class(TGEMDBExGrid)
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
    fOnError                : TErrorEvent;
    fOnStatus               : TStatusEvent;
    fSectionName            : String;
    FCell                   : TGridCoord; // currently selected cell
    FPostOnEnterKey         : Boolean;

    dblClicked      : boolean;
    originalRowHeight,
    lastRowHeight: integer;
    lastResizedRow: integer;
    myLeftCol: integer; // grid left column

    lastMouseX, lastMouseY: integer;
    grBmpTitle: TBitmap;
    grBmpActive: TBitmap;
    grBmpSelected: TBitmap;
    grBmpAlt1: TBitmap;
    grBmpAlt2: TBitmap;
    tempFont:TFont;

    bmpClipped,
    bmpDrawText:TBitmap;

    pmCommands:TPopupMenu;
    lastSearchStr: string;
    lastSearchColumn: TColumn;
    lastEditboxWndProc: TWndMethod;
    searchVisible, filtering: boolean;
    edtSearchCriteria: TEdit;
    searchFieldName: string;
    lastSortColumn: TColumn;
//    lastSortType: TSortType;
    lastRowCount: integer;
//    ri: array of TRowInfo;

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
    procedure WmEnable(var Msg: TMessage); message WM_ENABLE;
    procedure TitleClick(Column: TColumn); override;
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
    procedure SaveColumnsLayout(const Section: string);
    procedure RestoreColumnsLayout(const Section: string);
    function WordPosition(const N: Integer; const S: string;
                          const WordDelims: TSysCharSet): Integer;
    function ExtractWord(N: Integer; const S: string;
                         const WordDelims: TSysCharSet): string;

//    procedure DefineProperties(Filer: TFiler); override;
//    procedure ReadPostOnEnter(Reader: TReader);
//    procedure ReadAlternateRowColor(Reader: TReader);
//    procedure ReadAlternateRowFontColor(Reader: TReader);
//    function ActiveRowSelected: Boolean;
//    procedure WriteCellText(ARect: TRect; DX, DY: Integer; const Text: string;
//      Alignment: TAlignment; ARightToLeft: Boolean; FixCell: Boolean; Options: Integer = 0);
//    procedure CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  public
    destructor Destroy; override;
    constructor Create(AOwner:TComponent); override;

    procedure autoFitAll;
    procedure saveConfig(fn: String);
    procedure loadConfig(fn: String);

    procedure ErrorMsg(const msg: string);
    procedure StatusMsg(const msg: string);
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); virtual;
    procedure DefaultDataCellDraw(const Rect: TRect; Field: TField; State: TGridDrawState);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseWheelHandler(var Message: TMessage); override;

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
//    property AutoWidthAllColor: TColor read FAutoWidthAllColor
//      write SetAutoWidthAllColor default clBlue;
//    property OnBeforeFilter: TFilterEvent read FOnBeforeFilter
//      write SetOnBeforeFilter;
//    property AllowSort: boolean read FAllowSort write SetAllowSort default true ;
//    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
//
//        { ReadOnlyCellColor: The color of the cells that are read only => OnCanEditCell, not Field.CanModify }
//    property ReadOnlyCellColor: TColor read FReadOnlyCellColor write FReadOnlyCellColor default clDefault;

//    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
//    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property PostOnEnterKey: Boolean read FPostOnEnterKey write FPostOnEnterKey default False;
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
    property OnStatus       : TStatusEvent read fOnStatus write fOnStatus;
    property OnError        : TErrorEvent read fOnError write fOnError;

  end;

//procedure Register;

implementation

{.$R Button.res}
uses
  System.Math, System.IniFiles;

const
  TXT_MARG: TPoint = (x: 8; y: 2);
  BTN_WIDTH = 12;

type
  TBookmarks = class(TBookmarkList);


//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMDBGridExtended]);
//end;

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then begin
    Result := Component.Name;
    // only works with 2005 and greater
    F := GetParentForm(TControl(Component), false);
    if F <> nil then
      Result := Format('%s\%s',[F.ClassName, Result])
    else begin
      if TControl(Component).Parent <> nil then
        Result := Format('%s\%s',[TControl(Component).Parent.Name, Result]);
    end;
  end
  else
    result := '';
end;


{ TGEMDBGridExtended }


constructor TGEMDBGridExtendedNew.Create(AOwner: TComponent);
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

  fSectionName := GetDefaultSection(self);//Name+'\'+fDBGrid.Name;
  StatusMsg('Create: '+fSectionName);
  if Assigned(DataSource) and FSaveGridLayout then begin
    try
      RestoreColumnsLayout(fSectionName);
    except
      on err:exception do
        ErrorMsg('Could NOT fine Storage Ini file.')
    end;
  end;

  {$IFDEF  GRID_DEBUG}
  fDoFileCellTest := false;
  {$ENDIF}
end;


procedure TGEMDBGridExtendedNew.CreateParams(var Params: TCreateParams);
begin
  inherited;
//
end;

procedure TGEMDBGridExtendedNew.DefaultDataCellDraw(const Rect: TRect;
  Field: TField; State: TGridDrawState);
begin
//
end;

procedure TGEMDBGridExtendedNew.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
//
end;

destructor TGEMDBGridExtendedNew.Destroy;
begin
  if Assigned(theTitleArrowGlyph) then begin
    theTitleArrowGlyph.Free;
  end;
  fHighLightSelectedRow.Free;

  if Assigned(DateFieldCalendar) then
    FreeAndNil(DateFieldCalendar);
  inherited;
end;


function TGEMDBGridExtendedNew.GetVersion: string;
begin
  Result := '1.0';
end;



procedure TGEMDBGridExtendedNew.loadConfig(fn: String);
begin

end;

{$IFDEF GRID_DEBUG}
procedure TGEMDBGridExtendedNew.SetDoFileCellTest(const Value: Boolean);
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


procedure TGEMDBGridExtendedNew.CMHintShow(var Msg: TCMHintShow);
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


procedure TGEMDBGridExtendedNew.CMMouseEnter(var Message: TMessage);
begin
 //
end;

procedure TGEMDBGridExtendedNew.CMMouseLeave(var Message: TMessage);
begin
 //
end;

procedure TGEMDBGridExtendedNew.SetDatePullDownDateFields(Value: Boolean);
begin
  FDatePullDownDateFields := Value;
  Invalidate;
end;


procedure TGEMDBGridExtendedNew.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;


procedure TGEMDBGridExtendedNew.SetupCalendarForDateField(theField: TField; CLeft, CTop: integer);
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


procedure TGEMDBGridExtendedNew.autoFitAll;
begin

end;

procedure TGEMDBGridExtendedNew.CellClick(Column: TColumn);
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


procedure TGEMDBGridExtendedNew.DrawColumnCell(const Rect: TRect; DataCol: Integer;
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
function TGEMDBGridExtendedNew.GetBtnRect(CellRect: TRect; complete: boolean): TRect;

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


procedure TGEMDBGridExtendedNew.ColEnter;
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


procedure TGEMDBGridExtendedNew.ColExit;
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


procedure TGEMDBGridExtendedNew.AltColorsSet;
begin
//
end;


function TGEMDBGridExtendedNew.GetTitleOffset: Integer;
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


procedure TGEMDBGridExtendedNew.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
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


procedure TGEMDBGridExtendedNew.MouseMove(Shift: TShiftState; x, y: integer);
var
  gc: TGridCoord;
begin
  inherited;
  gc := MouseCoord(x, y);
  fMouseOverRow := gc.Y;
end;


procedure TGEMDBGridExtendedNew.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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


procedure TGEMDBGridExtendedNew.MouseWheelHandler(var Message: TMessage);
var
  LastRow: Integer;
begin
  LastRow := Row;
  inherited MouseWheelHandler(Message);
  if (Row <> LastRow) and (DataLink <> nil) and DataLink.Active then
    InvalidateCell(IndicatorOffset - 1, LastRow);
end;



procedure TGEMDBGridExtendedNew.OnCalendarCancelBtnClick(Sender: TObject);
begin
  FreeAndNil(DateFieldCalendar);
end;


procedure TGEMDBGridExtendedNew.OnCalendarOkBtnClick(Sender: TObject);
var
  theDate: TDate;
begin
  theDate := DateFieldCalendar.Date;
  Self.SelectedField.Dataset.Edit;
  Self.SelectedField.AsDateTime := theDate;
  Self.SelectedField.Dataset.Post;
  FreeAndNil(DateFieldCalendar);
end;


procedure TGEMDBGridExtendedNew.SaveBoolean;
begin
  Self.SelectedField.Dataset.Edit;
  Self.SelectedField.AsBoolean := not Self.SelectedField.AsBoolean;
  //Self.SelectedField.Dataset.Post;
end;


procedure TGEMDBGridExtendedNew.Scroll(Distance: Integer);
begin
  if AltRowColorUse and FixedBackground then
  begin
    inherited Scroll(0);     // In this case we have to repaint all the grid !!
    Invalidate;
  end
  else
    inherited Scroll(Distance);
end;


procedure TGEMDBGridExtendedNew.SetAlternateRowColor(const Value: TColor);
begin
  if FAlternateRowColor <> Value then
  begin
    FAlternateRowColor := Value;
    Invalidate;
  end;
end;


procedure TGEMDBGridExtendedNew.SetAltFontColor(Value: TColor);
begin
  if FAlternateRowFontColor <> Value then begin
    FAlternateRowFontColor := Value;
    Invalidate;
  end;
end;


procedure TGEMDBGridExtendedNew.SetCheckBoxes(Value: Boolean);
begin
  FBooleanFieldCheckBox := Value;
  Invalidate;
end;


procedure TGEMDBGridExtendedNew.SetFileName(const Value: string);
begin
  FColFileName := Value;
  FSaveGridLayout := FColFileName <> '';
end;


procedure TGEMDBGridExtendedNew.SetHighLightSelectedRow(const Value: TSelectedRowHighLight);
begin
  fHighLightSelectedRow.assign(Value);
end;

//procedure TGemDBGrid.SetHighLigthSelectedRow(const Value: boolean);
//begin
//  fHighLightSelectedRow := Value;
//  repaint;
//end;
//
procedure TGEMDBGridExtendedNew.SetMouseOverRow(const Value: integer);
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
procedure TGEMDBGridExtendedNew.SetScrollBars(Value: System.UITypes.TScrollStyle);
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


procedure TGEMDBGridExtendedNew.SetTitleArrow(const Value: Boolean);
begin
  if FTitleArrow <> Value then
  begin
    FTitleArrow := Value;
    Invalidate;
  end;
end;


procedure TGEMDBGridExtendedNew.SetUseAltRowColor(const Value: Boolean);
begin
  FAltRowColorUse := Value;
  Invalidate;
end;


procedure TGEMDBGridExtendedNew.ShowColumnsDialog;
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


procedure TGEMDBGridExtendedNew.ShowSelectColumnClick;
begin
  ShowColumnsDialog;
end;


procedure TGEMDBGridExtendedNew.UpdateScrollBar;
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


procedure TGEMDBGridExtendedNew.WmBeforeClose(var Msg: TMessage);
begin
    StatusMsg('in WmBeforeClose');
  if (Assigned(DataSource)) and (FSaveGridLayout) then begin
    if fColFileName <> '' then  begin
      try
        SaveColumnsLayout(fSectionName);
      except
        on err:exception do
          ErrorMsg('Could not Save Col Save File');
      end;
    end
    else
      ErrorMsg('No Storage file name.')
  end;


//  try
//    if FSaveGridLayout then begin
//      if fColFileName <> '' then
//        try
//          Self.Columns.SaveToFile(fColFileName);
//        except
//          on e: exception do
//            ShowMessage(E.Message);
//        end;
//  end;
//  except
//    on e: exception do
//      ShowMessage(E.Message);
//  end;
end;


procedure TGEMDBGridExtendedNew.WmBeforeCreate(var Msg: TMessage);
begin
  fSectionName := GetDefaultSection(self);//Name+'\'+fDBGrid.Name;
  StatusMsg('in WmBeforeCreate');

  if Assigned(DataSource) and FSaveGridLayout then begin
//    if FileExists(fColFileName) then
    try
      BeginLayout;
      RestoreColumnsLayout(fSectionName);
      EndLayout;
    except
      on err:exception do
        ErrorMsg('Could NOT fine Storage Ini file.')
    end;
  end;

//  try
//    if FSaveGridLayout then begin
//      if FileExists(fColFileName) then
//        try
//          Self.Columns.LoadFromFile(fColFileName);
//        except
//          on e: exception do
//            ShowMessage(E.Message);
//        end;
//    end;
//  except
//    on e: exception do
//      ShowMessage(E.Message);
//  end;
end;


procedure TGEMDBGridExtendedNew.WmEnable(var Msg: TMessage);
begin
  StatusMsg('in WmEnable');
  fSectionName := GetDefaultSection(self);//Name+'\'+fDBGrid.Name;
//  if  fSectionName = '' then
//      fSectionName := Name+'\'+Self.Name;
//  fSectionName := Parent.ClassName+'\'+fDBGrid.Name;

  if Assigned(DataSource) and FSaveGridLayout then begin
//    if FileExists(fColFileName) then
    try
      BeginLayout;
      RestoreColumnsLayout(fSectionName);
      EndLayout;

    except
      on err:exception do
        ErrorMsg('Could NOT fine Storage Ini file.')
    end;
  end;
end;

procedure TGEMDBGridExtendedNew.WMPaint(var Message: TWMPaint);
begin

end;

procedure TGEMDBGridExtendedNew.ErrorMsg(const msg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, msg);
end;


procedure TGEMDBGridExtendedNew.StatusMsg(const msg: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, msg);
end;


procedure TGEMDBGridExtendedNew.TitleClick(Column: TColumn);
var
  SavePlace: TBookmark;
  plc      : integer; // previous left column
begin
  inherited;

//  if dblClicked then
//  begin
//    // ignore DblClicks
//    dblClicked := false;
//    Exit;
//  end;
//
//  if not FAllowSort then
//    Exit;
//
//  if not checkDBPrerequisites then
//    Exit;
//
//  if not(DataSource.DataSet is TCustomADODataSet) then
//    Exit;
//
////  playSoundInMemory(FSortSoundEnabled, sndSort, 'Sort');
//
//  plc := leftCol;
//  SavePlace := DataSource.DataSet.GetBookmark;
//
  try

//    if lastSortColumn <> Column then
//    begin
//      // new column to sort
//      lastSortColumn := Column;
//      lastSortType := stAsc;
//
//      TCustomADODataSet(DataSource.DataSet).Sort := '[' + Column.FieldName
//          + '] ASC';
//    end
//    else
//    begin
//      // reverse sort order
//      if lastSortType = stAsc then
//      begin
//        lastSortType := stDesc;
//        TCustomADODataSet(DataSource.DataSet).Sort := '[' + Column.FieldName
//            + '] DESC';
//      end
//      else
//      begin
//        lastSortType := stAsc;
//        TCustomADODataSet(DataSource.DataSet).Sort := '[' + Column.FieldName
//            + '] ASC';
//      end;
//    end;

  except
      showmessage('Error in sorting !');
  end;


//  if DataSource.DataSet.BookmarkValid(SavePlace) then
//  begin
//    DataSource.DataSet.GotoBookmark(SavePlace);
//    DataSource.DataSet.FreeBookmark(SavePlace);
//  end;
//  leftCol := plc;
end;


//procedure TGEMDBGridExtendedNew.toggleTransparentColor;
//begin
//  if (not Assigned(bmpClipped)) or (not Assigned(bmpDrawText)) then
//    Exit;
//  try
//    if (bmpDrawText.TransparentColor and $ffffff)=clWhite then
//    begin
//      bmpDrawText.TransparentColor:=clBlack;
//      bmpDrawText.Canvas.Brush.Color:=clBlack;
//      bmpClipped.TransparentColor:=clBlack;
//      bmpClipped.Canvas.Brush.Color:=clBlack;
//    end
//    else
//    begin
//      bmpDrawText.TransparentColor:=clWhite;
//      bmpDrawText.Canvas.Brush.Color:=clWhite;
//      bmpClipped.TransparentColor:=clWhite;
//      bmpClipped.Canvas.Brush.Color:=clWhite;
//    end;
//  except
//    OutputDebugString('Error in toggling transparent color.');
//  end;
//end;

function TGEMDBGridExtendedNew.ExtractWord(N: Integer; const S: string;
                                       const WordDelims: TSysCharSet): string;

{$REGION 'SOURCE'}
// Next two routines
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJCLUtils.pas, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Andreas Hausladen
  Ralf Kaiser
  Vladimir Gaitanoff
  Dejoy den

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

-----------------------------------------------------------------------------}
{$ENDREGION}
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
    begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;


function TGEMDBGridExtendedNew.WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin
    { skip over delimiters }
    while (I <= Length(S)) and CharInSet(S[I], WordDelims) do
      Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then
      Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;


//procedure TGEMDBGridExtendedNew.Resize;
//begin
//  inherited;
//
//end;
//
procedure TGEMDBGridExtendedNew.RestoreColumnsLayout(const Section: string);

  function MyBoolToStr(aBool: Boolean): string;
  begin
    if aBool then
      Result := 'true'
    else
      Result := 'false';
  end;

const
  Delims = [' ', ','];
type
  TColumnInfo = record
    Column: TColumn;
    EndIndex: Integer;
  end;
  TColumnArray = array of TColumnInfo;
var
  I, J: Integer;
  SectionName,
  S          : string;
  ColumnArray: TColumnArray;
  fIniFile   : TIniFile;
begin
  if Section <> '' then
    SectionName := Section
  else begin
    ErrorMsg('Not Section Name found.');
    Exit;
  end;

//  StatusMsg(SectionName+ ' ==============================');

  fIniFile := TIniFile.Create(fColFileName);
  try
    try
    {$REGION 'DOC'}
      {
       This is taken from Jedi's dbgrid routines

       The ColumnArray are objects from the actual grid. When a value is changed
       in array, the change is reflected in the DbGrid.  Since there can not
       be columns in the grid with the same index value. Placing a column in the
       array with the same index a another columns will cause that column's
       index to be incremented. And then the next column that may have the
       increment columns index value.
      }
    {$ENDREGION}
      SetLength(ColumnArray, Self.Columns.Count);
    StatusMsg('in RestoreColumnsLayout');
      for I := 0 to Columns.Count - 1 do begin
        S := fIniFile.ReadString(SectionName, format('%s.%s', [Name,
                                 Columns.Items[I].FieldName]),
                                 '');
        StatusMsg('Section Data: ' + S + ' ==========================');

        ColumnArray[I].Column   := Columns.Items[I];
        ColumnArray[I].EndIndex := Columns.Items[I].Index;
        StatusMsg(Format('   ColunmName -- Column/Index: %s -- %s/%s',
                  [Columns.Items[I].FieldName, IntToStr(i),
                  IntToStr(ColumnArray[I].EndIndex)]));
        if S <> '' then begin
          ColumnArray[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
                                     ColumnArray[I].EndIndex);
          S := ExtractWord(2, S, Delims);
          Columns.Items[I].Width := StrToIntDef(S,
                                                Columns.Items[I].Width);
          Columns.Items[I].Visible := (S <> '-1');

          StatusMsg(Format('S <> '''' Field -- Width/Visible: %s -- %s/%s  Index = %s',
                    [Columns.Items[I].FieldName ,
                    IntToStr(Columns.Items[I].Width),
                    MyBoolToStr(Columns.Items[I].Visible),
                    IntToStr(ColumnArray[I].EndIndex)]));
        end
        else
          StatusMsg(Format('S = '''' Field -- Width/Visible: %s -- %s/%s  Index = %s',
                    [Columns.Items[I].FieldName ,
                    IntToStr(Columns.Items[I].Width),
                    MyBoolToStr(Columns.Items[I].Visible),
                    IntToStr(ColumnArray[I].EndIndex)]));

      end;

//      StatusMsg('======================');

      for I := 0 to Columns.Count - 1 do begin
        for J := 0 to Columns.Count - 1 do begin
          if ColumnArray[J].EndIndex = I then begin
            ColumnArray[J].Column.Index := ColumnArray[J].EndIndex;
            Break;
          end;
        end;
      end;

//      for i := 0 to Columns.Count - 1 do
//        InvalidateCol(i);
      Refresh;
      Invalidate;
    finally
      FreeAndNil(fIniFile);
    end;
  except
    ErrorMsg('Storage Ini file could not be Opened/Found')
  end;
end;


//procedure TGEMDBGridExtendedNew.RowHeightsChanged;
//begin
//  inherited;
//
//end;

procedure TGEMDBGridExtendedNew.SaveColumnsLayout(const Section: string);
var
  I: Integer;
  SectionName: string;
  fIniFile   : TIniFile;
begin
  StatusMsg('in SaveColumnsLayout Section = '+Section);
  if Section <> '' then
    SectionName := Section
  else begin
    ErrorMsg('Could not save Column layout = '+Section);
    Exit;
  end;

  StatusMsg('in SaveColumnsLayout File = '+Section);
  fIniFile := TIniFile.Create(fColFileName);
  try
    try
      fIniFile.EraseSection(SectionName);
      for I := 0 to Columns.Count - 1 do
        fIniFile.WriteString(SectionName, format('%s.%s', [Name, Columns.Items[I].FieldName]),
          Format('%d,%d', [Columns.Items[I].Index, Columns.Items[I].Width]));
        StatusMsg(Format('SaveColumnsLayout %s.%s --- Columns Index/Width %n/%n',[Name, Columns.Items[I].FieldName,
                          Columns.Items[I].Index, Columns.Items[I].Width]));
    finally
      FreeAndNil(fIniFile);
    end;
  except
    ErrorMsg('Storage Ini file could not be Created')
  end;
end;


procedure TGEMDBGridExtendedNew.saveConfig(fn: String);
begin

end;

procedure TGEMDBGridExtendedNew.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
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

{ TGEMDBExGrid }

function TGEMDBExGrid.BaseWndProc(Msg: Cardinal; WParam: WPARAM;
  LParam: LPARAM): LRESULT;
begin
 //
end;

function TGEMDBExGrid.BaseWndProc(Msg: Cardinal; WParam: WPARAM;
  LParam: TObject): LRESULT;
begin
//
end;

function TGEMDBExGrid.BaseWndProcEx(Msg: Cardinal; WParam: WPARAM;
  var StructLParam): LRESULT;
begin
//
end;

procedure TGEMDBExGrid.BoundsChanged;
begin
//
end;

procedure TGEMDBExGrid.ControlsListChanged(Control: TControl;
  Inserting: Boolean);
begin
//
end;

procedure TGEMDBExGrid.ControlsListChanging(Control: TControl;
  Inserting: Boolean);
begin
  //
end;

constructor TGEMDBExGrid.Create(AOwner: TComponent);
begin
  inherited;
    //
end;

procedure TGEMDBExGrid.CursorChanged;
begin
//
end;

function TGEMDBExGrid.DoEraseBackground(Canvas: TCanvas;
  Param: LPARAM): Boolean;
begin
//
end;

procedure TGEMDBExGrid.FocusChanged(AControl: TWinControl);
begin
//
end;

procedure TGEMDBExGrid.FocusKilled(NextWnd: THandle);
begin
//
end;

procedure TGEMDBExGrid.FocusSet(PrevWnd: THandle);
begin
//
end;

procedure TGEMDBExGrid.MouseEnter(AControl: TControl);
begin
  //
end;

procedure TGEMDBExGrid.MouseLeave(AControl: TControl);
begin
    //
end;

procedure TGEMDBExGrid.ShowHintChanged;
begin
//
end;

procedure TGEMDBExGrid.ShowingChanged;
begin
//
end;

procedure TGEMDBExGrid.WndProc(var Msg: TMessage);
begin
  inherited;
//
end;

end.
