unit GEMDBGrid;
  {.$DEFINE USE_CODESITE}
  {.$DEFINE GRID_DEBUG}

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.UITypes, System.Types,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms, VCL.Grids, VCL.DBGrids,
  Vcl.Menus, Vcl.Buttons, Vcl.Dialogs,

  Data.DB,

  GEMCalendar, GEMDBGridEdit, GEMComponentsGlobal{,

  CodeSiteLogging};

const
  bmArrow = 'DBGARROW2';
  bmEdit = 'DBEDIT2';
  bmInsert = 'DBINSERT2';
  bmMultiDot = 'DBMULTIDOT2';
  bmMultiArrow = 'DBMULTIARROW2';
  KeyboardShiftStates = [ssShift, ssAlt, ssCtrl];

type
  TgemDBGrid = class;

  TgemDBGridBitmap = class(TBitmap)
  end;

  TMyFieldModEvent = Procedure(AField:TField) of Object ;

  TDBGridCellHintPosition = (gchpDefault, gchpMouse);

  TTitleHintEvent = procedure(Sender: TObject; Field: TField;
                          var AHint: string; var ATimeOut: Integer) of object;
  //TCustomGridAccess = class(TCustomGrid);
  TGetCellPropsEvent = procedure(Sender: TObject; Field: TField;
                   AFont: TFont; var Background: TColor) of object; { obsolete }
  TGetCellParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TgemDBCanEditCellEvent = procedure(Grid: TGemDBGrid; Field: TField; var AllowEdit: Boolean) of object;


  TCheckTitleBtnEvent = procedure(Sender: TObject; ACol: Longint;
           Field: TField; var Enabled: Boolean) of object;


  TgemDBGridLayoutChangeKind = (lcLayoutChanged, lcSizeChanged, lcTopLeftChanged);
  TgemDBGridLayoutChangeEvent = procedure(Grid: TGemDBGrid; Kind: TgemDBGridLayoutChangeKind) of object;
  TgemDBGridLayoutChangeLink = class
  private
    FOnChange: TgemDBGridLayoutChangeEvent;
  public
    procedure DoChange(Grid: TGemDBGrid; Kind: TgemDBGridLayoutChangeKind);
    property OnChange: TgemDBGridLayoutChangeEvent read FOnChange write FOnChange;
  end;


  TgemGridPaintInfo = record
    MouseInCol: Integer; // the column that the mouse is in
    ColPressed: Boolean; // a column has been pressed
    ColPressedIdx: Integer; // idx of the pressed column
    ColSizing: Boolean; // currently sizing a column
    ColMoving: Boolean; // currently moving a column
  end;



  TgemExDBGrid = class(TDBGrid)
  private
//    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FMouseOver: Boolean;
    FHintWindowClass: THintWindowClass;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT; overload;
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: TObject): LRESULT; overload;
//    function BaseWndProcEx(Msg: Cardinal; WParam: WPARAM; var StructLParam): LRESULT;
  protected
    procedure WndProc(var Msg: TMessage); override;
    procedure FocusChanged(AControl: TWinControl); dynamic;
    procedure VisibleChanged; reintroduce; dynamic;
    procedure EnabledChanged; reintroduce; dynamic;
    procedure TextChanged; reintroduce; virtual;
    procedure ColorChanged; reintroduce; dynamic;
    procedure FontChanged; reintroduce; dynamic;
    procedure ParentFontChanged; reintroduce; dynamic;
    procedure ParentColorChanged; reintroduce; dynamic;
    procedure ParentShowHintChanged; reintroduce; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState): Boolean; virtual;
    function HintShow(var HintInfo: VCL.Controls.THintInfo): Boolean; reintroduce; dynamic;
    function HitTest(X, Y: Integer): Boolean; reintroduce; virtual;
    procedure MouseEnter(AControl: TControl); reintroduce; dynamic;
    procedure MouseLeave(AControl: TControl); reintroduce; dynamic;
    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property HintColor: TColor read FHintColor write FHintColor default clDefault;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
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



//  TGemDBGrid = class(TgemExDBGrid)
  TGemDBGrid = class(TgemExDBGrid)
  private
    fGridInfoFile     : string;
    FDefDrw           : Boolean;
    FScrollBars       : System.UITypes.TScrollStyle;
    FAlwaysShowEditor : Boolean;
    FDisableCount     : Integer;
    FSaveGridLayout   : Boolean;
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
    FCell                   : TGridCoord; // currently selected cell
    FPaintInfo              : TgemGridPaintInfo;
    FReadOnlyCellColor      : TColor;
//    fAltFontColor           : TColor;
    FCurrentDrawRow         : Integer;
    FOnBeforePaint          : TNotifyEvent;
    FOnAfterPaint           : TNotifyEvent;
    FOnGetCellProps         : TGetCellPropsEvent;
    FOnGetCellParams        : TGetCellParamsEvent;
    FOnCanEditCell          : TgemDBCanEditCellEvent;
    FCurrentControl         : TWinControl;
    FShowMemos              : Boolean;
    FWordWrap               : Boolean;
    FWordWrapAllFields      : Boolean;
    FReduceFlicker          : Boolean;

    FAltRowColorUse        : Boolean;
    FAlternateRowColor     : TColor;
    fAltRowFontColor       : TColor;
//    FSelecting             : Boolean;

    //    FOnCanEditCell: TJvDBCanEditCellEvent;

//    function ActiveRowSelected: Boolean;
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

    procedure OnCalendarOkBtnClick(Sender: TObject);
    procedure OnCalendarCancelBtnClick(Sender: TObject);

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMRButtonUp(var Msg: TWMMouse); message WM_RBUTTONUP;
//    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
//    function ReadAlternateRowColor: TColor;
    procedure SetMultiSelect(const Value: Boolean);
//    function DoKeyPress(var Msg: TWMChar): Boolean;
//    function GetRow: Longint;
    procedure SetShowMemos(const Value: Boolean);
    procedure WriteCellText(ARect: TRect; DX, DY: Integer; const Text: string;
          Alignment: TAlignment; ARightToLeft: Boolean; FixCell: Boolean; Options: Integer = 0);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetWordWrapAllFields(Value: Boolean);
//    procedure SetDoFileCellTest(const Value: Boolean);
  protected
    DateFieldCalendar      : TGEMCalendar;
    OriginalOptions        : TDBGridOptions;
    ColSelEditor           : TGEMDBGridEditForm;
    FMultiSelect           : Boolean;
    fVersion               : string;


     //=== text file stuff =============================
     {$IFDEF GRID_DEBUG}
     myFile : TextFile;
     DataStream: TFileStream;
     fDoFileCellTest: Boolean;
     {$ENDIF}
    function CanEditCell(AField: TField): Boolean; virtual;

    //=============================================================
    // overRide stuff =============================================

    procedure UpdateScrollBar; override;
    procedure Scroll(Distance: Integer); override;
    procedure Paint; override;
    procedure ColEnter(); override;
    procedure ColExit(); override;
    procedure CellClick(Column: TColumn); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
                                    Column: TColumn; State: TGridDrawState); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
                                     AState: TGridDrawState): Boolean; override;
    // overRide stuff =============================================
    //=============================================================

    procedure DoDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure SaveBoolean;
    procedure ShowSelectColumnClick; dynamic;
    procedure ShowColumnsDialog;

    function GetTitleOffset: Integer;
    procedure GetCellProps(Column: TColumn; AFont: TFont; var Background: TColor;
                           Highlight: Boolean); dynamic;

    procedure SetScrollBars(Value: System.UITypes.TScrollStyle);
    procedure SetAltRowFontColor(Value: TColor);
    procedure SetAlternateRowColor(const Value: TColor);
    function SetVersion: string;
    procedure SetTitleArrow(const Value: Boolean);
    procedure SetDoFileCellTest(const Value: Boolean);

    procedure AltColorsSet;
    function AcquireFocus: Boolean;
    procedure GridInvalidateRow(Row: Longint);
  public
  {Public}
    destructor Destroy; override;
    constructor Create(AOwner:TComponent); override;

    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); virtual;
    procedure DefaultDataCellDraw(const Rect: TRect; Field: TField; State: TGridDrawState);

    procedure DisableScroll;
    procedure EnableScroll;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property VisibleRowCount;
    property VisibleColCount;
    property TitleOffset: Integer read GetTitleOffset;
    property Canvas;
    property Col;
    property InplaceEditor;
    property LeftCol;
    property SelectedRows;
    {$IFDEF GRID_DEBUG}
    property DoFileCellTest: Boolean read fDoFileCellTest write SetDoFileCellTest default False;
    {$ENDIF}
    property CurrentDrawRow: Integer read FCurrentDrawRow;
    property DataLink;
  published
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property CellHintPosition: TDBGridCellHintPosition read FCellHintPosition write FCellHintPosition default gchpDefault;
    property ShowTitleHint: Boolean read FShowTitleHint write FShowTitleHint default False;
    property BooleanFieldCheckBox: Boolean read FBooleanFieldCheckBox write SetCheckBoxes;
    property GridInfoFile : string read fGridInfoFile write fGridInfoFile;
//    property AltFont: TColor read fAltFont write fAltFont;
    property AltRowFontColor: TColor read fAltRowFontColor write SetAltRowFontColor;
    property AlternateRowColor: TColor read FAlternateRowColor write SetAlternateRowColor;   //: the alternate color of a row.
    property AltRowColorUse: Boolean read FAltRowColorUse write SetUseAltRowColor default false;   //if the component "easy-read" feature is enabled.
    property FixedBackground: Boolean read FDefDrw write FDefDrw default false;  //selects two different kinds of scrolling the colored backgroun
    property ScrollBars: System.UITypes.TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property OnTitleArrowMenuEvent: TNotifyEvent read FOnTitleArrowMenuEvent write FOnTitleArrowMenuEvent;
//    property OnMouseDown;
//    property OnMouseUp;
    property ColSaveFileName: string read fColFileName write SetFileName;
    property TitlePopup: TPopupMenu read FTitlePopup write FTitlePopup;
    //property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ClearSelection: Boolean read FClearSelection write FClearSelection default True;
    property TitleArrow: Boolean read FTitleArrow write SetTitleArrow default False;
    property DatePullDownDateFields: Boolean read FDatePullDownDateFields write SetDatePullDownDateFields default False;
    property OnShowTitleHint: TTitleHintEvent read FOnShowTitleHint write FOnShowTitleHint;
    property TitleHint: string read FTitleHint write FTitleHint;
    property SaveGridLayout: Boolean read FSaveGridLayout write FSaveGridLayout default False;
    property Version: string read SetVersion;
    { ReadOnlyCellColor: The color of the cells that are read only => OnCanEditCell, not Field.CanModify }
    property ReadOnlyCellColor: TColor read FReadOnlyCellColor write FReadOnlyCellColor default clDefault;
    { OnBeforePaint: event triggered before the grid is painted. }
    property OnBeforePaint: TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps write FOnGetCellProps; { obsolete }
    { ShowMemos: if true, memo fields are shown as text }
    property ShowMemos: Boolean read FShowMemos write SetShowMemos default True;
    { WordWrap: if true, titles, memo and string fields are displayed on several lines }
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    { WordWrapAllFields: if true and WordWrap is true, not only memo and string fields are displayed on several lines }
    property WordWrapAllFields: Boolean read FWordWrapAllFields write SetWordWrapAllFields default False;
    { ReduceFlicker: improve (but slow) the display when painting/scrolling ? }
    property ReduceFlicker: Boolean read FReduceFlicker write FReduceFlicker default True;
//    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;

    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns stored False;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property ImeMode;
    property ImeName;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;  { obsolete }
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

//procedure Register;

implementation

{$R Button.res}
uses
  Math;

const
  TXT_MARG: TPoint = (x: 8; y: 2);
  BTN_WIDTH = 12;

type
  TBookmarks = class(TBookmarkList);

  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpObject, gpData,
    gpNotEmpty, gpMarkDown, gpMarkUp, gpChecked, gpUnChecked, gpPopup);

const
  GridBmpNames: array [TGridPicture] of PChar =
  ('JvDBGridBLOB', 'JvDBGridMEMO', 'JvDBGridPICT', 'JvDBGridOLE', 'JvDBGridOBJECT',
   'JvDBGridDATA', 'JvDBGridNOTEMPTY', 'JvDBGridSMDOWN', 'JvDBGridSMUP',
   'JvDBGridCHECKED', 'JvDBGridUNCHECKED', 'JvDBGridPOPUP');


//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGemDBGrid]);
//end;
//

function IsMemoField(AField: TField): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := AField.DataType in [ftMemo {$IFDEF COMPILER10_UP}, ftWideMemo {$ENDIF}];
end;



function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
  CtrlMask = $10000000;
  ShiftMask = $08000000;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( 'ShiftStateToKeyData' );{$ENDIF}

  Result := 0;
  if ssAlt in Shift then
    Result := Result or AltMask;
  if ssCtrl in Shift then
    Result := Result or CtrlMask;
  if ssShift in Shift then
    Result := Result or ShiftMask;
end;


function SmallPointToLong(const Pt: TSmallPoint): Longint;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( 'SmallPointToLong' );{$ENDIF}

  Result := Longint(Pt);
end;


function DispatchIsDesignMsg(Control: TControl; var Msg: TMessage): Boolean;
var
  Form: TCustomForm;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( 'DispatchIsDesignMsg' );{$ENDIF}

  Result := False;
  case Msg.Msg of
    WM_SETFOCUS, WM_KILLFOCUS, WM_NCHITTEST,
    WM_MOUSEFIRST..WM_MOUSELAST,
    WM_KEYFIRST..WM_KEYLAST,
    WM_CANCELMODE:
      Exit; // These messages are handled in TWinControl.WndProc before IsDesignMsg() is called
  end;
  if (Control <> nil) and (csDesigning in Control.ComponentState) then
  begin
    Form := GetParentForm(Control);
    if (Form <> nil) and (Form.Designer <> nil) and Form.Designer.IsDesignMsg(Control, Msg) then
      Result := True;
  end;
end;


function DrawBiDiText(DC: HDC; const Text: string; var R: TRect; Flags: UINT;
  Alignment: TAlignment; RightToLeft: Boolean; CanvasOrientation: TCanvasOrientation): Integer;
const
  AlignFlags: array [TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
  RTL: array [Boolean] of UINT = (0, DT_RTLREADING);
begin
  if CanvasOrientation = coRightToLeft then
    ChangeBiDiModeAlignment(Alignment);
  Result := WinApi.Windows.DrawText(DC, PChar(Text), Length(Text), R,
    AlignFlags[Alignment] or RTL[RightToLeft] or Flags);
end;


procedure CreateWMMessage(var Mesg: TMessage; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( 'CreateWMMessage' );{$ENDIF}

  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
end;


{ TDBGridGEM }


constructor TGemDBGrid.Create(AOwner: TComponent);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'Create' );{$ENDIF}

  inherited Create(AOwner);
  fGridInfoFile := '';
  FScrollBars := ssBoth;
  FAlwaysShowEditor := True;
//  FDisableCount := True;
  FClearSelection := True;
  fColFileName := '';
  FTitleArrowDown := False;
  theTitleArrowGlyph := TBitmap.Create;
  FInMouseClick := False;
  theTitleArrowGlyph.LoadFromResourceName(HInstance, 'ARROWDOWN');
  {$IFDEF  GRID_DEBUG}
  fDoFileCellTest := false;
  {$ENDIF}
  FPaintInfo.ColPressed := False;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
  FPaintInfo.ColMoving := False;
  FPaintInfo.ColSizing := False;
  FCell.X := -1;
  FCell.Y := -1;
  FReduceFlicker := True;
end;


procedure TGemDBGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
  if FScrollBars = ssVertical then
    Params.Style := Params.Style and not WS_HSCROLL;
end;


procedure TGemDBGrid.DefaultDataCellDraw(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  DefaultDrawDataCell(Rect, Field, State);
end;

procedure TGemDBGrid.DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
var
  MemoText: string;
begin
  if Assigned(Column.Field) and
    (WordWrapAllFields or (Column.Field is TStringField) or (ShowMemos and IsMemoField(Column.Field))) then
  begin
    MemoText := Column.Field.DisplayText;
    if FShowMemos and IsMemoField(Column.Field) then
    begin
      // The MemoField's default DisplayText is '(Memo)' but we want the content
      if not Assigned(Column.Field.OnGetText) then
        MemoText := Column.Field.AsString;
    end;
    WriteCellText(Rect, 2, 2, MemoText, Column.Alignment,
      UseRightToLeftAlignmentForField(Column.Field, Column.Alignment), False);
  end
  else
//    if GetImageIndex(Column.Field) < 0 then  // Mantis 5013: Must not call inherited drawer, or the text will be painted over
//    begin
////    if DrawThemedHighlighting(Canvas, Rect) then
////      Canvas.Brush.Style := bsClear;
      inherited DefaultDrawColumnCell(Rect, DataCol, Column, State);
//    end;
end;

destructor TGemDBGrid.Destroy;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'Destroy' );{$ENDIF}

  if Assigned(theTitleArrowGlyph) then begin
    theTitleArrowGlyph.Free;
  end;
  if Assigned(DateFieldCalendar) then
    FreeAndNil(DateFieldCalendar);

  inherited;
end;


procedure TGemDBGrid.DisableScroll;
begin
  Inc(FDisableCount);
end;

function TGemDBGrid.SetVersion: string;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetVersion' );{$ENDIF}

  Result := VersionGEMDBGrid;
end;


procedure TGemDBGrid.SetWordWrapAllFields(Value: Boolean);
begin
  if Value <> FWordWrapAllFields then
  begin
    FWordWrapAllFields := Value;
    if WordWrap then
      Invalidate;
  end;
end;



procedure TGemDBGrid.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;


{$IFDEF GRID_DEBUG}
procedure TGemDBGrid.SetDoFileCellTest(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetDoFileCellTest' );{$ENDIF}

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


procedure TGemDBGrid.CMHintShow(var Msg: TCMHintShow);
const
  C_TIMEOUT = 250;
var
  ACol, ARow, ATimeOut: Integer;
  AtCursorPosition: Boolean;
  InitialMousePos: TPoint;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'CMHintShow' );{$ENDIF}

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


procedure TGemDBGrid.CMMouseEnter(var Message: TMessage);
begin
  inherited;
//  {$IFDEF JVCLThemesEnabled}
//  lPt := Point(Mouse.CursorPos.X, Mouse.CursorPos.Y);
//  Cell := MouseCoord(lPt.X, lPt.Y);
//  if UseXPThemes and StyleServices.Enabled then
//    if (dgTitles in Options) and (Cell.Y = 0) then
//      InvalidateCell(Cell.X, Cell.Y);
//  {$ENDIF JVCLThemesEnabled}
end;


procedure TGemDBGrid.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FCell.X := -1;
  FCell.Y := -1;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
end;


procedure TGemDBGrid.SetDatePullDownDateFields(Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetDatePullDownDateFields' );{$ENDIF}

  FDatePullDownDateFields := Value;
  Invalidate;
end;


procedure TGemDBGrid.SetDoFileCellTest(const Value: Boolean);
begin
//
end;

procedure TGemDBGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Integer);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;


procedure TGemDBGrid.SetupCalendarForDateField(theField: TField; CLeft, CTop: integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetupCalendarForDateField' );{$ENDIF}

  DateFieldCalendar := TGEMCalendar.Create(nil);
  try
    DateFieldCalendar.Left := CLeft;
    DateFieldCalendar.Top := CTop;
    DateFieldCalendar.OnClick_OKButton     :=  OnCalendarOkBtnClick;
    DateFieldCalendar.OnClick_CancelButton := OnCalendarCancelBtnClick;

  finally
  end;
end;


procedure TGemDBGrid.CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'CallDrawCellEvent' );{$ENDIF}

  inherited DrawCell(ACol, ARow, ARect, AState);
end;


function TGemDBGrid.CanEditCell(AField: TField): Boolean;
begin
  Result := True;
  if Assigned(FOnCanEditCell) then
    FOnCanEditCell(Self, AField, Result);
end;

procedure TGemDBGrid.DoDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
{$IFDEF JVCLThemesEnabled}
var
  Details: TThemedElementDetails;
  lCaptionRect: TRect;
  lCellRect: TRect;
  Bmp: TBitmap;
  DC: HDC;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
  begin
    lCellRect := ARect;
    if StyleServices.Enabled and (ARow = 0) and (ACol - ColumnOffset >= 0) and (dgTitles in Options) then
    begin
      lCaptionRect := ARect;
      if not FPaintInfo.ColPressed or (FPaintInfo.ColPressedIdx <> ACol) then
      begin
        if (FPaintInfo.MouseInCol = -1) or (FPaintInfo.MouseInCol <> ACol) or (csDesigning in ComponentState) then
          Details := StyleServices.GetElementDetails(thHeaderItemNormal)
        else
          Details := StyleServices.GetElementDetails(thHeaderItemHot);
        lCellRect.Right := lCellRect.Right + 1;
        lCellRect.Bottom := lCellRect.Bottom + 2;
      end
      else if AllowTitleClick then
      begin
        Details := StyleServices.GetElementDetails(thHeaderItemPressed);
        InflateRect(lCaptionRect, -1, 1);
      end
      else
      begin
        if FPaintInfo.MouseInCol = ACol then
          Details := StyleServices.GetElementDetails(thHeaderItemHot)
        else
          Details := StyleServices.GetElementDetails(thHeaderItemNormal);
      end;
      StyleServices.DrawElement(Canvas.Handle, Details, lCellRect);
      { The column title isn't painted by DrawCell if the DataLink is not active. }
      if (DataLink = nil) or not DataLink.Active then
        if (ACol - ColumnOffset >= 0) and (ACol - ColumnOffset < Columns.Count) then
          DrawTitleCaption(Canvas, lCaptionRect, Columns[ACol - ColumnOffset]);
    end
    else if (ACol = 0) and (dgIndicator in Options) and StyleServices.Enabled then
    begin
      // indicator column
      if ARow < TitleOffset then
        Details := StyleServices.GetElementDetails(thHeaderItemNormal)
      else
        Details := StyleServices.GetElementDetails(thHeaderRoot);
      lCellRect.Right := lCellRect.Right + 1;
      lCellRect.Bottom := lCellRect.Bottom + 2;
      StyleServices.DrawElement(Canvas.Handle, Details, lCellRect);
      // draw the indicator
      if (DataLink.Active) and (ARow - TitleOffset = Datalink.ActiveRecord) then
      begin
        // Unfortunatelly the TDBGrid.FIndicators: TImageList is a private field so we have to
        // call the original painter for the indicator and draw it into a transparent bitmap
        // without the 3D border.
        Bmp := TBitmap.Create;
        try
//          Bmp.Canvas.Brush.Color := FixedColor;
          Bmp.Width := lCellRect.Right - lCellRect.Left;
          Bmp.Height := lCellRect.Bottom - lCellRect.Top;
          DC := Canvas.Handle;
          try
            Canvas.Handle := Bmp.Canvas.Handle;
            IntersectClipRect(Canvas.Handle, 2, 2, Bmp.Width - 2, Bmp.Height - 2);
//            CallDrawCellEvent(ACol, ARow, Rect(0, 0, Bmp.Width - 1, Bmp.Height - 1), [gdFixed]);
          finally
            Canvas.Handle := DC;
          end;
//          Bmp.TransparentColor := FixedColor;
          Bmp.Transparent := True;
//          Canvas.Draw(lCellRect.Left, lCellRect.Top, Bmp);
        finally
          Bmp.Free;
        end;
      end;
    end
    else
      CallDrawCellEvent(ACol, ARow, ARect, AState);
  end
  else
  {$ENDIF JVCLThemesEnabled}
    CallDrawCellEvent(ACol, ARow, ARect, AState);
end;

type
  TWinControlAccessProtected = class(TWinControl);


//function TGemDBGrid.DoKeyPress(var Msg: TWMChar): Boolean;
//var
//  Form: TCustomForm;
//  Ch: Char;
//begin
//  Result := True;
//  Form := GetParentForm(Self);
//  if Form <> nil then
//    if Form.KeyPreview and TWinControlAccessProtected(Form).DoKeyPress(Msg) then
//      Exit;
//
//  with Msg do
//  begin
//    if Assigned(OnKeyPress) then
//    begin
//      Ch := Char(CharCode);
//      OnKeyPress(Self, Ch);
//      CharCode := Word(Ch);
//    end;
//    if CharCode = 0 then
//      Exit;
//  end;
//  Result := False;
//end;

procedure TGemDBGrid.CellClick(Column: TColumn);
var
  where, PlaceCalendar: TPoint;
  ACol, ARow: integer;
  btnRect: TRect;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'CellClick' );{$ENDIF}

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


procedure TGemDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
Const
  CtrlState : array[Boolean] of Integer =(DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
var
  txtRect: TRect;
  btnRect: TRect;
  CheckBoxRectangle : TRect;
  NewBackgrnd: TColor;
  Highlight: Boolean;
//  Bmp: TBitmap;
  Field{, ReadOnlyTestField}: TField;
  {$IFDEF  GRID_DEBUG}
    s: string;
  {$ENDIF}
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'DrawColumnCell' );{$ENDIF}

  try
    Field := Column.Field;
    if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and
      (SelectedRows.IndexOf(DataSource.DataSet.Bookmark) > -1) then
      Include(State, gdSelected);
    NewBackgrnd := Canvas.Brush.Color;
    Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused);
    GetCellProps(Column, Canvas.Font, NewBackgrnd, Highlight{ or ActiveRowSelected});

    if not Highlight and (ReadOnlyCellColor <> clDefault) and
       (Field <> nil) and (not Field.CanModify or not CanEditCell(Field)) then
    begin
      if (gdSelected in State) and (Focused xor MultiSelect) then
        Canvas.Brush.Color := NewBackgrnd
      else
      begin
        Canvas.Brush.Color := ReadOnlyCellColor;
//        LookupInfo := GetColumnLookupInfo(Column);

        { When column works as a lookup, check the 'keyfield' }
//        if LookupInfo.IsLookup and LookupInfoValid(LookupInfo) then
//        begin
//          I := 1;
//          ReadOnlyTestField := Field.DataSet.FieldByName(ExtractFieldName(LookupInfo.KeyFields, I));
//          { Lookup fields do not have a FieldNo. In this case CanModify returns False }
//          if ReadOnlyTestField.CanModify and CanEditCell(ReadOnlyTestField) then
//            Canvas.Brush.Color := NewBackgrnd
//        end;
      end;
    end
    else
      Canvas.Brush.Color := NewBackgrnd;
//==========


//    Canvas.Brush.Color := Self.Color;
//    Canvas.FillRect(Rect);

//    if fAltRowColorUse then
//      if Odd(FCurrentDrawRow) then
//        Canvas.Brush.Color := FAlternateRowColor
//      else
//        Canvas.Brush.Color := Self.Color;
    DefaultDrawColumnCell(Rect, DataCol, Column, State) ;

    // Boolean Field check box
    if ({Column.}Field <> nil) and (FBooleanFieldCheckBox) and ({Column.}Field.DataType = ftBoolean) then begin
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
      DrawFrameControl(Canvas.Handle, CheckBoxRectangle, DFC_BUTTON, CtrlState[{Column.}Field.AsBoolean]);
    end;

    //  Date field button
    if ({Column.}Field <> nil) and (FDatePullDownDateFields) and ({Column.}Field.DataType = ftDate) then begin
      txtRect := Rect;

      Canvas.Font.Name := Font.Name;
      Canvas.Font.Size := Font.Size;
      txtRect.Left := Rect.left + TXT_MARG.x;
      txtRect.Right := txtRect.Right - (Rect.Right - Rect.Left) - TXT_MARG.x;

      DrawText(canvas.Handle, PChar ({Column.}Field.AsString), length({Column.}Field.AsString),
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
      Invalidate;
    end;
  except
    on e: exception do
      ShowMessage(E.Message);
  end;

//=================================

// var
//  I: Integer;
//  NewBackgrnd: TColor;
//  Highlight: Boolean;
//  Bmp: TBitmap;
//  Field, ReadOnlyTestField: TField;
////  LookupInfo: TJvDBGridColumnLookupInfo;
//  R: TRect;
//begin
//  Field := Column.Field;
//  if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active and
//    (SelectedRows.IndexOf(DataSource.DataSet.Bookmark) > -1) then
//    Include(State, gdSelected);
//  NewBackgrnd := Canvas.Brush.Color;
//  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or Focused);
//  GetCellProps(Column, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
//  if not Highlight and (ReadOnlyCellColor <> clDefault) and
//     (Field <> nil) {and (not Field.CanModify or not CanEditCell(Field))} then
//  begin
//    if (gdSelected in State) {and (Focused xor MultiSelect)} then
//      Canvas.Brush.Color := NewBackgrnd
//    else
//    begin
//      Canvas.Brush.Color := ReadOnlyCellColor;
////      LookupInfo := GetColumnLookupInfo(Column);
//
//      { When column works as a lookup, check the 'keyfield' }
////      if LookupInfo.IsLookup and LookupInfoValid(LookupInfo) then
////      begin
////        I := 1;
////        ReadOnlyTestField := Field.DataSet.FieldByName(ExtractFieldName(LookupInfo.KeyFields, I));
////        { Lookup fields do not have a FieldNo. In this case CanModify returns False }
////        if ReadOnlyTestField.CanModify and CanEditCell(ReadOnlyTestField) then
////          Canvas.Brush.Color := NewBackgrnd
////      end;
//    end;
//  end
//  else
//    Canvas.Brush.Color := NewBackgrnd;
//  if DefaultDrawing then
//  begin
//    I := 0;//GetImageIndex(Field);
//    if I >= 0 then
//    begin
////      Bmp := GetGridBitmap(TGridPicture(I));
////      Canvas.FillRect(Rect);
////      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right + 1 - Bmp.Width) div 2,
////        (Rect.Top + Rect.Bottom + 1 - Bmp.Height) div 2, Bmp, clOlive);
//    end
//    else
//    begin
//      DefaultDrawColumnCell(Rect, DataCol, Column, State);
//    end;
//  end;
//  if (Columns.State = csDefault) or not DefaultDrawing or (csDesigning in ComponentState) then
//    inherited DrawDataCell(Rect, Field, State);
//  inherited DrawColumnCell(Rect, DataCol, Column, State);
//  if DefaultDrawing and (gdFocused in State) and not (csDesigning in ComponentState) and
//    not (dgRowSelect in Options) and
//    (ValidParentForm(Self).ActiveControl = Self) then
//  begin
//    R := Rect;
//    {$IFDEF JVCLThemesEnabled}
//    if UseThemedHighlighting and not JvDBGridDisableUseThemedHighlighting and
//       UseXPThemes and StyleServices.Enabled then
//    begin
//      InflateRect(R, -1, -1);
//      Bmp := TBitmap.Create;
//      try
//        Bmp.Canvas.Brush.Color := clWhite;
//        Bmp.Width := R.Right - R.Left;
//        Bmp.Height := R.Bottom - R.Top;
//        Bmp.Canvas.DrawFocusRect(Types.Rect(0, 0, Bmp.Width, Bmp.Height));
//        Bmp.TransparentColor := clWhite;
//        Bmp.Transparent := True;
//        Canvas.Draw(R.Left, R.Top, Bmp);
//      finally
//        Bmp.Free;
//      end;
//    end
//    else
//    {$ENDIF JVCLThemesEnabled}
//      Canvas.DrawFocusRect(R);
//  end;

end;


type
  TGridDataLinkAccessProtected = class(TGridDataLink);

procedure TGemDBGrid.EnableScroll;
begin
  if FDisableCount <> 0 then
  begin
    Dec(FDisableCount);
    if (FDisableCount = 0) and DataLink.Active then
      TGridDataLinkAccessProtected(DataLink).DataSetScrolled(0);
  end;
end;

//Returns rectangle where button will be drawn:
function TGemDBGrid.GetBtnRect(CellRect: TRect; complete: boolean): TRect;

  function MakeBtnRect(Alignment: TAlignment; cellrect: TRect; complete: boolean): TRect;
  var
    rowHeight: integer;
  begin
    {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'GetBtnRect/MakeBtnRect' );{$ENDIF}

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
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'GetBtnRect' );{$ENDIF}

  result := Rect(0, 0, 0, 0);
  //Last visible row sometimes get truncated so we need to fix that
  if (cellrect.Bottom - cellrect.Top) < DefaultRowHeight then
    cellrect.Bottom := cellrect.top + DefaultRowheight;

  result := MakeBtnRect(taRightJustify, cellrect, complete);
end;


procedure TGemDBGrid.GetCellProps(Column: TColumn; AFont: TFont;
  var Background: TColor; Highlight: Boolean);

  function IsAfterFixedCols: Boolean;
  begin
    Result := Column.Index >= FixedCols;
  end;

begin
  if IsAfterFixedCols and (FCurrentDrawRow >= FixedRows) then
  begin
    if Odd(FCurrentDrawRow + FixedRows) then
    begin
//      if (FAlternateRowColor <> clNone) and (FAlternateRowColor <> Color) then
      if (fAltRowColorUse) and (FAlternateRowColor <> Color) then
      begin
        // Prefer the column's color
        if not ((cvColor in Column.AssignedValues) and (Column.Color <> Column.DefaultColor)) then
          Background := fAlternateRowColor;
      end;

//      if FAlternateRowFontColor <> clNone then
      if fAltRowColorUse then
      begin
        // Prefer the column's font.color if it has a prefered color
        if not ((cvColor in Column.AssignedValues) and (Column.Color <> Column.DefaultColor)) then
          AFont.Color := fAltRowFontColor;
      end;
    end;
  end(*
  else
    Background := FixedColor*);

  if Highlight then
  begin
    AFont.Color := clHighlightText;
    Background := clHighlight;
  end;
  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Column.Field, AFont, Background, Highlight)
  else
  if Assigned(FOnGetCellProps) then
    FOnGetCellProps(Self, Column.Field, AFont, Background);
end;


//function TGemDBGrid.GetRow: Longint;
//begin
//  Result := inherited Row;
//end;

procedure TGemDBGrid.ColEnter;
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


procedure TGemDBGrid.ColExit;
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


function TGemDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or ((InplaceEditor <> nil) and InplaceEditor.Focused) or
                         ((FCurrentControl <> nil) and FCurrentControl.Focused);
  end;
end;

//function TGemDBGrid.ActiveRowSelected: Boolean;
//var
//  Index: Integer;
//begin
////  if {MultiSelect and} DataLink.Active then
////    Result := SelectedRows.Find(DataLink.DataSet.Bookmark, Index)
////  else
////    Result := False;
//end;

procedure TGemDBGrid.AltColorsSet;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'AltColorsSet' );{$ENDIF}

//
end;


function TGemDBGrid.GetTitleOffset: Integer;
var
  I, J: Integer;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'GetTitleOffset' );{$ENDIF}

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


procedure TGemDBGrid.GridInvalidateRow(Row: Longint);
var
  I: Longint;
begin
  for I := 0 to ColCount - 1 do
    InvalidateCell(I, Row);
end;


function TGemDBGrid.HighlightCell(DataCol, DataRow: Integer;
                         const Value: string; AState: TGridDrawState): Boolean;
begin
  Result := false; //ActiveRowSelected;
  if not Result then
    Result := inherited HighlightCell(DataCol, DataRow, Value, AState);
end;


procedure TGemDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
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


procedure TGemDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
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


procedure TGemDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'Notification' );{$ENDIF}

  inherited Notification(AComponent, Operation);
//  if (Operation = opRemove) and (AComponent = FCurrentControl) then
//  begin
//    FCurrentControl.RemoveFreeNotification(Self);
//    FCurrentControl := nil;
//  end;
end;

procedure TGemDBGrid.OnCalendarCancelBtnClick(Sender: TObject);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'OnCalendarCancelBtnClick' );{$ENDIF}

  FreeAndNil(DateFieldCalendar);
end;


procedure TGemDBGrid.OnCalendarOkBtnClick(Sender: TObject);
var
  theDate: TDate;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'OnCalendarOkBtnClick' );{$ENDIF}

  theDate := DateFieldCalendar.Date;
  Self.SelectedField.Dataset.Edit;
  Self.SelectedField.AsDateTime := theDate;
  Self.SelectedField.Dataset.Post;

  FreeAndNil(DateFieldCalendar);
end;


procedure TGemDBGrid.Paint;
begin
  if Assigned(FOnBeforePaint) then
    FOnBeforePaint(Self);
  {$IFNDEF COMPILER14_UP}
  {$IFDEF JVCLThemesEnabled}
  if UseXPThemes and StyleServices.Enabled then
  begin
    // reset the inherited options but remove the goFixedVertLine and goFixedHorzLine values
    // as that causes the titles and indicator panels to have a black border
    TStringGrid(Self).Options := TStringGrid(Self).Options - [goFixedVertLine, goFixedHorzLine];
  end;
  {$ENDIF JVCLThemesEnabled}
  {$ENDIF ~COMPILER14_UP}
  inherited Paint;
  if not (csDesigning in ComponentState) and
    (dgRowSelect in Options) and DefaultDrawing and Focused then
  begin
    Canvas.Font.Color := clWindowText;
    with Selection do
      DrawFocusRect(Canvas.Handle, BoxRect(Left, Top, Right, Bottom));
  end;
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(Self);
end;

procedure TGemDBGrid.SaveBoolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SaveBoolean' );{$ENDIF}

  Self.SelectedField.Dataset.Edit;
  Self.SelectedField.AsBoolean := not Self.SelectedField.AsBoolean;
  //Self.SelectedField.Dataset.Post;
end;


procedure TGemDBGrid.Scroll(Distance: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'Scroll' );{$ENDIF}
  if FDisableCount = 0 then
  begin
    inherited Scroll(Distance);
    if ((fAlternateRowColor <> clNone) and (fAlternateRowColor <> Color)) or
       ((AltRowFontColor <> clNone) and (AltRowFontColor <> Font.Color)) then
      Invalidate;

    if FAlwaysShowEditor and HandleAllocated and ([dgRowSelect, dgEditing] * Options = [dgEditing]) and
       Focused then
    begin
      ShowEditor;
      InvalidateCol(Col);
    end;
  end;
end;


procedure TGemDBGrid.SetAlternateRowColor(const Value: TColor);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetAlternateRowColor' );{$ENDIF}

  if FAlternateRowColor <> Value then  begin
    FAlternateRowColor := Value;
    Invalidate;
  end;
end;

//function TGemDBGrid.ReadAlternateRowColor: TColor;
//begin
////  if Reader.ReadBoolean then
////    AlternateRowColor := JvDefaultAlternateRowColor // this was the previous default row color
////  else
////    AlternateRowColor := clNone;
//end;



procedure TGemDBGrid.SetAltRowFontColor(Value: TColor);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetAltRowFontColor' );{$ENDIF}

  if fAltRowFontColor <> Value then begin
    fAltRowFontColor := Value;
    Invalidate;
  end;
end;


procedure TGemDBGrid.SetCheckBoxes(Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetCheckBoxes' );{$ENDIF}

  FBooleanFieldCheckBox := Value;
  Invalidate;
end;


procedure TGemDBGrid.SetFileName(const Value: string);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetFileName' );{$ENDIF}

  FColFileName := Value;
  FSaveGridLayout := FColFileName <> '';
end;


procedure TGemDBGrid.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not Value then
      SelectedRows.Clear;
  end;
end;

//procedure TGemDBGrid.SetMultiSelect(Value: Boolean);
//begin
////
//end;
//
//
procedure TGemDBGrid.SetScrollBars(Value: System.UITypes.TScrollStyle);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetScrollBars' );{$ENDIF}

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


procedure TGemDBGrid.SetShowMemos(const Value: Boolean);
begin
  if FShowMemos <> Value then
  begin
    FShowMemos := Value;
    Invalidate;
  end;
end;


procedure TGemDBGrid.SetTitleArrow(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetTitleArrow' );{$ENDIF}

  if FTitleArrow <> Value then
  begin
    FTitleArrow := Value;
    Invalidate;
  end;
end;


procedure TGemDBGrid.SetUseAltRowColor(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'SetUseAltRowColor' );{$ENDIF}
  if FAltRowColorUse <> Value then
  begin
    FAltRowColorUse := Value;
    Invalidate;
  end;
end;


procedure TGemDBGrid.ShowColumnsDialog;
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


procedure TGemDBGrid.ShowSelectColumnClick;
begin
  ShowColumnsDialog;
end;


procedure TGemDBGrid.UpdateScrollBar;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'UpdateScrollBar' );{$ENDIF}

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


procedure TGemDBGrid.WmBeforeClose(var Msg: TMessage);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WmBeforeClose' );{$ENDIF}

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


procedure TGemDBGrid.WmBeforeCreate(var Msg: TMessage);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WmBeforeCreate' );{$ENDIF}

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


procedure TGemDBGrid.WMCancelMode(var Msg: TMessage);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WMCancelMode' );{$ENDIF}

  inherited;
end;


procedure TGemDBGrid.WMChar(var Msg: TWMChar);
begin
//  if Assigned(SelectedField) and EditWithBoolBox(SelectedField) and
//    ((Char(Msg.CharCode) = Backspace) or (Msg.CharCode >= 32)) then
//  begin
//    if not DoKeyPress(Msg) then
//      case Char(Msg.CharCode) of
//        #32:
//          begin
//            ShowEditor;
//            ChangeBoolean(JvGridBool_INVERT);
//          end;
//        Backspace, '0', '-':
//          begin
//            ShowEditor;
//            ChangeBoolean(JvGridBool_UNCHECK);
//          end;
//        '1', '+':
//          begin
//            ShowEditor;
//            ChangeBoolean(JvGridBool_CHECK);
//          end;
//      end;
//  end
//  else
//  begin
//    inherited;
//
//    if Assigned(FCurrentControl) then
//    begin
//      if FCurrentControl.Visible then
//        PostMessage(FCurrentControl.Handle, WM_CHAR, Msg.CharCode, Msg.KeyData);
//    end
//    else
//      if InplaceEditor = nil then
//        DoKeyPress(Msg); // This is needed to trigger an onKeyPressed event when the
//                         // default editor hasn't been created because the data type
//                         // of the selected field is binary or memo.
//  end;
end;


procedure TGemDBGrid.WMPaint(var Message: TWMPaint);
var
  R: TRect;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WMPaint' );{$ENDIF}

  if UseRightToLeftAlignment then
  begin
    { Workaround for a RightToLeft painting bug (QC #70075)
      Side effect: The grid needs more time to paint }
    R.TopLeft := ClientRect.TopLeft;
    R.BottomRight := ClientRect.BottomRight;
    WinApi.Windows.InvalidateRect(Handle, @R, False);
  end;
  inherited;
end;

procedure TGemDBGrid.WMRButtonUp(var Msg: TWMMouse);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WMRButtonUp' );{$ENDIF}


end;

procedure TGemDBGrid.WMVScroll(var Msg: TWMVScroll);
var
  ALeftCol: Integer;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WMVScroll' );{$ENDIF}

  if dgRowSelect in Options then
  begin
    ALeftCol := LeftCol;
    inherited;
    LeftCol := ALeftCol;
  end
  else
    inherited;
end;


procedure TGemDBGrid.WriteCellText(ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; ARightToLeft, FixCell: Boolean;
  Options: Integer);
const
  AlignFlags: array [TAlignment] of Integer =
    (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  DrawBitmap: TBitmap;
  Hold: Integer;
  B, R: TRect;
  DrawOptions: Integer;

  procedure DrawAText(CellCanvas: TCanvas);
  begin
    DrawOptions := DT_EXPANDTABS or DT_NOPREFIX;
    if Options <> 0 then
      DrawOptions := DrawOptions or Options;
    if WordWrap then
      DrawOptions := DrawOptions or DT_WORDBREAK;
    {$IFDEF JVCLThemesEnabled}
    if not FixCell or not (UseXPThemes and StyleServices.Enabled) then
    {$ENDIF JVCLThemesEnabled}
      {$IFDEF COMPILER14_UP}
      if not FixCell or (DrawingStyle in [gdsClassic, gdsThemed]) then
      {$ENDIF COMPILER14_UP}
      begin
        if CellCanvas.Brush.Style <> bsSolid then
          CellCanvas.Brush.Style := bsSolid;
//        if not DrawThemedHighlighting(CellCanvas, B) then
          CellCanvas.FillRect(B);
      end;
    SetBkMode(CellCanvas.Handle, TRANSPARENT);
    DrawBiDiText(CellCanvas.Handle, Text, R, DrawOptions, Alignment, ARightToLeft, Canvas.CanvasOrientation);
  end;

begin
  if ReduceFlicker
     {$IFDEF COMPILER14_UP} and not FixCell {$ENDIF}
     {$IFDEF JVCLThemesEnabled} and not (UseXPThemes and StyleServices.Enabled) {$ENDIF} then
  begin
    // Use offscreen bitmap to eliminate flicker and
    // brush origin tics in painting / scrolling.
    DrawBitmap := TBitmap.Create;
    try
      DrawBitmap.Canvas.Lock;
      try
        DrawBitmap.Width := Max(DrawBitmap.Width, ARect.Right - ARect.Left);
        DrawBitmap.Height := Max(DrawBitmap.Height, ARect.Bottom - ARect.Top);
        R := Rect(DX, DY, ARect.Right - ARect.Left - 1, ARect.Bottom - ARect.Top - 1);
        B := Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
        DrawBitmap.Canvas.Font := Canvas.Font;
        DrawBitmap.Canvas.Font.Color := Canvas.Font.Color;
        DrawBitmap.Canvas.Brush := Canvas.Brush;

        DrawAText(DrawBitmap.Canvas);
        if Canvas.CanvasOrientation = coRightToLeft then
        begin
          Hold := ARect.Left;
          ARect.Left := ARect.Right;
          ARect.Right := Hold;
        end;
        Canvas.CopyRect(ARect, DrawBitmap.Canvas, B);
      finally
        DrawBitmap.Canvas.Unlock;
      end;
    finally
      DrawBitmap.Free;
    end;
  end
  else
  begin
    // No offscreen bitmap - The display is faster but flickers
    if IsRightToLeft then
      R := Rect(ARect.Left, ARect.Top, ARect.Right - 1 - DX, ARect.Bottom - DY - 1)
    else
      R := Rect(ARect.Left + DX, ARect.Top + DY, ARect.Right - 1, ARect.Bottom - 1);
    B := ARect;
    DrawAText(Canvas);
  end;
end;


procedure TGemDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  DrawColumn: TColumn;
//  Bmp       : TgemDBGridBitmap;

begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'DrawCell' );{$ENDIF}
  FCurrentDrawRow := ARow;

  Canvas.Font := Self.Font;
  if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
    (ACol < Columns.Count) then
  begin
    DrawColumn := Columns[ACol];
    if DrawColumn <> nil then
      Canvas.Font := DrawColumn.Font;
  end;

  DoDrawCell(ACol, ARow, ARect, AState);
//  if FTitleArrow and (ARow = 0) and (ACol = 0) and
//    (dgIndicator in Options) and (dgTitles in Options) then
//  begin
//    Bmp := GetGridBitmap(gpPopup);
//    DrawBitmapTransparent(Canvas, (ARect.Left + ARect.Right - Bmp.Width) div 2,
//      (ARect.Top + ARect.Bottom - Bmp.Height) div 2, Bmp, clWhite);
//  end;



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

{ TgemExDBGrid }

constructor TgemExDBGrid.Create(AOwner: TComponent);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'Create' );{$ENDIF}
  inherited Create(AOwner);
  FHintColor := clDefault;
end;


function TgemExDBGrid.BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT;
var
  Mesg: TMessage;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'BaseWndProc' );{$ENDIF}

  CreateWMMessage(Mesg, Msg, WParam, LParam);
  inherited WndProc(Mesg);
  Result := Mesg.Result;
end;

function TgemExDBGrid.BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: TObject): LRESULT;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'BaseWndProc' );{$ENDIF}

  Result := BaseWndProc(Msg, WParam, WinApi.Windows.LPARAM(LParam));
end;




//function TgemExDBGrid.BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: TObject): LRESULT;
//var
//  Mesg: TMessage;
//begin
//  CreateWMMessage(Mesg, Msg, WParam, LParam);
//  inherited WndProc(Mesg);
//  Result := Mesg.Result;
//end;
//
//
//function TgemExDBGrid.BaseWndProc(Msg: Cardinal; WParam: WPARAM;
//                                  LParam: LPARAM): LRESULT;
//begin
//  Result := BaseWndProc(Msg, WParam, WinApi.Windows.LPARAM(LParam));
//end;


//function TgemExDBGrid.BaseWndProcEx(Msg: Cardinal; WParam: WPARAM;
//  var StructLParam): LRESULT;
//begin
//  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'BaseWndProcEx' );{$ENDIF}
//  Result := BaseWndProc(Msg, WParam, WinApi.Windows.LPARAM(@StructLParam));
//end;


procedure TgemExDBGrid.BoundsChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'BoundsChanged' );{$ENDIF}


end;


procedure TgemExDBGrid.ColorChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ColorChanged' );{$ENDIF}

  BaseWndProc(CM_COLORCHANGED);
end;


procedure TgemExDBGrid.ControlsListChanged(Control: TControl;
  Inserting: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ControlsListChanged' );{$ENDIF}


end;


procedure TgemExDBGrid.ControlsListChanging(Control: TControl;
  Inserting: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ControlsListChanging' );{$ENDIF}


end;


procedure TgemExDBGrid.CursorChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'CursorChanged' );{$ENDIF}

  BaseWndProc(CM_CURSORCHANGED);
end;


function TgemExDBGrid.DoEraseBackground(Canvas: TCanvas;
  Param: LPARAM): Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'DoEraseBackground' );{$ENDIF}

  Result := BaseWndProc(WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
end;


procedure TgemExDBGrid.EnabledChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'EnabledChanged' );{$ENDIF}

  BaseWndProc(CM_ENABLEDCHANGED);
end;


procedure TgemExDBGrid.FocusChanged(AControl: TWinControl);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'FocusChanged' );{$ENDIF}

  BaseWndProc(CM_FOCUSCHANGED, 0, AControl);
end;


procedure TgemExDBGrid.FocusKilled(NextWnd: THandle);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'FocusKilled' );{$ENDIF}

  BaseWndProc(WM_KILLFOCUS, WPARAM(NextWnd), 0);
end;


procedure TgemExDBGrid.FocusSet(PrevWnd: THandle);
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'FocusSet' );{$ENDIF}

  BaseWndProc(WM_SETFOCUS, WPARAM(PrevWnd), 0);
end;


procedure TgemExDBGrid.FontChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'FontChanged' );{$ENDIF}

  BaseWndProc(CM_FONTCHANGED);
end;


function TgemExDBGrid.HintShow(var HintInfo: Vcl.Controls.THintInfo): Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'HintShow' );{$ENDIF}

//  GetHintColor(HintInfo, Self, FHintColor);
//  if FHintWindowClass <> nil then
//    HintInfo.HintWindowClass := FHintWindowClass;
//  Result := BaseWndProcEx(CM_HINTSHOW, 0, HintInfo) <> 0;
end;


function TgemExDBGrid.HitTest(X, Y: Integer): Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'HitTest' );{$ENDIF}
//  result := false;
  Result := BaseWndProc(CM_HITTEST, 0, SmallPointToLong(PointToSmallPoint(Point(X, Y)))) <> 0;
end;


procedure TgemExDBGrid.MouseEnter(AControl: TControl);
begin
  FMouseOver := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  BaseWndProc(CM_MOUSEENTER, 0, AControl);
end;


procedure TgemExDBGrid.MouseLeave(AControl: TControl);
begin
  FMouseOver := False;
  BaseWndProc(CM_MOUSELEAVE, 0, AControl);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;


procedure TgemExDBGrid.ParentColorChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ParentColorChanged' );{$ENDIF}

  BaseWndProc(CM_PARENTCOLORCHANGED);
  if Assigned(OnParentColorChange) then
    OnParentColorChange(Self);
end;


procedure TgemExDBGrid.ParentFontChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ParentFontChanged' );{$ENDIF}

  BaseWndProc(CM_PARENTFONTCHANGED);
end;


procedure TgemExDBGrid.ParentShowHintChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ParentShowHintChanged' );{$ENDIF}

  BaseWndProc(CM_PARENTSHOWHINTCHANGED);
end;


procedure TgemExDBGrid.ShowHintChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ShowHintChanged' );{$ENDIF}

  BaseWndProc(CM_SHOWHINTCHANGED);
end;

procedure TgemExDBGrid.ShowingChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'ShowingChanged' );{$ENDIF}

  BaseWndProc(CM_SHOWINGCHANGED);
end;


procedure TgemExDBGrid.TextChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'TextChanged' );{$ENDIF}

  BaseWndProc(CM_TEXTCHANGED);
end;


procedure TgemExDBGrid.VisibleChanged;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'VisibleChanged' );{$ENDIF}

  BaseWndProc(CM_VISIBLECHANGED);
end;


function TgemExDBGrid.WantKey(Key: Integer; Shift: TShiftState): Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WantKey' );{$ENDIF}

  Result := BaseWndProc(CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;


procedure TgemExDBGrid.WndProc(var Msg: TMessage);
//procedure TgemExDBGrid.WndProc(var Message: TMessage);
var
  IdSaveDC: Integer;
//  DlgCodes: TDlgCodes;
//  Canvas: TCanvas;
begin
  {$IFDEF USE_CODESITE}CodeSite.TraceMethod( Self, 'WndProc' );{$ENDIF}

  if not DispatchIsDesignMsg(Self, Msg) then
  begin
//    {$IFDEF USE_CODESITE}CodeSite.SendMsg( Format('Send Msg: %n', [Msg.Msg]) );{$ENDIF}
    case Msg.Msg of
//      CM_DENYSUBCLASSING: Msg.Result := LRESULT(Ord(GetInterfaceEntry(IJvDenySubClassing) <> nil));

      CM_DIALOGCHAR: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_DIALOGCHAR') ;{$ENDIF}
      {$ENDIF}

        with TCMDialogChar{$IFDEF CLR}.Create{$ENDIF}(Msg) do
          Result := LRESULT(Ord(WantKey(CharCode, KeyDataToShiftState(KeyData))));
      end;

      CM_HINTSHOW: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_HINTSHOW') ;{$ENDIF}
      {$ENDIF}
        with TCMHintShow(Msg) do
         Result := LRESULT(HintShow(HintInfo^));
      end;

      CM_HITTEST: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_HITTEST') ;{$ENDIF}
      {$ENDIF}
        with TCMHitTest(Msg) do
          Result := LRESULT(HitTest(XPos, YPos));
      end;

      CM_MOUSEENTER: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_MOUSEENTER') ;{$ENDIF}
      {$ENDIF}
        MouseEnter(TControl(Msg.LParam));
      end;

      CM_MOUSELEAVE: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_MOUSELEAVE') ;{$ENDIF}
      {$ENDIF}
        MouseLeave(TControl(Msg.LParam));
      end;

      CM_VISIBLECHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_VISIBLECHANGED') ;{$ENDIF}
      {$ENDIF}
        VisibleChanged;
      end;

      CM_ENABLEDCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_ENABLEDCHANGED') ;{$ENDIF}
      {$ENDIF}
        EnabledChanged;
      end;

      CM_TEXTCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_TEXTCHANGED') ;{$ENDIF}
      {$ENDIF}
        TextChanged;
      end;

      CM_FONTCHANGED:  begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_TEXTCHANGED') ;{$ENDIF}
      {$ENDIF}
        FontChanged;
      end;

      CM_COLORCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_COLORCHANGED') ;{$ENDIF}
      {$ENDIF}
        ColorChanged;
      end;

      CM_FOCUSCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_FOCUSCHANGED') ;{$ENDIF}
      {$ENDIF}
        FocusChanged(TWinControl(Msg.LParam));
      end;

      CM_PARENTFONTCHANGED:  begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_PARENTFONTCHANGED') ;{$ENDIF}
      {$ENDIF}
        ParentFontChanged;
      end;

      CM_PARENTCOLORCHANGED:  begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_PARENTCOLORCHANGED') ;{$ENDIF}
      {$ENDIF}
         ParentColorChanged;
      end;

      CM_PARENTSHOWHINTCHANGED:  begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_PARENTSHOWHINTCHANGED') ;{$ENDIF}
      {$ENDIF}
        ParentShowHintChanged;
      end;

      CM_CURSORCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_CURSORCHANGED') ;{$ENDIF}
      {$ENDIF}
        CursorChanged;
      end;

      CM_SHOWINGCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_SHOWINGCHANGED') ;{$ENDIF}
      {$ENDIF}
        ShowingChanged;
      end;

      CM_SHOWHINTCHANGED: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_SHOWHINTCHANGED') ;{$ENDIF}
      {$ENDIF}
        ShowHintChanged;
      end;

      CM_CONTROLLISTCHANGE: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_CONTROLLISTCHANGE') ;{$ENDIF}
      {$ENDIF}
        if Msg.LParam <> 0 then
          ControlsListChanging(TControl(Msg.WParam), True)
        else
          ControlsListChanged(TControl(Msg.WParam), False);
      end;

      CM_CONTROLCHANGE: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_CONTROLCHANGE') ;{$ENDIF}
      {$ENDIF}
        if Msg.LParam = 0 then
          ControlsListChanging(TControl(Msg.WParam), False)
        else
          ControlsListChanged(TControl(Msg.WParam), True);
      end;

      WM_SETFOCUS:  begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'WM_SETFOCUS') ;{$ENDIF}
      {$ENDIF}
        FocusSet(THandle(Msg.WParam));
      end;

      WM_KILLFOCUS:  begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'WM_KILLFOCUS') ;{$ENDIF}
      {$ENDIF}
        FocusKilled(THandle(Msg.WParam));
      end;

      WM_SIZE, WM_MOVE: begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'CM_TEXTCHANGED') ;{$ENDIF}
      {$ENDIF}
        inherited WndProc(Msg);
        BoundsChanged;
      end;

      WM_ERASEBKGND: ;
//        if (Msg.WParam <> 0) and not IsDefaultEraseBackground(DoEraseBackground, @TJvExCustomDBGrid.DoEraseBackground) then begin
//          IdSaveDC := SaveDC(HDC(Msg.WParam)); // protect DC against Stock-Objects from Canvas
//          Canvas := TCanvas.Create;
//          try
//            Canvas.Handle := HDC(Msg.WParam);
//            Msg.Result := Ord(DoEraseBackground(Canvas, Msg.LParam));
//          finally
//            Canvas.Handle := 0;
//            Canvas.Free;
//            RestoreDC(HDC(Msg.WParam), IdSaveDC);
//          end;
//        end
//        else
//          inherited WndProc(Msg);
      {$IFNDEF DELPHI2007_UP}
      WM_PRINTCLIENT, WM_PRINT: begin// VCL bug fix
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'WM_PRINTCLIENT') ;{$ENDIF}
      {$ENDIF}
        IdSaveDC := SaveDC(HDC(Msg.WParam)); // protect DC against changes
        try
          inherited WndProc(Msg);
        finally
          RestoreDC(HDC(Msg.WParam), IdSaveDC);
        end;
      end;
      {$ENDIF ~DELPHI2007_UP}
      WM_GETDLGCODE: begin
  //        inherited WndProc(Msg);
  //        DlgCodes := [dcNative] + DlgcToDlgCodes(Msg.Result);
  //        GetDlgCode(DlgCodes);
  //        if not (dcNative in DlgCodes) then
  //          Msg.Result := DlgCodesToDlgc(DlgCodes);
        end;

      else begin
      {$IFDEF DEBUG}
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'Send Msg: '+ 'else begin') ;{$ENDIF}
      {$ENDIF}
        inherited WndProc(Msg);
      end;
    end;
//    case Msg.Msg of // precheck message to prevent access violations on released controls
//      CM_MOUSEENTER, CM_MOUSELEAVE, WM_KILLFOCUS, WM_SETFOCUS, WM_NCPAINT:
//        if DotNetHighlighting then
//          HandleDotNetHighlighting(Self, Msg, MouseOver, Color);
//    end;
  end;
end;

{ TgemDBGridLayoutChangeLink }

procedure TgemDBGridLayoutChangeLink.DoChange(Grid: TGemDBGrid;
  Kind: TgemDBGridLayoutChangeKind);
begin
  if Assigned(OnChange) then
    OnChange(Grid, Kind);
end;

end.









