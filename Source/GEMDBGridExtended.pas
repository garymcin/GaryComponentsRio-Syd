unit GEMDBGridExtended;
  {.$DEFINE USE_CODESITE}
  {.$DEFINE GRID_DEBUG}

interface

uses
  WinApi.Windows, WinApi.Messages, Winapi.GDIPAPI, Winapi.GDIPOBJ,
  Winapi.MMSystem,

  System.SysUtils, System.Classes, System.UITypes, System.Types, System.TypInfo,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms, VCL.Grids, VCL.DBGrids,
  Vcl.Menus, Vcl.Buttons, Vcl.Dialogs, Vcl.ImgList,

  Data.DB, Data.Win.adodb,

  GEMCalendar, GEMDBGridEdit, GEMComponentsGlobal;

const
  KeyboardShiftStates = [ssShift, ssAlt, ssCtrl];
//  DefJvGridOptions = [dgEditing, dgTitles, dgIndicator, dgColumnResize,
//    dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit
//    {$IFDEF COMPILER14_UP}
//    , dgTitleClick, dgTitleHotTrack
//    {$ENDIF COMPILER14_UP}];
//  {$NODEFINE DefJvGridOptions}

  GEMDefaultAlternateRowColor = TColor($00CCCCCC); // Light gray
  GEMDefaultAlternateRowFontColor = TColor($00000000); // Black

  bmArrow = 'DBGARROW2';
  bmEdit = 'DBEDIT2';
  bmInsert = 'DBINSERT2';
  bmMultiDot = 'DBMULTIDOT2';
  bmMultiArrow = 'DBMULTIARROW2';


type
  TSortType = (stNone, stAsc, stDesc);

  TActiveChangedEvent = procedure(Sender: TObject; dataset: TDataSet) of object;

  TFilterEvent = procedure(Sender: TObject; searchCol: TColumn;
    searchStr:String; var filterStr: string) of object;

  TOnPopupCommandEvent = procedure(Sender: TObject; commandID, rowNo: integer ) of object;


  TMyFieldModEvent = Procedure(AField:TField) of Object ;

  TDBGridCellHintPosition = (gchpDefault, gchpMouse);

  TTitleHintEvent = procedure(Sender: TObject; Field: TField;
                          var AHint: string; var ATimeOut: Integer) of object;
  //TCustomGridAccess = class(TCustomGrid);

  TCheckTitleBtnEvent = procedure(Sender: TObject; ACol: Longint;
           Field: TField; var Enabled: Boolean) of object;

  TRowInfo = record
    recNo,
    top,
    bottom: integer;
  end;

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

  TGEMDBGridExtended = class;

  TJvGridPaintInfo = record
    MouseInCol: Integer; // the column that the mouse is in
    ColPressed: Boolean; // a column has been pressed
    ColPressedIdx: Integer; // idx of the pressed column
    ColSizing: Boolean; // currently sizing a column
    ColMoving: Boolean; // currently moving a column
  end;


  TSortMarker = (smNone, smDown, smUp);

  TGetCellParamsEvent = procedure(Sender: TObject; Field: TField;
            AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TGetBtnParamsEvent = procedure(Sender: TObject; Field: TField;
            AFont: TFont; var Background: TColor; var ASortMarker: TSortMarker;
            IsDown: Boolean) of object;

  TErrorEvent  = procedure(Sender: TObject; const aStrParam: string) of object;
  TStatusEvent = procedure(Sender: TObject; const aStrParam: string) of object;

  TGEMDBGridExtended = class(TCustomDBGrid)
  private
    FCurrentDrawRow        : Integer;
    FAltRowColorUse        : Boolean;

    FAlternateRowColor     : TColor;
    FAlternateRowFontColor : TColor;
    fAltFontColor          : TColor;
    FReadOnlyCellColor     : TColor;
    FSortArrowColor        : TColor;
    FAutoWidthAllColor     : TColor;
    FActiveCellFontColor   : TColor;
    FSelectedCellFontColor : TColor;


    FAltRowColor2Finish          : TColor;
    FAltRowColor2Start           : TColor;
    FAltRowColor1Finish          : TColor;
    FAltRowColor1Start           : TColor;
    FSelectedColorFinish         : TColor;
    FSelectedColorStart          : TColor;
    FTitleColorCenter            : TColor;
    FAltRowColor2Steps           : integer;
    FAltRowColor1Steps           : integer;
    FSelectedColorSteps          : integer;
    FAutoFocus                   : boolean;
    FHotTrack                    : boolean;
    FAltRowColor1CenterPosition  : integer;
    FAltRowColor2CenterPosition  : integer;
    FAltRowColor1Center          : TColor;
    FAltRowColor2Center          : TColor;
    FSelectedColorCenterPosition : integer;
    FSelectedColorCenter         : TColor;
    FTitleColorCenterPosition    : integer;
    FTitleColorSteps             : integer;
    FTitleColorFinish            : TColor;
    FTitleColorStart             : TColor;
    FAutoWidthMin                : integer;
    FAutoWidthMax                : integer;
    FMoveSoundEnabled            : boolean;
//    FSortArrowColor            : TColor;
//    FAutoWidthAllColor         : TColor;
    FDblClickSoundEnabled        : boolean;
    FSortSoundEnabled            : boolean;
    FEscSoundEnabled             : boolean;
    FAllowFilter                 : boolean;
    FOnBeforeFilter              : TFilterEvent;
    FAllowRowResize              : boolean;
    FAllowSort                   : boolean;
    FActiveColorCenterPosition   : integer;
    FActiveColorSteps            : integer;
    FActiveColorStart            : TColor;
    FActiveColorFinish           : TColor;
    FActiveColorCenter           : TColor;
    FPopupMenuCommands           : TStrings;
    FOnPopupCommand              : TOnPopupCommandEvent;
//    FActiveCellFontColor       : TColor;
//    FSelectedCellFontColor     : TColor;


    FSelecting             : Boolean;
    FMultiSelect           : Boolean;

    DateFieldCalendar      : TGEMCalendar;
    OriginalOptions        : TDBGridOptions;
    ColSelEditor           : TGEMDBGridEditForm;
    FPostOnEnterKey        : Boolean;
    FPaintInfo            : TJvGridPaintInfo;
    fHighLightSelectedRow : TSelectedRowHighLight;
    fMouseOverRow         : integer;
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
//    FAllowSort              : boolean;
//    FAutoWidthMin           : integer;
//    FAutoWidthMax           : integer;
//    FOnBeforeFilter         : TFilterEvent;

    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetBtnParams : TGetBtnParamsEvent;
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
    lastSortType: TSortType;
    lastRowCount: integer;
    ri: array of TRowInfo;

    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure WmBeforeCreate(var Msg: TMessage); message WM_CREATE;
    procedure WmEnable(var Msg: TMessage); message WM_ENABLE;
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
    procedure GetCellProps(Column: TColumn; AFont: TFont;
      var Background: TColor; Highlight: Boolean);
    procedure SetMultiSelect(const Value: Boolean);
//    procedure SetHighLigthSelectedRow(const Value: boolean);
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure ControlWndProc(var Message: TMessage);
    function checkDBPrerequisites: boolean;
    function pointInRect(p: TPoint; r: TRect): boolean;
    procedure autoFitColumn(ix: integer; canDisableControls:boolean);
    procedure SetAllowSort(const Value: boolean);
    function isMultiSelectedRow: Boolean;
    procedure myDrawText(s:string; outputCanvas: Tcanvas; drawRect: TRect;
                  drawAlignment:TAlignment ; drawFont:TFont);
    procedure toggleTransparentColor;
    function  isVisibleColumn(cl: TColumn): boolean;
    function  cellWidth(ACol, ARow: integer): integer;
    function  cellHeight(ACol, ARow: integer): integer;
    procedure drawTriangleInRect(r: TRect; st: TSortType; al: TAlignment);
    procedure drawCircleInRect(r: TRect);
    procedure ClearFilter;
    procedure ClearSort;
    procedure SetOnBeforeFilter(const Value: TFilterEvent);
    procedure SetAutoWidthAllColor(const Value: TColor);
    procedure produceTitleGradient;
    procedure produceActiveGradient;
    procedure produceSelectedGradient;
    procedure produceAltRow1Gradient;
    procedure produceAltRow2Gradient;
    procedure drawVerticalGradient(var grBmp: TBitmap; gHeight: integer;
      color1, color2, color3: TColor; centerPosition: integer = 50);
    procedure extractRGB(cl: TColor; var red, green, blue: byte);

  protected
    //fAltRowColor: TColor;

    myIndicators: TImageList;

    procedure DblClick; override;
    procedure TitleClick(Column: TColumn); override;
    procedure DrawCell(ACol, ARow: integer; ARect: TRect;
      AState: TGridDrawState); override;
    procedure DrawColumnCell(const Rect: TRect; DataCol: integer;
      Column: TColumn; State: TGridDrawState); override;

    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure edtSearchCriteriaWindowProc(var Message: TMessage);

    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;

    procedure Scroll(Distance: integer); override;
    procedure CalcSizingState(X: integer; Y: integer; var State: TGridState;
      var Index: integer; var SizingPos: integer; var SizingOfs: integer;
      var FixedInfo: TGridDrawInfo); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

    procedure onPopupMenuItemClick(Sender : TObject);

   //===================================



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
//    procedure Scroll(Distance: Integer); override;
    procedure ColEnter(); override;
    procedure ColExit(); override;
    procedure CellClick(Column: TColumn); override;
//    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
//    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
//    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
//    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
//    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
//                                    Column: TColumn; State: TGridDrawState); override;
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

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPostOnEnter(Reader: TReader);
    procedure ReadAlternateRowColor(Reader: TReader);
    procedure ReadAlternateRowFontColor(Reader: TReader);
    function ActiveRowSelected: Boolean;
    procedure WriteCellText(ARect: TRect; DX, DY: Integer; const Text: string;
      Alignment: TAlignment; ARightToLeft: Boolean; FixCell: Boolean; Options: Integer = 0);
    procedure CallDrawCellEvent(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
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
    property AutoWidthAllColor: TColor read FAutoWidthAllColor
      write SetAutoWidthAllColor default clBlue;
    property OnBeforeFilter: TFilterEvent read FOnBeforeFilter
      write SetOnBeforeFilter;
    property AllowSort: boolean read FAllowSort write SetAllowSort default true ;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;

        { ReadOnlyCellColor: The color of the cells that are read only => OnCanEditCell, not Field.CanModify }
    property ReadOnlyCellColor: TColor read FReadOnlyCellColor write FReadOnlyCellColor default clDefault;

    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property PostOnEnterKey: Boolean read FPostOnEnterKey write FPostOnEnterKey default False;
    property HighLightSelectedRow: TSelectedRowHighLight read fHighLightSelectedRow write SetHighLightSelectedRow;
    property CellHintPosition: TDBGridCellHintPosition read FCellHintPosition write FCellHintPosition default gchpDefault;
    property ShowTitleHint: Boolean read FShowTitleHint write FShowTitleHint default False;
    property BooleanFieldCheckBox: Boolean read FBooleanFieldCheckBox write SetCheckBoxes;
//    property GridInfoFile : string read fGridInfoFile write fGridInfoFile;
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
//    property OnMouseDown;
    property OnMouseMove;
//    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
    property DataLink;

  end;

//procedure Register;

implementation

//{..$R Button.res}
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


{ TGEMDBGridExtended }


constructor TGEMDBGridExtended.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  fGridInfoFile := '';
  FScrollBars := ssBoth;
  FAlwaysShowEditor := True;
  FDisableCount := True;
  FClearSelection := True;
  fColFileName := '';
  FTitleArrowDown := False;
  theTitleArrowGlyph := TBitmap.Create;
  FInMouseClick := False;
  FCell.X := -1;
  FCell.Y := -1;
  theTitleArrowGlyph.LoadFromResourceName(HInstance, 'ARROWDOWN');
  searchVisible := false;
  FSortArrowColor := clBlack;

  grBmpSelected := nil;
  grBmpAlt1 := nil;
  grBmpAlt2 := nil;


  FTitleColorStart := clGreen;
  FTitleColorCenter := clGreen;
  FTitleColorCenterPosition := 50;
  FTitleColorFinish := $00BBFFBB;
  FTitleColorSteps := 50;


  FActiveColorStart := $00FFD7EB;
  FActiveColorCenter := $00FF0080;
  FActiveColorCenterPosition := 50;
  FActiveColorFinish := $00FFD7EB;
  FActiveColorSteps := 50;




  FSelectedColorStart := $00EDB6B6;
  FSelectedColorCenter := $00F31212;
  FSelectedColorCenterPosition := 50;
  FSelectedColorFinish := $00EDB6B6;
  FSelectedColorSteps := 50;

  FAltRowColor1Start := $00DAFEFC;
  FAltRowColor1Center := $000EFAEE;
  FAltRowColor1CenterPosition := 50;
  FAltRowColor1Finish := $00DAFEFC;
  FAltRowColor1Steps := 50;

  FAltRowColor2Start := $00ECD9FF;
  FAltRowColor2Center := $00B164FF;
  FAltRowColor2CenterPosition := 50;
  FAltRowColor2Finish := $00ECD9FF;
  FAltRowColor2Steps := 50;

  FMoveSoundEnabled := false;
  FSortArrowColor := clRed;
  FAutoWidthAllColor := clBlue;

  produceTitleGradient;
  produceActiveGradient;
  produceSelectedGradient;
  produceAltRow1Gradient;
  produceAltRow2Gradient;

  tempFont:=TFont.Create;



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


procedure TGEMDBGridExtended.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  {$IFDEF COMPILER9_UP}
  // The grid can only handle ssNone and ssHorizontal. We have to emulate the other modes.
  if FScrollBars = ssVertical then
    Params.Style := Params.Style and not WS_HSCROLL;
  {$ENDIF COMPILER9_UP}
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


function TGEMDBGridExtended.isMultiSelectedRow: Boolean;
var
    Index: Integer;
begin
    Result := (dgMultiSelect in Options) and
      SelectedRows.Find(Datalink.Datasource.Dataset.Bookmark, Index);
end;

function TGEMDBGridExtended.isVisibleColumn(cl: TColumn): boolean;
var
  tw, i: integer;
  di: TGridDrawInfo;
begin
  Result := (cellWidth(cl.Index + 1, 0) > 0);
  Exit;

  if myLeftCol > (cl.Index + 1) then
  begin
    Result := false;
    Exit;
  end
  else if myLeftCol = (cl.Index + 1) then
  begin
    Result := true;
    Exit;
  end;

  CalcDrawInfo(di);
  tw := 0;
  for i := 0 to di.Horz.FixedCellCount - 1 do
    tw := cellWidth(i, 0) + di.Vert.EffectiveLineWidth;

  // iterate through data columns up to one before this column
  for i := myLeftCol - 1 to cl.Index - 1 do
    tw := tw + Columns[i].Width + di.Vert.EffectiveLineWidth;
  Result := (tw > 0) and (tw <= ClientWidth);
end;

procedure TGEMDBGridExtended.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TGEMDBGridExtended.loadConfig(fn: String);
var
  cf: TIniFile;
begin
  try
    cf := TIniFile.create(fn);
//    TitleColorStart := cf.ReadInteger('config', 'TCS', clWhite);
//    TitleColorCenter := cf.ReadInteger('config', 'TCC', clWhite);
//    TitleColorCenterPosition := cf.ReadInteger('config', 'TCCP', 50);
//    TitleColorFinish := cf.ReadInteger('config', 'TCF', clWhite);
//    TitleColorSteps := cf.ReadInteger('config', 'TCSTP', 50);
//
//    ActiveColorStart := cf.ReadInteger('config', 'ACS', clWhite);
//    ActiveColorCenter := cf.ReadInteger('config', 'ACC', clNavy);
//    ActiveColorCenterPosition := cf.ReadInteger('config', 'ACCP', 50);
//    ActiveColorFinish := cf.ReadInteger('config', 'ACF', clWhite);
//    ActiveColorSteps := cf.ReadInteger('config', 'ACSTP', 50);
//    ActiveCellFontColor := cf.ReadInteger('config', 'ACFC', clBlack);
//
//
//    SelectedColorStart := cf.ReadInteger('config', 'SCS', clWhite);
//    SelectedColorCenter := cf.ReadInteger('config', 'SCC', clWhite);
//    SelectedColorCenterPosition := cf.ReadInteger('config', 'SCCP', 50);
//    SelectedColorFinish := cf.ReadInteger('config', 'SCF', clWhite);
//    SelectedColorSteps := cf.ReadInteger('config', 'SCSTP', 50);
//    SelectedCellFontColor := cf.ReadInteger('config', 'SCFC', clBlack);
//
//    AltRowColor1Start := cf.ReadInteger('config', 'A1CS', clWhite);
//    AltRowColor1Center := cf.ReadInteger('config', 'A1CC', clWhite);
//    AltRowColor1CenterPosition := cf.ReadInteger('config', 'A1CCP', 50);
//    AltRowColor1Finish := cf.ReadInteger('config', 'A1CF', clWhite);
//    AltRowColor1Steps := cf.ReadInteger('config', 'A1CSTP', 50);
//
//    AltRowColor2Start := cf.ReadInteger('config', 'A2CS', clWhite);
//    AltRowColor2Center := cf.ReadInteger('config', 'A2CC', clWhite);
//    AltRowColor2CenterPosition := cf.ReadInteger('config', 'A2CCP', 50);
//    AltRowColor2Finish := cf.ReadInteger('config', 'A2CF', clWhite);
//    AltRowColor2Steps := cf.ReadInteger('config', 'A2CSTP', 50);
//
//    SortArrowColor := cf.ReadInteger('config', 'SortArrowColor', clYellow);
//    AutoWidthAllColor := cf.ReadInteger('config', 'AutoWidthAllColor',
//      clYellow);
//
//    AutoWidthMin := cf.ReadInteger('config', 'AutoWidthMin', 0);
//    AutoWidthMax := cf.ReadInteger('config', 'AutoWidthMax', 0);
//
//    AllowSort := cf.ReadBool('config', 'AllowSort', false);
//    AllowRowResize := cf.ReadBool('config', 'AllowRowResize', false);
//    AllowFilter := cf.ReadBool('config', 'AllowFilter', false);
//    MoveSoundEnabled := cf.ReadBool('config', 'MoveSoundEnabled', false);
//    SortSoundEnabled := cf.ReadBool('config', 'SortSoundEnabled', false);
//    DblClickSoundEnabled := cf.ReadBool('config',
//      'DblClickSoundEnabled', false);
//    EscSoundEnabled := cf.ReadBool('config', 'EscSoundEnabled', false);
//    HotTrack := cf.ReadBool('config', 'HotTrack', false);
//    AutoFocus := cf.ReadBool('config', 'AutoFocus', false);
//
    // *********************************************************************

    Font.Name := cf.ReadString('config', 'FN', 'Tahoma');
    Font.Size := cf.ReadInteger('config', 'FS', 8);
    Font.Color := cf.ReadInteger('config', 'FC', clBlack);
    SetSetProp(Font, 'Style', cf.ReadString('config', 'FSTL', ''));
    Color := cf.ReadInteger('config', 'Color', clWhite);
    FixedColor := cf.ReadInteger('config', 'FixColor', clBtnFace);
    TitleFont.Name := cf.ReadString('config', 'TFN', 'Tahoma');
    TitleFont.Color := cf.ReadInteger('config', 'TFC', clBlack);
    TitleFont.Size := cf.ReadInteger('config', 'TFS', 8);
    SetSetProp(TitleFont, 'Style', cf.ReadString('config', 'TFSTL', ''));

    SetSetProp(Self, 'Options', cf.ReadString('config', 'OPT',
      'dgEditing,dgTitles,dgIndicator,dgColumnResize,dgColLines,dgRowLines,dgTabs,dgConfirmDelete,dgCancelOnExit')
      );
  finally
    cf.Free;
  end;
end;


procedure TGEMDBGridExtended.CMMouseEnter(var Message: TMessage);
begin

end;

procedure TGEMDBGridExtended.CMMouseLeave(var Message: TMessage);
begin
  inherited;
//  {$IFNDEF COMPILER14_UP}
//  {$IFDEF JVCLThemesEnabled}
//  if UseXPThemes and StyleServices.Enabled then
//    if ValidCell(FCell) then
//      InvalidateCell(FCell.X, FCell.Y);
//  {$ENDIF JVCLThemesEnabled}
//  {$ENDIF ~COMPILER14_UP}
  FCell.X := -1;
  FCell.Y := -1;
  FPaintInfo.MouseInCol := -1;
  FPaintInfo.ColPressedIdx := -1;
end;


procedure TGEMDBGridExtended.WMPaint(var Message: TWMPaint);
var
  R: TRect;
begin
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

procedure TGEMDBGridExtended.WMVScroll(var Msg: TWMVScroll);
var
  ALeftCol: Integer;
begin
  if dgRowSelect in Options then
  begin
    ALeftCol := LeftCol;
    inherited;
    LeftCol := ALeftCol;
  end
  else
    inherited;
end;



procedure TGEMDBGridExtended.WndProc(var Message: TMessage);
begin
  inherited;

end;

function TGEMDBGridExtended.ActiveRowSelected: Boolean;
var
  Index: Integer;
begin
  if MultiSelect and DataLink.Active then
    Result := SelectedRows.Find(DataLink.DataSet.Bookmark, Index)
  else
    Result := False;
end;



procedure TGEMDBGridExtended.GetCellProps(Column: TColumn; AFont: TFont;
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
      if (FAlternateRowColor <> clNone) and (FAlternateRowColor <> Color) then
      begin
        // Prefer the column's color
        if not ((cvColor in Column.AssignedValues) and (Column.Color <> Column.DefaultColor)) then
          Background := AltRowColor;
      end;
      if FAlternateRowFontColor <> clNone then
      begin
        // Prefer the column's font.color if it has a prefered color
        if not ((cvColor in Column.AssignedValues) and (Column.Color <> Column.DefaultColor)) then
          AFont.Color := AltRowColor;
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
    FOnGetCellParams(Self, Column.Field, AFont, Background, Highlight);
//  else
//  if Assigned(FOnGetCellProps) then
//    FOnGetCellProps(Self, Column.Field, AFont, Background);
end;


procedure TGEMDBGridExtended.DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer;
                                                   Column: TColumn; State: TGridDrawState);
var
  MemoText: string;
begin
  if Assigned(Column.Field) and
    ({WordWrapAllFields or} (Column.Field is TStringField) {or (ShowMemos and IsMemoField(Column.Field))}) then
  begin
    MemoText := Column.Field.DisplayText;
//    if FShowMemos and IsMemoField(Column.Field) then
//    begin
//      // The MemoField's default DisplayText is '(Memo)' but we want the content
//      if not Assigned(Column.Field.OnGetText) then
//        MemoText := Column.Field.AsString;
//    end;
    WriteCellText(Rect, 2, 2, MemoText, Column.Alignment, UseRightToLeftAlignmentForField(Column.Field, Column.Alignment), False);
  end;
//  else if GetImageIndex(Column.Field) < 0 then  // Mantis 5013: Must not call inherited drawer, or the text will be painted over
//  begin
////    if DrawThemedHighlighting(Canvas, Rect) then
////      Canvas.Brush.Style := bsClear;
    inherited DefaultDrawColumnCell(Rect, DataCol, Column, State);
//  end;
end;


procedure TGEMDBGridExtended.WriteCellText(ARect: TRect; DX, DY: Integer; const Text: string;
  Alignment: TAlignment; ARightToLeft: Boolean; FixCell: Boolean; Options: Integer = 0);
//const
//  AlignFlags: array [TAlignment] of Integer =
//    (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
//     DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
//     DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
//  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
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
//    if WordWrap then
//      DrawOptions := DrawOptions or DT_WORDBREAK;
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
//  if ReduceFlicker
//     {$IFDEF COMPILER14_UP} and not FixCell {$ENDIF}
//     {$IFDEF JVCLThemesEnabled} and not (UseXPThemes and StyleServices.Enabled) {$ENDIF} then
//  begin
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
//  end
//  else
//  begin
//    // No offscreen bitmap - The display is faster but flickers
//    if IsRightToLeft then
//      R := Rect(ARect.Left, ARect.Top, ARect.Right - 1 - DX, ARect.Bottom - DY - 1)
//    else
//      R := Rect(ARect.Left + DX, ARect.Top + DY, ARect.Right - 1, ARect.Bottom - 1);
//    B := ARect;
//    DrawAText(Canvas);
//  end;
end;



procedure TGEMDBGridExtended.DblClick;
var
  plc, // previous left column
  i: integer;
  p: TPoint;
  r: TRect;

begin
  inherited;

  if not checkDBPrerequisites then
    Exit;

//  playSoundInMemory(FDblClickSoundEnabled, sndDblClick, 'DblClick');

  plc := leftCol;
  p := CalcCursorPos;

  // if cell is the corner one then autofit all columns
  if pointInRect(p, CellRect(0, 0)) then
  begin
    autoFitAll;
    Exit;
  end;

  // find the column that should be auto widthed
  for i := 0 to Columns.Count - 1 do
  begin
    r := CellRect(i + 1, 0);
    // if you want just title DblClicks uncomment this line
    // if (p.Y>=r.Top) and (p.Y<=r.Bottom) then
    begin
      if (UseRightToLeftAlignment and (abs(p.X - r.Left) < 5)) or
        ((not UseRightToLeftAlignment) and (abs(p.X - r.Right) < 5)) then
      begin
        autoFitColumn(i, true);
        leftCol := plc;
        // don't allow an extra click event
        dblClicked := true;
        break;
      end
    end;
  end;
end;


procedure TGEMDBGridExtended.DefaultDataCellDraw(const Rect: TRect; Field: TField;
                                                   State: TGridDrawState);
begin
  DefaultDrawDataCell(Rect, Field, State);
end;

procedure TGEMDBGridExtended.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('AlternRowColor', ReadAlternateRowColor, nil, False);
  Filer.DefineProperty('AlternRowFontColor', ReadAlternateRowFontColor, nil, False);
  Filer.DefineProperty('PostOnEnter', ReadPostOnEnter, nil, False);

  // We need to migrate the Options set for Delphi 2010 due to the added flags
//  Filer.DefineProperty('Delphi2010OptionsMigrated', ReadDelphi2010OptionsMigrated, WriteDelphi2010OptionsMigrated,
//    {$IFDEF COMPILER14_UP}
//    [dgTitleClick, dgTitleHotTrack] * Options = [] // if one of them is set we already know that we are migrated
//    {$ELSE}
//    False
//    {$ENDIF COMPILER14_UP}
//  );
end;


procedure TGEMDBGridExtended.ReadPostOnEnter(Reader: TReader);
begin
  PostOnEnterKey := Reader.ReadBoolean;
end;


procedure TGEMDBGridExtended.ReadAlternateRowColor(Reader: TReader);
begin
  if Reader.ReadBoolean then
    AltRowColor := GEMDefaultAlternateRowColor // this was the previous default row color
  else
    AltRowColor := clNone;
end;


procedure TGEMDBGridExtended.ReadAlternateRowFontColor(Reader: TReader);
begin
  if Reader.ReadBoolean then
    AltFont := GEMDefaultAlternateRowFontColor
  else
    AltFont := clNone;
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


procedure TGEMDBGridExtended.ClearFilter;
var
  lrh, llc: integer;
begin
  try
    if not checkDBPrerequisites then
      Exit;
    if DataSource.DataSet.Filtered then
    begin

      llc := leftCol;
      lrh := DefaultRowHeight;

      DataSource.DataSet.Filtered := false;
      DataSource.DataSet.Filter := '';

      lastSearchStr := '';
      searchFieldName := '';
      lastSearchColumn := nil;

      edtSearchCriteria.Text := '';
      edtSearchCriteria.Visible := false;
      searchVisible := false;

      leftCol := llc;
      filtering := false;

      DefaultRowHeight := lrh;

      invalidate;

    end;
  except
    ShowMessage('Error in clearing filter !')
  end;
end;

procedure TGEMDBGridExtended.ClearSort;
begin
  lastSortColumn := nil;
  lastSortType := stNone;
  if DataSource.DataSet is TCustomADODataSet then
    TCustomADODataSet(DataSource.DataSet).Sort := '';
end;

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


procedure TGEMDBGridExtended.CalcSizingState(X, Y: integer;
  var State: TGridState; var Index, SizingPos, SizingOfs: integer;
  var FixedInfo: TGridDrawInfo);
begin
  inherited;

end;

procedure TGEMDBGridExtended.CallDrawCellEvent(ACol, ARow: Longint;
  ARect: TRect; AState: TGridDrawState);
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
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


function TGEMDBGridExtended.cellHeight(ACol, ARow: integer): integer;
var
  r: TRect;
begin
  r := CellRect(ACol, ARow);
  Result := r.bottom - r.top;
end;

function TGEMDBGridExtended.cellWidth(ACol, ARow: integer): integer;
var
  r: TRect;
begin
  r := CellRect(ACol, ARow);
  Result := r.Right - r.Left;
end;

function TGEMDBGridExtended.checkDBPrerequisites: boolean;
begin
  Result := false;
  if not Assigned(DataSource) then
    Exit;
  if not Assigned(DataSource.DataSet) then
    Exit;
  if not DataSource.DataSet.Active then
    Exit;
  Result := true;
end;


procedure TGEMDBGridExtended.DrawColumnCell(const Rect: TRect; DataCol: Integer;
                                            Column: TColumn; State: TGridDrawState);
var
  row, i: integer;
  r:trect;
begin
  try
    inherited;

    if not checkDBPrerequisites then
      Exit;

    row := DataSource.DataSet.recNo;

    // if number of rows have been changed then reallocate memory for their info
    if RowCount <> lastRowCount then begin
      SetLength(ri, RowCount);
      lastRowCount := RowCount;
      // reset all records
      for i := 0 to RowCount - 1 do begin
        ri[i].recNo := -1;
        ri[i].top := 0;
        ri[i].bottom := 0;
      end;
    end;

    // find first empty rowInfo element or same row position
    // and store this row info
    for i := 0 to RowCount - 1 do
      if (ri[i].recNo = -1) or
        ((ri[i].top = Rect.top) and (ri[i].bottom = Rect.bottom)) then begin
        ri[i].recNo := row;
        ri[i].top := Rect.top;
        ri[i].bottom := Rect.bottom;
        break;
      end;

    //save previous column font
    tempFont.Assign(Column.Font);

    if (gdSelected in State) then begin
      // draw gradient background
      Canvas.StretchDraw(Rect, grBmpActive);
      tempFont.Color := FActiveCellFontColor;
    end
    else if isMultiSelectedRow then begin
      Canvas.StretchDraw(Rect, grBmpSelected);
      tempFont.Color := FSelectedCellFontColor;
    end
    else if Odd(row) then begin
      Canvas.StretchDraw(Rect, grBmpAlt1);
    end
    else begin
      Canvas.StretchDraw(Rect, grBmpAlt2);
    end;

    if Column.Field<>nil then
      myDrawText(Column.Field.DisplayText, Canvas, Rect, Column.alignment, tempFont);

    //draw border
    r:=rect;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color:=clGray;
    InflateRect(r, 1,1);
    Canvas.Rectangle(r);

  except
    OutputDebugString(PChar(format('Error in DCC : %d %d',
      [Rect.top, Rect.Left])));
  end;


//Const
//  CtrlState : array[Boolean] of Integer =(DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
//
//
//
//
//
//var
//  txtRect: TRect;
//  btnRect: TRect;
//  CheckBoxRectangle : TRect;
//  {$IFDEF  GRID_DEBUG}
//    s: string;
//  {$ENDIF}
//begin
//  try
//    //inherited;
//    Canvas.Brush.Color := Self.Color;
//    Canvas.FillRect(Rect);
//
//    if AltRowColorUse then
//      if (DataLink.ActiveRecord mod 2)=0 then
//        Canvas.Brush.Color := FAlternateRowColor
//      else
//        Canvas.Brush.Color := Self.Color;
//    if (Columns.State = csDefault) {or not DefaultDrawing} or (csDesigning in ComponentState) then
//      inherited DrawDataCell(Rect, Column.Field, State);
//    inherited DefaultDrawColumnCell(Rect, DataCol, Column, State);
//
//    // Boolean Field check box
//    if (Column.Field <> nil) and (FBooleanFieldCheckBox) and (Column.Field.DataType = ftBoolean) then begin
//      //ShowMessage('DrawCheckBox');
//      Canvas.FillRect(Rect);
//      {$IFDEF  GRID_DEBUG}
//      if fDoFileCellTest then begin
//        s := '...Rect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, Rect.TopLeft.X, Rect.TopLeft.y, Rect.BottomRight.X, Rect.BottomRight.y]);
//        DataStream.WriteBuffer(S[1], Length(S));
//
//        Writeln(myFile, ' == Check box ====================================');
//        Writeln(myFile, '...Rect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, Rect.TopLeft.X, Rect.TopLeft.y, Rect.BottomRight.X, Rect.BottomRight.y]));
//      end;
//      {$ENDIF}
//      CheckBoxRectangle.Left := Rect.Left + 2;
//      CheckBoxRectangle.Right := Rect.Right - 2;
//      CheckBoxRectangle.Top := Rect.Top + 2;
//      CheckBoxRectangle.Bottom := Rect.Bottom - 2;
//      DrawFrameControl(Canvas.Handle, CheckBoxRectangle, DFC_BUTTON, CtrlState[Column.Field.AsBoolean]);
//    end;
//
//    //  Date field button
//    if (Column.Field <> nil) and (FDatePullDownDateFields) and (Column.Field.DataType = ftDate) then begin
//      txtRect := Rect;
//
//      Canvas.Font.Name := Font.Name;
//      Canvas.Font.Size := Font.Size;
//      txtRect.Left := Rect.left + TXT_MARG.x;
//      txtRect.Right := txtRect.Right - (Rect.Right - Rect.Left) - TXT_MARG.x;
//
//
//
//      DrawText(canvas.Handle, PChar (Column.Field.AsString), length(Column.Field.AsString),
//               txtRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER{ or DT_END_ELLIPSIS});
//
//      // draw button
//      btnRect := GetBtnRect(Rect, False);
//
//      {$IFDEF  GRID_DEBUG}
//      if fDoFileCellTest then begin
//        Writeln(myFile, ' == Date Pull Down  ==============================');
//        Writeln(myFile, '...Rect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, Rect.TopLeft.X, Rect.TopLeft.y, Rect.BottomRight.X, Rect.BottomRight.y]));
//        Writeln(myFile, 'TxtRect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, txtRect.TopLeft.X, txtRect.TopLeft.y, txtRect.BottomRight.X, txtRect.BottomRight.y]));
//        Writeln(myFile, 'BTNRect: '+Format('Col: %d -- %d, %d By %d, %d', [DataCol, btnRect.TopLeft.X, btnRect.TopLeft.y, btnRect.BottomRight.X, btnRect.BottomRight.y]));
//      end;
//      {$ENDIF}
//      Canvas.Brush.Color := clBtnFace;
//      Canvas.Pen.Style := psClear;
//      Canvas.Rectangle(btnRect);
//
//      DrawEdge(canvas.Handle, btnRect, EDGE_RAISED, BF_FLAT or BF_RECT or BF_ADJUST);
//      Canvas.Font.Name := 'Arial';
//      Canvas.Font.Size := 8;
//      Canvas.Font.Color := clBlack;
//      //DrawText(canvas.Handle, '...', -1, btnRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
//      DrawText(canvas.Handle, '...', length('...'), btnRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
//    end;
//
//    if fHighLightSelectedRow.HighLightSelectedRow then begin
//      if Not ((gdFocused in State) or (gdSelected in State)) and
//             (fMouseOverRow = 1 + DataLink.ActiveRecord) then
//      begin
//        Canvas.Brush.Color := fHighLightSelectedRow.BrushColor;
//        Canvas.Font.Color  := fHighLightSelectedRow.FontColor;
//      end;
//      if (Columns.State = csDefault) {or not DefaultDrawing} or (csDesigning in ComponentState) then
//        inherited DrawDataCell(Rect, Column.Field, State);
//      inherited DefaultDrawColumnCell(Rect, DataCol, Column, State);
//    end;
//  except
//    on e: exception do
//      ShowMessage(E.Message);
//  end;


 end;


procedure TGEMDBGridExtended.drawTriangleInRect(r: TRect; st: TSortType;
  al: TAlignment);
const
  OFFSET=2;
var
  goLeft: integer;
begin
  //if IsRightToLeft then
  begin
    if al = taLeftJustify then
      goLeft := 0
    else
      goLeft := r.Right - r.Left - 17;
  end;

  // draw shadow
  Canvas.Brush.Color := clGray;
  Canvas.Pen.Color := clGray;
  if st = stAsc then
    Canvas.Polygon([point(r.Right - 2 - OFFSET - goLeft, r.top + 10 + OFFSET),
      point(r.Right - 7 - OFFSET - goLeft, r.top + 5 + OFFSET),
      point(r.Right - 12 - OFFSET - goLeft, r.top + 10 + OFFSET)])

  else if st = stDesc then
    Canvas.Polygon([point(r.Right - 2 - OFFSET - goLeft, r.top + 5 + OFFSET),
      point(r.Right - 7 - OFFSET - goLeft, r.top + 10 + OFFSET),
      point(r.Right - 12 - OFFSET - goLeft, r.top + 5 + OFFSET)]);

  // draw triangle
  Canvas.Brush.Color := FSortArrowColor;
  Canvas.Pen.Color := FSortArrowColor;
  if st = stAsc then
    Canvas.Polygon([point(r.Right - 2 - goLeft, r.top + 10),
      point(r.Right - 7 - goLeft, r.top + 5), point(r.Right - 12 - goLeft,
      r.top + 10)])

  else if st = stDesc then
    Canvas.Polygon([point(r.Right - 2 - goLeft, r.top + 5),
      point(r.Right - 7 - goLeft, r.top + 10), point(r.Right - 12 - goLeft,
      r.top + 5)]);
end;

procedure TGEMDBGridExtended.edtSearchCriteriaWindowProc(var Message: TMessage);
var
  plc, psp: integer;
  Msg: tagMSG;
  critStr: string;
begin
  // windowproc for search criteria edit box

  // edtbox doesn't know what to do whith WM_MOUSEWHEEL so we have diverted it
  if Message.Msg = WM_MOUSEWHEEL then
    PostMessage(Handle, Message.Msg, Message.WParam, Message.LParam)
  else
    lastEditboxWndProc(Message);

  if not(( Message.Msg = WM_KEYDOWN ) or ( Message.Msg = WM_CHAR )) then
    exit;

  if Message.Msg = WM_KEYDOWN then
  begin

    if Message.WParam = VK_ESCAPE then
    begin

//      playSoundInMemory(FEscSoundEnabled, sndEsc, 'Escape');

      // first escape disappears the search box
      // second escape disables searchs and  sortings
      if searchVisible then
      begin
        // there are some remaining messages that cause windows to play an
        // exclamation sound because editbox is not visible after this.
        // by removing remaining messages we prevent that unwanted sounds
        while (GetQueueStatus(QS_ALLINPUT)) > 0 do
          PeekMessage(Msg, 0, 0, 0, PM_REMOVE);

        edtSearchCriteria.Visible := false;
        searchVisible := false;
        edtSearchCriteria.invalidate;
      end
      else
        ClearFilter;

    end
    else if (Message.WParam = VK_DOWN) then
    begin
      // if user presses down arrow it means that he/she needs to go forward
      // in records
      DataSource.DataSet.Next;
      Winapi.windows.SetFocus(Handle);
    end
    else if (Message.WParam = VK_UP) then
    begin
      DataSource.DataSet.Prior;
      Winapi.windows.SetFocus(Handle);
    end;
  end;

  // there was a change in search criteria
  if lastSearchStr<>edtSearchCriteria.Text then
  begin
    if filtering then
    begin
      plc := leftCol;
      lastSearchStr := edtSearchCriteria.Text;
      psp:=edtSearchCriteria.SelStart;

      if lastSearchStr <> '' then
      begin
        DataSource.DataSet.Filtered := false;

        critStr := '[' + searchFieldName + '] LIKE ''%' + lastSearchStr + '%''';
        //critStr := '[' + searchFieldName + '] = ''' + lastSearchStr + '*''';
        if Assigned(FOnBeforeFilter) then
          FOnBeforeFilter(Self, lastSearchColumn, lastSearchStr, critStr);
        DataSource.DataSet.Filter := critStr;

        try
          DataSource.DataSet.Filtered := true;
        except
          ShowMessage('Couldn''t filter data.');
        end;
      end
      else
      begin
        DataSource.DataSet.Filtered := false;
      end;

      leftCol := plc;
      if not edtSearchCriteria.Focused then
      begin
        Winapi.windows.SetFocus(edtSearchCriteria.Handle);
        edtSearchCriteria.SelStart:=psp;
      end;
    end;
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


procedure TGEMDBGridExtended.ColWidthsChanged;
begin
  inherited;

end;

procedure TGEMDBGridExtended.ControlWndProc(var Message: TMessage);
var
  EscapeKey: Boolean;
//  CurrentEditor: TJvDBGridControl;
begin
//  if Message.Msg = WM_CHAR then
//  begin
//    if not DoKeyPress(TWMChar(Message)) then
//      with TWMKey(Message) do
//      begin
//        CurrentEditor := FControls.ControlByName(FCurrentControl.Name);
//        if (CharCode = VK_RETURN) and (PostOnEnterKey or CurrentEditor.LeaveOnEnterKey) then
//        begin
//          CloseControl;
//          if PostOnEnterKey then
//            DataSource.DataSet.CheckBrowseMode;
//        end
//        else
//        if CharCode = VK_TAB then
//        begin
//          CloseControl;
//          PostMessage(Handle, WM_KEYDOWN, VK_TAB, KeyData);
//        end
//        else
//        begin
//          EscapeKey := (CharCode = VK_ESCAPE);
//          FOldControlWndProc(Message);
//          if EscapeKey then
//          begin
//            CloseControl;
//            if Assigned(SelectedField) then
//            begin
//              // OldValue is only available when State=dsEdit, otherwise it can throw an AV.
//              if (SelectedField.DataSet.State = dsEdit) and (SelectedField.OldValue <> SelectedField.Value) then
//                SelectedField.Value := SelectedField.OldValue
//              else if (SelectedField.DataSet.State = dsInsert) and not SelectedField.IsNull then
//                SelectedField.Clear;
//            end;
//          end;
//        end;
//      end;
//  end
//  else
//  if Message.Msg = WM_KEYDOWN then
//  begin
//    with TWMKey(Message) do
//    begin
//      CurrentEditor := FControls.ControlByName(FCurrentControl.Name);
//      if (CurrentEditor <> nil) and CurrentEditor.LeaveOnUpDownKey and
//         ((CharCode = VK_UP) or (CharCode = VK_DOWN)) and (KeyDataToShiftState(KeyData) = []) then
//      begin
//        CloseControl;
//        DataSource.DataSet.CheckBrowseMode;
//        PostMessage(Handle, WM_KEYDOWN, CharCode, KeyData);
//      end
//      else
//        FOldControlWndProc(Message);
//    end;
//  end
//  else
//  begin
//    FOldControlWndProc(Message);
//    case Message.Msg Of
//      WM_GETDLGCODE:
//        begin
//          CurrentEditor := FControls.ControlByName(FCurrentControl.Name);
//          if CurrentEditor <> nil then
//          begin
//            Message.Result := Message.Result or DLGC_WANTTAB;
//            if CurrentEditor.LeaveOnUpDownKey then
//              Message.Result := Message.Result or DLGC_WANTARROWS;
//          end;
//        end;
//      CM_EXIT:
//        HideCurrentControl;
//    end;
//  end;
end;


procedure TGEMDBGridExtended.AltColorsSet;
begin
//
end;


procedure TGEMDBGridExtended.autoFitAll;
var
  i: integer;
begin
  if not checkDBPrerequisites then
    Exit;

  //just one disablecontrols makes autofitcolumn faster but
  //it makes a longtime to respond to user in huge datasets
  //no application.processmessages to prevent application confusion
  DataSource.DataSet.DisableControls;

  for i := 0 to Columns.Count - 1 do
    autoFitColumn(Columns[i].Index, false);

  DataSource.DataSet.EnableControls;

  DefaultRowHeight := originalRowHeight;
end;

procedure TGEMDBGridExtended.autoFitColumn(ix: integer;
  canDisableControls: boolean);
var
  mw, cw: integer;
  p: pointer;
  SavePlace: TBookmark;
begin
  if not checkDBPrerequisites then
    Exit;

  mw := 0;

  with DataSource.DataSet do
  begin
    if canDisableControls then
    begin
      SavePlace := GetBookmark;
      DisableControls;
    end;


    First;
    while not Eof do
    begin
      cw := Canvas.TextWidth(FieldByName(Columns[ix].FieldName).AsString);
      if cw > mw then
        mw := cw;
      Next;
    end;

    if canDisableControls then
    begin
      EnableControls;

      if BookmarkValid(SavePlace) then
      begin
        GotoBookmark(SavePlace);
        FreeBookmark(SavePlace);
      end;
    end;
  end;

  mw := mw + 5; // put a margin aside

  if (FAutoWidthMin <> 0) then
    mw := Max(FAutoWidthMin, mw);

  if (FAutoWidthMax <> 0) then
    mw := Min(FAutoWidthMax, mw);

  Columns[ix].Width := mw;
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


procedure TGEMDBGridExtended.MouseWheelHandler(var Message: TMessage);
var
  LastRow: Integer;
begin
  LastRow := Row;
  inherited MouseWheelHandler(Message);
  if (Row <> LastRow) and (DataLink <> nil) and DataLink.Active then
    InvalidateCell(IndicatorOffset - 1, LastRow);
end;


procedure TGEMDBGridExtended.myDrawText(s: string; outputCanvas: Tcanvas;
  drawRect: TRect; drawAlignment: TAlignment; drawFont: TFont);
const
  drawFlags : array [TAlignment] of Integer =
    (DT_WORDBREAK or DT_LEFT  or DT_NOPREFIX,
     DT_WORDBREAK or DT_RIGHT  or DT_NOPREFIX,
     DT_WORDBREAK or DT_CENTER or DT_NOPREFIX );
var
  r:trect;
  bw, bh, cw, ch:integer;
begin
    if s='' then
      exit;

    if UseRightToLeftAlignment then
      case drawAlignment of
        taLeftJustify:  drawAlignment := taRightJustify;
        taRightJustify: drawAlignment := taLeftJustify;
      end;

    r:= drawRect;
    cw:=ClientWidth;
    ch:=ClientHeight;


    //set dimentions for output
    bmpDrawText.Width := ( r.Right - r.Left);
    bmpDrawText.Height := r.Bottom- r.Top;
    bw := bmpDrawText.Width;
    bh := bmpDrawText.Height;


    //set drawing area in output bmp
    drawRect.Left:=0;
    drawRect.Top:=0;
    drawRect.Right:=bw;
    drawRect.Bottom:=bh;

    // if the drawing font color is same as transparent color
    //change transparent color
    if ColorToRGB( drawFont.Color )=(ColorToRGB( bmpDrawText.TransparentColor) and $ffffff) then
       toggleTransparentColor;

    //to make entire surface of canvas transparent
    bmpDrawText.Canvas.FillRect(drawRect);


    //shrink the rectangle
    InflateRect(drawRect, -2,-2);

    //Canvas.Font.Color:=clRed;
    bmpDrawText.Canvas.Font:= drawFont;

    DrawText(bmpDrawText.Canvas.Handle,
               pchar(s), length(s), drawRect,
               drawFlags[drawAlignment]
               );

    if UseRightToLeftAlignment then
    begin
       if r.Right > ClientWidth then
       begin
          bmpClipped.Width:=cw-r.Left;
          bmpClipped.Height:=bh;
          bmpClipped.Canvas.CopyRect(bmpClipped.Canvas.ClipRect, bmpDrawText.Canvas, Rect(bw, 0, bw-( cw - r.Left ), bh) );
          outputCanvas.StretchDraw(rect(r.Left , r.Top, cw, r.Bottom), bmpClipped);
       end
       else
          outputCanvas.StretchDraw(Rect(r.Right, r.Top, r.Left, r.Bottom), bmpDrawText);
    end
    else
       outputCanvas.Draw(r.Left, r.top, bmpDrawText);
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


procedure TGEMDBGridExtended.onPopupMenuItemClick(Sender: TObject);
begin

end;

procedure TGEMDBGridExtended.Paint;
begin
  inherited;

end;

function TGEMDBGridExtended.pointInRect(p: TPoint; r: TRect): boolean;
begin
  Result := (p.X >= r.Left) and (p.X <= r.Right) and (p.Y >= r.top) and
    (p.Y <= r.bottom);
end;


procedure TGEMDBGridExtended.produceActiveGradient;
begin
  drawVerticalGradient(grBmpActive, FActiveColorSteps, FActiveColorStart,
    FActiveColorCenter, FActiveColorFinish, FActiveColorCenterPosition);
end;

procedure TGEMDBGridExtended.produceAltRow1Gradient;
begin
  drawVerticalGradient(grBmpAlt1, FAltRowColor1Steps, FAltRowColor1Start,
    FAltRowColor1Center, FAltRowColor1Finish, FAltRowColor1CenterPosition);
end;

procedure TGEMDBGridExtended.produceAltRow2Gradient;
begin
  drawVerticalGradient(grBmpAlt2, FAltRowColor2Steps, FAltRowColor2Start,
    FAltRowColor2Center, FAltRowColor2Finish, FAltRowColor2CenterPosition);
end;

procedure TGEMDBGridExtended.produceSelectedGradient;
begin
  drawVerticalGradient(grBmpSelected, FSelectedColorSteps, FSelectedColorStart,
    FSelectedColorCenter, FSelectedColorFinish, FSelectedColorCenterPosition);
end;

procedure TGEMDBGridExtended.produceTitleGradient;
begin
  drawVerticalGradient(grBmpTitle, FTitleColorSteps, FTitleColorStart,
    FTitleColorCenter, FTitleColorFinish, FTitleColorCenterPosition);
end;


procedure TGEMDBGridExtended.extractRGB(cl: TColor; var red, green, blue: byte);
begin
  red := getRValue(cl);
  green := getGValue(cl);
  blue := getBValue(cl);
end;



procedure TGEMDBGridExtended.drawVerticalGradient(var grBmp: TBitmap; gHeight: integer;
  color1, color2, color3: TColor; centerPosition: integer);
var
  graphics: TGPGraphics;
  linGrBrush: TGPLinearGradientBrush;
  r1, g1, b1, r2, g2, b2, r3, g3, b3: byte;
  colors: array [0 .. 2] of TGPColor;
  blendPoints: array [0 .. 2] of single;
begin
  try
    if Assigned(grBmp) then
      grBmp.Free;
    grBmp := TBitmap.create;

    grBmp.Width := 1;
    grBmp.Height := gHeight;

    extractRGB(color1, r1, g1, b1);
    extractRGB(color2, r2, g2, b2);
    extractRGB(color3, r3, g3, b3);
    graphics := TGPGraphics.create(grBmp.Canvas.Handle);
    linGrBrush := TGPLinearGradientBrush.create(MakePoint(0, 0),
      MakePoint(0, gHeight), MakeColor(255, 255, 255, 255),
      MakeColor(255, 255, 255, 255));

    colors[0] := MakeColor(r1, g1, b1);
    blendPoints[0] := 0;
    colors[1] := MakeColor(r2, g2, b2);
    blendPoints[1] := centerPosition / 100;
    colors[2] := MakeColor(r3, g3, b3);
    blendPoints[2] := 1;

    linGrBrush.SetInterpolationColors(@colors, @blendPoints, 3);

    graphics.FillRectangle(linGrBrush, 0, 0, 1, gHeight);

    linGrBrush.Free;
    graphics.Free;
  except
    OutputDebugString('Error in creating gradient.');
  end;
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


procedure TGEMDBGridExtended.SetAllowSort(const Value: boolean);
begin
  FAllowSort := Value;
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


procedure TGEMDBGridExtended.SetAutoWidthAllColor(const Value: TColor);
begin
  FAutoWidthAllColor := Value;
  InvalidateCell(0, 0);
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

procedure TGEMDBGridExtended.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect := Value;
end;

procedure TGEMDBGridExtended.SetOnBeforeFilter(const Value: TFilterEvent);
begin
  FOnBeforeFilter := Value;
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


procedure TGEMDBGridExtended.WmBeforeCreate(var Msg: TMessage);
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


procedure TGEMDBGridExtended.WmEnable(var Msg: TMessage);
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

procedure TGEMDBGridExtended.ErrorMsg(const msg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, msg);
end;


procedure TGEMDBGridExtended.StatusMsg(const msg: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, msg);
end;


procedure TGEMDBGridExtended.TitleClick(Column: TColumn);
var
  SavePlace: TBookmark;
  plc      : integer; // previous left column
begin
  inherited;

  if dblClicked then
  begin
    // ignore DblClicks
    dblClicked := false;
    Exit;
  end;

  if not FAllowSort then
    Exit;

  if not checkDBPrerequisites then
    Exit;

  if not(DataSource.DataSet is TCustomADODataSet) then
    Exit;

//  playSoundInMemory(FSortSoundEnabled, sndSort, 'Sort');

  plc := leftCol;
  SavePlace := DataSource.DataSet.GetBookmark;

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


  if DataSource.DataSet.BookmarkValid(SavePlace) then
  begin
    DataSource.DataSet.GotoBookmark(SavePlace);
    DataSource.DataSet.FreeBookmark(SavePlace);
  end;
  leftCol := plc;
end;


procedure TGEMDBGridExtended.toggleTransparentColor;
begin
  if (not Assigned(bmpClipped)) or (not Assigned(bmpDrawText)) then
    Exit;
  try
    if (bmpDrawText.TransparentColor and $ffffff)=clWhite then
    begin
      bmpDrawText.TransparentColor:=clBlack;
      bmpDrawText.Canvas.Brush.Color:=clBlack;
      bmpClipped.TransparentColor:=clBlack;
      bmpClipped.Canvas.Brush.Color:=clBlack;
    end
    else
    begin
      bmpDrawText.TransparentColor:=clWhite;
      bmpDrawText.Canvas.Brush.Color:=clWhite;
      bmpClipped.TransparentColor:=clWhite;
      bmpClipped.Canvas.Brush.Color:=clWhite;
    end;
  except
    OutputDebugString('Error in toggling transparent color.');
  end;
end;

function TGEMDBGridExtended.ExtractWord(N: Integer; const S: string;
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


function TGEMDBGridExtended.WordPosition(const N: Integer; const S: string;
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


procedure TGEMDBGridExtended.Resize;
begin
  inherited;

end;

procedure TGEMDBGridExtended.RestoreColumnsLayout(const Section: string);

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


procedure TGEMDBGridExtended.RowHeightsChanged;
begin
  inherited;

end;

procedure TGEMDBGridExtended.SaveColumnsLayout(const Section: string);
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



procedure TGEMDBGridExtended.saveConfig(fn: String);
var
  cf: TIniFile;
begin
  try
    cf := TIniFile.create(fn);
//    cf.WriteInteger('config', 'TCS', TitleColorStart);
//    cf.WriteInteger('config', 'TCC', TitleColorCenter);
//    cf.WriteInteger('config', 'TCCP', TitleColorCenterPosition);
//    cf.WriteInteger('config', 'TCF', TitleColorFinish);
//    cf.WriteInteger('config', 'TCSTP', TitleColorSteps);
//
//    cf.WriteInteger('config', 'ACS', ActiveColorStart);
//    cf.WriteInteger('config', 'ACC', ActiveColorCenter);
//    cf.WriteInteger('config', 'ACCP', ActiveColorCenterPosition);
//    cf.WriteInteger('config', 'ACF', ActiveColorFinish);
//    cf.WriteInteger('config', 'ACSTP', ActiveColorSteps);
//    cf.WriteInteger('config', 'ACFC', ActiveCellFontColor);
//
//    cf.WriteInteger('config', 'SCS', SelectedColorStart);
//    cf.WriteInteger('config', 'SCC', SelectedColorCenter);
//    cf.WriteInteger('config', 'SCCP', SelectedColorCenterPosition);
//    cf.WriteInteger('config', 'SCF', SelectedColorFinish);
//    cf.WriteInteger('config', 'SCSTP', SelectedColorSteps);
//    cf.WriteInteger('config', 'SCFC', SelectedCellFontColor);
//
//    cf.WriteInteger('config', 'A1CS', AltRowColor1Start);
//    cf.WriteInteger('config', 'A1CC', AltRowColor1Center);
//    cf.WriteInteger('config', 'A1CCP', AltRowColor1CenterPosition);
//    cf.WriteInteger('config', 'A1CF', AltRowColor1Finish);
//    cf.WriteInteger('config', 'A1CSTP', AltRowColor1Steps);
//
//    cf.WriteInteger('config', 'A2CS', AltRowColor2Start);
//    cf.WriteInteger('config', 'A2CC', AltRowColor2Center);
//    cf.WriteInteger('config', 'A2CCP', AltRowColor2CenterPosition);
//    cf.WriteInteger('config', 'A2CF', AltRowColor2Finish);
//    cf.WriteInteger('config', 'A2CSTP', AltRowColor2Steps);
//
//    cf.WriteInteger('config', 'SortArrowColor', SortArrowColor);
//    cf.WriteInteger('config', 'AutoWidthAllColor', AutoWidthAllColor);
//
//    cf.WriteInteger('config', 'AutoWidthMin', AutoWidthMin);
//    cf.WriteInteger('config', 'AutoWidthMax', AutoWidthMax);
//
//    cf.WriteBool('config', 'AllowFilter', AllowFilter);
//    cf.WriteBool('config', 'AllowSort', AllowSort);
//    cf.WriteBool('config', 'AllowRowResize', AllowRowResize);
//    cf.WriteBool('config', 'MoveSoundEnabled', MoveSoundEnabled);
//    cf.WriteBool('config', 'SortSoundEnabled', SortSoundEnabled);
//    cf.WriteBool('config', 'DblClickSoundEnabled', DblClickSoundEnabled);
//    cf.WriteBool('config', 'EscSoundEnabled', EscSoundEnabled);
//    cf.WriteBool('config', 'HotTrack', HotTrack);
//    cf.WriteBool('config', 'AutoFocus', AutoFocus);
//
//    // ************************************************************************
//
    cf.WriteString('config', 'FN', Font.Name);
    cf.WriteInteger('config', 'FS', Font.Size);
    cf.WriteInteger('config', 'FC', Font.Color);
    cf.WriteString('config', 'FSTL', SetToString(GetPropInfo(Font, 'Style'),
      GetOrdProp(Font, 'Style')));
    cf.WriteInteger('config', 'Color', Color);
    cf.WriteInteger('config', 'FixColor', FixedColor);
    cf.WriteString('config', 'TFN', TitleFont.Name);
    cf.WriteInteger('config', 'TFC', TitleFont.Color);
    cf.WriteInteger('config', 'TFS', TitleFont.Size);
    cf.WriteString('config', 'TFSTL',
      SetToString(GetPropInfo(TitleFont, 'Style'), GetOrdProp(TitleFont,
      'Style')));

    cf.WriteString('config', 'OPT', SetToString(GetPropInfo(Self, 'Options'),
      GetOrdProp(Self, 'options')));
  finally
    cf.Free;
  end;
end;

procedure TGEMDBGridExtended.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  ar: TRect;
  MultiSelected: Boolean;
  myLeft, indicIndex, prevousActive: integer;
begin
  try
    if not checkDBPrerequisites then
      Exit;

    if ARow > 0 then  //draw contents
    begin

      if ACol = 0 then  // draw indicators
      begin
        dec(ARow);
        Canvas.StretchDraw(ARect, grBmpTitle);
        // shape borders like a button
        DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);

        if (gdFixed in AState) then
        begin
          if Assigned(DataLink) and DataLink.Active  then
          begin
            MultiSelected := False;
            if ARow >= 0 then
            begin
              prevousActive := DataLink.ActiveRecord;
              try
                Datalink.ActiveRecord := ARow;
                MultiSelected := isMultiSelectedRow;
              finally
                Datalink.ActiveRecord := prevousActive;
              end;
            end;
            if (ARow = DataLink.ActiveRecord) or MultiSelected then
            begin
              indicIndex := 0;
              if DataLink.DataSet <> nil then
                case DataLink.DataSet.State of
                  dsEdit: indicIndex := 1;
                  dsInsert: indicIndex := 2;
                  dsBrowse:
                    if MultiSelected then
                      if (ARow <> Datalink.ActiveRecord) then
                        indicIndex := 3
                      else
                        indicIndex := 4;  // multiselected and current row
                end;
              myIndicators.BkColor := FixedColor;
              myLeft := ARect.Right - myIndicators.Width - 1;
              if Canvas.CanvasOrientation = coRightToLeft then Inc(myLeft);
              myIndicators.Draw(Canvas, myLeft,
                (ARect.Top + ARect.Bottom - myIndicators.Height) shr 1, indicIndex, dsTransparent, itImage,True);
            end;
          end;
        end;
        inc(ARow);
      end
      else // draw grid content
        inherited;
    end
    else // draw titles
    begin
      Canvas.StretchDraw(ARect, grBmpTitle);

      ar:=ARect;
      // shape borders like a button
      DrawEdge(Canvas.Handle, AR, BDR_RAISEDOUTER, BF_RECT);

      // write title
      if ACol > 0 then
        myDrawText(Columns[ACol - 1].Title.Caption, Canvas, AR, Columns[ACol - 1].Title.Alignment , Columns[ACol - 1].Title.Font)

    end;

    // make search editbox visible if it is necessary
    if lastSearchColumn <> nil then
      if (ACol > 0) and (ARow = 0) then
      begin

        if searchVisible then
        begin
          edtSearchCriteria.Visible := isVisibleColumn(lastSearchColumn);

          // reposition edit box
          if (Columns[ACol - 1].FieldName = searchFieldName) then
          begin
            // adjust search edit box position
            ar := CellRect(ACol, 0);
            if edtSearchCriteria.Visible then
            begin
              if UseRightToLeftAlignment then
                edtSearchCriteria.Left := ClientWidth - ARect.Right
              else
                edtSearchCriteria.Left := ARect.Left;
              edtSearchCriteria.Width := ARect.Right - ARect.Left;
            end;
          end;

        end
      end;

    if (ARow = 0) and (ACol = 0) then
      drawCircleInRect(ARect);

    // draw an arrow in sorted columns
    if (lastSortColumn <> nil) then
    begin
      if (lastSortColumn.Index + 1 = ACol) and (ARow = 0) then
        drawTriangleInRect(ARect, lastSortType,
             Columns[ACol - 1].Title.Alignment);
    end;

  except
    OutputDebugString(pchar(format('Error in DrawCell c = %d  r = %d' , [ACol,ARow])));
  end;
end;

procedure TGEMDBGridExtended.drawCircleInRect(r: TRect);
begin
  Canvas.Lock;
  Canvas.Brush.Color := clGray;
  Canvas.Pen.Color := clGray;
  Canvas.Ellipse(1, 4, 7, 10);

  Canvas.Brush.Color := FAutoWidthAllColor;
  Canvas.Pen.Color := FAutoWidthAllColor;
  Canvas.Ellipse(3, 3, 9, 9);
  Canvas.Unlock;
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
