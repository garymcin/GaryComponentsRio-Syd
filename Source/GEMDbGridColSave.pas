unit GEMDbGridColSave;

interface
uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.UITypes, System.Types,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms, VCL.Grids, VCL.DBGrids,
  Vcl.Menus, Vcl.Buttons, Vcl.Dialogs,

  Data.DB,

  SelColVisGrid, GEMDBGridEdit, GEMComponentsGlobal
{$IFDEF USE_CODESITE}
 , CodeSiteLogging
{$ENDIF};

const
  bmArrow = 'DBGARROW2';
  bmEdit = 'DBEDIT2';
  bmInsert = 'DBINSERT2';
  bmMultiDot = 'DBMULTIDOT2';
  bmMultiArrow = 'DBMULTIARROW2';
  KeyboardShiftStates = [ssShift, ssAlt, ssCtrl];

type
//  TgemDBGrid = class;

  TgemDBGridBitmap = class(TBitmap)
  end;

  TGEMDbGridColSave = class(TCustomDBGrid)
  strict private
    class constructor Create;
    class destructor Destroy;
  public
    property Canvas;
    property SelectedRows;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    [Stored(False)]
    property Columns stored False; //StoreColumns;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DrawingStyle;
    property Enabled;
    property FixedColor;
    property GradientEndColor;
    property GradientStartColor;
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
    property Touch;
    property Visible;
    property StyleElements;
    property StyleName;
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
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;






end.
