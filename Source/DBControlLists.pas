unit DBControlLists;

interface

uses

  Winapi.Windows, WinApi.Messages,
  System.Generics.Collections,
  System.UITypes, // for TBrush.GetColor inline
  Types, Variants, Classes, Graphics, Controls, Forms, DB, DBCtrls;
  //JvDBUtils, JvToolEdit, JvComponent, JvExControls;

const
  // (rom) renamed
  DefFieldsDelimiter = ',';

//type

//  TDlgCode =
//   (dcWantAllKeys, dcWantArrows, dcWantChars, dcButton, dcHasSetSel, dcWantTab,
//    dcNative); // if dcNative is in the set the native allowed keys are used and GetDlgCode is ignored
//  TDlgCodes = set of TDlgCode;
//
//  TLookupListStyle = (lsFixed, lsDelimited);
//  TLookupControl = class;
//  TGetImageEvent = procedure(Sender: TObject; IsEmpty: Boolean;
//    var Graphic: TGraphic; var TextMargin: Integer) of object;

//  TLookupSourceLink = class(TDataLink)
//  private
//    FDataControl: TLookupControl;
//  protected
//    procedure ActiveChanged; override;
//    procedure LayoutChanged; override;
//    procedure DataSetChanged; override;
//    procedure DataSetScrolled(Distance: Integer); override;
//  end;
//
//  TLookupControl = class(TCustomControl)
//  private
//    FLookupSource: TDataSource;
//    FDataLink: TDataSourceLink;
//    FLookupLink: TLookupSourceLink;
//    FDataFieldName: string;
//    FLookupFieldName: string;
//    FLookupDisplay: string;
//    FDisplayIndex: Integer;
//    FDataField: TField;
//    FMasterField: TField;
//    FKeyField: TField;
//    FDisplayField: TField;
//    FListFields: TList{$IFDEF RTL240_UP}<TField>{$ENDIF RTL240_UP};
//    FValue: string;
//    FDisplayValue: string;
//    FDisplayEmpty: string;
//    FSearchText: string;
//    FEmptyValue: string;
//    FEmptyStrIsNull: Boolean;
//    FEmptyItemColor: TColor;
//    FListActive: Boolean;
//    FPopup: Boolean;
//    FFocused: Boolean;
//    //FLocate: TLocateObject;
//    //FLocate: TJvLocateObject;
//    FIndexSwitch: Boolean;
//    FIgnoreCase: Boolean;
//    FItemHeight: Integer;
//    FFieldsDelimiter: Char;
//    FListStyle: TLookupListStyle;
//    FLookupFormat: string;
//    FOnChange: TNotifyEvent;
//    FOnGetImage: TGetImageEvent;
//    FLookupMode: Boolean;
//    FUseRecordCount: Boolean;
//    FRightTrimmedLookup: Boolean;
//    procedure CheckNotFixed;
//    procedure SetLookupMode(Value: Boolean);
//    function GetKeyValue: Variant;
//    procedure SetKeyValue(const Value: Variant);
//    function CanModify: Boolean;
//    procedure CheckNotCircular;
//    procedure DataLinkActiveChanged;
//    procedure CheckDataLinkActiveChanged;
//    function GetBorderSize: Integer;
//    function GetField: TField;
//    function GetDataSource: TDataSource;
//    function GetLookupField: string;
//    function GetLookupSource: TDataSource;
//    function GetTextHeight: Integer;
//    function DefaultTextHeight: Integer;
//    function GetItemHeight: Integer;
//    function LocateKey: Boolean;
//    function LocateDisplay: Boolean;
//    function ValueIsEmpty(const S: string): Boolean;
//    function StoreEmpty: Boolean;
//    procedure ProcessSearchKey(Key: Char);
//    procedure UpdateKeyValue;
//    procedure SelectKeyValue(const Value: string);
//    procedure SetDataFieldName(const Value: string);
//    procedure SetDataSource(Value: TDataSource);
//    procedure SetDisplayEmpty(const Value: string);
//    procedure SetEmptyValue(const Value: string);
//    procedure SetEmptyStrIsNull(const Value: Boolean);
//    procedure SetEmptyItemColor(Value: TColor);
//    procedure SetLookupField(const Value: string);
//    procedure SetValueKey(const Value: string);
//    procedure SetValue(const Value: string);
//    procedure SetDisplayValue(const Value: string);
//    procedure SetListStyle(Value: TLookupListStyle); virtual;
//    procedure SetFieldsDelimiter(Value: Char); virtual;
//    procedure SetLookupDisplay(const Value: string);
//    procedure SetLookupFormat(const Value: string);
//    procedure SetLookupSource(Value: TDataSource);
//    procedure SetItemHeight(Value: Integer);
//    procedure SetUseRecordCount(const Value: Boolean);
//    function ItemHeightStored: Boolean;
//    procedure DrawPicture(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
//    procedure UpdateDisplayValue;
//    function EmptyRowVisible: Boolean;
//    procedure SetDisplayIndex(const Value: Integer);
//  protected
//    //procedure FocusKilled(NextWnd: THandle); override;
//    //procedure FocusSet(PrevWnd: THandle); override;
//    //procedure GetDlgCode(var Code: TDlgCodes); override;
//    function GetReadOnly: Boolean; virtual;
//    procedure SetReadOnly(Value: Boolean); virtual;
//    procedure Change; dynamic;
//    procedure KeyValueChanged; virtual;
//    procedure DisplayValueChanged; virtual;
//    function DoFormatLine: string;
//    procedure DataLinkRecordChanged(Field: TField); virtual;
//    procedure DataLinkUpdateData; virtual;
//    procedure ListLinkActiveChanged; virtual;
//    procedure ListLinkDataChanged; virtual;
//    procedure ListLinkDataSetChanged; virtual;
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; virtual;
//    procedure UpdateDisplayEmpty(const Value: string); virtual;
//    function SearchText(var AValue: string): Boolean;
//    function GetWindowWidth: Integer;
//    property DataField: string read FDataFieldName write SetDataFieldName;
//    property DataSource: TDataSource read GetDataSource write SetDataSource;
//    property DisplayEmpty: string read FDisplayEmpty write SetDisplayEmpty;
//    property EmptyValue: string read FEmptyValue write SetEmptyValue stored StoreEmpty;
//    property EmptyStrIsNull: Boolean read FEmptyStrIsNull write SetEmptyStrIsNull default True;
//    property EmptyItemColor: TColor read FEmptyItemColor write SetEmptyItemColor default clWindow;
//    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase default True;
//    property IndexSwitch: Boolean read FIndexSwitch write FIndexSwitch default True;
//    property ItemHeight: Integer read GetItemHeight write SetItemHeight stored ItemHeightStored;
//    property ListStyle: TLookupListStyle read FListStyle write SetListStyle default lsFixed;
//    property FieldsDelimiter: Char read FFieldsDelimiter write SetFieldsDelimiter default DefFieldsDelimiter;
//    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
//    property LookupDisplayIndex: Integer read FDisplayIndex write SetDisplayIndex default 0;
//    property LookupField: string read GetLookupField write SetLookupField;
//    property LookupFormat: string read FLookupFormat write SetLookupFormat;
//    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
//    property ParentColor default False;
//    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
//    property TabStop default True;
//    property UseRecordCount: Boolean read FUseRecordCount write SetUseRecordCount default False;
//    property Value: string read FValue write SetValue stored False;
//    property DisplayValue: string read FDisplayValue write SetDisplayValue stored False;
//    property KeyValue: Variant read GetKeyValue write SetKeyValue stored False;
//    property RightTrimmedLookup: Boolean read FRightTrimmedLookup write FRightTrimmedLookup default False;
//    procedure SetFieldValue(Field: TField; const Value: string);
//    property OnChange: TNotifyEvent read FOnChange write FOnChange;
//    property OnGetImage: TGetImageEvent read FOnGetImage write FOnGetImage;
//  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
//    procedure ClearValue;
//    function Locate(const SearchField: TField; const AValue: string; Exact: Boolean): Boolean;
//    procedure ResetField; virtual;
//    function ExecuteAction(Action: TBasicAction): Boolean; override;
//    function UpdateAction(Action: TBasicAction): Boolean; override;
//    function UseRightToLeftAlignment: Boolean; override;
//    property Field: TField read GetField;
//  end;

//  {$IFDEF RTL230_UP}
//  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
//  {$ENDIF RTL230_UP}
//  TDBLookupList = class(TLookupControl)
//  private
//    FRecordIndex: Integer;
//    FRecordCount: Integer;
//    FRowCount: Integer;
//    FBorderStyle: TBorderStyle;
//    FKeySelected: Boolean;
//    FTracking: Boolean;
//    FTimerActive: Boolean;
//    FLockPosition: Boolean;
//    FSelectEmpty: Boolean;
//    FMousePos: Integer;
//    function GetKeyIndex: Integer;
//    procedure ListDataChanged;
//    procedure SelectCurrent;
//    procedure SelectItemAt(X, Y: Integer);
//    procedure SetBorderStyle(Value: TBorderStyle);
//    procedure SetRowCount(Value: Integer);
//    procedure StopTimer;
//    procedure StopTracking;
//    procedure TimerScroll;
//    procedure UpdateScrollBar;
//    procedure UpdateBufferCount(Rows: Integer);
//    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
//    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
//    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
//    procedure WMTimer(var Msg: TMessage); message WM_TIMER;
//    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
//  protected
//    procedure FontChanged; override;
//    procedure CreateParams(var Params: TCreateParams); override;
//    procedure CreateWnd; override;
//    procedure KeyValueChanged; override;
//    procedure DisplayValueChanged; override;
//    procedure ListLinkActiveChanged; override;
//    procedure ListLinkDataChanged; override;
//    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
//    procedure KeyPress(var Key: Char); override;
//    procedure Loaded; override;
//    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
//      X, Y: Integer); override;
//    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
//    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
//      X, Y: Integer); override;
//    procedure Paint; override;
//    procedure UpdateDisplayEmpty(const Value: string); override;
//    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
//    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
//  public
//    constructor Create(AOwner: TComponent); override;
//    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
//    procedure DrawItemText(Canvas: TCanvas; Rect: TRect;
//      Selected, IsEmpty: Boolean); virtual;
//    property RowCount: Integer read FRowCount write SetRowCount stored False;
//    property DisplayValue;
//    property Value;
//    property KeyValue;
//  published
//    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
//    property Align;
//    property AutoSize;
//    property Color;
//    property DataField;
//    property DataSource;
//    property DisplayEmpty;
//    property DragCursor;
//    property DragMode;
//    property EmptyItemColor;
//    property EmptyValue;
//    property EmptyStrIsNull;
//    property Enabled;
//    property FieldsDelimiter;
//    property Font;
//    property IgnoreCase;
//    property Anchors;
//    property BevelEdges;
//    property BevelInner;
//    property BevelKind default bkNone;
//    property BevelOuter;
//    property BiDiMode;
//    property Constraints;
//    property DragKind;
//    property ParentBiDiMode;
//    property ImeMode;
//    property ImeName;
//    property IndexSwitch;
//    property ItemHeight;
//    property ListStyle;
//    property LookupField;
//    property LookupDisplay;
//    property LookupDisplayIndex;
//    property LookupFormat;
//    property LookupSource;
//    property ParentColor;
//    property ParentFont;
//    property ParentShowHint;
//    property PopupMenu;
//    property ReadOnly;
//    property ShowHint;
//    property TabOrder;
//    property TabStop;
//    property Visible;
//    property UseRecordCount;
//    property OnClick;
//    property OnDblClick;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDrag;
//    property OnEnter;
//    property OnExit;
//    property OnGetImage;
//    property OnKeyDown;
//    property OnKeyPress;
//    property OnKeyUp;
//    property OnMouseDown;
//    property OnMouseMove;
//    property OnMouseUp;
//    property OnStartDrag;
//    property OnContextPopup;
//    property OnMouseWheelDown;
//    property OnMouseWheelUp;
//    property OnEndDock;
//    property OnStartDock;
//  end;

//  TPopupDataList = class(TDBLookupList)
//  private
//    FCombo: TLookupControl;
//    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
//    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
//  protected
//    procedure Click; override;
//    procedure CreateParams(var Params: TCreateParams); override;
//    procedure KeyPress(var Key: Char); override;
//  public
//    constructor Create(AOwner: TComponent); override;
//  end;
//


//TComponent = class(TPersistent, IInterface, IInterfaceComponentReference)
//  private
//    FOwner: TComponent;
//    function GetComponent(Index: Integer): TComponent;
//    function GetComponentCount: Integer;
//    function GetComponentIndex: Integer;
//    procedure SetComponentIndex(const Value: Integer);
//public
//  constructor Create(AOwner: TComponent); virtual;
//  procedure DestroyComponents;
//  function FindComponent(const AName: string): TComponent;
//  procedure InsertComponent(AComponent: TComponent);
//  procedure RemoveComponent(AComponent: TComponent);
//
//  property Components[Index: Integer]: TComponent read GetComponent;
//  property ComponentCount: Integer read GetComponentCount;
//  property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
//  property Owner: TComponent read FOwner;
//end;
//

implementation




//=== { TInternalInplaceEdit } ===============================================

//type
//  TInternalInplaceEdit = class(TInplaceEditList)
//  private
//    FDataList: TLookupList; //  TDBLookupListBox           //
//    FUseDataList: Boolean;
//    FLookupSource: TDataSource;
//  protected
//    procedure CloseUp(Accept: Boolean); override;
//    procedure DoEditButtonClick; override;
//    procedure DropDown; override;
//    procedure UpdateContents; override;
//    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
//    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
//      MousePos: TPoint): Boolean; override;
//  public
//    constructor Create(Owner: TComponent); override;
//    property DataList: TLookupList read FDataList; //  TDBLookupListBox
//    property OnChange;
//  end;
//
//constructor TInternalInplaceEdit.Create(Owner: TComponent);
//begin
//  inherited Create(Owner);
//  FLookupSource := TDataSource.Create(Self);
//end;
//
//procedure TInternalInplaceEdit.CloseUp(Accept: Boolean);
//var
//  MasterField: TField;
//  ListValue: Variant;
//begin
//  if ListVisible then
//  begin
//    if GetCapture <> 0 then
//      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
//    if ActiveList = DataList then
//      ListValue := DataList.KeyValue
//    else
//    if PickList.ItemIndex <> -1 then
//      ListValue := PickList.Items[PickList.ItemIndex]
//    else
//      ListValue := Null;
//    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
//      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
//    ListVisible := False;
//    if Assigned(FDataList) then
//      FDataList.LookupSource := nil; //  ListSource
//    FLookupSource.DataSet := nil;
//    Invalidate;
//    if Accept then
//      if ActiveList = DataList then
//        with TCustomDBGrid(Grid), TDBGrid(Grid).Columns[SelectedIndex].Field do
//        begin
//          MasterField := DataSet.FieldByName(KeyFields);
//          if MasterField.CanModify and (Grid as IJvDataControl).GetDataLink.Edit then
//            MasterField.Value := ListValue;
//        end
//      else
//      if (not VarIsNull(ListValue)) and EditCanModify then
//        with TCustomDBGrid(Grid), TDBGrid(Grid).Columns[SelectedIndex].Field do
//          Text := ListValue;
//  end;
//end;
//
//procedure TInternalInplaceEdit.DoEditButtonClick;
//begin
//  TJvDBGrid(Grid).EditButtonClick; //   TCustomDBGrid
//end;
//
//procedure TInternalInplaceEdit.DropDown;
//var
//  Column: TColumn;
//begin
//  if not ListVisible then
//  begin
//    with TDBGrid(Grid) do
//      Column := Columns[SelectedIndex];
//    if ActiveList = FDataList then
//      with Column.Field do
//      begin
//        FDataList.Color := Color;
//        FDataList.Font := Font;
//        FDataList.RowCount := Column.DropDownRows;
//        FLookupSource.DataSet := LookupDataSet;
//        FDataList.LookupField := LookupKeyFields; //  KeyField
//        FDataList.LookupDisplay := LookupResultField; //  ListField
//        FDataList.LookupSource := FLookupSource; //  ListSource
//        FDataList.KeyValue := DataSet.FieldByName(KeyFields).Value;
//      end
//    else
//    if ActiveList = PickList then
//    begin
//      PickList.Items.Assign(Column.PickList);
//      DropDownRows := Column.DropDownRows;
//    end;
//  end;
//  inherited DropDown;
//end;
//
//procedure TInternalInplaceEdit.UpdateContents;
//var
//  Column: TColumn;
//begin
//  inherited UpdateContents;
//  if FUseDataList then
//  begin
//    if FDataList = nil then
//    begin
//      FDataList := TJvPopupDataList.Create(Self);
//      FDataList.Visible := False;
//      FDataList.Parent := Self;
//      FDataList.OnMouseUp := ListMouseUp;
//    end;
//    ActiveList := FDataList;
//  end;
//  with TDBGrid(Grid) do
//    Column := Columns[SelectedIndex];
//  Self.ReadOnly := Column.ReadOnly;
//  Font.Assign(Column.Font);
//  ImeMode := Column.ImeMode;
//  ImeName := Column.ImeName;
//end;
//
//type
//  TSelection = record
//    StartPos: Integer;
//    EndPos: Integer;
//  end;
//
//procedure TInternalInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
//
//  procedure SendToParent;
//  begin
//    TJvDBGrid(Grid).KeyDown(Key, Shift);
//    Key := 0;
//  end;
//
//  procedure ParentEvent;
//  var
//    GridKeyDown: TKeyEvent;
//  begin
//    GridKeyDown := TJvDBGrid(Grid).OnKeyDown;
//    if Assigned(GridKeyDown) then
//      GridKeyDown(Grid, Key, Shift);
//  end;
//
//  function ForwardMovement: Boolean;
//  begin
//    Result := dgAlwaysShowEditor in TJvDBGrid(Grid).Options;
//  end;
//
//  function Ctrl: Boolean;
//  begin
//    Result := (Shift * KeyboardShiftStates = [ssCtrl]);
//  end;
//
//  function Selection: TSelection;
//  begin
//    SendMessage(Handle, EM_GETSEL, WPARAM(@Result.StartPos), LPARAM(@Result.EndPos));
//  end;
//
//  function CaretPos: Integer;
//  var
//    P: TPoint;
//  begin
//    Windows.GetCaretPos(P);
//    Result := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
//  end;
//
//  function RightSide: Boolean;
//  begin
//    with Selection do
//      Result := {(CaretPos = GetTextLen) and  }
//        ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen);
//  end;
//
//  function LeftSide: Boolean;
//  begin
//    with Selection do
//      Result := (CaretPos = 0) and (StartPos = 0) and
//        ((EndPos = 0) or (EndPos = GetTextLen));
//  end;
//
//begin
//  case Key of
//    VK_LEFT:
//      if ForwardMovement and (Ctrl or LeftSide) then
//        SendToParent;
//    VK_RIGHT:
//      if ForwardMovement and (Ctrl or RightSide) then
//        SendToParent;
//  end;
//  inherited KeyDown(Key, Shift);
//end;
//
//function TInternalInplaceEdit.DoMouseWheel(Shift: TShiftState;
//  WheelDelta: Integer; MousePos: TPoint): Boolean;
//var
//  DataLink: TDataLink;
//begin
//  // Do not validate a record by error
//  DataLink := (Grid as IJvDataControl).GetDataLink;
//  if DataLink.Active and (DataLink.DataSet.State <> dsBrowse) then
//    DataLink.DataSet.Cancel;
//
//  // Ideally we would transmit the action to the DataList but
//  // DoMouseWheel is protected
//  //  Result := FDataList.DoMouseWheel(Shift, WheelDelta, MousePos);
//  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
//end;

end.
