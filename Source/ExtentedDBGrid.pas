unit ExtentedDBGrid;

 interface

 uses
   WinApi.Windows, WinApi.Messages,

   System.SysUtils, System.Classes, System.Math,

   VCL.Controls, Vcl.StdCtrls, VCL.DBGrids, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBCtrls,

   Data.DB,

   gemArrow;

type
  TScrollBarType = (sbtVertical, sbtHorisontal, sbtBoth);

  TExtendedDBGrid = class;
  TComponentCollectionItem = class;
  TComponentCollection = class;

//  TFieldDataLink = class(TDataLink)
//  private
//    FField: TField;
//    FFieldName: string;
//    FControl: TComponent;
//    FEditing: Boolean;
//    FModified: Boolean;
//    FOnDataChange: TNotifyEvent;
//    FOnEditingChange: TNotifyEvent;
//    FOnUpdateData: TNotifyEvent;
//    FOnActiveChange: TNotifyEvent;
//    function GetCanModify: Boolean;
//    procedure SetEditing(Value: Boolean);
//    procedure SetField(Value: TField);
//    procedure SetFieldName(const Value: string);
//    procedure UpdateField;
//    procedure UpdateRightToLeft;
//  protected
//    procedure ActiveChanged; override;
//    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
//    procedure EditingChanged; override;
//    procedure FocusControl(Field: TFieldRef); override;
//    procedure LayoutChanged; override;
//    procedure RecordChanged(Field: TField); override;
//    procedure UpdateData; override;
//  public
//    constructor Create;
//    function Edit: Boolean;
//    procedure Modified;
//    procedure Reset;
//    property CanModify: Boolean read GetCanModify;
//    property Control: TComponent read FControl write FControl;
//    property Editing: Boolean read FEditing;
//    property Field: TField read FField;
//    property FieldName: string read FFieldName write SetFieldName;
//    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
//    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
//    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
//    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
//  end;


  TComponentCollectionItem = class(TCollectionItem)
  private
    //FDataLink: TFieldDataLink;
    FField: TField;
//    FFieldName: string;
    procedure SetDataSource(const Value: TDataSource);
    //procedure SetDataField(const Value: String);
    //function GetField: TField;
    //procedure SetField(const Value: TField);
    //procedure SetFieldName(const Value: string);
    //function GetDataField: string;
    //function GetGrid: TExtendedDBGrid;

  protected
    FFirstString: string;
    fComponentItem: TWinControl;
    FDataSource: TDataSource;

    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    //property FirstString: string read FFirstString write FFirstString;
    //property ChildCollection: TComponentCollection read FChildCollection write FChildCollection;
    property ComponentItem: TWinControl read fComponentItem write fComponentItem;
    property theDataSource: TDataSource read FDataSource write SetDataSource;
    property theDataField: TField read FField  write FField;
    //property FieldName: string read GetDataField write SetDataField; //[Stored('IsFontStored')]
  end;

  TComponentCollection = class(TOwnedCollection)
  private
    FGrid: TCustomDBGrid;
  protected
    //function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TComponentCollectionItem;
    procedure SetItem(Index: Integer; Value: TComponentCollectionItem);
    //procedure UpDate(Item: TComponentCollectionItem);
  public
    constructor Create(AOwner: TPersistent);

    //function Add: TComponentCollection;
    //function Insert: TComponentCollection;
    property Grid: TCustomDBGrid read FGrid;
    property Items[Index: Integer]: TComponentCollectionItem read GetItem write SetItem;
  end;


{ ... }






  TGridColVisibleCheckBox = class(TCheckBox)
  private
    fDBColumn          : TColumn;
    fGridComponentName : string;
    procedure SetfDBColumn(Value: TColumn);
  public
    procedure Click; override;
  published
    property DBColumn : TColumn read fDBColumn write SetfDBColumn;
    property GridComponentName: string read fGridComponentName write fGridComponentName;
  end;

  // this record holds the array of check boxes that will be placed on the panel    //
  //  on the form     //
  TDataGridCheckBoxArray = record
  private
    fCount       : Integer;
    fParent      : TWinControl;
    fCreateRun   : Boolean;
    fPosX, fPosY : integer;
  public
    DataGridCheckBoxArray: array of TCheckBox;
    constructor Create(TheParent: TWinControl; StartX, StartY: integer);
    procedure DestroyAllCheckBoxes;
    procedure MakeACheckBox;
    property CreateRun: Boolean read fCreateRun;
    property Count: Integer read fCount;
    property PosY: Integer read fPosY;
  end;




   TExtendedColumn = class(TColumn)
   private
     { Private declarations }
     FTag: Integer;
   protected
     { Protected declarations }
   published
     { Published declarations }
     property Tag: Integer read FTag write FTag;
   end;


  TGEMDBGridControlSize = (
    fcCellSize,     // Fit the control into the cell
    fcDesignSize,   // Leave the control as it was at design time
    fcBiggest       // Take the biggest size between Cell size and Design time size
  );

  TGEMDBGridControl = class(TCollectionItem)
  private
    FControlName: string;
    FFieldName: string;
    FFitCell: TGEMDBGridControlSize;
    FLeaveOnEnterKey: Boolean;
    FLeaveOnUpDownKey: Boolean;
    //FDesignWidth: Integer;  // value set when needed by PlaceControl
    //FDesignHeight: Integer; // value set when needed by PlaceControl
  public
    //procedure Assign(Source: TPersistent); override;
  published
    property ControlName: string read FControlName write FControlName;
    property FieldName: string read FFieldName write FFieldName;
    property FitCell: TGEMDBGridControlSize read FFitCell write FFitCell;
    property LeaveOnEnterKey: Boolean read FLeaveOnEnterKey write FLeaveOnEnterKey default False;
    property LeaveOnUpDownKey: Boolean read FLeaveOnUpDownKey write FLeaveOnUpDownKey default False;
  end;

  TGEMDBGridControls = class(TCollection)
  private
   // FParentDBGrid: TExtendedDBGrid;
    //function GetItem(Index: Integer): TGEMDBGridControl;
    //procedure SetItem(Index: Integer; Value: TGEMDBGridControl);
  protected
    //function GetOwner: TPersistent; override;
  public
    //constructor Create(ParentDBGrid: TExtendedDBGrid);
    //function Add: TGEMDBGridControl;
    //function ControlByField(const FieldName: string): TGEMDBGridControl;
    //function ControlByName(const CtrlName: string): TGEMDBGridControl;
    //property Items[Index: Integer]: TGEMDBGridControl read GetItem write SetItem; default;
  end;


  TGEMVisibleColumnControls = class
  private
    //fDBGridParentControl: TExtendedDBGrid;
    FArrowControl: TgemArrow;
  protected
    property ArrowControl: TgemArrow read FArrowControl;

  public
    constructor Create(AOwner: TComponent );

  end;


  TExtendedDBGrid = class(TDBGrid)
  private
    { Private declarations }
    FScrollBarType        : TScrollBarType;
    fOpenColVisibleBtn    : TGEMVisibleColumnControls;
    fUseColumnVisible     : Boolean;

    FRowHeight            : Integer ;
    pDragAndDrop          : Boolean; //is Drag/Drop now
    pOnBeforeMouseUp      : TMouseEvent;
    pOnBeforeMouseDown    : TMouseEvent;
    pOnAfterMouseUp       : TMouseEvent;
    pOnAfterMouseDown     : TMouseEvent;
    FGridState            : TGridState;

    FComponentCollection  : TComponentCollection;

    procedure WMNCCalcSize(var msg: TMessage); message WM_NCCALCSIZE;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;

    procedure SetScrollBarType(AValue: TScrollBarType);
    procedure SetUseColumnVisible(AValue: Boolean);
    procedure SetupColVisibleBtn;
  protected
     { Protected declarations }
    procedure MouseUp(Button     : TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    procedure MouseDown(Button   : TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    function CreateColumns       : TDBGridColumns; override;
    procedure SetRowHeight(Value :Integer);
  public
     { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartDragAndDrop(Var Message : TMessage);virtual;

    property GridState : TGridState Read FGridState Write FGridState;
  published
    { Published declarations }      //
    property ControlsForFields : TComponentCollection read FComponentCollection write FComponentCollection;
    property UseColumnVisible  : Boolean read fUseColumnVisible write SetUseColumnVisible;
    property ScrollBarType     : TScrollBarType read FScrollBarType write SetScrollBarType default sbtBoth;
    property RowHeight         : Integer Read FRowHeight Write SetRowHeight ;
    property DragAndDrop       : Boolean Read pDragAndDrop Write pDragAndDrop default False;
    property OnBeforeMouseUp   : TMouseEvent Read pOnBeforeMouseUp Write pOnBeforeMouseUp;
    property OnBeforeMouseDown : TMouseEvent Read pOnBeforeMouseDown Write pOnBeforeMouseDown;
    property OnAfterMouseUp    : TMouseEvent Read pOnAfterMouseUp Write pOnAfterMouseUp;
    property OnAfterMouseDown  : TMouseEvent Read pOnAfterMouseDown Write pOnAfterMouseDown;
  end;

// procedure Register;

 implementation

// procedure Register;
// begin
//   RegisterComponents('Gary"s Stuff', [TExtendedDBGrid]);
// end;



{ TComponentCollectionItem ====================================================}

procedure TComponentCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TComponentCollectionItem then
    ComponentItem := TComponentCollectionItem(Source).ComponentItem
  else
    inherited; // raises an exception
end;

//
//procedure TComponentCollectionItem.SetDataField(const Value: String);
//begin
//  //if not (csDesigning in ComponentState) then
//  //  ResetMaxLength;
//  FDataLink.FieldName := Value;
//end;

//procedure TComponentCollectionItem.SetFieldName(const Value: string);
//var
//  AField: TField;
//  Grid: TExtendedDBGrid;
//begin
//  AField := nil;
// //Grid := GetGrid;
//  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
//    not (csLoading in Grid.ComponentState) and (Length(Value) > 0) then
//      AField := Grid.DataLink.DataSet.FindField(Value); { no exceptions }
//  FFieldName := Value;
//  SetField(AField);
//  Changed(False);
//
////  AField := nil;
////  Grid := GetGrid;
////  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
////    not (csLoading in Grid.ComponentState) and (Length(Value) > 0) then
////      AField := Grid.DataLink.DataSet.FindField(Value); { no exceptions }
////  FFieldName := Value;
////  SetField(AField);
////  Changed(False);
////
//end;

//function TComponentCollectionItem.GetDataField: string;
//begin
//
//  Result := FDataLink.FieldName;
//end;

function TComponentCollectionItem.GetDisplayName: string;
begin
  Result := Format('Component %d', [index]);
end;


procedure TComponentCollectionItem.SetDataSource(const Value: TDataSource);
begin
  FDataSource := Value;
  //fComponentItem
end;

//function TComponentCollectionItem.GetGrid: TExtendedDBGrid;
//begin
//  if Assigned(Collection) and (Collection is TDBGridColumns) then
//    Result := TDBGridColumns(Collection).Grid
//  else
//    Result := nil;
//end;
//

//function TComponentCollectionItem.GetField: TField;
//begin    { Returns Nil if FieldName can't be found in dataset }
//  Result := FDataLink.Field;
//end;


//procedure TComponentCollectionItem.SetField(const Value: TField);
//begin
//  Result := FDataLink.Field;
//
//
//
////  if FField = Value then Exit;
////  if Assigned(FField) and (GetGrid <> nil) then
////    FField.RemoveFreeNotification(GetGrid);
////  if Assigned(Value) and (csDestroying in Value.ComponentState) then
////    Value := nil;    // don't acquire references to fields being destroyed
////  if FField = Value then Exit;
////  FField := Value;
////  if Assigned(Value) then
////  begin
////    if GetGrid <> nil then
////      FField.FreeNotification(GetGrid);
////    FFieldName := Value.FullName;
////  end;
////  if not IsStored then
////  begin
////    if Value = nil then
////      FFieldName := '';
////    RestoreDefaults;
////  end;
////  Changed(False);
//end;

{ TComponentCollection ========================================================}

constructor TComponentCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TComponentCollectionItem);
end;

function TComponentCollection.GetItem(Index: Integer): TComponentCollectionItem;
begin
  Result := TComponentCollectionItem(inherited GetItem(Index));
end;

procedure TComponentCollection.SetItem(Index: Integer; Value: TComponentCollectionItem);
begin
  inherited SetItem(Index, Value);
end;


{ TGEMVisibleColumnControl ====================================================}

constructor TGEMVisibleColumnControls.Create(AOwner: TComponent);
begin
  //inherited Create(TGEMVisibleColumnControls);
  FArrowControl := TgemArrow.Create(AOwner);
  //FArrowControl := AOwner;
///  fOpenColVisibleBtn := TGEMVisibleColumnControl.Create();
end;


{ TGridColVisibleCheckBox =====================================================}

procedure TGridColVisibleCheckBox.Click;
begin
  if fGridComponentName <> '' then
    fDBColumn.Visible := Checked;
  inherited;
end;


procedure TGridColVisibleCheckBox.SetfDBColumn(Value: TColumn);
begin
  fDBColumn := Value;
  //Caption := fDBColumn.FieldName;
  Caption := fDBColumn.Title.Caption;
  Checked := fDBColumn.Visible;
end;



{ TDataGridCheckBoxArray ======================================================}

constructor TDataGridCheckBoxArray.Create(TheParent: TWinControl; StartX, StartY: Integer);
begin
  if fCount > 0 then
    DestroyAllCheckBoxes;
  // this is the number of checkboxes created.
  fCount := 0;
  //  need the parent the checkbox is place on or the checkbox will not be visible.     //
  fParent := TheParent;
  fCreateRun := True;
  // the start of the check box placement.  fPosx is held constant and the fPosY        //
  //  is incremented by the heigth of the check box created.  This forms a              //
  //  column of check boxes
  fPosX := StartX;
  fPosY := StartY;
end;


procedure TDataGridCheckBoxArray.DestroyAllCheckBoxes;
// this has to be in all forms destroy method, unless you like memory leaks          //
var
  cnt: Word;
begin
  //ShowMessage(IntToStr(High(DataGridCheckBoxArray)));
  if fCount > 0 then
    for cnt := Low(DataGridCheckBoxArray) to High(DataGridCheckBoxArray) do
      FreeAndNil(DataGridCheckBoxArray[cnt]);
  fCount := 0;
  fCreateRun := False;
end;


procedure TDataGridCheckBoxArray.MakeACheckBox;
begin
  if fCreateRun then  begin
    SetLength(DataGridCheckBoxArray, fCount + 1);
    DataGridCheckBoxArray[fCount] := TCheckBox.Create(nil) ;
    DataGridCheckBoxArray[fCount].Parent := fParent;

    DataGridCheckBoxArray[fCount].Left := fPosX;
    DataGridCheckBoxArray[fCount].Top := fPosY;

    Inc(fPosY, DataGridCheckBoxArray[fCount].Height);
    Inc(fCount);
  end;
end;


{ TExtentedDBGrid =============================================================}

constructor TExtendedDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentCollection := TComponentCollection.Create(Self);

  FScrollBarType := sbtBoth;
  fOpenColVisibleBtn := TGEMVisibleColumnControls.Create(self);

  fOpenColVisibleBtn.ArrowControl.Visible := True;
  fOpenColVisibleBtn.ArrowControl.Shape   := atDown;
  fOpenColVisibleBtn.ArrowControl.Left    := 0;
  fOpenColVisibleBtn.ArrowControl.Top     := 0;
  fOpenColVisibleBtn.ArrowControl.Width   := 220;
  fOpenColVisibleBtn.ArrowControl.Height  := 220;

  pDragAndDrop       :=False;
  pOnBeforeMouseUp   :=Nil;
  pOnAfterMouseUp    :=Nil;
  pOnBeforeMouseDown :=Nil;
  pOnAfterMouseDown  :=Nil;
end;


 function TExtendedDBGrid.CreateColumns: TDBGridColumns;
 begin
   Result := TDBGridColumns.Create(Self,TExtendedColumn);
 end;


 destructor TExtendedDBGrid.Destroy;
 begin
   FreeAndNil(fOpenColVisibleBtn);
   FreeAndNil(FComponentCollection);
   inherited Destroy;
 end;


 procedure TExtendedDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  If Assigned(pOnBeforeMouseDown) Then
    pOnBeforeMouseDown(Self,Button,Shift,X,Y);

  inherited MouseDown(Button,Shift,X,Y);

  If Assigned(pOnAfterMouseDown) Then
    pOnAfterMouseDown(Self,Button,Shift,X,Y);
end;


procedure TExtendedDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  If Assigned(pOnBeforeMouseUp) Then
    pOnBeforeMouseUp(Self,Button,Shift,X,Y);

  inherited MouseUp(Button,Shift,X,Y);

  If Assigned(pOnAfterMouseUp) Then
    pOnAfterMouseUp(Self,Button,Shift,X,Y);
end;


procedure TExtendedDBGrid.SetRowHeight(Value: Integer);
begin
  if FRowHeight <> Value Then begin
    FRowHeight := Value ;
    DefaultRowHeight := FRowHeight ;
    // Force Grid to update the RowCount.
    // The method I need to call is
    // UpdateRowCount, but it's declared
    // as private in the parent.  This
    // calls it by making the grid think it has
    // been resized.
    if Self.DataLink.Active Then begin
       Perform(WM_SIZE,0,0);
    end;
  end;
end;

procedure TExtendedDBGrid.SetScrollBarType(AValue: TScrollBarType);
begin
  FScrollBarType := AValue;
  RECREATEWND;
end;


procedure TExtendedDBGrid.SetupColVisibleBtn;
begin
  if fUseColumnVisible then begin
    //fOpenColVisibleBtn.
  //TDrawGrid.
    fOpenColVisibleBtn.ArrowControl.Visible := True;
    fOpenColVisibleBtn.ArrowControl.Shape := atDown;
    fOpenColVisibleBtn.ArrowControl.Left := 0;
    fOpenColVisibleBtn.ArrowControl.Top := 0;
    fOpenColVisibleBtn.ArrowControl.Width:= 15;
    fOpenColVisibleBtn.ArrowControl.Height:= 15;
  end
  else
    fOpenColVisibleBtn.ArrowControl.Visible := False;

end;

procedure TExtendedDBGrid.SetUseColumnVisible(AValue: Boolean);
begin
  fUseColumnVisible := AValue;
  SetupColVisibleBtn;
  Invalidate;
  //InvalidateGrid;
end;


procedure TExtendedDBGrid.StartDragAndDrop(var Message: TMessage);
begin
  BeginDrag(False);
end;


procedure TExtendedDBGrid.WMLButtonDown(var Message: TMessage);
begin
  inherited;
  If (DragMode=dmManual) And DragAndDrop Then
    StartDragAndDrop(Message);
end;

procedure TExtendedDBGrid.WMNCCalcSize(var msg: TMessage);
var
  style: Integer;
begin
  style := getWindowLong( handle, GWL_STYLE );

  if (style and WS_HSCROLL) <> 0 then
    if not (FScrollBarType = sbtHorisontal) then
      SetWindowLong( handle, GWL_STYLE, style and not WS_HSCROLL );

  if (style and WS_VSCROLL) <> 0 then
    if not (sbtVertical = FScrollBarType) then
      SetWindowLong( handle, GWL_STYLE, style and not WS_VSCROLL );

//   if (style and WS_HSCROLL) <> 0 then
//     if not FHorizontalBar then
//       SetWindowLong( handle, GWL_STYLE, style and not WS_HSCROLL );
//   if (style and WS_VSCROLL) <> 0 then
//     if not FVerticalBar then
//       SetWindowLong( handle, GWL_STYLE, style and not WS_VSCROLL );
  inherited;
 end;


//procedure TExtendedDBGrid.GEMDrawText(s:string; outputCanvas: Tcanvas; drawRect: TRect;
//                  drawAlignment:TAlignment ; drawFont:TFont);
//const
//  drawFlags : array [TAlignment] of Integer =
//    (DT_WORDBREAK or DT_LEFT  or DT_NOPREFIX,
//     DT_WORDBREAK or DT_RIGHT  or DT_NOPREFIX,
//     DT_WORDBREAK or DT_CENTER or DT_NOPREFIX );
//var
//  r:trect;
//  bw, bh, cw, ch, difX:integer;
//begin
//    if s='' then
//      exit;
//
//    if UseRightToLeftAlignment then
//      case drawAlignment of
//        taLeftJustify:  drawAlignment := taRightJustify;
//        taRightJustify: drawAlignment := taLeftJustify;
//      end;
//
//    r:= drawRect;
//    cw:=ClientWidth;
//    ch:=ClientHeight;
//
//    //set dimensions for output
//    bmpDrawText.Width:=( r.Right - r.Left);
//    bmpDrawText.Height:=r.Bottom- r.Top;
//    bw:=bmpDrawText.Width;
//    bh:=bmpDrawText.Height;
//
//    //set drawing area in output bmp
//    drawRect.Left:=0;
//    drawRect.Top:=0;
//    drawRect.Right:=bw;
//    drawRect.Bottom:=bh;
//
//    // if the drawing font color is same as transparent color
//    //change transparent color
//    if ColorToRGB( drawFont.Color )=(ColorToRGB
//	( bmpDrawText.TransparentColor) and $ffffff) then
//       toggleTransparentColor;
//
//    //to make entire surface of canvas transparent
//    bmpDrawText.Canvas.FillRect(drawRect);
//
//    //shrink the rectangle
//    InflateRect(drawRect, -2,-2);
//
//    bmpDrawText.Canvas.Font:= drawFont;
//
//    DrawText(bmpDrawText.Canvas.Handle,
//               pchar(s), length(s), drawRect,
//               drawFlags[drawAlignment]
//               );
//
//    if UseRightToLeftAlignment then
//    begin
//       if r.Right > ClientWidth then
//       begin
//          bmpClipped.Width:=cw-r.Left;
//          bmpClipped.Height:=bh;
//          bmpClipped.Canvas.CopyRect(bmpClipped.Canvas.ClipRect,
//		bmpDrawText.Canvas, Rect(bw, 0, bw-( cw - r.Left ), bh) );
//          outputCanvas.StretchDraw(rect(r.Left , r.Top, cw, r.Bottom), bmpClipped);
//       end
//       else
//          outputCanvas.StretchDraw(Rect(r.Right, r.Top, r.Left, r.Bottom), bmpDrawText);
//    end
//    else
//       outputCanvas.Draw(r.Left, r.top, bmpDrawText);
//end;




{ TFieldDataLink }

//procedure TFieldDataLink.ActiveChanged;
//begin
//  UpdateField;
//  if Assigned(FOnActiveChange) then FOnActiveChange(Self);
//end;
//
//constructor TFieldDataLink.Create;
//begin
//  inherited Create;
//  VisualControl := True;
//end;
//
//procedure TFieldDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
//begin
//  inherited;
//  if Event = deDisabledStateChange then
//  begin
//    if Boolean(Info) then
//      UpdateField
//    else
//      FField := nil;
//  end;
//end;
//
//function TFieldDataLink.Edit: Boolean;
//begin
//  if CanModify then inherited Edit;
//  Result := FEditing;
//end;
//
//procedure TFieldDataLink.EditingChanged;
//begin
//  SetEditing(inherited Editing and CanModify);
//end;
//
//procedure TFieldDataLink.FocusControl(Field: TFieldRef);
//begin
//  if (Field^ <> nil) and (Field^ = FField) and (FControl is TWinControl) then
//    if TWinControl(FControl).CanFocus then
//    begin
//      Field^ := nil;
//      TWinControl(FControl).SetFocus;
//    end;
//end;
//
//function TFieldDataLink.GetCanModify: Boolean;
//begin
//  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
//end;
//
//procedure TFieldDataLink.LayoutChanged;
//begin
//  UpdateField;
//end;
//
//procedure TFieldDataLink.Modified;
//begin
//  FModified := True;
//end;
//
//procedure TFieldDataLink.RecordChanged(Field: TField);
//begin
//  if (Field = nil) or (Field = FField) then
//  begin
//    if Assigned(FOnDataChange) then FOnDataChange(Self);
//    FModified := False;
//  end;
//end;
//
//procedure TFieldDataLink.Reset;
//begin
//  RecordChanged(nil);
//end;
//
//procedure TFieldDataLink.SetEditing(Value: Boolean);
//begin
//  if FEditing <> Value then
//  begin
//    FEditing := Value;
//    FModified := False;
//    if Assigned(FOnEditingChange) then FOnEditingChange(Self);
//  end;
//end;
//
//procedure TFieldDataLink.SetField(Value: TField);
//begin
//  if FField <> Value then
//  begin
//    FField := Value;
//    if (Dataset = nil) or not DataSet.ControlsDisabled then
//    begin
//      EditingChanged;
//      RecordChanged(nil);
//      UpdateRightToLeft;
//    end;
//  end;
//end;
//
//procedure TFieldDataLink.SetFieldName(const Value: string);
//begin
//  if FFieldName <> Value then
//  begin
//    FFieldName :=  Value;
//    UpdateField;
//  end;
//end;
//
//procedure TFieldDataLink.UpdateData;
//begin
//  if FModified then
//  begin
//    if (Field <> nil) and Assigned(FOnUpdateData) then FOnUpdateData(Self);
//    FModified := False;
//  end;
//end;
//
//procedure TFieldDataLink.UpdateField;
//begin
//  if Active and (FFieldName <> '') then
//  begin
//    FField := nil;
//    if Assigned(FControl) then
//      SetField(GetFieldProperty(DataSource.DataSet, FControl, FFieldName)) else
//      SetField(DataSource.DataSet.FieldByName(FFieldName));
//  end else
//    SetField(nil);
//end;
//
//procedure TFieldDataLink.UpdateRightToLeft;
//var
//  IsRightAligned: Boolean;
//  AUseRightToLeftAlignment: Boolean;
//begin
//  if Assigned(FControl) and (FControl is TWinControl) then
//    with FControl as TWinControl do
//      if IsRightToLeft then
//      begin
//        IsRightAligned :=
//          (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
//        AUseRightToLeftAlignment :=
//          DBUseRightToLeftAlignment(TControl(FControl), Field);
//        if (IsRightAligned and (not AUseRightToLeftAlignment)) or
//           ((not IsRightAligned) and AUseRightToLeftAlignment) then
//          Perform(CM_RECREATEWND, 0, 0);
//      end;
//end;

end.

