unit GEMTrafficLight2;

interface
uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls;


type
  TTrafficLightState = (tlsRed, tlsYellow, tlsGreen, tlsNone);

const
  TrafficLightColors : array[TTrafficLightState] of TColor = (clRed, clYellow, clGreen, clBlack);

type
  TGEMExShape2 = class(TShape)
  private
    FHintColor: TColor;
    FMouseOver: Boolean;
    FHintWindowClass: THintWindowClass;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT; overload;
//    function BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: TObject): LRESULT; overload;
//    function BaseWndProcEx(Msg: Cardinal; WParam: WPARAM; var StructLParam): LRESULT;
  protected
    procedure WndProc(var Msg: TMessage); override;
//    procedure FocusChanged(AControl: TWinControl); dynamic;
//    procedure VisibleChanged; reintroduce; dynamic;
//    procedure EnabledChanged; reintroduce; dynamic;
//    procedure TextChanged; reintroduce; virtual;
//    procedure ColorChanged; reintroduce; dynamic;
//    procedure FontChanged; reintroduce; dynamic;
//    procedure ParentFontChanged; reintroduce; dynamic;
//    procedure ParentColorChanged; reintroduce; dynamic;
//    procedure ParentShowHintChanged; reintroduce; dynamic;
//    function WantKey(Key: Integer; Shift: TShiftState): Boolean; virtual;
//    function HintShow(var HintInfo: Vcl.Controls.THintInfo): Boolean; reintroduce; dynamic;
    function HitTest(X, Y: Integer): Boolean; reintroduce; virtual;
//    procedure MouseEnter(AControl: TControl); reintroduce; dynamic;
//    procedure MouseLeave(AControl: TControl); reintroduce; dynamic;
//    property MouseOver: Boolean read FMouseOver write FMouseOver;
//    property HintColor: TColor read FHintColor write FHintColor default clDefault;
//    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
//    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
//    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    property HintWindowClass: THintWindowClass read FHintWindowClass write FHintWindowClass;
  published
  end;

  TGEMShape2 = class(TGEMExShape2)
  published
    property Anchors;
    property Constraints;
//    property HintColor;
    property Align;
    property Brush;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property Pen;
    property Shape;
    property ShowHint;
    property Touch;
    property Visible;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnEndDock;
    property OnStartDock;
//    property OnParentColorChange;
    property OnMouseActivate;
    property OnGesture;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;


  TGEMTrafficLight2 = class(TCustomGridPanel)
    RedLight   : TGEMShape2;
    YellowLight: TGEMShape2;
    GreenLight : TGEMShape2;
  private
  { Private declarations }
    fState: TTrafficLightState;
    fLightsOutLineColor: tColor;

    fOnClick_RedLight   : TNotifyEvent;
    fOnClick_YellowLight: TNotifyEvent;
    fOnClick_GreenLight : TNotifyEvent;
    procedure Click_OkRedLightTransfer(Sender: TObject);    { TNotifyEvent }
    procedure Click_OkYellowLightTransfer(Sender: TObject); { TNotifyEvent }
    procedure Click_OkGreenLightransfer(Sender: TObject);   { TNotifyEvent }
    procedure SetState(const Value: TTrafficLightState);
    procedure SetPenShapeColor(const Value: tColor);
  protected
   { Protected declarations }
    procedure CreateWindowHandle(const Params: TCreateParams); override;
//    procedure CreateWindowHandle(const Params: TCreateParams); override;
//    procedure Paint; override;
  Public
  { Public declarations }
    constructor Create(AOwner: TComponent); override;
//    destructor Destory;

  published
   { Published properties and events }
    property LightsOutLineColor: tColor read fLightsOutLineColor write SetPenShapeColor default clWhite;
    property State: TTrafficLightState read fState write SetState default tlsNone;

    property OnClick_RedLight   : TNotifyEvent read fOnClick_RedLight write fOnClick_RedLight;
    property OnClick_YellowLight: TNotifyEvent read fOnClick_YellowLight write fOnClick_YellowLight;
    property OnClick_GreenLight : TNotifyEvent read fOnClick_GreenLight write fOnClick_GreenLight;

    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    {$IFDEF  VER340}
    property StyleName;
    {$ENDIF}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
 end;


implementation

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
  CtrlMask = $10000000;
  ShiftMask = $08000000;
begin
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
  Result := Longint(Pt);
end;


function DispatchIsDesignMsg(Control: TControl; var Msg: TMessage): Boolean;
var
  Form: TCustomForm;
begin
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


procedure GetHintColor(var HintInfo: Vcl.Controls.THintInfo; AControl: TControl; HintColor: TColor);
var
  AHintInfo: Vcl.Controls.THintInfo;
begin
  case HintColor of
    clNone:
      HintInfo.HintColor := Application.HintColor;
    clDefault:
      begin
        if Assigned(AControl) and Assigned(AControl.Parent) then
        begin
          AHintInfo := HintInfo;
          AControl.Parent.Perform(CM_HINTSHOW, 0, LPARAM(@AHintInfo));
          HintInfo.HintColor := AHintInfo.HintColor;
        end;
      end;
  else
    HintInfo.HintColor := HintColor;
  end;
end;


procedure CreateWMMessage(var Mesg: TMessage; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM);
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
end;



constructor TGEMTrafficLight2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RowCollection.Add;     //Default is 2 rows.  Need 3.
  ColumnCollection[1].Destroy; // default is 2 columns.  only need one.

  Width := 65;
  Height := 139;
  Color := clBlack;
  ParentBackground := False;

  RedLight := tGEMShape2.create(self);
  RedLight.Parent := self;

  YellowLight := tGEMShape2.create(self);
  YellowLight.Parent := self;

  GreenLight := tGEMShape2.create(self);
  GreenLight.parent := self;

  fLightsOutLineColor := clWhite;
end;


procedure TGEMTrafficLight2.CreateWindowHandle(const Params: TCreateParams);
{ Calls inherited CreateWindowHandle and initializes subcomponents. }
begin
  inherited CreateWindowHandle(Params);

  with RedLight do
  begin
    AlignWithMargins := True;
    Left := 4;
    Top := 4;
    Width := 57;
    Height := 40;
    Align := alClient;
    Shape := stCircle;
    if fState = tlsRed then
      Brush.Color := TrafficLightColors[tlsRed]
    else
      Brush.Color := TrafficLightColors[tlsNone];
    Pen.Color := fLightsOutLineColor;
    TabOrder := 0;
    Brush.OnChange := StyleChanged;
    Pen.Onchange := StyleChanged;
    OnClick := Click_OkRedLightTransfer;
  end;

  with YellowLight do
  begin
    AlignWithMargins := True;
    Left := 4;
    Top := 50;
    Width := 57;
    Height := 40;
    Align := alClient;
    Shape := stCircle;
    if fState = tlsYellow then
      Brush.Color := TrafficLightColors[tlsYellow]
    else
      Brush.Color := TrafficLightColors[tlsNone];
    Pen.Color := fLightsOutLineColor;
    TabOrder := 1;
    Brush.OnChange := StyleChanged;
    Pen.Onchange := StyleChanged;
    OnClick := Click_OkYellowLightTransfer;
  end;

  with GreenLight do
  begin
    AlignWithMargins := True;
    Left := 4;
    Top := 96;
    Width := 57;
    Height := 39;
    Align := alClient;
    Shape := stCircle;
    if fState = tlsGreen then
      Brush.Color := TrafficLightColors[tlsGreen]
    else
      Brush.Color := TrafficLightColors[tlsNone];
    Pen.Color := fLightsOutLineColor;
    TabOrder := 2;
    Brush.OnChange := StyleChanged;
    Pen.Onchange := StyleChanged;
    OnClick := Click_OkGreenLightransfer;
  end;

  while RowCollection.Count < 3 do
    RowCollection.Add;

  RowCollection.BeginUpdate;
    RowCollection[0].SizeStyle := ssPercent;
    RowCollection[1].SizeStyle := ssPercent;
    RowCollection[2].SizeStyle := ssPercent;

    RowCollection[0].Value := 33.00;
    RowCollection[1].Value := 34.00;
    RowCollection[2].Value := 33.00;
  RowCollection.EndUpdate;

  ControlCollection.AddControl(RedLight, 0,0);
  ControlCollection.AddControl(YellowLight, 0,1);
  ControlCollection.AddControl(GreenLight, 0,2);

  Padding.Left   := 1;
  Padding.Top    := 1;
  Padding.Right  := 1;
  Padding.Bottom := 2;

  SetState(fState);
end; { CreateWindowHandle }

//--------------------------------------

procedure TGEMTrafficLight2.SetPenShapeColor(const Value: tColor);
begin
  fLightsOutLineColor   := Value;
  RedLight.Pen.color    := fLightsOutLineColor;
  YellowLight.Pen.color := fLightsOutLineColor;
  GreenLight.Pen.color  := fLightsOutLineColor;
end;


procedure TGEMTrafficLight2.SetState(const Value: TTrafficLightState);
begin
  RedLight.Brush.color    := clBlack;
  YellowLight.Brush.color := clBlack;
  GreenLight.Brush.color  := clBlack;
//  SetPenShapeColor(fLightsOutLineColor);
  case Value of
    tlsRed   : RedLight.Brush.color := clRed;
    tlsYellow: YellowLight.Brush.color := clYellow;
    tlsGreen : GreenLight.Brush.color := clGreen;
    tlsNone  : ;
  end;

  fState := Value;
end;


procedure TGEMTrafficLight2.Click_OkRedLightTransfer(Sender: TObject);  { TNotifyEvent }
begin
  SetState(tlsRed);
  if assigned(fOnClick_RedLight) then
    fOnClick_RedLight(Self); { Substitute Self for subcomponent's Sender. }
end;


procedure TGEMTrafficLight2.Click_OkYellowLightTransfer(Sender: TObject);
begin
  SetState(tlsYellow);
  if assigned(fOnClick_YellowLight) then
    fOnClick_YellowLight(Self); { Substitute Self for subcomponent's Sender. }
end;


procedure TGEMTrafficLight2.Click_OkGreenLightransfer(Sender: TObject);
begin
  SetState(tlsGreen);
  if assigned(fOnClick_GreenLight) then
    fOnClick_GreenLight(Self); { Substitute Self for subcomponent's Sender. }
end;

{ tGEMShape }
//
//procedure tGEMShape.Click;
//begin
//  inherited;
//
//end;

{ TGEMExShape2 }

function TGEMExShape2.BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT;
var
  Mesg: TMessage;
begin
  CreateWMMessage(Mesg, Msg, WParam, LParam);
  inherited WndProc(Mesg);
  Result := Mesg.Result;
end;

//function TGEMExShape.BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: TObject): LRESULT;
//begin
//  Result := BaseWndProc(Msg, WParam, WinApi.Windows.LPARAM(LParam));
//end;

//function TGEMExShape.BaseWndProcEx(Msg: Cardinal; WParam: WPARAM;
//  var StructLParam): LRESULT;
//begin
//  Result := BaseWndProc(Msg, WParam, WinApi.Windows.LPARAM(@StructLParam));
//end;


constructor TGEMExShape2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clDefault;
end;

//procedure TGEMExShape.ColorChanged;
//begin
//  BaseWndProc(CM_COLORCHANGED);
//end;
//
//procedure TGEMExShape.EnabledChanged;
//begin
//  BaseWndProc(CM_ENABLEDCHANGED);
//end;
//
//function TGEMExShape.HintShow(var HintInfo: Vcl.Controls.THintInfo): Boolean;
//begin
//  GetHintColor(HintInfo, Self, FHintColor);
//  if FHintWindowClass <> nil then
//    HintInfo.HintWindowClass := FHintWindowClass;
//  Result := BaseWndProcEx(CM_HINTSHOW, 0, HintInfo) <> 0;
//end;
//
//procedure TGEMExShape.ParentColorChanged;
//begin
//  BaseWndProc(CM_PARENTCOLORCHANGED);
//  if Assigned(OnParentColorChange) then
//    OnParentColorChange(Self);
//end;
//
//procedure TGEMExShape.ParentFontChanged;
//begin
//  BaseWndProc(CM_PARENTFONTCHANGED);
//end;
//
//procedure TGEMExShape.ParentShowHintChanged;
//begin
//  BaseWndProc(CM_PARENTSHOWHINTCHANGED);
//end;
//
//procedure TGEMExShape.TextChanged;
//begin
//  BaseWndProc(CM_TEXTCHANGED);
//end;
//
//procedure TGEMExShape.VisibleChanged;
//begin
//  BaseWndProc(CM_VISIBLECHANGED);
//end;
//
//function TGEMExShape.WantKey(Key: Integer; Shift: TShiftState): Boolean;
//begin
//  Result := BaseWndProc(CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
//end;
//
//procedure TGEMExShape.FontChanged;
//begin
//  BaseWndProc(CM_FONTCHANGED);
//end;

//procedure TGEMExShape.FocusChanged(AControl: TWinControl);
//begin
//  BaseWndProc(CM_FOCUSCHANGED, 0, AControl);
//end;
//

function TGEMExShape2.HitTest(X, Y: Integer): Boolean;
begin
  Result := BaseWndProc(CM_HITTEST, 0, SmallPointToLong(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

//procedure TGEMExShape.MouseEnter(AControl: TControl);
//begin
//  FMouseOver := True;
//  if Assigned(FOnMouseEnter) then
//    FOnMouseEnter(Self);
//  BaseWndProc(CM_MOUSEENTER, 0, AControl);
//end;
//
//procedure TGEMExShape.MouseLeave(AControl: TControl);
//begin
//  FMouseOver := False;
//  BaseWndProc(CM_MOUSELEAVE, 0, AControl);
//  if Assigned(FOnMouseLeave) then
//    FOnMouseLeave(Self);
//end;


procedure TGEMExShape2.WndProc(var Msg: TMessage);
begin
  if not DispatchIsDesignMsg(Self, Msg) then
    case Msg.Msg of
////    CM_DENYSUBCLASSING:
////      Msg.Result := LRESULT(Ord(GetInterfaceEntry(IJvDenySubClassing) <> nil));
////      CM_DIALOGCHAR:
////        with TCMDialogChar{$IFDEF CLR}.Create{$ENDIF}(Msg) do
////          Result := LRESULT(Ord(WantKey(CharCode, KeyDataToShiftState(KeyData))));
      CM_HITTEST:
        with TCMHitTest(Msg) do
          Result := LRESULT(HitTest(XPos, YPos));
////      CM_MOUSEENTER: MouseEnter(TControl(Msg.LParam));
////      CM_MOUSELEAVE:  MouseLeave(TControl(Msg.LParam));
////      CM_VISIBLECHANGED: VisibleChanged;
////      CM_ENABLEDCHANGED: EnabledChanged;
////      CM_TEXTCHANGED: TextChanged;
////      CM_FONTCHANGED: FontChanged;
////      CM_COLORCHANGED: ColorChanged;
////      CM_FOCUSCHANGED:  FocusChanged(TWinControl(Msg.LParam));
////      CM_PARENTFONTCHANGED: ParentFontChanged;
////      CM_PARENTCOLORCHANGED: ParentColorChanged;
////      CM_PARENTSHOWHINTCHANGED: ParentShowHintChanged;
////      CM_HINTSHOW:
////        with TCMHintShow(Msg) do
////          Result := LRESULT(HintShow(HintInfo^));
      else
        inherited WndProc(Msg);
    end;
end;

end.
