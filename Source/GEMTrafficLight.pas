unit GEMTrafficLight;

interface
uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.Types,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  GEMComponentsGlobal;

type
  TTrafficLightState = (tlsRed, tlsYellow, tlsGreen, tlsNone);

const
  TrafficLightColors : array[TTrafficLightState] of TColor = (clRed, clYellow, clGreen, clBlack);

type
  TGEMExShape = class(TShape)
  private
//    FHintColor: TColor;
//    FMouseOver: Boolean;
//    FHintWindowClass: THintWindowClass;
//    FOnMouseEnter: TNotifyEvent;
//    FOnMouseLeave: TNotifyEvent;
//    FOnParentColorChanged: TNotifyEvent;
    function BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT; overload;
  protected
    procedure WndProc(var Msg: TMessage); override;
    function HitTest(X, Y: Integer): Boolean; reintroduce; virtual;
  public
    constructor Create(AOwner: TComponent); override;
//    property HintWindowClass: THintWindowClass read FHintWindowClass write FHintWindowClass;
  published
  end;

  TGEMShape = class(TGEMExShape)
  published
    property Anchors;
    property Constraints;
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



  TGEMTrafficLight = class(TCustomGridPanel)
    RedLight   : tGEMShape;
    YellowLight: tGEMShape;
    GreenLight : tGEMShape;
  private
  { Private declarations }
    fState                : TTrafficLightState;
    fLightsOutLineColor   : tColor;
    fPenWidthLightOutLine : integer;
    fLightOffColor        : TColor;
//    fVisibleRedLight    : Boolean;

    fOnClick_RedLight     : TNotifyEvent;
    fOnClick_YellowLight  : TNotifyEvent;
    fOnClick_GreenLight   : TNotifyEvent;

    procedure Click_OkRedLightTransfer(Sender: TObject);    { TNotifyEvent }
    procedure Click_OkYellowLightTransfer(Sender: TObject); { TNotifyEvent }
    procedure Click_OkGreenLightransfer(Sender: TObject);   { TNotifyEvent }
    procedure SetState(const Value: TTrafficLightState);
    procedure SetPenShapeColor(const Value: tColor);
    procedure SetPenWidthLightOutLine(const Value: Integer);
  protected
   { Protected declarations }
    procedure CreateWindowHandle(const Params: TCreateParams); override;
  Public
  { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
   { Published properties and events }
    property LightsOutLineColor: tColor read fLightsOutLineColor write SetPenShapeColor default clWhite;
    property PenWidthLightOutLine: Integer read fPenWidthLightOutLine write SetPenWidthLightOutLine  default 1;
    property LightOffColor: TColor read fLightOffColor write fLightOffColor default clBlack;

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
//    property Caption;
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


function SmallPointToLong(const Pt: TSmallPoint): Longint;
begin
  Result := Longint(Pt);
end;


procedure CreateWMMessage(var Mesg: TMessage; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM);
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
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

//==============================
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


//===============================

constructor TGEMTrafficLight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RowCollection.Add;     //Default is 2 rows.  Need 3.
  ColumnCollection[1].Destroy; // default is 2 columns.  only need one.

  Width := 65;
  Height := 139;
  Color := clBlack;
  ParentBackground := False;

  RedLight := tGEMShape.create(self);
  RedLight.Parent := self;

  YellowLight := tGEMShape.create(self);
  YellowLight.Parent := self;

  GreenLight := tGEMShape.create(self);
  GreenLight.parent := self;

  fLightsOutLineColor := clWhite;
  fPenWidthLightOutLine := 1;
  fLightOffColor := clBlack;

  ShowCaption := False;
end;


procedure TGEMTrafficLight.CreateWindowHandle(const Params: TCreateParams);
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
      Brush.Color := fLightOffColor;//TrafficLightColors[tlsNone];
    Pen.Color := fLightsOutLineColor;
    Pen.Width := fPenWidthLightOutLine;
    TabOrder  := 0;
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
      Brush.Color := fLightOffColor;//TrafficLightColors[tlsNone];
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
      Brush.Color := fLightOffColor;//TrafficLightColors[tlsNone];
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

procedure TGEMTrafficLight.SetPenShapeColor(const Value: tColor);
begin
  fLightsOutLineColor   := Value;
  RedLight.Pen.color    := fLightsOutLineColor;
  YellowLight.Pen.color := fLightsOutLineColor;
  GreenLight.Pen.color  := fLightsOutLineColor;
end;


procedure TGEMTrafficLight.SetPenWidthLightOutLine(const Value: Integer);
begin
  fPenWidthLightOutLine := Value;
  RedLight.Pen.Width    := fPenWidthLightOutLine;
  YellowLight.Pen.Width := fPenWidthLightOutLine;
  GreenLight.Pen.Width  := fPenWidthLightOutLine;
end;
//

procedure TGEMTrafficLight.SetState(const Value: TTrafficLightState);
begin
  RedLight.Brush.color    := fLightOffColor;//clBlack;
  YellowLight.Brush.color := fLightOffColor;//clBlack;
  GreenLight.Brush.color  := fLightOffColor;//clBlack;
  case Value of
    tlsRed   : RedLight.Brush.color := clRed;
    tlsYellow: YellowLight.Brush.color := clYellow;
    tlsGreen : GreenLight.Brush.color := clGreen;
    tlsNone  : ;
  end;

  fState := Value;
end;


procedure TGEMTrafficLight.Click_OkRedLightTransfer(Sender: TObject);  { TNotifyEvent }
begin
  SetState(tlsRed);
  if assigned(fOnClick_RedLight) then
    fOnClick_RedLight(Self); { Substitute Self for subcomponent's Sender. }
end;


procedure TGEMTrafficLight.Click_OkYellowLightTransfer(Sender: TObject);
begin
  SetState(tlsYellow);
  if assigned(fOnClick_YellowLight) then
    fOnClick_YellowLight(Self); { Substitute Self for subcomponent's Sender. }
end;


procedure TGEMTrafficLight.Click_OkGreenLightransfer(Sender: TObject);
begin
  SetState(tlsGreen);
  if assigned(fOnClick_GreenLight) then
    fOnClick_GreenLight(Self); { Substitute Self for subcomponent's Sender. }
end;

{ TGEMExShape }

function TGEMExShape.BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT;
var
  Mesg: TMessage;
begin
  CreateWMMessage(Mesg, Msg, WParam, LParam);
  inherited WndProc(Mesg);
  Result := Mesg.Result;
end;


constructor TGEMExShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


function TGEMExShape.HitTest(X, Y: Integer): Boolean;
begin
  Result := BaseWndProc(CM_HITTEST, 0, SmallPointToLong(PointToSmallPoint(Point(X, Y)))) <> 0;
end;


procedure TGEMExShape.WndProc(var Msg: TMessage);
begin
  if not DispatchIsDesignMsg(Self, Msg) then
    case Msg.Msg of
      CM_HITTEST:
        with TCMHitTest(Msg) do
          Result := LRESULT(HitTest(XPos, YPos));
      else
        inherited WndProc(Msg);
    end;
end;

end.
