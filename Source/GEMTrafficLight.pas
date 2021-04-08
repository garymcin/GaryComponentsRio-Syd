unit GEMTrafficLight;

interface
uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  JvExExtCtrls, JvExtComponent, JvShape;

type
  TTrafficLightState = (tlsRed, tlsYellow, tlsGreen, tlsNone);

const
  TrafficLightColors : array[tlsRed..tlsNone] of TColor = (clRed, clYellow, clGreen, clBlack);

type
  tGEMShape = class(TShape) // this adds OnClick
  private
  published
  public
    property Align;
    property Anchors;
    property Brush;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property ParentShowHint;
    property Pen;
    property Shape;
    property ShowHint;
    property Touch;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
  end;


  TGEMTrafficLight = class(TCustomGridPanel)
    RedLight   : tGEMShape;
    YellowLight: tGEMShape;
    GreenLight : tGEMShape;
  private
  { Private declarations }
    fState: TTrafficLightState;
    fLightsOutLineColor: tColor;
    fPenWidthLightOutLine: integer;
    fVisibleRedLight     : Boolean;

    fOnClick_RedLight   : TNotifyEvent;
    fOnClick_YellowLight: TNotifyEvent;
    fOnClick_GreenLight : TNotifyEvent;

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


constructor TGEMTrafficLight.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RowCollection.Add;     //Default is 2 rows.  Need 3.
  ColumnCollection[1].Destroy; // default is 2 columns.  only need one.

  Width := 74;
  Height := 140;
  Color := clBlack;
  ParentBackground := False;

  RedLight := tGEMShape.create(self);
  RedLight.Parent := self;

  YellowLight := tGEMShape.create(self);
  YellowLight.Parent := self;

  GreenLight := tGEMShape.create(self);
  GreenLight.parent := self;

  fLightsOutLineColor := clWhite;
end;


procedure TGEMTrafficLight.CreateWindowHandle(const Params: TCreateParams);
{ Calls inherited CreateWindowHandle and initializes subcomponents. }
begin
inherited CreateWindowHandle(Params);

  with RedLight do
  begin
    Left := 17;
    Top := 2;
    Width := 41;
    Height := 41;
    Align := alClient;
    if fState = tlsRed then
      Brush.Color := TrafficLightColors[tlsRed]
    else
      Brush.Color := TrafficLightColors[tlsNone];
    Pen.Color := fLightsOutLineColor;
    Shape := stCircle;
    TabOrder := 0;
    Brush.OnChange := StyleChanged;
    Pen.Onchange := StyleChanged;
    OnClick := Click_OkRedLightTransfer;
  end;

  with YellowLight do
  begin
    Left := 17;
    Top := 49;
    Width := 41;
    Height := 41;
    Align := alClient;
    if fState = tlsYellow then
      Brush.Color := TrafficLightColors[tlsYellow]
    else
      Brush.Color := TrafficLightColors[tlsNone];
    Pen.Color := fLightsOutLineColor;
    Shape := stCircle;
    TabOrder := 1;
    Brush.OnChange := StyleChanged;
    Pen.Onchange := StyleChanged;
    OnClick := Click_OkYellowLightTransfer;
  end;

  with GreenLight do
  begin
    Left := 17;
    Top := 96;
    Width := 41;
    Height := 41;
    Align := alClient;
    if fState = tlsGreen then
      Brush.Color := TrafficLightColors[tlsGreen]
    else
      Brush.Color := TrafficLightColors[tlsNone];
    Shape := stCircle;
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
  RedLight.Pen.Width    := fLightsOutLineColor;
  YellowLight.Pen.Width := fLightsOutLineColor;
  GreenLight.Pen.Width  := fLightsOutLineColor;
end;


procedure TGEMTrafficLight.SetState(const Value: TTrafficLightState);
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

end.
