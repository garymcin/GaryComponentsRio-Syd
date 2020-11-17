unit gemCustomButton;
  {.$DEFINE USE_CODESITE}

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.Math, System.UITypes,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.GraphUtil, VCL.dialogs,
  Vcl.Menus, Vcl.Forms;

type
  TgemShapes = (drCircle, drSquare, drElliptic, drRoundRect, drRect, drUpArrow,
                drDownArrow, drLeftArrow, drRightArrow);

  TgemTextStyle = (txNone, txShadowed);
  TgemLineStyle = (fmFlat, fmRelief);

  TgemShapeStyle = class(TPersistent)
  private
    { Private declarations }
    fFillColor: tColor;
    fLineColor: tColor;
    fLineWidth: integer;
    fTextStyle: TgemTextStyle;
    fShadowColor: TColor;
    fLineStyle: TgemLineStyle;
    fMouseDownLuma: integer;
    //fShape: TgemShapes;
    fOnShapeStyleChange: TNotifyEvent;
    fXRadius: integer;
    fYRadius: integer;
    procedure DoChanges;
    procedure SetShadowColor(const Value: TColor);
    procedure SetFillColor(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineStyle(const Value: TgemLineStyle);
    procedure SetLineWidth(const Value: Integer);
    procedure SetXRadius(const Value: integer);
    procedure SetYRadius(const Value: integer);
    procedure SetMouseDownLuma(const NewLuma: integer);
    procedure SetTextStyle(const NewTextStyle: TgemTextStyle);
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property TextStyle: TgemTextStyle read fTextStyle write SetTextStyle;
    property LineColor: TColor read fLineColor write SetLineColor;
    property LineStyle: TgemLineStyle read fLineStyle write SetLineStyle;
    property LineWidth: Integer read fLineWidth write SetLineWidth;
    property FillColor: TColor read fFillColor write SetFillColor;
    property ShadowColor: TColor read fShadowColor write SetShadowColor;
    property XRadius: integer read fXRadius write SetXRadius;
    property YRadius: integer read fYRadius write SetYRadius;
    property MouseDownLuma: integer read fMouseDownLuma write SetMouseDownLuma;
    property OnShapeStyleChange: TNotifyEvent read fOnShapeStyleChange write fOnShapeStyleChange;

  end;

  //////////////////////////////////////////////////////////
  //TgemShapeBtn = class(TGraphicControl)///////////////////
  //////////////////////////////////////////////////////////
  TgemShapeBtn = class(TGraphicControl)
  private
    { Private declarations }
    fShapesStyle   : TgemShapeStyle;
    //fEdit          : TEdit;
    fShape         : TgemShapes;
    fBorder        : Boolean;
    fMouseIsDown   : Boolean;
    fMouseInRegion : Boolean;
    fTextT, ftextL : integer;

    function GetShape: TgemShapes;
    procedure SetShape(const NewShape: TgemShapes);
    procedure SetBorder(const Value: Boolean);
    procedure UpdateChanges(Sender: TObject);
    procedure DrawReliefShape;
    procedure DrawFlatShape;
    procedure CMEnabledChanged(var message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var message: TMessage); message CM_TEXTCHANGED;

  protected
    { Protected declarations }
    StarPoints: array[0..7] of TPoint;

    fPullDownMenu: TPopupMenu;
    procedure WriteCaption;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure LayoutSetting;
    function MouseIsInRegion(X, Y: Integer): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDropDownMenu(const Value: TPopupMenu);

  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property ShapeStyle: TgemShapeStyle read fShapesStyle  write fShapesStyle;
    property Shape: TgemShapes read GetShape  write SetShape;
    // showing a border or not
    property Border : Boolean read FBorder write SetBorder;

    procedure UpdateDropDownMenu;
    // inherited stuff
    property Action;
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Align;
    property Visible;
    property ShowHint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property PullDownMemu: TPopupMenu read fPullDownMenu write SetDropDownMenu;
  end;


implementation


{ TgemShapeStyle }

procedure TgemShapeStyle.SetFillColor(const Value: TColor);
begin
  if value<>fFillColor then
  begin
    fFillColor:=Value;
    DoChanges;
  end;
end;

procedure TgemShapeStyle.SetLineColor(const Value: TColor);
begin
  if Value <> fLineColor then
  begin
    fLineColor:=Value;
    DoChanges;
  end;
end;


procedure TgemShapeStyle.SetLineStyle(const Value: TgemLineStyle);
begin
  if Value<>fLineStyle then
  begin
    fLineStyle:=Value;
    DoChanges;
  end;
  //fLineStyle := Value;
end;

procedure TgemShapeStyle.SetLineWidth(const Value: Integer);
begin
  if Value <> fLineWidth then
  begin
    fLineWidth := Value;
    if fLineWidth < 0 then
      fLineWidth := 0;
    if fLineWidth > 14 then
      fLineWidth := 14;
    DoChanges;
  end;
  //fLineWidth := Value;
end;

procedure TgemShapeStyle.SetMouseDownLuma(const NewLuma: integer);
begin
  if NewLuma <> fMouseDownLuma then begin
    fMouseDownLuma := NewLuma;
    DoChanges;
  end;
end;

procedure TgemShapeStyle.SetShadowColor(const Value: TColor);
begin
  if Value <> fShadowColor then begin
    fShadowColor := Value;
    DoChanges;
  end;
end;

procedure TgemShapeStyle.SetTextStyle(const NewTextStyle: TgemTextStyle);
begin
  if NewTextStyle <> fTextStyle then  begin
    fTextStyle := NewTextStyle;
    DoChanges;
  end;
end;

procedure TgemShapeStyle.SetXRadius(const Value: integer);
begin
  if Value <> fXRadius then
  begin
    fXRadius := Value;
    if fXRadius < 0 then
      fXRadius := 0;
    if fXRadius > 145 then
      fXRadius := 145;
    DoChanges;
  end;
end;


procedure TgemShapeStyle.SetYRadius(const Value: integer);
begin
  if Value<>fYRadius then
  begin
    fYRadius := Value;
    if fYRadius < 0 then
      fYRadius := 0;
    if fYRadius > 145 then
      fYRadius := 145;
    DoChanges;
  end;
end;



procedure TgemShapeStyle.DoChanges;
begin
  if Assigned(fOnShapeStyleChange) then
    fOnShapeStyleChange(Self);
end;




//////////////////////////////////////////////
{ TgemCustomBtn }  ///////////////////////////
//////////////////////////////////////////////

constructor TgemShapeBtn.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:=[csClickEvents, csCaptureMouse, csSetCaption];
  Enabled:=True;

  fShapesStyle := TgemShapeStyle.Create;

  with fShapesStyle do begin
    fFillColor := clBtnFace;
    LineColor := clGray;
    fLineWidth := 3;
    fShadowColor:=clBlue;
    fXRadius := 5;
    fYRadius := 5;
    fTextStyle := txNone;
    fLineStyle := fmRelief;
    fMouseDownLuma := 35;
    OnShapeStyleChange := UpdateChanges;
  end;

  fShape := drRoundRect;
 	Height := 30;
	Width := 100;
  fBorder := true;
end;


destructor TgemShapeBtn.Destroy;
begin
  fShapesStyle.Free;
  inherited;
end;


procedure TgemShapeBtn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = fPullDownMenu) and (Operation = opRemove) then
    fPullDownMenu := nil;
end;


procedure TgemShapeBtn.UpdateDropDownMenu;
begin
  if Assigned (fPullDownMenu) then
end;


procedure TgemShapeBtn.SetDropDownMenu(const Value: TPopupMenu);
begin
  if Assigned(fPullDownMenu) then
    fPullDownMenu.RemoveFreeNotification(Self);
  fPullDownMenu := Value;
  if Assigned(fPullDownMenu) then
    fPullDownMenu.RemoveFreeNotification(Self);
end;


procedure TgemShapeBtn.CMEnabledChanged(var message: TMessage);
begin
   inherited;
   Invalidate;
end;


procedure TgemShapeBtn.CMTextChanged(var message: TMessage);
begin
  inherited;
  Invalidate;
end;


function TgemShapeBtn.GetShape: TgemShapes;
begin
  result := fShape;
end;


procedure TgemShapeBtn.Paint;
begin
  inherited;
  Canvas.Font := Self.Font;
  Canvas.Brush.Style := bsClear;

  case fShapesStyle.fLineStyle of
    fmRelief:  DrawReliefShape;
    fmFlat:    DrawFlatShape;
  end;
  LayoutSetting;
  WriteCaption;
end;


procedure TgemShapeBtn.SetBorder(const Value: Boolean);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    Invalidate;
  end;end;


procedure TgemShapeBtn.SetShape(const NewShape: TgemShapes);
begin
  if NewShape <> fShape then
  begin
    //showmessage('Shape changed');
    fShape := NewShape;
    Invalidate;
  end;
end;


procedure TgemShapeBtn.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    exit;
  Invalidate;
end;


procedure TgemShapeBtn.DrawFlatShape;
var
  Rgn: HRgn;
begin
  Canvas.Brush.Color := fShapesStyle.fFillColor;
  Canvas.Pen.Color := fShapesStyle.fLineColor;

  Rgn := CreateRectRgn(0, 0, Width, Height);
  try
    case fShape of
      drCircle: begin
                    if Height > Width then
                      Width := Height;
                    if Width > Height then
                      Height := Width;

                  Rgn := CreateEllipticRgn(0, 0, Width, Height);
      end;

      drSquare: begin
                    if Height > Width then
                      Width := Height;
                    if Width > Height then
                      Height := Width;

                  Rgn := CreateRectRgn(0, 0, Width, Height);
      end;

      drElliptic: Rgn := CreateEllipticRgn(0, 0, Width, Height);

      drRoundRect:  Rgn := CreateRoundRectRgn(0, 0, Width, Height,
                                              fShapesStyle.fXRadius,
                                              fShapesStyle.fYRadius);

      drUpArrow: begin
        StarPoints[0].X := (Width) div 2;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width);
        StarPoints[1].Y := (Height) div 3;

        StarPoints[2].X := (Width ) div 3 * 2;
        StarPoints[2].Y := (Height) div 3;

        StarPoints[3].X := (Width ) div 3 * 2;
        StarPoints[3].Y := (Height);

        StarPoints[4].X := (Width ) div 3;
        StarPoints[4].Y := Height ;

        StarPoints[5].X := (Width ) div 3;
        StarPoints[5].Y := (Height) div 3;

        StarPoints[6].X := 0;
        StarPoints[6].Y := (Height) div 3;

        StarPoints[7].X := (Width ) div 2;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drDownArrow: begin
        StarPoints[0].X := (Width) div 3;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width) div 3 * 2;
        StarPoints[1].Y := 0;

        StarPoints[2].X := (Width) div 3 * 2;
        StarPoints[2].Y := (Height) div 3 * 2;

        StarPoints[3].X := Width ;
        StarPoints[3].Y := (Height) div 3 * 2;

        StarPoints[4].X := (Width ) div 2;
        StarPoints[4].Y := Height ;

        StarPoints[5].X := 0;
        StarPoints[5].Y := (Height) div 3 * 2;

        StarPoints[6].X := (Width ) div 3;
        StarPoints[6].Y := (Height) div 3 * 2;

        StarPoints[7].X := (Width ) div 3;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

        drRightArrow: begin
        StarPoints[0].X := (Width) div 3 * 2;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width );
        StarPoints[1].Y := (Height) div 2;

        StarPoints[2].X := (Width ) div 3 * 2;
        StarPoints[2].Y := (Height);

        StarPoints[3].X := (Width ) div 3 * 2;
        StarPoints[3].Y := (Height) div 3 * 2;

        StarPoints[4].X := 0;
        StarPoints[4].Y := (Height) div 3 * 2;

        StarPoints[5].X := 0;
        StarPoints[5].Y := (Height) div 3;

        StarPoints[6].X := (Width ) div 3 * 2;
        StarPoints[6].Y := (Height) div 3;

        StarPoints[7].X := (Width) div 3 * 2;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drLeftArrow: begin
        StarPoints[0].X := (Width) div 3;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width ) div 3;
        StarPoints[1].Y := (Height) div 3;

        StarPoints[2].X := (Width );
        StarPoints[2].Y := (Height) div 3 ;

        StarPoints[3].X := Width ;
        StarPoints[3].Y := (Height) div 3 * 2;

        StarPoints[4].X := (Width ) div 3;
        StarPoints[4].Y := (Height) div 3 * 2;

        StarPoints[5].X := (Width ) div 3;
        StarPoints[5].Y := (Height);

        StarPoints[6].X := 0;
        StarPoints[6].Y := (Height) div 2;

        StarPoints[7].X := (Width ) div 3;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

    else{drRect:}
      Rgn := CreateRectRgn(0, 0, Width, Height);

      //drArrow: Rgn :=
    end; // case

    if fMouseIsDown then
      Canvas.Brush.Color := ColorAdjustLuma(Canvas.Brush.Color ,
                                          -fShapesStyle.fMouseDownLuma, false);

    FillRgn(Canvas.Handle, Rgn, Canvas.Brush.Handle);

    if fBorder then
      FrameRgn(Canvas.Handle, Rgn, Canvas.Pen.Handle, fShapesStyle.fLineWidth,
                                                      fShapesStyle.fLineWidth);
  finally
    DeleteObject(Rgn);
  end;  // try
end;



procedure TgemShapeBtn.DrawReliefShape;
var
  LineC, FillC: TColor;
  R, G, B: Byte;
  RO, GO,BO,   // orginal fill color      //
  RL, GL, BL,  // line                       //
  RF, GF, BF,  // Fill      //           /
  n, w : integer;
  RI, GI, BI  // Interval      //
  : Double;
  Rgn: HRgn;
  //TempValue: real;         //

  ////////////////
  procedure Draw(R, G, B, i: integer);
  begin
    if fMouseIsDown then
       Canvas.Brush.Color := ColorAdjustLuma(RGB(R, G, B),
                                           -fShapesStyle.fMouseDownLuma, false)
    else
      Canvas.Brush.Color := RGB(R, G, B);

    case fShape of
      drCircle: begin
                  if Height > Width then
                    Width := Height;
                  if Width > Height then
                    Height := Width;

                  Rgn := CreateEllipticRgn(i, i, Width - i, Height - i);
      end;

      drSquare: begin
                  if Height > Width then
                    Width := Height;
                  if Width > Height then
                    Height := Width;
                  Rgn := CreateRectRgn(i, i, Width - i, Height - i);
      end;

      drElliptic: Rgn:= CreateEllipticRgn(i, i, Width - i, Height - i);

      drRoundRect:  Rgn := CreateRoundRectRgn(i, i, Width - i, Height - i,
                                              fShapesStyle.fXRadius,
                                              fShapesStyle.fYRadius);

      drRect: Rgn:= CreateRectRgn(i, i, Width - i, Height - i);

      drUpArrow: begin
        StarPoints[0].X := (Width-i) div 2;
        StarPoints[0].Y := i;

        StarPoints[1].X := (Width - i);
        StarPoints[1].Y := (Height - i) div 3;

        StarPoints[2].X := (Width - i) div 3 * 2;
        StarPoints[2].Y := (Height - i) div 3;

        StarPoints[3].X := (Width - i) div 3 * 2;
        StarPoints[3].Y := (Height - i);

        StarPoints[4].X := (Width - i) div 3;
        StarPoints[4].Y := Height - i;

        StarPoints[5].X := (Width - i) div 3;
        StarPoints[5].Y := (Height - i) div 3;

        StarPoints[6].X := i;
        StarPoints[6].Y := (Height - i) div 3;

        StarPoints[7].X := (Width - i) div 2;
        StarPoints[7].Y := i;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drDownArrow: begin
        StarPoints[0].X := (Width - i) div 3;
        StarPoints[0].Y := i;

        StarPoints[1].X := (Width - i) div 3 * 2;
        StarPoints[1].Y := i;

        StarPoints[2].X := (Width - i) div 3 * 2;
        StarPoints[2].Y := (Height - i) div 3 * 2;

        StarPoints[3].X := Width - i;
        StarPoints[3].Y := (Height - i) div 3 * 2;

        StarPoints[4].X := (Width - i) div 2;
        StarPoints[4].Y := Height - i;

        StarPoints[5].X := i;
        StarPoints[5].Y := (Height - i) div 3 * 2;

        StarPoints[6].X := (Width - i) div 3;
        StarPoints[6].Y := (Height - 1) div 3 * 2;

        StarPoints[7].X := (Width - i) div 3;
        StarPoints[7].Y := i;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drRightArrow: begin
        StarPoints[0].X := (Width-i) div 3 * 2;
        StarPoints[0].Y := i;

        StarPoints[1].X := (Width - i);
        StarPoints[1].Y := (Height - i) div 2;

        StarPoints[2].X := (Width - i) div 3 * 2;
        StarPoints[2].Y := (Height - i);

        StarPoints[3].X := (Width - i) div 3 * 2;
        StarPoints[3].Y := (Height - i) div 3 * 2;

        StarPoints[4].X := i;
        StarPoints[4].Y := (Height - i) div 3 * 2;

        StarPoints[5].X := i;
        StarPoints[5].Y := (Height - i) div 3;

        StarPoints[6].X := (Width - i) div 3 * 2;
        StarPoints[6].Y := (Height - i) div 3;

        StarPoints[7].X := (Width - i) div 3 * 2;
        StarPoints[7].Y := i;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drLeftArrow: begin
        StarPoints[0].X := (Width - i) div 3;
        StarPoints[0].Y := i;

        StarPoints[1].X := (Width - i) div 3;
        StarPoints[1].Y := (Height - i) div 3;

        StarPoints[2].X := (Width - i);
        StarPoints[2].Y := (Height - i) div 3 ;

        StarPoints[3].X := Width - i;
        StarPoints[3].Y := (Height - i) div 3 * 2;

        StarPoints[4].X := (Width - i) div 3;
        StarPoints[4].Y := (Height - i) div 3 * 2;

        StarPoints[5].X := (Width - i) div 3;
        StarPoints[5].Y := (Height - i);

        StarPoints[6].X := i;
        StarPoints[6].Y := (Height - 1) div 2;

        StarPoints[7].X := (Width - i) div 3;
        StarPoints[7].Y := i;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;
    end; // case

    try
      FillRgn(Canvas.Handle, Rgn, Canvas.Brush.Handle);
    finally
      DeleteObject(Rgn);
    end;  // try
  end;
  //////////////
begin
  FillC := ColorToRGB(fShapesStyle.fFillColor);
  LineC := ColorToRGB(fShapesStyle.fLineColor);

  RO := GetRValue(FillC);
  GO := GetGValue(FillC);
  BO := GetBValue(FillC);


  RL := GetRValue(LineC);
  RF := GetRValue(FillC);
  GL := GetGValue(LineC);
  GF := GetGValue(FillC);
  BL := GetBValue(LineC);
  BF := GetBValue(FillC);

  RI := (RF - RL) / fShapesStyle.fLineWidth;
  GI := (GF - GL) / fShapesStyle.fLineWidth;
  BI := (BF - BL) / fShapesStyle.fLineWidth;

  w := 0;
  for n := 0 to fShapesStyle.fLineWidth - 1 do
  begin
    R := RL + Ceil(n * RI);
    G := GL + Ceil(n * GI);
    B := BL + Ceil(n * BI);
    Draw(R, G, B, n);
    w := n;
  end;
  Draw(RO, GO, BO, w + 1 );
end;


procedure TgemShapeBtn.MouseDown(Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
var
  pnt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) {and (fMouseInRegion) }then begin
    fMouseIsDown := true;
    Invalidate;
  end;

  if (Button = mbRight) {and (fMouseInRegion)} and Assigned(fPullDownMenu) then begin
    if GetCursorPos(pnt) then
      fPullDownMenu.Popup(pnt.X, pnt.Y);
    fMouseIsDown := true;
    Invalidate;
  end;
end;


procedure TgemShapeBtn.MouseUp(Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) {and (fMouseInRegion)} then begin
    fMouseIsDown := false;
    Invalidate;
  end;
end;


function TgemShapeBtn.MouseIsInRegion(X, Y: Integer): Boolean;
var
  Rgn: HRgn;
begin
  Result:=False;
  case Shape of
    drCircle:    Rgn := CreateEllipticRgn(0, 0, Width, Width);

    drSquare:    Rgn := CreateRectRgn(0, 0, Width, Width);

    drElliptic:  Rgn := CreateEllipticRgn(0, 0, Width, Height);

    drRoundRect: Rgn := CreateRoundRectRgn(0, 0, Width, Height,
                                            fShapesStyle.fYRadius,
                                            fShapesStyle.fXRadius);

    drRect:      Rgn := CreateRectRgn(0, 0, Width, Height);

      drUpArrow: begin
        StarPoints[0].X := (Width) div 2;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width);
        StarPoints[1].Y := (Height) div 3;

        StarPoints[2].X := (Width ) div 3 * 2;
        StarPoints[2].Y := (Height) div 3;

        StarPoints[3].X := (Width ) div 3 * 2;
        StarPoints[3].Y := (Height);

        StarPoints[4].X := (Width ) div 3;
        StarPoints[4].Y := Height ;

        StarPoints[5].X := (Width ) div 3;
        StarPoints[5].Y := (Height) div 3;

        StarPoints[6].X := 0;
        StarPoints[6].Y := (Height) div 3;

        StarPoints[7].X := (Width ) div 2;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drDownArrow: begin
        StarPoints[0].X := (Width) div 3;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width) div 3 * 2;
        StarPoints[1].Y := 0;

        StarPoints[2].X := (Width) div 3 * 2;
        StarPoints[2].Y := (Height) div 3 * 2;

        StarPoints[3].X := Width ;
        StarPoints[3].Y := (Height) div 3 * 2;

        StarPoints[4].X := (Width ) div 2;
        StarPoints[4].Y := Height ;

        StarPoints[5].X := 0;
        StarPoints[5].Y := (Height) div 3 * 2;

        StarPoints[6].X := (Width ) div 3;
        StarPoints[6].Y := (Height) div 3 * 2;

        StarPoints[7].X := (Width ) div 3;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

        drRightArrow: begin
        StarPoints[0].X := (Width) div 3 * 2;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width );
        StarPoints[1].Y := (Height) div 2;

        StarPoints[2].X := (Width ) div 3 * 2;
        StarPoints[2].Y := (Height);

        StarPoints[3].X := (Width ) div 3 * 2;
        StarPoints[3].Y := (Height) div 3 * 2;

        StarPoints[4].X := 0;
        StarPoints[4].Y := (Height) div 3 * 2;

        StarPoints[5].X := 0;
        StarPoints[5].Y := (Height) div 3;

        StarPoints[6].X := (Width ) div 3 * 2;
        StarPoints[6].Y := (Height) div 3;

        StarPoints[7].X := (Width) div 3 * 2;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;

      drLeftArrow: begin
        StarPoints[0].X := (Width) div 3;
        StarPoints[0].Y := 0;

        StarPoints[1].X := (Width ) div 3;
        StarPoints[1].Y := (Height) div 3;

        StarPoints[2].X := (Width );
        StarPoints[2].Y := (Height) div 3 ;

        StarPoints[3].X := Width ;
        StarPoints[3].Y := (Height) div 3 * 2;

        StarPoints[4].X := (Width ) div 3;
        StarPoints[4].Y := (Height) div 3 * 2;

        StarPoints[5].X := (Width ) div 3;
        StarPoints[5].Y := (Height);

        StarPoints[6].X := 0;
        StarPoints[6].Y := (Height) div 2;

        StarPoints[7].X := (Width ) div 3;
        StarPoints[7].Y := 0;

        Rgn := CreatePolygonRgn(StarPoints, 8, ALTERNATE);
      end;
  else
    Rgn :=  CreateRectRgn(0, 0, Width, Height);
  end;  // of case

  try
    if PtInRegion(Rgn, X, Y) then
      Result := true;
  finally
    DeleteObject(Rgn);
  end;
end;


procedure TgemShapeBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;


procedure TgemShapeBtn.WriteCaption;
var Flags: Word;
    BtnL, BtnT, BtnR, BtnB: Integer;
    R, TR: TRect;
    FTextColor: TColor;
begin
  R:=ClientRect;
  TR:=ClientRect;
  Canvas.Brush.Style:=bsClear;
  Flags := DT_CENTER or DT_SINGLELINE;
  Canvas.Font := Font;

  if (fMouseInRegion) and (not fMouseIsDown) then
    FTextColor := fShapesStyle.ShadowColor
  else
    FTextColor := Self.Font.Color;

  with canvas do
  begin
    BtnT:=FTextT;
    BtnB:=BtnT+TextHeight(Caption);
    BtnL:=FTextL;
    BtnR:=BtnL+TextWidth(Caption);
    TR:=Rect(BtnL, BtnT, BtnR, BtnB);
    R:=TR;
    if (fShapesStyle.TextStyle=txShadowed) and fMouseIsDown then
    begin
      Font.Color:=clBtnShadow;
      OffsetRect(TR, 3, 3);
      DrawText(Handle, PChar(Caption), Length(Caption), TR, Flags);
    end
    else
      if (fShapesStyle.TextStyle=txShadowed) and not fMouseIsDown then
      begin
        Font.Color:=clBtnShadow;
        OffsetRect(TR, 2, 2);
        DrawText(Handle, PChar(Caption), Length(Caption), TR, Flags);
      end;

    if Enabled then Font.Color:=FTextColor
    else if (fShapesStyle.TextStyle=txShadowed) and not Enabled then
      Font.Color:=clBtnFace
    else Font.Color:=clBtnShadow;
    if fMouseIsDown then OffsetRect(R, 1, 1);
    DrawText(Handle, PChar(Caption), Length(Caption), R, Flags);
  end;
end;


procedure TgemShapeBtn.LayoutSetting;
var
  wt, ht: Integer;
  FGlyphL: Integer;
begin
  wt:=Canvas.TextWidth(Caption);
  ht:=Canvas.TextHeight(Caption);

  if Width > wt then
    FGlyphL:=(Width - wt) div 2
  else
    FGlyphL:=0;
 FTextL:=FGlyphL;
 FTextT:=(Height-ht) div 2;
end;


end.


