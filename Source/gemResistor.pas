unit gemResistor;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,

  VCL.Controls, VCL.Graphics, Math;

type
  TResistorDirection = (drLeftRight, drUpDown);

  TElectronicComponent = (drResistor, drCapacitor, drElectroCapacitor, drNPNTransistor,
                          drPNPTransistor, drWire, drPowerSupply, drOpAmp, drGround,
                          drConnection);

  TReverseDraw = (drTrue, drFalse);

  TPoint = record
    X: Longint;
    Y: Longint;
  end;

  TgemResistor = class(TGraphicControl)
  private
    { Private declarations }
    FLineDir: TResistorDirection;
    FComponent: TElectronicComponent;
    FReverseDraw: TReverseDraw;

    function GetLineWidth: Integer;
    function GetLineColour: TColor;
    procedure PlotArc(const Center: TPoint;
                      const Radius: Integer;
                      const StartAngle: Single;
                      const StopAngle: Single);

    procedure SetLineWidth(const NewWidth: Integer);
    procedure SetLineColour(const NewColour: TColor);
    procedure SetLineDir(const NewDir: TResistorDirection);
    procedure SetElectronicComponent(const NewComponent: TElectronicComponent);
    procedure SetReverseDraw(const NewReverseDraw: TReverseDraw);
    procedure DrawResistor;
    procedure DrawWire;
    procedure DrawPowerSupply;
    procedure DrawPNPTransistor;
    procedure DrawNPNTransistor;
    procedure DrawTransistor(isNPN: boolean);
    procedure DrawElectroCap;
    procedure DrawCap;
    procedure DrawOpAmp;
    procedure DrawGround;
    procedure DrawConnection;

  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property ReverseDraw:  TReverseDraw read FReverseDraw write SetReverseDraw;
    property Component: TElectronicComponent read FComponent write SetElectronicComponent;
    property Direction: TResistorDirection read FLineDir write SetLineDir;
    property LineColour: TColor read GetLineColour write SetLineColour;
    property LineWidth: Integer read GetLineWidth write SetLineWidth;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Electronic', [TgemResistor]);
//end;

function TgemResistor.GetLineWidth: Integer;
begin
  Result := Canvas.Pen.Width;
end;

function TgemResistor.GetLineColour: TColor;
begin
  Result := Canvas.Pen.Color;
end;

///////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.SetElectronicComponent(const NewComponent: TElectronicComponent);
begin
  if NewComponent <> FComponent then
  begin
    FComponent := NewComponent;
    Invalidate;
  end;
end;

procedure TgemResistor.SetReverseDraw(const NewReverseDraw: TReverseDraw);
begin
  if NewReverseDraw <> FReverseDraw then
  begin
    FReverseDraw := NewReverseDraw;
    Invalidate;
  end;
end;


///////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.SetLineWidth(const NewWidth: Integer);
begin
  if NewWidth <> Canvas.Pen.Width then
  begin
    Canvas.Pen.Width := NewWidth;
    Invalidate; // redraws the component
  end;
end;

///////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.SetLineColour(const NewColour: TColor);
begin
  if NewColour <> Canvas.Pen.Color then
  begin
    Canvas.Pen.Color := NewColour;
    Invalidate;
  end;
end;



procedure TgemResistor.SetLineDir(const NewDir: TResistorDirection);
begin
  if NewDir <> FLineDir then
  begin
    FLineDir := NewDir;
    Invalidate;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.DrawResistor;
var
  start: Integer;
  AddTo: integer;
begin
  case FLineDir of
    drLeftRight:
      begin
        AddTo := Round(Width / 9);
        start := (Height - Canvas.Pen.Width) div 2;
        Canvas.MoveTo(0, start);
        Canvas.LineTo(AddTo, Height);
        Canvas.LineTo(2*AddTo, 0);
        Canvas.LineTo(3*AddTo, Height);
        Canvas.LineTo(4*AddTo, 0);
        Canvas.LineTo(5*AddTo, Height);
        Canvas.LineTo(6*AddTo, 0);
        Canvas.LineTo(7*AddTo, Height);
        Canvas.LineTo(8*AddTo, 0);
        Canvas.LineTo(Width, Start);

        //Canvas.TextOut(Round(Width/2), round(height/2),'V 1');
      end;
    drUpDown:
      begin
        AddTo := Round(Height / 9);
        start := (Width - Canvas.Pen.Width) div 2;
        Canvas.MoveTo(start, 0);
        Canvas.LineTo(Width, AddTo);
        Canvas.LineTo(0, 2*AddTo);
        Canvas.LineTo(Width, 3*AddTo);
        Canvas.LineTo(0, 4*AddTo);
        Canvas.LineTo(Width, 5*AddTo);
        Canvas.LineTo(0, 6*AddTo);
        Canvas.LineTo(Width, 7*AddTo);
        Canvas.LineTo(0, 8*AddTo);
        Canvas.LineTo(Start, Height);
      end;
  end;
end;

/////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.DrawWire;
var
  start: Integer;
begin
  case FLineDir of
    drUpDown:
      begin
        start := (Width - Canvas.Pen.Width) div 2;
        Canvas.MoveTo(Start, 0);
        Canvas.LineTo(Start, Height);
      end;
     drLeftRight:
      begin
        start := (Height - Canvas.Pen.Width) div 2;
        Canvas.MoveTo(0, start);
        Canvas.LineTo(Width, Start);
      end;
  end;
end;

//////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.DrawElectroCap;
var
  LineLength, CPoint: integer;
  Point: TPoint;


  function CenterOfLine(Point1, Point2: integer): integer;
  begin
    result := Round(abs(Point1 - Point2) div 2) +  min(Point1, Point2);
  end;

begin
  if Width > Height then
    Width := Height;
  if Width < Height then
    Height := Width;

  case FLineDir of
    drUpDown:
      begin
        // line down
        Canvas.MoveTo(Width div 2, 0);
        Canvas.LineTo(Width div 2, Height div 3);

        Canvas.MoveTo(Width div 2, Height);
        Canvas.LineTo(Width div 2, Height div 2);
        // positive line
        Canvas.MoveTo(Width div 4, Height div 3);
        Canvas.LineTo((Width div 4) * 3, Height div 3);
        // negative arc
        LineLength := Width div 2;
        Point.X :=  Width div 2;
        Point.Y :=  Height;
        PlotArc(Point, LineLength , 315, 360);
        PlotArc(Point, LineLength , 0, 45);
        // put plus on cap
        Canvas.MoveTo((Width * 5) div 8, Height div 8);
        Canvas.LineTo((Width *7) div 8, Height div 8);
        // center of the line will be the x value
        CPoint := CenterOfLine((Width * 5) div 8,(Width * 7) div 8);
        Canvas.MoveTo(Cpoint, (Height * 1) div 32);
        Canvas.LineTo(CPoint, (Height * 1) div 4);

      end;
     drLeftRight:
      begin
        // line down
        Canvas.MoveTo(0, Height div 2);
        Canvas.LineTo(Width div 3, Height div 2);

        Canvas.MoveTo(Width, Height div 2);
        Canvas.LineTo(Width div 2, Height div 2);
        // positive line
        Canvas.MoveTo(Width div 3, Height div 4);
        Canvas.LineTo(Width div 3, (Height div 4) * 3);
        // negative arc
        LineLength := Height div 2;
        Point.Y :=  Height div 2;
        Point.X :=  Width;
        PlotArc(Point, LineLength , 225, 315);
        // put plus on cap
        Canvas.MoveTo(Width div 8, (Height * 1) div 8);
        Canvas.LineTo(Width div 8, (Height * 3) div 8);
        // center of the line will be the x value
        CPoint := CenterOfLine((Height * 1) div 8,(Height * 3) div 8);
        Canvas.MoveTo((Width * 1) div 32, CPoint);
        Canvas.LineTo((Width * 1) div 4, CPoint);
      end;
  end;
end;


// This procedure plots an arc on a canvas.

procedure TgemResistor.PlotArc(const Center: TPoint;
                            const Radius: Integer;
                            const StartAngle: Single;
                            const StopAngle: Single);

// This is a nested function for PlotArc.

  function GetPositionForAngle(const Angle: Single): TPoint;
    var
      CosAngle: Extended;
      SinAngle: Extended;

  begin
    SinCos(DegToRad(Angle), SinAngle, CosAngle);
    Result.X := Round(Center.X + Radius * SinAngle);
    Result.Y := Round(Center.Y - Radius * CosAngle);
  end;

// Main part of PlotArc.

var
  Index: Integer;

begin
  with GetPositionForAngle(StartAngle) do
    Canvas.MoveTo(X, Y);

  for Index := Ceil(StartAngle) to Floor(StopAngle) do
    with GetPositionForAngle(Index) do
      Canvas.LineTo(X, Y);

  with GetPositionForAngle(StopAngle) do
    Canvas.LineTo(X, Y);
end;


 ////////////////////////
procedure TgemResistor.DrawCap;
begin
  if Width > Height then
    Width := Height;
  if Width < Height then
    Height := Width;

  case FLineDir of
    drUpDown:
      begin
        Canvas.MoveTo(Width div 2, 0);
        Canvas.LineTo(Width div 2, Height div 3);

        Canvas.MoveTo(Width div 2, Height);
        Canvas.LineTo(Width div 2, (Height div 3) * 2);

        Canvas.MoveTo(Width div 3, Height div 3);
        Canvas.LineTo((Width div 3) * 2, Height div 3);

        Canvas.MoveTo(Width div 3, (Height div 3) * 2);
        Canvas.LineTo((Width div 3) * 2, (Height div 3) * 2);
      end;
     drLeftRight:
      begin
        Canvas.MoveTo(0, Height div 2);
        Canvas.LineTo(Width div 3, Height div 2);

        Canvas.MoveTo(Width, Height div 2);
        Canvas.LineTo(Width div 3 * 2, Height div 2);

        Canvas.MoveTo(Width div 3, Height div 3);
        Canvas.LineTo(Width div 3, Height div 3 * 2);

        Canvas.MoveTo(Width div 3 * 2, Height div 3);
        Canvas.LineTo((Width div 3) * 2, (Height div 3) * 2);
      end;
  end;
end;


procedure TgemResistor.DrawGround;
begin
  if Width > Height then
    Width := Height;
  if Width < Height then
    Height := Width;

  Canvas.MoveTo(Width div 2, 0);
  Canvas.LineTo(Width div 2, Height div 2);

  Canvas.MoveTo(0, Height div 2);
  Canvas.LineTo(Width, Height div 2);
  Canvas.LineTo(Width div 2, Height);
  Canvas.LineTo(0, Height div 2);
end;


////////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.DrawPowerSupply;
var
  start: Integer;
  AddTo: integer;
  HalfNeg, HalfPos: integer;
begin
  case FLineDir of
    drLeftRight:
      begin
        AddTo := Width div 5;
        start := (Height - Canvas.Pen.Width) div 2;
        HalfNeg := Start div 2;
        HalfPos := HalfNeg + Start;

        if fReverseDraw = drFalse then begin
          Canvas.MoveTo(0, start);
          Canvas.LineTo(AddTo, Height div 2);
          Canvas.LineTo(AddTo, HalfNeg);
          Canvas.MoveTo(AddTo, HalfPos);
          Canvas.LineTo(AddTo, Height div 2);

          Canvas.MoveTo(2 * AddTo, Height);
          Canvas.LineTo(2 * AddTo, 0);

          Canvas.MoveTo(3 * AddTo, HalfNeg);
          Canvas.LineTo(3 * AddTo, HalfPos);

          Canvas.MoveTo(4 * AddTo, Height);
          Canvas.LineTo(4 * AddTo, 0);

          Canvas.MoveTo(4 * AddTo, Height div 2);
          Canvas.LineTo(Width, Height div 2);
        end
        else begin
          // draw plus symbol
          //Canvas.MoveTo();

          Canvas.MoveTo(0, start);
          Canvas.LineTo(AddTo, Height div 2);

          Canvas.MoveTo(AddTo, 0);
          Canvas.LineTo(AddTo, Height);

          Canvas.MoveTo(2 * AddTo, HalfNeg);
          Canvas.LineTo(2 * AddTo, HalfPos);

          Canvas.MoveTo(3 * AddTo, Height);
          Canvas.LineTo(3 * AddTo, 0);

          Canvas.MoveTo(4 * AddTo, HalfNeg);
          Canvas.LineTo(4 * AddTo, HalfPos);

          Canvas.MoveTo(4 * AddTo, Height div 2);
          Canvas.LineTo(Width, Height div 2);
        end;
      end;
    drUpDown:
      begin
        AddTo := Round(Height / 5);
        start := (Width - Canvas.Pen.Width) div 2;
        HalfNeg := Start div 2;
        HalfPos := HalfNeg + Start;

        if fReverseDraw = drFalse then begin
          Canvas.MoveTo(start, 0);
          Canvas.LineTo(Width div 2, AddTo);

          Canvas.MoveTo(HalfNeg, AddTo);
          Canvas.LineTo(HalfPos, AddTo);

          Canvas.MoveTo(Width, 2 * AddTo);
          Canvas.LineTo(0, 2 * AddTo);

          Canvas.MoveTo(HalfNeg, 3 * AddTO);
          Canvas.LineTo(HalfPos, 3 * AddTo);

          Canvas.MoveTo(Width, 4 * AddTo);
          Canvas.LineTo(0, 4 * AddTo);

          Canvas.MoveTo(Width div 2, 4 * AddTo);
          Canvas.LineTo(Width div 2, Height);
        end
        else begin
          Canvas.MoveTo(start, 0);
          Canvas.LineTo(Width div 2, AddTo);

          Canvas.MoveTo(Width, AddTo);
          Canvas.LineTo(0, AddTo);

          Canvas.MoveTo(HalfNeg, 2 * AddTo);
          Canvas.LineTo(HalfPos, 2 * AddTo);

          Canvas.MoveTo(Width, 3 * AddTO);
          Canvas.LineTo(0, 3 * AddTo);

          Canvas.MoveTo(HalfNeg, 4 * AddTo);
          Canvas.LineTo(HalfPos, 4 * AddTo);

          Canvas.MoveTo(Width div 2, 4 * AddTo);
          Canvas.LineTo(Width div 2, Height);
        end;
      end;
  end;
end;

////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.DrawTransistor(isNPN: boolean);
var
  {start,} LineLength, OneThird, TwoThirds, mpx,mpy,x1,x2,y1,y2,m,b: integer;
const
  PercentLenghtOfArrowTop = 0.1;
  //PercentFormStartOfWidth = 0.666;
  //=====================================
  function NegReciprocalSlope(x1,x2: integer):integer;
  begin
    result := x1 - x2;
  end;
  //=====================================
  function SlopeTop(x1,x2: integer):integer;
  begin
    result := x1 - x2;
  end;
  //=====================================
  function SlopeBottom(y1,y2: integer):integer;
  begin
    result := y1 - y2;
  end;
  //=====================================
  function MidPointy(y1,y2: integer): integer;
  begin
    result := (y1+y2) div 2;
  end;
  //======================================
  function MidPointx(x1,x2: integer): integer;
  begin
    result := (x1+x2) div 2;
  end;
  //======================================
begin
  if Width > Height then
    Width := Height;
  if Width < Height then
    Height := Width;
  Canvas.Ellipse(0, 0, Width , Height);

  case fReverseDraw of
    drTrue:
      begin
        // gate line
        Canvas.MoveTo(Width, Height div 2);
        Canvas.LineTo((Width div 3) * 2, Height div 2);

        // Transistor line
        Canvas.MoveTo((Width div 3) * 2, Height div 4);
        Canvas.LineTo((Width div 3) * 2, (Height div 4)*3);
        //
        LineLength := (Height div 4)*3 - Height div 4;
        OneThird := LineLength div 3 + Height div 4;
        TwoThirds := ((LineLength div 3) * 2) + Height div 4;

        // collector line
        Canvas.MoveTo((Width div 3) * 2, OneThird);
        Canvas.LineTo(Width div 3, 0);

        // emittor line
        Canvas.MoveTo((Width div 3) * 2, TwoThirds);
        Canvas.LineTo(Width div 3, Height);

        // the rest is for the arrow


        // find mid point for arrow
        mpx := MidPointx((Width div 3) * 2, Width div 3);
        mpy := MidPointY(Height - TwoThirds, Height-Height); // 2 y values Converted to rectangular coordinates

        // find negative reciprocal slope
        x1 := SlopeTop((Width div 3) * 2, (Width div 3));
        y1 := SlopeBottom(Height - TwoThirds, Height-Height);
        m := -1*(y1 div x1);

        // find the y intercept b
        b := mpy - (m*mpx);

        // calculate the two end points for the top of arrow
        x1 := mpx - round(Height * PercentLenghtOfArrowTop);
        y1 := Height - (m*x1+b);

        x2 := mpx + round(Height * PercentLenghtOfArrowTop);
        y2 := Height - (m*x2 + b);

        // draw the arrow with a fill color of black
        Canvas.Brush.Color := clBlack;
        if isNPN then
          Canvas.Polygon([point(x1,y1), point(x2,y2), point(Width div 3, Height)])
        else
          Canvas.Polygon([point(x1,y1), point(x2,y2), point((Width div 3) * 2, TwoThirds)]);

        Canvas.Brush.Color := clWhite;
      end;
    drFalse:
      begin
        // gate line
        Canvas.MoveTo(0, Height div 2);
        Canvas.LineTo(Width div 3, Height div 2);

        // Transistor line
        Canvas.MoveTo(Width div 3, Height div 4);
        Canvas.LineTo(Width div 3, (Height div 4)*3);

        //
        LineLength := (Height div 4)*3 - Height div 4;
        OneThird := LineLength div 3 + Height div 4;
        TwoThirds := ((LineLength div 3) * 2) + Height div 4;

        // collector line
        Canvas.MoveTo(Width div 3, OneThird);
        Canvas.LineTo((Width div 3)*2, 0);

        // emittor line
        Canvas.MoveTo(Width div 3, TwoThirds);
        Canvas.LineTo((Width div 3)*2, Height);


        // the rest is for the arrow


        // find mid point for arrow
        mpx := MidPointx(Width div 3, (Width div 3)*2);
        mpy := MidPointY(Height - TwoThirds, Height-Height); // 2 y values Converted to rectangular coordinates

        // find negative reciprocal slope
        x1 := SlopeTop(Width div 3, (Width div 3)*2);
        y1 := SlopeBottom(Height - TwoThirds, Height-Height);
        m := -1*(y1 div x1);

        // find the y intercept b
        b := mpy - (m*mpx);

        // calculate the two end points for the top of arrow
        x1 := mpx - round(Height * PercentLenghtOfArrowTop);
        y1 := Height - (m*x1+b);

        x2 := mpx + round(Height * PercentLenghtOfArrowTop);
        y2 := Height - (m*x2 + b);

        // draw the arrow with a fill color of black
        Canvas.Brush.Color := clBlack;
        if isNPN then
          Canvas.Polygon([point(x1,y1), point(x2,y2), point((Width div 3)*2, Height)])
        else
          Canvas.Polygon([point(x1,y1), point(x2,y2), point(Width div 3, TwoThirds)]);

        Canvas.Brush.Color := clWhite;

      end;
  end;
end;



procedure TgemResistor.DrawPNPTransistor;
begin
  DrawTransistor(false);
end;


////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.DrawNPNTransistor;
begin
  DrawTransistor(true);
end;

//////////////////////////////////////
//////////////////////////////////////


procedure TgemResistor.DrawOpAmp;
  //=====================================
  function MidPointy(y1,y2: integer): integer;
  begin
    result := (y1+y2) div 2;
  end;
  //======================================
  function MidPointx(x1,x2: integer): integer;
  begin
    result := (x1+x2) div 2;
  end;
  //======================================


begin
  if Width > Height then
    Width := Height;
  if Width < Height then
    Height := Width;

  Canvas.MoveTo(Width div 8, 0);
  Canvas.LineTo(Width div 8, Height);

  Canvas.LineTo((Width *7) div 8, Height div 2);
  Canvas.LineTo(Width div 8, 0);

  // output
  Canvas.MoveTo((Width *7) div 8, Height div 2);
  Canvas.LineTo(Width, Height div 2);
 // inputs
  Canvas.TextOut(Width div 32, Height div 8, '+');
  Canvas.MoveTo(Width div 8, Height div 3);
  Canvas.LineTo(0, Height div 3);

  Canvas.TextOut(Width div 32, (Height * 2) div 3, '-');
  Canvas.MoveTo(Width div 8, (Height * 2) div 3);
  Canvas.LineTo(0, (Height * 2) div 3);
  // power --- we will need the mid point of the 2 sides of the triangle
    // first the positive power line
  Canvas.TextOut(MidPointX((Width *7) div 8, Width div 8), Height div 32, '+');
  Canvas.MoveTo(MidPointX((Width *7) div 8, Width div 8), MidPointy(Height div 2, 0));
  Canvas.LineTo(MidPointX((Width *7) div 8, Width div 8), 0);
    // second the negative power line
  Canvas.TextOut(MidPointX((Width * 7) div 8, Width div 8), (Height * 7) div 8, '-');
  Canvas.MoveTo(MidPointX(Width div 8, (Width *7) div 8), MidPointy(Height div 2, Height));
  Canvas.LineTo(MidPointX(Width div 8, (Width *7) div 8), Height);

end;


//////////////////////////////////////////
///////////////////////////////////////////

procedure TgemResistor.DrawConnection;
begin
  if Width > Height then
    Width := Height;
  if Width < Height then
    Height := Width;

  Canvas.Brush.Color := clBlack;
  Canvas.Ellipse(0, 0, Width , Height);
  Canvas.Brush.Color := clWhite;
end;

/////////////////////
////////////////////////
/////////////////////////////////////////////////////////////////////////////

procedure TgemResistor.Paint;
//var
//  start: Integer;

begin
  inherited;

  if FComponent = drResistor then
    DrawResistor;

  if FComponent = drWire then
    DrawWire;

  if fComponent = drPowerSupply then
    DrawPowerSupply;

  if fComponent = drNPNTransistor then
    DrawPNPTransistor;

  if fComponent = drPNPTransistor then
    DrawNPNTransistor;

  if FComponent = drCapacitor then
    DrawCap;

  if FComponent = drElectroCapacitor then
    DrawElectroCap;

  if FComponent = drOpAmp then
    DrawOpAmp;

  if FComponent = drGround then
    DrawGround;


  if FComponent = drConnection then
    DrawConnection;

end;


end.
