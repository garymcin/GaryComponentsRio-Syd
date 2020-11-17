unit gemArrow;
 (*
    Orginal code from project jedi.  Code ported to use dephi's libs not
    JEDI libs by:
    Gary E Mcintosh
    SlickRock Software Design
    2/2/1012

    This component was removed from project Jedi a few years ago, but I still
    was using it, so the port.  The code was removed from all the jcl and jcvl
    units an set to run using only delphi units.
 *)
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrow.pas, released November 1999.

The Initial Developer of the Original Code is Russell Fox.
Portions created by Anthony Steele are Copyright (C) 1999-2001 Russell Fox.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26       //


-----------------------------------------------------------------------------}

interface

uses
  WinAPI.Windows, WinAPI.Messages,

  System.SysUtils, System.Classes, System.Types,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms,

  GEMComponentsGlobal;


type
  TArrowType = (atLeft, atRight, atUp, atDown, atDownRight, atDownLeft,
                atUpRight, atUpLeft, atRightDown, atLeftDown, atRightUp, atLeftUp,
                atTopLeftBottomRight, atBottomRightTopLeft, atTopRightBottomLeft,
                atBottomLeftTopRight);


  TgemCustomArrow = class(TGraphicControl)
  private
    { Private declarations }
    FArrowSize            : Integer;
    FArrowWidth           : Integer;
    FBrush                : TBrush;
    FPen                  : TPen;
    FShape                : TArrowType;
    fColor                : TColor;
    fTransparent          : boolean;
    FHoverColor           : TColor;
    FBackBeforeHoverColor : TColor;
    fNotEnabledColor      : TColor;
    fVersion              : string;

    procedure SetColor(const Value: TColor);
    procedure SetArrowSize(const Value: Integer);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetBrush(const Value: TBrush);
    procedure SetPen(const Value: TPen);
    procedure SetArrow(const Value: TArrowType);
    procedure SetTransparent(value: boolean);
    procedure DrawArrow(FromX, FromY, ToX, ToY, Size, Width: Integer);
    procedure setHoverColor(const Value: TColor);
    procedure SetNotEnabledColor(const Value: TColor);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure WndProc(var Message : TMessage); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Color: TColor read FColor write SetColor default clWhite;
    property HoverColor: TColor Read fHoverColor write setHoverColor default clSilver;
    property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;
    property ArrowSize: Integer read FArrowSize write SetArrowSize default 5;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 5;
    property Pen: TPen read FPen write SetPen;
    property Shape: TArrowType read FShape write SetArrow default atDownRight;
    property Brush: TBrush read FBrush write SetBrush;
    property Transparent: boolean read fTransparent write SetTransparent default true;
    property NotEnabledColor: TColor read fNotEnabledColor write SetNotEnabledColor ;
    procedure StyleChanged(Sender: TObject);
  published
    { Published declarations }
        // inherited stuff
    property Action;
  end;

  /////////////////////////

  TgemArrow = class(TgemCustomArrow)
  private
    function GetVersion: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
        // inherited stuff
    property Action;
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
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnClick;

    property Transparent;
    property ArrowSize;
    property ArrowWidth;
    property Color;
    property HoverColor;
    property NotEnabledColor;
    property Verson: string read GetVersion;
  end;



//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TgemArrow]);
//end;

{ TgemCustomArrow }

constructor TgemCustomArrow.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle    := ControlStyle + [csReplicatable];
  Width           := 65;
  Height          := 65;
  ArrowSize       := 5;
  ArrowWidth      := 5;
  FPen            := TPen.Create;
  FPen.OnChange   := StyleChanged;
  FBrush          := TBrush.Create;
  FBrush.OnChange := StyleChanged;
  FShape          := atDownRight;
  FColor          := clWhite;
  fHoverColor     := clSilver;
  fNotEnabledColor:= clBlack;
  fTransparent    := true;
end;



destructor TgemCustomArrow.Destroy;
begin
  FPen.Free;
  FBrush.Free;

  inherited;
end;


procedure TgemCustomArrow.WndProc(var Message : TMessage);
begin
  if (Message.Msg = CM_MOUSELEAVE) then begin
    Color := BackBeforeHoverColor;
    invalidate;
  end;

  if (Message.Msg = CM_MOUSEENTER) then begin
    BackBeforeHoverColor := Color;
    if enabled then
      Color := HoverColor
    else
      Color := fNotEnabledColor;
    invalidate;
  end;

  inherited;
end; (*WndProc*)


procedure TgemCustomArrow.SetTransparent(value: boolean);
begin
  FTransparent := value;
  Invalidate;
end;


procedure TgemCustomArrow.SetNotEnabledColor(const Value: TColor);
begin
  if (not FTransparent) and (fNotEnabledColor <> Value) then begin
    fNotEnabledColor := Value;
    Invalidate;
  end;
end;


procedure TgemCustomArrow.SetHoverColor(const Value: TColor);
begin
  if (not FTransparent) and (FHoverColor <> Value) then begin
    fHoverColor := Value;
    Invalidate;
  end;
end;


procedure TgemCustomArrow.SetColor(const Value: TColor);
begin
  if not FTransparent then begin
    FColor := Value;
    Invalidate;
  end;
end;


 (*

{ .... }

var
   BeginPoint: TPoint;

{ .... }

uses Math;

{ .... }

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
begin
   BeginPoint.X := X;
   BeginPoint.Y := Y;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
var
   B, deltaX, deltaY: Extended;
begin
   Image1.Canvas.PenPos := BeginPoint;
   // Beginpoint is the point from where the use drew the line
   Image1.Canvas.LineTo(X, Y);

   if BeginPoint.X <> X then // checks for division by zero
   begin
     if (BeginPoint.X > X) then
       B := DegToRad(135) - ArcTan((BeginPoint.Y - Y) / (BeginPoint.X - X))
     else
       B := DegToRad(45) - ArcTan((BeginPoint.Y - Y) / (BeginPoint.X - X));
     // the arrow will have a 45 deg corner

     deltaX := 15 * Cos(B); // 15 is the length of the arrow
     deltaY := 15 * Sin(B);

     if (BeginPoint.X > X) then
     begin
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X - Trunc(deltaX), Y + Trunc(deltaY));
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X + Trunc(deltaY), Y + Trunc(deltaX));
     end
     else
     begin
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X - Trunc(deltaX), Y + Trunc(deltaY));
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X - Trunc(deltaY), Y - Trunc(deltaX));
     end;
   end
   else
   begin
     if BeginPoint.Y > Y then
     begin
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X + 10, Y + 10);
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X - 10, Y + 10);
     end
     else
     begin
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X + 10, Y - 10);
       Image1.Canvas.PenPos := Point(X, Y);
       Image1.Canvas.LineTo(X - 10, Y - 10);
     end;
   end;
end;


 *)

procedure TgemCustomArrow.Paint;
var
  X, Y, W, H  : Integer;
  ArrowPoints : array [1..3] of TPoint;
  liSign      : Integer;
  Arrow_FromX : Integer;
  Arrow_FromY : Integer;
  Arrow_ToX   : Integer;
  Arrow_ToY   : Integer;
  GUI_PAD     : Integer;
  rect        : tRect;

const
  PadArrowTip = 5;

begin
  inherited;
  if Shape in[atLeft, atUp, atDown, atRight] then
    GUI_PAD := ArrowSize  + 5
  else
    if ArrowWidth > ArrowSize then
      GUI_PAD := ArrowWidth + 2
    else
      GUI_PAD := ArrowSize + 2;
  rect.Left   := 0;
  Rect.Right  := width;
  rect.Top    := 0;
  rect.Bottom := Height;

  if not fTransparent then begin
    Canvas.Brush.Color := fColor;
    Canvas.FillRect(rect);
    Canvas.FloodFill(1,1,fColor, fsBorder);
  end;
  Canvas.Pen   := FPen;
  Canvas.Brush := FBrush;
  if Shape in[atLeft, atUp, atDown, atRight] then begin
    X     := 0;
    Y     := X;
    W     := Width;
    H     := Height;
  end
  else begin
    X     := Canvas.Pen.Width div 2;
    Y     := X;
    W     := Width - Canvas.Pen.Width + 1;
    H     := Height - Canvas.Pen.Width + 1;
  end;

  if Canvas.Pen.Width = 0 then
  begin
    Dec(W);
    Dec(H);
  end;

  case Shape of
    atDown:
      begin
        ArrowPoints[1].x := W div 2;
        ArrowPoints[1].y := 0;
        ArrowPoints[2].x := ArrowPoints[1].x;
        ArrowPoints[2].y := H - GUI_PAD;
        ArrowPoints[3].x := ArrowPoints[2].x ;
        ArrowPoints[3].y := ArrowPoints[2].y;
      end;
    atUp:
      begin
        ArrowPoints[1].x := W div 2;
        ArrowPoints[1].y := H;
        ArrowPoints[2].x := ArrowPoints[1].x;
        ArrowPoints[2].y := GUI_PAD;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y;
      end;
    atLeft:
      begin
        ArrowPoints[1].x := W;
        ArrowPoints[1].y := H div 2;
        ArrowPoints[2].x := GUI_PAD;
        ArrowPoints[2].y := ArrowPoints[1].y;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y ;
      end;
    atRight:
      begin
        ArrowPoints[1].x := 0;
        ArrowPoints[1].y := H div 2;
        ArrowPoints[2].x := W - GUI_PAD;
        ArrowPoints[2].y := ArrowPoints[1].y;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y ;
      end;
    atRightDown:
      begin
        ArrowPoints[1].x := X + GUI_PAD;
        ArrowPoints[1].y := Y + GUI_PAD;
        ArrowPoints[2].x := (X + (W - GUI_PAD));
        ArrowPoints[2].y := Y + GUI_PAD;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := (Y + (H - GUI_PAD));
      end;
    atDownLeft:
      begin
        ArrowPoints[1].x := (X + (W - GUI_PAD));
        ArrowPoints[1].y := Y + GUI_PAD;
        ArrowPoints[2].x := ArrowPoints[1].x;
        ArrowPoints[2].y := (Y + (H - GUI_PAD));
        ArrowPoints[3].x := X + GUI_PAD;
        ArrowPoints[3].y := (Y + (H - GUI_PAD));
      end;
    atLeftDown:
      begin
        ArrowPoints[1].x := (X + (W - GUI_PAD));
        ArrowPoints[1].y := Y + GUI_PAD;
        ArrowPoints[2].x := X + GUI_PAD;
        ArrowPoints[2].y := ArrowPoints[1].y;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := (Y + (H - GUI_PAD));
      end;
    atUpLeft:
      begin
        ArrowPoints[1].x := (X + (W - GUI_PAD));
        ArrowPoints[1].y := (Y + (H - GUI_PAD));
        ArrowPoints[2].x := ArrowPoints[1].x;
        ArrowPoints[2].y := Y + GUI_PAD;
        ArrowPoints[3].x := X + GUI_PAD;
        ArrowPoints[3].y := Y + GUI_PAD;
      end;
    atLeftUp:
      begin
        ArrowPoints[1].x := (X + (W - GUI_PAD));
        ArrowPoints[1].y := (Y + (H - GUI_PAD));
        ArrowPoints[2].x := X + GUI_PAD;
        ArrowPoints[2].y := ArrowPoints[1].y;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := Y + GUI_PAD;
      end;
    atUpRight:
      begin
        ArrowPoints[1].x := X + GUI_PAD;
        ArrowPoints[1].y := (Y + (H - GUI_PAD));
        ArrowPoints[2].x := ArrowPoints[1].x;
        ArrowPoints[2].y := Y + GUI_PAD;
        ArrowPoints[3].x := (X + (W - GUI_PAD));
        ArrowPoints[3].y := Y + GUI_PAD;
      end;
    atRightUp:
      begin
        ArrowPoints[1].x := X + GUI_PAD;
        ArrowPoints[1].y := (Y + (H - GUI_PAD));
        ArrowPoints[2].x := (X + (W - GUI_PAD));
        ArrowPoints[2].y := ArrowPoints[1].y;
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := Y + GUI_PAD;
      end;
    atTopLeftBottomRight:
      begin
        ArrowPoints[1].x := X + GUI_PAD;
        ArrowPoints[1].y := Y + GUI_PAD;
        ArrowPoints[2].x := (X + (W - GUI_PAD));
        ArrowPoints[2].y := (Y + (H - GUI_PAD));
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y;
      end;
    atBottomRightTopLeft:
      begin
        ArrowPoints[2].x := X + GUI_PAD;
        ArrowPoints[2].y := Y + GUI_PAD;
        ArrowPoints[1].x := (X + (W - GUI_PAD));
        ArrowPoints[1].y := (Y + (H - GUI_PAD));
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y;
      end;
    atTopRightBottomLeft:
      begin
        ArrowPoints[1].x := (X + (W - GUI_PAD));
        ArrowPoints[1].y := Y + GUI_PAD;
        ArrowPoints[2].x := X + GUI_PAD;
        ArrowPoints[2].y := (Y + (H - GUI_PAD));
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y;
      end;
    atBottomLeftTopRight:
      begin
        ArrowPoints[2].x := (X + (W - GUI_PAD));
        ArrowPoints[2].y := Y + GUI_PAD;
        ArrowPoints[1].x := X + GUI_PAD;
        ArrowPoints[1].y := (Y + (H - GUI_PAD));
        ArrowPoints[3].x := ArrowPoints[2].x;
        ArrowPoints[3].y := ArrowPoints[2].y;
      end;
  else
    ArrowPoints[1].x := X + GUI_PAD;
    ArrowPoints[1].y := Y + GUI_PAD;
    ArrowPoints[2].x := ArrowPoints[1].x;
    ArrowPoints[2].y := (Y + (H - GUI_PAD));
    ArrowPoints[3].x := (X + (W - GUI_PAD));
    ArrowPoints[3].y := (Y + (H - GUI_PAD));
  end;
    {draw lines}
  Canvas.PolyLine(ArrowPoints);

  {------------------------ARROWS----------------------------}

  if Shape in [atDownLeft, atDownRight, atUpLeft, atUpRight, atRight, atLeft] then
  begin
    {left or right}
    if Shape in [atUpLeft, atDownLeft, atLeft] then
      liSign := -1
    else
      liSign := +1;
    Arrow_FromX := ArrowPoints[3].x;
    Arrow_FromY := ArrowPoints[3].y;
    Arrow_ToX   := ArrowPoints[3].x + (ArrowSize * liSign);
    Arrow_ToY   := ArrowPoints[3].y;
  end
  else
  if Shape in [atTopLeftBottomRight, atBottomLeftTopRight] then
  begin
//      Arrow_FromX := 0;
//      Arrow_FromY := 0;
//      Arrow_ToY := ArrowPoints[3].y + ArrowSize;
    Arrow_ToX := ArrowPoints[3].x + ArrowSize;

    {down or up}
    if Shape in [atBottomLeftTopRight] then
      Arrow_ToY := ArrowPoints[3].y - ArrowSize
    else
      Arrow_ToY := ArrowPoints[3].y + ArrowSize;

    Arrow_FromX := ArrowPoints[3].x;
    Arrow_FromY := ArrowPoints[3].y;
  end
  else if Shape in [atBottomRightTopLeft, atTopRightBottomLeft] then
  begin
//      Arrow_FromX := 0;
//      Arrow_FromY := 0;
    Arrow_ToX := ArrowPoints[3].X - ArrowSize;
    {down or up}
    if Shape in [atBottomRightTopLeft] then
      Arrow_ToY := ArrowPoints[3].y - ArrowSize
    else
      Arrow_ToY := ArrowPoints[3].y + ArrowSize;
    Arrow_FromX := ArrowPoints[3].x;
    Arrow_FromY := ArrowPoints[3].y;
  end
  else
  begin
    {down or up}
    if Shape in [atLeftUp, atRightUp, atUp] then
      liSign := -1
    else
      liSign := +1;
    Arrow_FromX := ArrowPoints[3].x;
    Arrow_FromY := ArrowPoints[3].y;
    Arrow_ToX   := ArrowPoints[3].x;
    Arrow_ToY   := ArrowPoints[3].y + (ArrowSize * liSign);
  end;

  DrawArrow(Arrow_FromX, Arrow_FromY, Arrow_ToX, Arrow_ToY, ArrowSize, ArrowWidth);
end;



procedure TgemCustomArrow.SetArrow(const Value: TArrowType);
begin
  FShape := Value;
  Invalidate;
end;


procedure TgemCustomArrow.SetArrowSize(const Value: Integer);
begin
  FArrowSize := Value;
  Invalidate;
end;


procedure TgemCustomArrow.SetArrowWidth(const Value: Integer);
begin
  FArrowWidth := Value;
  Invalidate;
end;


procedure TgemCustomArrow.SetBrush(const Value: TBrush);
begin
  FBrush := Value;
  Invalidate;
end;


procedure TgemCustomArrow.SetPen(const Value: TPen);
begin
  FPen := Value;
  Invalidate;
end;


procedure TgemCustomArrow.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;


{ *** DrawArrow Procedure  ***
  Written By Scott M. Straley (straley@fast.net) -- March 15, 1995}
procedure TgemCustomArrow.DrawArrow(FromX, FromY, ToX, ToY, Size, Width: Integer);
var
  Line1, Line2, ShortLine1, ShortLine2, ArrowX,
  ArrowY, Point1X, Point1Y, Point2X, Point2Y: Integer;
  Angle: Real;
begin
  {determining angle of X2 of line based on:

     X1
     |\
     | \  hypotneus
  L1 |  \
     |   \
     -----X2
       L2                                     }

  Line1 := (FromY - ToY);
  Line2 := (FromX - ToX);

  {We need this code to prevent DivByZero errors}

  if Line2 <> 0 then
  begin
    Angle := ArcTan(Line1 / Line2);
  end
  else
  begin
    if Line1 > 0 then
      Angle := -1.5707
    else
      Angle := 1.5707;
  end;

  {now determine where the back of the arrow is}

  if ToX > FromX then
  begin
    ShortLine1 := Round(Size * Sin(Angle));
    ShortLine2 := Round(Size * Cos(Angle));
    ArrowX     := ToX - ShortLine2;
    ArrowY     := ToY - ShortLine1;
  end
  else
  begin
    ShortLine1 := Round(Size * Sin(Angle));
    ShortLine2 := Round(Size * Cos(Angle));
    ArrowX     := ToX + ShortLine2;
    ArrowY     := ToY + ShortLine1;
  end;

  {now determine points perpendictular to the
   arrow line}

  Point1X := ArrowX - Round(Width * (Sin(Angle)));
  Point1Y := ArrowY + Round(Width * (Cos(Angle)));
  Point2X := ArrowX + Round(Width * (Sin(Angle)));
  Point2Y := ArrowY - Round(Width * (Cos(Angle)));

  Canvas.MoveTo(FromX, FromY);
  Canvas.LineTo(ToX, ToY);
  // 11/18/99 Michael Beck
  // need to adjust for "FromX=ToX" as the current Polygon is drawing
  // Arrowhead in the other direction
  if FromX = ToX then
    Canvas.Polygon([Point(Point2X, ToY - (Point2Y - ToY)),
      Point(Point1X, ToY - (Point2Y - ToY)), Point(ToX, ToY)])
  else
  //end of Beck's correction
    Canvas.Polygon([Point(Point2X, Point2Y), Point(Point1X, Point1Y), Point(ToX, ToY)]);
end;



{ TgemArrow }

function TgemArrow.GetVersion: string;
begin
  fVersion := VersionGEMArrow;
end;

end.
