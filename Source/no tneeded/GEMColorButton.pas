unit GEMColorButton;

interface

uses
  Winapi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes,

  Vcl.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ExtCtrls, {VCL.Buttons,}

  GEMComponentsGlobal;


type
  TRgbCOlors = Record
    R,G,B,A:Byte;
  End;

  TAlignment =
    (alTopLeft, alTopCenter, alTopRight,
    alMiddleLeft, alMiddleCenter, alMiddleRight,
    alBottomLeft, alBottomCenter, alBottomRight);

  TButtonBevel = (bbLowered, bbNone, bbRaised);

  TButtonStyles = (bsAutoSize, bsCenter, bsStretch, bsShowFocus, bsShowKey);

  TButtonStyle = set of TButtonStyles;

  TButtonState = (bsUp, bsDown, bsDisabled);


  TClientAddressLoc = record
    Rect1Top,
    Rect1Left,
    Rect1Bottom,
    Rect1Right: integer;

    Rect2Top,
    Rect2Left,
    Rect2Bottom,
    Rect2Right: integer;

    Add1,
    Add2,
    City,
    State,
    Zip: string;
  end;

  TGEMColorButton = class(TCustomControl)
  private
    fClientAddressLoc : TClientAddressLoc;

    FAlignment            : TAlignment;
    FBevelStyle           : TButtonBevel;
    FBevelSize            : Integer;
    FPicture              : TPicture;
    FSpacing              : Integer;
    FStyle                : TButtonStyle;
		FFocused              : Boolean;
    FState                : TButtonState;

    fArrayIndex           : integer;
    fAccountID            : integer;
    fRouteID              : integer;
    fRouteName            : string;
    fName                 : string;
    fAddress              : string;
    fRectangle            : TRect;
    FLat, FLng            : extended;
    fGeoCodeType          : Integer;

    FColor                : TColor;
//    FBackBeforeHoverColor : TColor;
    FHoverColor           : TColor;
    fSelectedColor        : TColor;
    fRouteColor           : TColor;   // the color for the delivery or default route

    fSelected             : Boolean;

    procedure SetCaption(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetFocusOff(var Message: TMessage); message CM_LOSTFOCUS;
    procedure SetFocusOn(var Message: TMessage); message CM_GOTFOCUS;
    procedure SetFont(var Message: TMessage); message CM_FONTCHANGED;
//    procedure SetEnabled(var Message: TMessage); message CM_ENABLEDCHANGED;

    procedure SetRouteColor(Value: TColor);
    procedure SetColor(Value: TColor);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevelStyle(Value: TButtonBevel);
    procedure SetBevelSize(Value: Integer);
    procedure SetPicture(Value: TPicture);
    procedure SetSpacing(Value: Integer);
    procedure SetStyle(Value: TButtonStyle);
    procedure SetSelectedColor(Value: TColor);
    procedure SetSelected(Value: Boolean);
    procedure SetHoverColor(const Value: TColor);

    procedure KeyAccel(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    //property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;

  protected
    procedure SetEnabled(Value: Boolean); override; //message CM_ENABLEDCHANGED;
    procedure WndProc(var Message : TMessage); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    function GetVersion: string;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Paint; override;
    property RouteColor: TColor read fRouteColor write setRouteColor;
  published
    property ClientAddressLocAdd1: String read fClientAddressLoc.add1 write fClientAddressLoc.add1;
    property ClientAddressLocAdd2: String read fClientAddressLoc.add2 write fClientAddressLoc.add2;

    property ClientAddressCity: String read fClientAddressLoc.City write fClientAddressLoc.City;
    property ClientAddressState: String read fClientAddressLoc.State write fClientAddressLoc.State;
    property ClientAdressZip: String read fClientAddressLoc.Zip write fClientAddressLoc.Zip;

    property ArrayIndex: integer read fArrayIndex write fArrayIndex;
    property AccountID: integer read fAccountID write fAccountID;
    property RouteID: integer read fRouteID write fRouteID;
    property RouteName: string read fRouteName write fRouteName;
    property ClientName: string read fName write fName;
    property ClientAddress: string read fAddress write fAddress;
    property TheRectangle: TRect read fRectangle write fRectangle;
    property Lat: extended read fLat write fLat;
    property Lng: extended read fLng write fLng;
    property GeoCodeType: Integer read fGeoCodeType write fGeoCodeType;

    property Version: string read GetVersion;

    property Action;
    property Anchors;
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default alMiddleCenter;
    property BevelStyle: TButtonBevel read FBevelStyle write SetBevelStyle
      default bbRaised;
    property BevelSize: Integer read FBevelSize write SetBevelSize default 2;
    property Caption;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property Left;
    property Name;
    property Picture: TPicture read FPicture write SetPicture;
    property PopUpMenu;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Style: TButtonStyle read FStyle
      write SetStyle default [bsCenter, bsShowFocus];
    property ShowHint;
    property Tag;
    property TabOrder;
    property TabStop;
    property Top;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property Selected: Boolean read fSelected write SetSelected default False;
    property SelectedColor: TColor read fSelectedColor write SetSelectedColor;
    property HoverColor: TColor read FHoverColor write SetHoverColor;
  end;


//procedure Register;

//function Smallest(X, Y: Integer): Integer;
//function Largest(X, Y: Integer): Integer;
//function GetHighlightColor(BaseColor: TColor): TColor;
//function GetShadowColor(BaseColor: TColor): TColor;
//function GetSpeedKey(var Caption: String): Integer;

implementation


//procedure Register;
//begin
// RegisterComponents('Gary"s Stuff', [TGEMColorButton]);
//end;


// Global procedures and functions
/////////////////////////////////////////////////////////////
function Smallest(X, Y: Integer): Integer;
begin
  if X < Y then Result := X else Result := Y;
end;

function Largest(X, Y: Integer): Integer;
begin
  if X > Y then Result := X else Result := Y;
end;

function GetHighlightColor(BaseColor: TColor): TColor;
begin
  Result := RGB(
  Smallest(GetRValue(ColorToRGB(BaseColor)) + 64, 255),
  Smallest(GetGValue(ColorToRGB(BaseColor)) + 64, 255),
  Smallest(GetBValue(ColorToRGB(BaseColor)) + 64, 255));
end;


function GetShadowColor(BaseColor: TColor): TColor;
begin
  Result := RGB(
  Largest(GetRValue(ColorToRGB(BaseColor)) - 64, 0),
  Largest(GetGValue(ColorToRGB(BaseColor)) - 64, 0),
  Largest(GetBValue(ColorToRGB(BaseColor)) - 64, 0));
end;


function GetSpeedKey(var Caption: String): Integer;
var
  keyPos: Integer;
begin
  // Find the speed key location
  keyPos := Pos('&', Caption);
  // Delete the '&' symbol
  Delete(Caption, keyPos, 1);
  // Return the location of the speed key
  Result := keyPos;
end;


// ColorButton procedures and functions
//////////////////////////////////////////////////////////////////
function TGEMColorButton.GetVersion: string;
begin
  Result := '1.1';
end;


constructor TGEMColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment	:= alMiddleCenter;
  FBevelStyle	:= bbRaised;
  FBevelSize	:= 2;
  FColor	:= clBtnFace;
  FPicture := TPicture.Create;
  FSpacing := 2;
  FStyle	:= [bsCenter, bsShowFocus, bsShowKey];
  FFocused := False;
  FState := bsUp;
  Width := 75; Height := 25;
  Enabled := True;
  TabStop := True;
end;


destructor TGEMColorButton.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;


procedure TGEMColorButton.Loaded;
begin
  inherited Loaded;
  if Enabled then FState := bsUp else FState := bsDisabled;
end;




procedure TGEMColorButton.Paint;

  procedure DrawCaption(Offset: Integer);
  var
    xLoc,
    yLoc,
    edgeSize,
    keyPos: Integer;
    newCaption: String;
  begin
    edgeSize := (FBevelSize + FSpacing);
    newCaption := Caption;
    keyPos := GetSpeedKey(newCaption);
    with inherited Canvas do begin
      // Work out text location
     case FAlignment of
       alTopLeft: begin
         xLoc := edgeSize + Offset;
         yLoc := edgeSize + Offset;
       end;
      alTopCenter:
      begin
        xLoc := edgeSize + Offset + ((Width - (edgeSize * 2))
          - TextWidth(newCaption)) div 2;
        yLoc := edgeSize + Offset;
      end;
      alTopRight:
      begin
        xLoc := Width - edgeSize - TextWidth(newCaption) + Offset;
        yLoc := edgeSize + Offset;
       end;
      alMiddleLeft:
      begin
        xLoc := edgeSize + Offset;
        yLoc := edgeSize + Offset + ((Height - (edgeSize * 2))
          - TextHeight(newCaption)) div 2;
      end;
      alMiddleCenter:
        begin
          xLoc := edgeSize + Offset + ((Width - (edgeSize * 2))
            - TextWidth(newCaption)) div 2;
          yLoc := edgeSize + Offset + ((Height - (edgeSize * 2))
            - TextHeight(newCaption)) div 2;
        end;
      alMiddleRight:
      begin
        xLoc := Width - edgeSize - TextWidth(newCaption) + Offset;
        yLoc := edgeSize + Offset + ((Height - (edgeSize * 2))
          - TextHeight(newCaption)) div 2;
      end;
      alBottomLeft:
      begin
        xLoc := edgeSize + Offset; yLoc := Height - edgeSize
          - TextHeight(newCaption) + Offset;
      end;
      alBottomCenter:
      begin
        xLoc := edgeSize + Offset + ((Width - (edgeSize * 2))
          - TextWidth(newCaption)) div 2;
        yLoc := Height - edgeSize - TextHeight(newCaption) + Offset;
      end;
      alBottomRight:
      begin
        xLoc := Width - edgeSize - TextWidth(newCaption) + Offset;
        yLoc := Height - edgeSize - TextHeight(newCaption) + Offset;
      end;
      else
        // Just in-case...
        xLoc := edgeSize + Offset + ((Width - (edgeSize * 2))
           - TextWidth(newCaption)) div 2;
        yLoc := edgeSize + Offset + ((Height - (edgeSize * 2))
           - TextHeight(newCaption)) div 2;
      end;

      // Draw the text
      TextOut(xLoc, yLoc, newCaption);
      // Draw the speed key
      if ((keyPos > 0) and (bsShowKey in FStyle)) then
      begin
        // Can't use underscore character - unlikely to be correct width
        Pen.Color := Font.Color;
        MoveTo(xLoc + (TextWidth(Copy(newCaption, 1, keyPos - 1))),
          yLoc + (TextHeight('ABC')));
        LineTo(xLoc + (TextWidth(Copy(newCaption, 1, keyPos))),
          yLoc + (TextHeight('ABC')));
      end;
    end;
  end;

var
  Client, Picture: TRect;
  clHigh, clLow: TColor;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FState := bsDisabled
  else
    if FState = bsDisabled then FState := bsUp;
  if ((not (FPicture.Graphic = nil)) and (bsAutoSize in FStyle)) then
  begin
    Width := FPicture.Width + (FBevelSize * 2);
    Height := FPicture.Height + (FBevelSize * 2);
  end;
  Client := Bounds(0, 0, Width, Height);
  Canvas.Font.Assign(Font);
  with inherited Canvas do
  begin
    // Clear the background
    Brush.Color := FColor;
    FillRect(Client);
    // Draw the button bevel
    if not (FBevelStyle = bbNone) then
    begin
   // Get the bevel colors
     if ((FState = bsDown) xor (FBevelStyle = bbLowered)) then
     begin
      clHigh := GetShadowColor(FColor);
      clLow := GetHighlightColor(FColor);
     end
    else
     begin
      clHigh := GetHighlightColor(FColor);
      clLow := GetShadowColor(FColor);
     end;
    Frame3D(Canvas, Client, clHigh, clLow, FBevelSize);
  end;
   // Draw the focus
   if (FFocused and (bsShowFocus in FStyle)) and Enabled then
    DrawFocusRect(Rect(
      Client.Left + FSpacing - 1, Client.Top + FSpacing - 1,
       Client.Right - FSpacing + 1, Client.Bottom - FSpacing + 1
       ));
   // Draw the picture
   if (FPicture <> nil) then
   begin
     if (bsStretch in FStyle) then
       Picture := Rect(
         FBevelSize + FSpacing,
         FBevelSize + FSpacing, Width - (FBevelSize + FSpacing),
         Height - (FBevelSize + FSpacing))
     else if (bsCenter in FStyle) then
       Picture := Bounds(
        (Width - FPicture.Width) div 2, (Height - FPicture.Height) div 2,
        FPicture.Width, FPicture.Height
        )
    else
      case FAlignment of
       alTopLeft, alTopCenter, alTopRight:
         Picture := Bounds(
           (Width - FPicture.Width) div 2,
           ((Height - (FBevelSize + FSpacing)) - FPicture.Height),
           FPicture.Width, FPicture.Height);
       alMiddleLeft:
          Picture := Bounds(
           ((Width - (FBevelSize + FSpacing)) - FPicture.Width),
            (Height - FPicture.Height) div 2,
            FPicture.Width, FPicture.Height);
       alMiddleCenter:
         Picture := Bounds(
           (Width - FPicture.Width) div 2,
          (Height - FPicture.Height) div 2,
          FPicture.Width, FPicture.Height);
       alMiddleRight:
          Picture := Bounds(
           (FBevelSize + FSpacing),
            (Height - FPicture.Height) div 2,
           FPicture.Width, FPicture.Height);
       alBottomLeft, alBottomCenter, alBottomRight:
         Picture := Bounds(
           (Width - FPicture.Width) div 2,
            (FBevelSize + FSpacing),
           FPicture.Width, FPicture.Height);
      end;
      StretchDraw(Picture, FPicture.Graphic);
   end
   else
    begin
     Brush.Color := FColor;
     FillRect(Rect(FBevelSize, FBevelSize, Width - FBevelSize,
       Height - FBevelSize));
    end;
   // Draw the caption
   if (Caption <> '') then
   begin
     Brush.Style := bsClear;
     if ((not Enabled) and (not (csDesigning in ComponentState))) then
     begin
       Font.Color := GetHighlightColor(FColor); DrawCaption(1);
       Font.Color := GetShadowColor(FColor); DrawCaption(0);
     end
     else
       DrawCaption(0);
    end;
  end;
end;


procedure TGEMColorButton.DoEnter;
begin
  fFocused := True;
  Repaint;
  inherited DoEnter;
end;


procedure TGEMColorButton.DoExit;
begin
  fFocused := False;
  Repaint;
  inherited DoExit;
end;


procedure TGEMColorButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_SPACE then
    if Enabled then
    begin
      FState := bsDown;
      Repaint;
    end;
end;


procedure TGEMColorButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = VK_SPACE then
    if Enabled then
    begin
      FState := bsUp;
      Click; Repaint;
    end;
   if Key = VK_RETURN then
     if not (FState = bsDisabled) then Click;
end;


procedure TGEMColorButton.KeyAccel(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else inherited;
  end;
end;


procedure TGEMColorButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Enabled then
  begin
    FState := bsDown;
    Repaint;
  end;
end;


procedure TGEMColorButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Enabled then
  begin
    FState := bsUp;
    Repaint;
  end;
end;


procedure TGEMColorButton.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  Repaint;
end;


procedure TGEMColorButton.SetBevelStyle(Value: TButtonBevel);
begin
  FBevelStyle := Value;
  Repaint;
end;


procedure TGEMColorButton.SetBevelSize(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FBevelSize := Value;
  Repaint;
end;


procedure TGEMColorButton.SetCaption(var Message: TMessage);
begin
  Repaint;
end;


procedure TGEMColorButton.SetColor(Value: TColor);
begin
  FColor := Value;
  Repaint;
end;

(*
procedure TControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Perform(CM_ENABLEDCHANGED, 0, 0);
  end;
end;

*)


procedure TGEMColorButton.SetEnabled(Value: Boolean);//(var Message: TMessage);
begin
  inherited;
  if Enabled then
    FState := bsUp
  else FState := bsDisabled;
  Repaint;
end;


procedure TGEMColorButton.SetFocusOff(var Message: TMessage);
begin
  inherited;
  FFocused := False;
  Repaint;
end;


procedure TGEMColorButton.SetFocusOn(var Message: TMessage);
begin
  inherited;
  FFocused := True;
  Repaint;
end;


procedure TGEMColorButton.SetFont(var Message: TMessage);
begin
  inherited;
  Repaint;
end;


procedure TGEMColorButton.SetPicture(Value: TPicture);
begin
  if FPicture <> Value then
  begin
    FPicture.Assign(Value);
    Repaint;
  end;
end;


procedure TGEMColorButton.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then begin
    FHoverColor:= Value;
    Invalidate;
  end;
end;


procedure TGEMColorButton.SetSelectedColor(Value: TColor);
begin
  if FSelectedColor <> Value then begin
    FSelectedColor:= Value;
    Invalidate;
  end;
end;


procedure TGEMColorButton.SetSpacing(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Repaint;
  end;
end;


procedure TGEMColorButton.SetStyle(Value: TButtonStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    Repaint;
  end;
end;


procedure TGEMColorButton.SetRouteColor(Value: TColor);
begin
  if FRouteColor <> Value then begin
    FRouteColor:= Value;
    //Invalidate;
  end;
end;

procedure TGEMColorButton.SetSelected(Value: Boolean);
begin
  fSelected := Value;

  if fSelected then begin
    //ShowMessage('selected True');
    //RouteColor := BackBeforeHoverColor;
    fColor := fSelectedColor;
  end
  else begin
    //ShowMessage('selected False');
    //BackBeforeHoverColor := fRouteColor;
    fColor := fRouteColor;
  end;

  Invalidate;
end;


procedure TGEMColorButton.WndProc(var Message: TMessage);
begin
//  if (Message.Msg = CM_MOUSELEAVE) then
//  begin
//    if selected then
//      fColor := SelectedColor
//    else
//      fColor := BackBeforeHoverColor;
//    invalidate;
//  end;
//
//  if (Message.Msg = CM_MOUSEENTER) then
//  begin
//    BackBeforeHoverColor := fColor;
//    fColor := fHoverColor;
//    invalidate;
//  end;

  inherited;
end; (*WndProc*)

end.