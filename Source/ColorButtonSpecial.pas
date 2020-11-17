unit ColorButtonSpecial;

{
Article:

TColorButton - button with Color properties

http://delphi.about.com/library/weekly/aa061104a.htm

Full source code of the TColorButton Delphi component,
an extension to the standard TButton control,
with font color, background color and mouse over color properties.

}

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.UITypes,

  VCL.Graphics, VCL.Controls, VCL.StdCtrls, VCL.Buttons, VCL.ExtCtrls, Vcl.Dialogs,

  GEMComponentsGlobal;

type

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



  TColorButtonSpecial = class(TButton)
  private
    FBackBeforeHoverColor: TColor;
  private
    FCanvas           : TCanvas;
    IsFocused         : Boolean;

    FBackColor        : TColor;
    FForeColor        : TColor;
    FHoverColor       : TColor;
    fSelectedColor    : TColor;

    fArrayIndex       : integer;
    fAccountID        : integer;
    fRouteID          : integer;
    fName             : string;
    fAddress          : string;
    fRectangle        : TRect;
    FLat, FLng        : extended;

    fClientAddressLoc : TClientAddressLoc;
    fSelected         : Boolean;
    saveColor         : TColor;
//    fVersion          : string;

    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);

    property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;
  protected
    function SetVersion: string;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;

    procedure SetButtonStyle(Value: Boolean); override;
    procedure DrawButton(Rect: TRect; State: UINT);

    procedure SetSelected(Value: Boolean);

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ClientAddressLocAdd1: String read fClientAddressLoc.add1 write fClientAddressLoc.add1;
    property ClientAddressLocAdd2: String read fClientAddressLoc.add2 write fClientAddressLoc.add2;

    property ClientAddressCity: String read fClientAddressLoc.City write fClientAddressLoc.City;
    property ClientAddressState: String read fClientAddressLoc.State write fClientAddressLoc.State;
    property ClientAdressZip: String read fClientAddressLoc.Zip write fClientAddressLoc.Zip;

    //property ClientAddressLocRect1.Rectang1.Top: integer read fClientAddressLoc.Rectang1.Top write fClientAddressLoc.Rectang1.Top;
    //property ClientAddressLocRect1.Rectang1.Left: integer read fClientAddressLoc.Rectang1.Left write fClientAddressLoc.Rectang1.Left;
    //property ClientAddressLocRect1.Rectang1.Bottom: integer read fClientAddressLoc.Rectang1.Bottom write fClientAddressLoc.Rectang1.Top;
    //property ClientAddressLocRect1.Rectang1.Right: integer read fClientAddressLoc.Rectang1.Right write fClientAddressLoc.Rectang1.Right;
    //property ClientAddressLocRect1: integer read fClientAddressLoc.Rectang1 write fClientAddressLoc.Rectang1.Right;

    //property ClientAddressLocRect2: tRect read fClientAddressLoc.Rectang2 write fClientAddressLoc.Rectang2;

    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property ForeColor: TColor read FForeColor write SetForeColor default clBtnText;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clBtnFace;
    property ArrayIndex: integer read fArrayIndex write fArrayIndex;
    property AccountID: integer read fAccountID write fAccountID;
    property RouteID: integer read fRouteID write fRouteID;
    property ClientName: string read fName write fName;
    property ClientAddress: string read fAddress write fAddress;
    property TheRectangle: TRect read fRectangle write fRectangle;
    property Lat: extended read fLat write fLat;
    property Lng: extended read fLng write fLng;
    property Version: string read SetVersion;
    property Selected: Boolean read fSelected write SetSelected default False;
    property SelectedColor: TColor read fSelectedColor write SetSelectedColor;
  end;

//procedure Register;

implementation

  { Register }

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TColorButtonSpecial]);
//end;

  { TColorButtonSpecial }

constructor TColorButtonSpecial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TCanvas.Create;
  BackColor := clBtnFace;
  ForeColor := clBtnText;
  HoverColor := clBtnFace;
  SelectedColor := clBtnFace;
  Selected := False;
end; (*Create*)


destructor TColorButtonSpecial.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end; (*Destroy*)


procedure TColorButtonSpecial.SetSelected(Value: Boolean);
begin
  fSelected := Value;

  if fSelected then begin
    //ShowMessage('selected True');
    saveColor := BackBeforeHoverColor;
    BackColor := fSelectedColor;
  end
  else begin
    //ShowMessage('Not selected True');
    BackBeforeHoverColor := saveColor;
  end;

  Invalidate;


//  fSelected := Value;
//
//  if fSelected then begin
//    //ShowMessage('selected True');
//    saveColor := FBackColor;
//    //FForeColor := fSelectedColor;
//    FBackColor := fSelectedColor;
//  end
//  else begin
//    //ShowMessage('Not selected True');
//    //FForeColor := saveColor;
//    FBackColor := saveColor;
//  end;
//
//  Invalidate;
end;


procedure TColorButtonSpecial.WndProc(var Message : TMessage);
begin
  if (Message.Msg = CM_MOUSELEAVE) then
  begin
    if selected then
      BackColor := SelectedColor
    else
      BackColor := BackBeforeHoverColor;
    //BackColor := BackBeforeHoverColor;
    invalidate;
  end;

  if (Message.Msg = CM_MOUSEENTER) then
  begin
    BackBeforeHoverColor := BackColor;
    BackColor := HoverColor;

    // := BackColor;
    //BackColor := HoverColor;
    invalidate;
  end;

  inherited;
end; (*WndProc*)


procedure TColorButtonSpecial.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end; (*CreateParams*)


procedure TColorButtonSpecial.SetButtonStyle(Value: Boolean);
begin
  if Value <> IsFocused then
  begin
    IsFocused := Value;
    Invalidate;
  end;
end; (*SetButtonStyle*)


procedure TColorButtonSpecial.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth  := Width;
    itemHeight := Height;
  end;
end; (*CNMeasureItem*)


procedure TColorButtonSpecial.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    SaveIndex := SaveDC(hDC);
    FCanvas.Lock;
    try
      FCanvas.Handle := hDC;
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      DrawButton(rcItem, itemState);
    finally
      FCanvas.Handle := 0;
      FCanvas.Unlock;
      RestoreDC(hDC, SaveIndex);
    end;
  end;
  Message.Result := 1;
end; (*CNDrawItem*)


procedure TColorButtonSpecial.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end; (*CMEnabledChanged*)


procedure TColorButtonSpecial.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end; (*CMFontChanged*)


procedure TColorButtonSpecial.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then begin
    FBackColor:= Value;
    Invalidate;
  end;
end; (*SetButtonColor*)


procedure TColorButtonSpecial.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then begin
    FSelectedColor:= Value;
    Invalidate;
  end;
end;


procedure TColorButtonSpecial.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then begin
    FForeColor:= Value;
    Invalidate;
  end;
end; (*SetForeColor*)


procedure TColorButtonSpecial.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then begin
    FHoverColor:= Value;
    Invalidate;
  end;
end;


function TColorButtonSpecial.SetVersion: string;
begin
  Result := VersionColorButtonSpecial;
end;

(*SetHoverColor*)


procedure TColorButtonSpecial.DrawButton(Rect: TRect; State: UINT);
var
  Flags, OldMode: Longint;
  IsDown, IsDefault, IsDisabled: Boolean;
  OldColor: TColor;
  OrgRect: TRect;
begin
  OrgRect := Rect;
  Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  IsDown := State and ODS_SELECTED <> 0;
  IsDefault := State and ODS_FOCUS <> 0;
  IsDisabled := State and ODS_DISABLED <> 0;

  if IsDown then
    Flags := Flags or DFCS_PUSHED;
  if IsDisabled then
    Flags := Flags or DFCS_INACTIVE;

  if IsFocused or IsDefault then
  begin
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Style := bsClear;
    FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    InflateRect(Rect, - 1, - 1);
  end;

  if IsDown then
  begin
    FCanvas.Pen.Color := clBtnShadow;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Color := clBtnFace;
    FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    InflateRect(Rect, - 1, - 1);
  end
  else
    DrawFrameControl(FCanvas.Handle, Rect, DFC_BUTTON, Flags);

  if IsDown then
    OffsetRect(Rect, 1, 1);

  OldColor := FCanvas.Brush.Color;
  FCanvas.Brush.Color := BackColor;
  FCanvas.FillRect(Rect);
  FCanvas.Brush.Color := OldColor;
  OldMode := SetBkMode(FCanvas.Handle, TRANSPARENT);
  FCanvas.Font.Color := ForeColor;
  if IsDisabled then
    DrawState(FCanvas.Handle, FCanvas.Brush.Handle, nil, LParam(Caption), 0,
    ((Rect.Right - Rect.Left) - FCanvas.TextWidth(Caption)) div 2,
    ((Rect.Bottom - Rect.Top) - FCanvas.TextHeight(Caption)) div 2,
      0, 0, DST_TEXT or DSS_DISABLED)
//    DrawState(FCanvas.Handle, FCanvas.Brush.Handle, nil, Integer(Caption), 0,
//    ((Rect.Right - Rect.Left) - FCanvas.TextWidth(Caption)) div 2,
//    ((Rect.Bottom - Rect.Top) - FCanvas.TextHeight(Caption)) div 2,
//      0, 0, DST_TEXT or DSS_DISABLED)
  else
    DrawText(FCanvas.Handle, PChar(Caption), - 1, Rect,
                                     DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  SetBkMode(FCanvas.Handle, OldMode);

  if IsFocused and IsDefault then
  begin
    Rect := OrgRect;
    InflateRect(Rect, - 4, - 4);
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, Rect);
  end;
end; (*DrawButton*)

end.

