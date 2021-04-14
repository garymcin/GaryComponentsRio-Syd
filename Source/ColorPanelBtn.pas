unit ColorPanelBtn;

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



  TColorPanelSpecial = class(TPanel)
  private
 //   FBackBeforeHoverColor: TColor;
  private
//    FCanvas           : TCanvas;
//    IsFocused         : Boolean;

    fSelected         : Boolean;

    fSelectedColor    : TColor;
    fHoverColor       : TColor;
    //DisplayColor      : TColor;
    fRouteColor        : TColor;

    fClientAddressLoc : TClientAddressLoc;

    fArrayIndex       : integer;
    fAccountID        : integer;
    fRouteID          : integer;
    fName             : string;
    fAddress          : string;
    fRectangle        : TRect;
    FLat, FLng        : extended;
//    fVersion          : string;
    procedure SetHoverColor(const Value: TColor);
    procedure SetRouteColor(const Value: TColor);

//    property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;
  protected
    function SetVersion: string;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;

    //procedure SetButtonStyle(Value: Boolean); override;
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
    property RouteColor: TColor read fRouteColor write SetRouteColor;
  end;


//procedure Register;

implementation

  { Register }

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TColorPanelSpecial]);
//end;


{ TColorPanelSpecial }

constructor TColorPanelSpecial.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TColorPanelSpecial.CreateParams(var Params: TCreateParams);
begin
  inherited;

end;

destructor TColorPanelSpecial.Destroy;
begin

  inherited;
end;


procedure TColorPanelSpecial.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TColorPanelSpecial.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TColorPanelSpecial.CNDrawItem(var Message: TWMDrawItem);
begin

end;

procedure TColorPanelSpecial.CNMeasureItem(var Message: TWMMeasureItem);
begin

end;

procedure TColorPanelSpecial.DrawButton(Rect: TRect; State: UINT);
begin

end;

//procedure TColorPanelSpecial.SetButtonStyle(Value: Boolean);
//begin
//  inherited;
//
//end;

procedure TColorPanelSpecial.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then begin
    FHoverColor:= Value;
    Invalidate;
  end;
end;

procedure TColorPanelSpecial.SetSelected(Value: Boolean);
begin
  fSelected := Value;

  if fSelected then begin
    //RouteColor := Color;
    Color := fSelectedColor;
  end
  else begin
    Color := RouteColor;
  end;

  Invalidate;
end;


procedure TColorPanelSpecial.SetRouteColor(const Value: TColor);
begin
  fRouteColor := Value;
end;


function TColorPanelSpecial.SetVersion: string;
begin
//  Result := VersionColorPanelBtnSpecial;
end;


procedure TColorPanelSpecial.WndProc(var Message: TMessage);
begin
  inherited;

end;

end.
