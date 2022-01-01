unit GEMCustomBtns;


interface

uses
  Winapi.Windows, WinApi.Messages, Winapi.CommCtrl,

  System.SysUtils, System.Classes, System.UITypes,

  Vcl.Graphics, VCL.Controls, VCL.ExtCtrls, Vcl.Menus, Vcl.ActnList, VCL.Forms, 
  Vcl.Themes, VCL.StdCtrls, Vcl.ImgList,

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

  TAlignment =
    (alTopLeft, alTopCenter, alTopRight,
    alMiddleLeft, alMiddleCenter, alMiddleRight,
    alBottomLeft, alBottomCenter, alBottomRight);

  TButtonBevel = (bbLowered, bbNone, bbRaised);

  TGEMButtonStyles = (bsAutoSize, bsCenter, bsStretch, bsShowFocus, bsShowKey);

  TGEMButtonStyle = set of TGEMButtonStyles;

  TGEMButtonState = (bsUp, bsDown, bsDisabled);


  TGEMButtonControl = class;

  TButtonActionLink = class(TWinControlActionLink)
  protected
    FClient: TGEMButtonControl;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TButtonActionLinkClass = class of TButtonActionLink;

  //===============  TGEMButtonControl = class(TCustomControl)
  TGEMButtonControl = class(TCustomControl) // difference is the TCustomControl VS TWinControl
  private
    FClicksDisabled: Boolean;
    FWordWrap: Boolean;
    function IsCheckedStored: Boolean;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    property Checked: Boolean read GetChecked write SetChecked stored IsCheckedStored default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //===============  TGEMCustomButton = class(TGEMButtonControl)

  TGEMCustomButton = class(TGEMButtonControl)
  public type
    TButtonStyle = (bsPushButton, bsCommandLink, bsSplitButton);
  private
    fPicture              : TPicture;
    fFontColor            : TColor;

    fAlignment            : TAlignment;
    fBevelStyle           : TButtonBevel;
    fBevelSize            : Integer;

    fSpacing              : Integer;
    fBackBeforeHoverColor : TColor;
    FHoverColor           : TColor;
    fSelectedColor        : TColor;
    fColor                : TColor;

  	aFocused              : Boolean;
    aState                : TGEMButtonState;

    fActive               : Boolean;
    fCancel               : Boolean;
    fCommandLinkHint      : string;
    fDefault              : Boolean;
    fDropDownMenu         : TPopupMenu;
    fElevationRequired    : Boolean;
    fModalResult          : TModalResult;
    fStyle                : TButtonStyle;
    fGEMStyle             : TGEMButtonStyle;
    fOnDropDownClick      : TNotifyEvent;

    class constructor Create;
    class destructor Destroy;

    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNCtlColorBtn(var Message: TWMCtlColorBtn); message CN_CTLCOLORBTN;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    procedure SetSpacing(Value: Integer);
    procedure SetBevelSize(const Value: Integer);
    procedure SetSelectedColor(Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetCommandLinkHint(const Value: string);
    procedure SetDefault(Value: Boolean);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetElevationRequired(const Value: Boolean);
    procedure SetElevationRequiredState;
    procedure SetStyle(const Value: TButtonStyle);
    procedure SetGEMStyle(const Value: TGEMButtonStyle);
    procedure UpdateCommandLinkHint;
    procedure SetColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetPicture(const Value: TPicture);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevelStyle(const Value: TButtonBevel);
    procedure SetSelected(const Value: Boolean);
  protected
    property Active: Boolean read FActive;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetButtonStyle(ADefault: Boolean); virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property Picture: TPicture read FPicture write SetPicture;
    procedure Click; override;
    property FontColor : TColor read fFontColor write SetFontColor;
    property ClicksDisabled;

    property BevelSize: Integer read FBevelSize write SetBevelSize default 2;
    function UseRightToLeftAlignment: Boolean; override;
    property Cancel: Boolean read FCancel write FCancel default False;
    property CommandLinkHint: string read FCommandLinkHint write SetCommandLinkHint;
    property Default: Boolean read FDefault write SetDefault default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property ElevationRequired: Boolean read FElevationRequired write SetElevationREquired default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Style: TButtonStyle read fStyle write SetStyle default bsPushButton;
    property GEMStyle: TGEMButtonStyle read fGEMStyle write SetGEMStyle;
    property TabStop default True;
    property SelectedColor: TColor read fSelectedColor write SetSelectedColor;
    property HoverColor: TColor read fHoverColor write SetHoverColor;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Alignment: TAlignment read fAlignment write SetAlignment
      default alMiddleCenter;
    property BevelStyle: TButtonBevel read fBevelStyle write SetBevelStyle
      default bbRaised;

    property OnDropDownClick: TNotifyEvent read FOnDropDownClick write FOnDropDownClick;
  end;


  //===============  TGEMColorButton = class(TGEMCustomButton)


  TGEMColorButton = class(TGEMCustomButton)
  private
  protected
    procedure SetEnabled(Value: Boolean); override; //message CM_ENABLEDCHANGED;
    function GetVersion: string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Version: string read GetVersion;
    property ModalResult;//: TModalResult read FModalResult write FModalResult default 0;

    property Action;
    property Anchors;
    property Align;
    property Caption;
    property Color;
    property Cursor;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property Left;
    property Name;
    property Picture;
    property PopUpMenu;
//    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Style;
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
    property WordWrap;

    property Checked;
    property ClicksDisabled;
//    property Selected;
    property SelectedColor;
    property HoverColor;
  end;

  //===============  TGEMAddredssColorButton = class(TGEMCustomButton)

  TGEMAddredssColorButton = class(TGEMCustomButton)
  private
    fSelected         : Boolean;
//    fSpacing          : Integer;
    fArrayIndex       : Integer;
    fAccountID        : integer;
    fRouteID          : integer;
    fRouteName        : string;
    fName             : string;
    fAddress          : string;
    fTheRectangle     : TRect;
    fLat              : extended;
    fLng              : extended;
    fGeoCodeType      : Integer;
    fClientAddressLoc : TClientAddressLoc;
    fRouteColor: TColor;
    procedure SetRouteColor(Value: TColor);
    procedure SetSelected(Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property TheRectangle: TRect read fTheRectangle write fTheRectangle;
    property Lat: extended read fLat write fLat;
    property Lng: extended read fLng write fLng;
    property GeoCodeType: Integer read fGeoCodeType write fGeoCodeType;
    property Selected: Boolean read fSelected write SetSelected default False;

//    property Version;
    property ModalResult;

    property Action;
    property Anchors;
    property Align;
    property BevelSize;
    property Caption;
    property Color;
    property Cursor;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property Left;
    property Name;
    property Picture;
    property DropDownMenu;
    property Spacing;//: Integer read fSpacing write SetSpacing default 2;
    property Style;
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
    property WordWrap;
    property Checked;
    property ClicksDisabled;
    property SelectedColor;
    property HoverColor;
  end;

implementation

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


{ TGEMButtonControl }

procedure TGEMButtonControl.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
var
  OldClicksDisabled: Boolean;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
      if not CheckDefaults or (Self.Checked = False) then
      begin
        // prevent generating Action.OnExecute when the control gets checked
        OldClicksDisabled := ClicksDisabled;
        ClicksDisabled := True;

        Self.Checked := Checked;

        ClicksDisabled := OldClicksDisabled;
      end;
end;

procedure TGEMButtonControl.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  with StyleServices do
    if ThemeControl(Self) then
    begin
      if (Parent <> nil) and Parent.DoubleBuffered then
        PerformEraseBackground(Self, Message.ChildDC)
      else
        DrawParentBackground(Handle, Message.ChildDC, nil, False);
      { Return an empty brush to prevent Windows from overpainting we just have created. }
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
end;

constructor TGEMButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if SysLocale.FarEast and (Win32Platform = VER_PLATFORM_WIN32_NT) then
    ImeMode := imDisable;
  TipMode := tipClose;
end;

procedure TGEMButtonControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FWordWrap then
    Params.Style := Params.Style or BS_MULTILINE;
end;

function TGEMButtonControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TButtonActionLink;
end;

function TGEMButtonControl.GetChecked: Boolean;
begin
  Result := False;
end;

function TGEMButtonControl.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not TButtonActionLink(ActionLink).IsCheckedLinked;
end;

procedure TGEMButtonControl.SetChecked(Value: Boolean);
begin

end;

procedure TGEMButtonControl.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

procedure TGEMButtonControl.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  { Under theme services the background is drawn in CN_CTLCOLORSTATIC. }
  if StyleServices(Self).Enabled then
    Message.Result := 1
  else
    inherited;
end;

procedure TGEMButtonControl.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if not (csDesigning in ComponentState) and not Focused then
      begin
        FClicksDisabled := True;
        Winapi.Windows.SetFocus(Handle);
        FClicksDisabled := False;
        if not Focused then Exit;
      end;
    CN_COMMAND:
      if FClicksDisabled then Exit;
  end;
  inherited WndProc(Message);
end;

{ TGEMCustomButton }

procedure TGEMCustomButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  inherited Click;
end;

procedure TGEMCustomButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TGEMCustomButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if  (((CharCode = VK_RETURN) and FActive) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TGEMCustomButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Click;
end;

procedure TGEMCustomButton.CNCtlColorBtn(var Message: TWMCtlColorBtn);
begin
  with StyleServices do
    if Enabled then
    begin
      if (Parent <> nil) and Parent.DoubleBuffered then
        PerformEraseBackground(Self, Message.ChildDC)
      else
        DrawParentBackground(Handle, Message.ChildDC, nil, False);
      { Return an empty brush to prevent Windows from overpainting we just have created. }
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
end;

procedure TGEMCustomButton.CNNotify(var Message: TWMNotify);
var
  Pt: TPoint;
  LRect: TRect;
begin
  if Message.NMHdr.code = BCN_DROPDOWN then
  begin
    if Assigned(FOnDropDownClick) then
      FOnDropDownClick(Self);
    if Assigned(FDropDownMenu) then
    begin
{$IF DEFINED(CLR)}
      LRect := TWMNotifyButton.Create(Message.OriginalMessage).BCDropDown.rcButton;
{$ELSE}
      LRect := PNMBCDropDown(Message.NMHdr).rcButton;
{$ENDIF}
      Pt := ClientToScreen(Point(LRect.Left, LRect.Bottom));
      FDropDownMenu.Popup(Pt.X, Pt.Y);
    end;
  end;
end;

class constructor TGEMCustomButton.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TGEMCustomButton, TButtonStyleHook);
end;

constructor TGEMCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment  := alMiddleCenter;
  FBevelStyle := bbRaised;
  FBevelSize  := 2;
  FColor      := clBtnFace;
  FPicture    := TPicture.Create;
  FSpacing    := 2;
  FGEMStyle   := [bsCenter, bsShowFocus, bsShowKey];
  aFocused    := False;
  aState      := bsUp;
  Width       := 75; Height := 25;
  Enabled     := True;
  TabStop     := True;

  ControlStyle := [csSetCaption, csDoubleClicks];
  Width := 75;
  Height := 25;
  TabStop := True;
//  FImageChangeLink := TChangeLink.Create;
//  FImageChangeLink.OnChange := ImageListChange;
//  FImageMargins := TImageMargins.Create;
//  FImageMargins.OnChange := ImageMarginsChange;
//  FInternalImageList := nil;
//  FCommandLinkHint := '';
//  FDisabledImageIndex := -1;
  FElevationRequired := False;
//  FHotImageIndex := -1;
//  FImageAlignment := iaLeft;
//  FImageIndex := -1;
//  FPressedImageIndex := -1;
//  FSelectedImageIndex := -1;
  FStyle := bsPushButton;
//  FStylusHotImageIndex := -1;
end;

procedure TGEMCustomButton.CreateWnd;
begin
  inherited CreateWnd;
  FActive := FDefault;
  if not (csLoading in ComponentState) then
  begin
    SetElevationRequiredState;
//    UpdateImageList;
    if FStyle = bsCommandLink then
      UpdateCommandLinkHint;
  end;
end;

destructor TGEMCustomButton.Destroy;
begin
//  FreeAndNil(FImageChangeLink);
//  if FInternalImageList <> nil then
//    FreeAndNil(FInternalImageList);
//  FreeAndNil(FImageMargins);
  inherited;
end;

class destructor TGEMCustomButton.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TCustomButton, TButtonStyleHook);
end;

procedure TGEMCustomButton.DoEnter;
begin
  aFocused := True;
  Repaint;
  inherited DoEnter;
end;

procedure TGEMCustomButton.DoExit;
begin
  aFocused := False;
  Repaint;
  inherited DoExit;
end;


//procedure TGEMCustomButton.KeyAccel(var Message: TCMDialogChar);
//begin
//
//end;

procedure TGEMCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_SPACE then
    if Enabled then
    begin
      aState := bsDown;
      Repaint;
    end;
end;

procedure TGEMCustomButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = VK_SPACE then
    if Enabled then
    begin
      aState := bsUp;
      Click; Repaint;
    end;
   if Key = VK_RETURN then
     if not (aState = bsDisabled) then Click;
end;

procedure TGEMCustomButton.Loaded;
begin
  inherited;
  SetElevationRequiredState;
  //UpdateImageList;
  if FStyle = bsCommandLink then
    UpdateCommandLinkHint;
end;

procedure TGEMCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Enabled then
  begin
    aState := bsDown;
    Repaint;
  end;
end;

procedure TGEMCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Enabled then
  begin
    aState := bsUp;
    Repaint;
  end;
end;

procedure TGEMCustomButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FDropDownMenu then
      FDropDownMenu := nil;
//    if AComponent = FImages then
//    begin
//      FImages := nil;
//      UpdateImageList;
//    end;
//    if AComponent = FDisabledImages then
//    begin
//      FDisabledImages := nil;
//      UpdateImageList;
//    end;
  end;
end;

procedure TGEMCustomButton.Paint;

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
     case fAlignment of
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
      if ((keyPos > 0) and (bsShowKey in fGEMStyle)) then
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
    aState := bsDisabled
  else
    if aState = bsDisabled then
      aState := bsUp;
//  if ((not (FPicture.Graphic = nil)) and (bsAutoSize in FGEMStyle)) then
  if ((not (FPicture.Graphic = nil)) and (bsAutoSize in FGEMStyle)) then
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
     if ((aState = bsDown) xor (FBevelStyle = bbLowered)) then
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
   if (aFocused and (bsShowFocus in fGEMStyle)) and Enabled then
    DrawFocusRect(Rect(
      Client.Left + FSpacing - 1, Client.Top + FSpacing - 1,
       Client.Right - FSpacing + 1, Client.Bottom - FSpacing + 1
       ));
   // Draw the picture
   if (fPicture <> nil) then
   begin
     if (bsStretch in fGEMStyle) then
       Picture := Rect(
         FBevelSize + FSpacing,
         FBevelSize + FSpacing, Width - (FBevelSize + FSpacing),
         Height - (FBevelSize + FSpacing))
     else if (bsCenter in fGEMStyle) then
       Picture := Bounds(
         (Width - fPicture.Width) div 2, (Height - fPicture.Height) div 2,
         fPicture.Width, fPicture.Height
         )
     else
       case FAlignment of
        alTopLeft, alTopCenter, alTopRight:
          Picture := Bounds(
            (Width - fPicture.Width) div 2,
            ((Height - (FBevelSize + FSpacing)) - fPicture.Height),
            fPicture.Width, fPicture.Height);
        alMiddleLeft:
           Picture := Bounds(
            ((Width - (FBevelSize + FSpacing)) - fPicture.Width),
             (Height - fPicture.Height) div 2,
             fPicture.Width, fPicture.Height);
        alMiddleCenter:
          Picture := Bounds(
            (Width - fPicture.Width) div 2,
           (Height - fPicture.Height) div 2,
           fPicture.Width, fPicture.Height);
        alMiddleRight:
           Picture := Bounds(
            (FBevelSize + FSpacing),
             (Height - fPicture.Height) div 2,
            fPicture.Width, fPicture.Height);
        alBottomLeft, alBottomCenter, alBottomRight:
          Picture := Bounds(
            (Width - fPicture.Width) div 2,
             (FBevelSize + FSpacing),
            fPicture.Width, fPicture.Height);
       end;
       StretchDraw(Picture, fPicture.Graphic);
     end
    else begin
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


procedure TGEMCustomButton.SetAlignment(const Value: TAlignment);
begin
  fAlignment := Value;
  Repaint;
end;

procedure TGEMCustomButton.SetBevelSize(const Value: Integer);
begin
  if Value < 1 then 
    fBevelSize := 1
  else
    fBevelSize := Value;
  Repaint;
end;

procedure TGEMCustomButton.SetBevelStyle(const Value: TButtonBevel);
begin
  fBevelStyle := Value;
  Repaint;
end;

procedure TGEMCustomButton.SetButtonStyle(ADefault: Boolean);
const
  BS_MASK = $000F;
  NormalStyles: array[Boolean] of Integer = (BS_PUSHBUTTON, BS_DEFPUSHBUTTON);
  CommandLinkStyles: array[Boolean] of Integer = (BS_COMMANDLINK, BS_DEFCOMMANDLINK);
  SplitButtonStyles: array[Boolean] of Integer = (BS_SPLITBUTTON, BS_DEFSPLITBUTTON);
var
  LStyle: Integer;
begin
  if HandleAllocated then
  begin
    if Win32MajorVersion >= 6 then
      case FStyle of
        bsCommandLink: LStyle := CommandLinkStyles[ADefault];
        bsSplitButton: LStyle := SplitButtonStyles[ADefault];
      else
        LStyle := NormalStyles[ADefault];
      end
    else
      LStyle := NormalStyles[ADefault];
    if (GetWindowLong(Handle, GWL_STYLE) and BS_MASK) <> LStyle then
      SendMessage(Handle, BM_SETSTYLE, LStyle, 1);
  end;
end;

//procedure TGEMCustomButton.SetCaption(var Message: TMessage);
//begin
//  Repaint;
//end;


procedure TGEMCustomButton.SetColor(const Value: TColor);
begin
  fColor := Value;
  Repaint;
end;

procedure TGEMCustomButton.SetCommandLinkHint(const Value: string);
begin
  if Value <> fCommandLinkHint then
  begin
    fCommandLinkHint := Value;
    if HandleAllocated and (FStyle = bsCommandLink) then
      UpdateCommandLinkHint;
  end;
end;

procedure TGEMCustomButton.SetDefault(Value: Boolean);
var
  Form: TCustomForm;
begin
  FDefault := Value;
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
{$IF DEFINED(CLR)}
      (Form as IWinControl).FocusChanged(Form.ActiveControl);
{$ELSE}
      Form.Perform(CM_FOCUSCHANGED, 0, LPARAM(Form.ActiveControl));
{$ENDIF}
  end;
end;


procedure TGEMCustomButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  if Value <> FDropDownMenu then
  begin
    FDropDownMenu := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TGEMCustomButton.SetElevationRequired(const Value: Boolean);
begin
  if Value <> FElevationRequired then
  begin
    FElevationRequired := Value;
    if HandleAllocated then
    begin
      SetElevationRequiredState;
//      if not FElevationRequired then begin
//        UpdateImageList; // rebuild imagelist for button
//      end;  
    end;
  end;
end;

procedure TGEMCustomButton.SetElevationRequiredState;
begin
  if CheckWin32Version(6, 0) and HandleAllocated then
    Button_SetElevationRequiredState(Handle, FElevationRequired);
end;

//procedure TGEMCustomButton.SetFocusOff(var Message: TMessage);
//begin
//
//end;
//
//procedure TGEMCustomButton.SetFocusOn(var Message: TMessage);
//begin
//
//end;
//
//procedure TGEMCustomButton.SetFont(var Message: TMessage);
//begin
//
//end;

procedure TGEMCustomButton.SetFontColor(const Value: TColor);
begin
  fFontColor := Value;
  Self.Font.Color := Value;
  Invalidate;
end;


procedure TGEMCustomButton.SetGEMStyle(const Value: TGEMButtonStyle);
begin
  if fGEMStyle <> Value then begin
    fGEMStyle := Value;
    Repaint;
  end;
end;

procedure TGEMCustomButton.SetHoverColor(const Value: TColor);
begin
  if fHoverColor <> Value then begin
    fHoverColor:= Value;
    Invalidate;
  end;
end;


procedure TGEMCustomButton.SetPicture(const Value: TPicture);
begin
  if fPicture <> Value then
  begin
    fPicture.Assign(Value);
    Repaint;
  end;
end;


procedure TGEMCustomButton.SetSelected(const Value: Boolean);
begin

end;

procedure TGEMCustomButton.SetSelectedColor(Value: TColor);
begin
  if fSelectedColor <> Value then begin
    fSelectedColor:= Value;
    Invalidate;
  end;
end;


procedure TGEMCustomButton.SetSpacing(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Repaint;
  end;
end;


procedure TGEMCustomButton.SetStyle(const Value: TButtonStyle);
const
  DefCmdLinkWidth = 175;
  DefCmdLinkHeights: array[Boolean] of Integer = (57, 41);
var
  LAligning: Boolean;
begin
  if Value <> FStyle then
  begin
    if (csLoading in ComponentState) then
      FStyle := Value
    else
      case Value of
        bsPushButton,
        bsSplitButton:
          begin
            if FStyle = bsCommandLink then
              SetBounds(Left, Top, ExplicitWidth, ExplicitHeight);
            FStyle := Value;
            RecreateWnd;
          end;
        bsCommandLink:
          begin
            LAligning := csAligning in ControlState;
            if not LAligning then
              ControlState := ControlState + [csAligning];
            try
              if Height < DefCmdLinkHeights[FCommandLinkHint = ''] then
                Height := DefCmdLinkHeights[FCommandLinkHint = ''];
              if Width < DefCmdLinkWidth then
                Width := DefCmdLinkWidth;
              FStyle := Value;
              RecreateWnd;
            finally
              if not LAligning then
                ControlState := ControlState - [csAligning];
            end;
          end;
      end;
  end;
end;


procedure TGEMCustomButton.UpdateCommandLinkHint;
begin
  if Win32MajorVersion >= 6 then
    Button_SetNote(Handle, FCommandLinkHint);
end;


function TGEMCustomButton.UseRightToLeftAlignment: Boolean;
begin
  result := False;
end;

procedure TGEMCustomButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if StyleServices(Self).Enabled then
    Message.Result := 1
  else
    DefaultHandler(Message);
end;

{ TGEMColorButton }

constructor TGEMColorButton.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TGEMColorButton.Destroy;
begin

  inherited;
end;

function TGEMColorButton.GetVersion: string;
begin

end;

procedure TGEMColorButton.SetEnabled(Value: Boolean);
begin
  inherited;

end;

{ TGEMAddredssColorButton }

constructor TGEMAddredssColorButton.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TGEMAddredssColorButton.Destroy;
begin

  inherited;
end;

procedure TGEMAddredssColorButton.SetRouteColor(Value: TColor);
begin

end;


procedure TGEMAddredssColorButton.SetSelected(Value: Boolean);
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


{ TButtonActionLink }

procedure TButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TGEMButtonControl;
end;

function TButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = TCustomAction(Action).Checked);
end;

procedure TButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
  begin
    FClient.ClicksDisabled := True;
    try
      FClient.Checked := Value;
    finally
      FClient.ClicksDisabled := False;
    end;
  end;
end;

end.
