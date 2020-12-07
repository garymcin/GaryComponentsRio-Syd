unit gemPanelImageBtn;

interface
uses
  Winapi.Windows, Winapi.Messages,

  System.Classes, System.SysUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ActnList,

  CodeSiteLogging,

  JvImage, JvExtComponent, JvCaptionPanel;

type
  TgemCaptionBtnImagePanel = class;

//  TCapPnlEventMouseEnter = procedure(sender: TgemCaptionBtnImagePanel) of object;
//  TCapPnlEventMouseLeave = procedure(sender: TgemCaptionBtnImagePanel) of object;

  TgemImPnlBtnState = (gim_MouseOver, gim_Seletected, gim_Normal);
//  TJvDrawPosition   = (dpLeft, dpTop, dpRight, dpBottom);

//
//  TToggleButton = class(TButton)
//  private
//    FChecked: Boolean;
//    FGroupIndex: Integer;
//    procedure Toggle;
//    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
//    procedure SetGroupIndex(const Value: Integer);
//    procedure TurnSiblingsOff;
//  protected
//    procedure SetButtonStyle(ADefault: Boolean); override;
//    procedure CreateParams(var Params: TCreateParams); override;
//    procedure CreateWnd; override;
//
//    function GetChecked: Boolean; override;
//    procedure SetChecked(Value: Boolean); override;
//  published
//    property Checked;
//    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
//  end;



  TgemCaptionBtnImagePanel = class(TJvCaptionPanel)
//    fImage              : TJvImage;
  private
//    fOnPnlMouseEnter    : TCapPnlEventMouseEnter;
//    fOnPnlMouseLeave    : TCapPnlEventMouseLeave;
    fOnImage_MouseLeave : TNotifyEvent;
    fOnImage_MouseEnter : TNotifyEvent;
    FCaption            : string;
    FCaptionFont        : TFont;
    fCaptionHeight      : integer;
    fCaptionPosition    : TJvDrawPosition;
    fCaptionColor       : TColor;
    FCaptionRect        : TRect;
    FResizable          : Boolean;
    fSelectedColor      : TColor;
    fMouseOverColor     : TColor;
    fBtnImPnlGroupIndex : Integer;
    fgemNoramlColor     : TColor;
    FGroupIndex         : Integer;
    fSelected           : Boolean;
    FClicksDisabled     : Boolean;
    FAutoDrag           : Boolean;
    FFlat               : Boolean;
    FBevel              : Integer;
    FDragging           : Boolean;
    FEndDrag            : TNotifyEvent;
    FOnStartAutoDrag: TJvAutoDragStartEvent;
    FOutlookLook: Boolean;
    FIcon: TIcon;
    FOffset: Integer;
    FMouseDown: Boolean;
    FAnchorPos: TPoint;
    FCaptionOffsetSmall: Integer;
    FCaptionOffsetLarge: Integer;

    procedure DrawRotatedText(Rotation: Integer);
    procedure Toggle;
//    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetGroupIndex(const Value: Integer);
    procedure TurnSiblingsOff;

//    procedure CMPanelMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
//    procedure CMPanelMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;

//    procedure Click_ImageMouseLeaveHandler(Sender: TObject); { TNotifyEvent }
//    procedure Click_ImageMouseEnterHandler(Sender: TObject); { TNotifyEvent }

    function GetImage_Picture: TPicture;
    procedure SetImage_Picture(const Value: TPicture);
    procedure SetCaption(const Value: string);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCaptionHeight(const Value: Integer);
    procedure SetCaptionPosition(const Value: TJvDrawPosition);
    function GetImage_Proportional: Boolean;
    function GetImage_Stretch: Boolean;
    function GetImage_Visible: Boolean;
    procedure SetImage_Proportional(const Value: Boolean);
    procedure SetImage_Strech(const Value: Boolean);
    procedure SetImage_Visible(const Value: Boolean);
    function GetImage_AutoSize: Boolean;
    function GetImage_Center: Boolean;
    procedure SetImage_AutoSize(const Value: Boolean);
    procedure SetImage_Center(const Value: Boolean);
    procedure SetResizable(const Value: Boolean);
    procedure SetButtons(const Value: TJvCapBtnStyles);
    procedure SetFlat(const Value: Boolean);
  protected
    function GetEffectiveCaptionHeight: Integer;
    procedure DoLeaveDrag; virtual;
    function CanStartDrag: Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetChecked: Boolean; //override;
    procedure SetChecked(Value: Boolean); //override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destory;
  published
    property Image_Picture: TPicture read GetImage_Picture write SetImage_Picture;
    property Image_Proportional: Boolean read GetImage_Proportional write SetImage_Proportional;
    property Image_Stretch: Boolean read GetImage_Stretch write SetImage_Strech;
    property Image_Visible: Boolean read GetImage_Visible write SetImage_Visible;
    property Image_Center: Boolean read GetImage_Center write  SetImage_Center;
    property Image_AutoSize: Boolean read GetImage_AutoSize write  SetImage_AutoSize;

    property OnClick_ImageMouseEnter: TNotifyEvent read fOnImage_MouseEnter write fOnImage_MouseEnter;
    property OnClick_ImageMouseLeave: TNotifyEvent read fOnImage_MouseLeave write fOnImage_MouseLeave;

//    property Align;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Selected: Boolean read fSelected write fSelected default false;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;

    property MouseOverColor: TColor read FMouseOverColor write FMouseOverColor default clGray;
    property SelectedColor : TColor read FSelectedColor write FSelectedColor default clBlack;
    property gemNoramlStateColor: TColor read fgemNoramlColor write fgemNoramlColor default clBtnFace;
    property BtnImPnlGroupIndex: Integer read fBtnImPnlGroupIndex write fBtnImPnlGroupIndex default 0;

    property BorderStyle default bsSingle;
    property Caption: string read FCaption write SetCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clActiveCaption;
    property CaptionPosition: TJvDrawPosition read FCaptionPosition write SetCaptionPosition default dpBottom;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 0;
//    property FlatButtons: Boolean read FFlat write SetFlat default False;
//    property OnStartAutoDrag: TJvAutoDragStartEvent read FOnStartAutoDrag write FOnStartAutoDrag;
//    property OnEndAutoDrag: TNotifyEvent read FEndDrag write FEndDrag;
//    property OutlookLook: Boolean read FOutlookLook write FOutlookLook default False;// SetOutlookLook default false;
//    property Color;
//    property Cursor;
//    property ShowHint;
//    property TabOrder;
//    property TabStop;
//    property Visible;
//    property ParentColor;
//    property ParentFont;
//    property ParentShowHint;
//    property Font;
//    property Hint;
//    property Enabled;
//    property PopupMenu;
//    property Resizable:Boolean read FResizable write SetResizable default True;

//    property OnClick;
//    property OnDblClick;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDrag;
//    property OnEnter;
//    property OnExit;
//    property OnMouseDown;
//    property OnMouseMove;
//    property OnMouseUp;
//    property OnStartDrag;
//    property OnResize;
//    property OnMouseEnter: TCapPnlEventMouseEnter read fOnPnlMouseEnter write fOnPnlMouseEnter;
//    property OnMouseLeave: TCapPnlEventMouseLeave read fOnPnlMouseLeave write fOnPnlMouseLeave;


    property Align;
    property AutoDrag: Boolean read FAutoDrag write FAutoDrag default True;
//    property Buttons: TJvCapBtnStyles read FButtons write SetButtons;
//    property BorderStyle default bsSingle;
//    property Caption: string read FCaption write SetCaption;
//    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clActiveCaption;
//    property CaptionPosition: TJvDrawPosition read FCaptionPosition write SetCaptionPosition default dpLeft;
//    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
//    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 0;
    property Color;
    property Cursor;
    property DragCursor;
    property FullRepaint;
    property Locked;
    property DragMode;
    property Enabled;
//    property FlatButtons: Boolean read FFlat write SetFlat default False;
    property Font;
    property Hint;
//    property Icon: TIcon read FIcon write SetIcon;
//    property OutlookLook: Boolean read FOutlookLook write SetOutlookLook;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable:Boolean read FResizable write SetResizable default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
//    property OnButtonClick: TJvCapBtnEvent read FButtonClick write FButtonClick;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
//    property OnStartAutoDrag: TJvAutoDragStartEvent read FOnStartAutoDrag write FOnStartAutoDrag;
//    property OnEndAutoDrag: TNotifyEvent read FEndDrag write FEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnResize;

  end;


implementation

function DrawText(Canvas: TCanvas; const Text: string; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( 'DrawText' );{$ENDIF}
  {$IFDEF UNICODE}
  Result := Winapi.Windows.DrawText(Canvas.Handle, PChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
  {$ELSE}
  Result := DrawText(Canvas, PAnsiChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
  {$ENDIF UNICODE}
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( 'DrawText' );{$ENDIF}
end;

{ TgemCaptionBtnImagePanel }

function TgemCaptionBtnImagePanel.CanStartDrag: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CanStartDrag' );{$ENDIF}
  Result := Align = alNone;
  if Assigned(FOnStartAutoDrag) then
    FOnStartAutoDrag(Self, Result);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CanStartDrag' );{$ENDIF}
end;

//procedure TgemCaptionBtnImagePanel.Click_ImageMouseEnterHandler(
//  Sender: TObject);
//begin
//  if fSelectedColor <> color then
//    Color := fMouseOverColor;
//{ Place your event-handling code here. }
//end;
//
//procedure TgemCaptionBtnImagePanel.Click_ImageMouseLeaveHandler(
//  Sender: TObject);
//begin
//  if fSelectedColor <> color then
//    Color := fgemNoramlColor;
//{ Place your event-handling code here. }
//end;
//
//procedure TgemCaptionBtnImagePanel.CMPanelMouseEnter(var Msg: TMessage);
//begin
//  if fSelectedColor <> color then
//    Color := fMouseOverColor;
//
//  DoMouseEnter;
//  inherited;
//end;
//
//procedure TgemCaptionBtnImagePanel.CMPanelMouseLeave(var Msg: TMessage);
//begin
//  if fSelectedColor <> color then
//    Color := fgemNoramlColor;
//  DoMouseLeave;
//  inherited;
//end;

//procedure TgemCaptionBtnImagePanel.CNCommand(var Message: TWMCommand);
//begin
//  if Message.NotifyCode = BN_CLICKED then
//    Toggle
//  else
//    inherited;
//end;
//

constructor TgemCaptionBtnImagePanel.Create(AOwner: TComponent);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Create' );{$ENDIF}
  inherited Create(AOwner);
  FCaptionFont := TFont.Create;

  Width := 75;
  Height := 95;
  BevelOuter := bvNone;
  TabOrder := 0;
  CaptionPosition := dpBottom;

//  fImage := TJvImage.Create(Self);
//  fImage.Parent := Self;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Create' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.CreateParams(var Params: TCreateParams);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateParams' );{$ENDIF}
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_PUSHLIKE  or BS_CHECKBOX;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateParams' );{$ENDIF}
end;

destructor TgemCaptionBtnImagePanel.Destory;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Destory' );{$ENDIF}
//  FCaptionFont.Free;
  inherited Destroy;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Destory' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.DoLeaveDrag;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoLeaveDrag' );{$ENDIF}
  if Assigned(FEndDrag) then
    FEndDrag(Self);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoLeaveDrag' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.DoMouseEnter;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoMouseEnter' );{$ENDIF}
//  if Assigned(FOnPnlMouseEnter) then
//    FOnPnlMouseEnter(Self);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoMouseEnter' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.DoMouseLeave;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoMouseLeave' );{$ENDIF}
//  if Assigned(FOnPnlMouseLeave) then
//    FOnPnlMouseLeave(Self);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoMouseLeave' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.DrawRotatedText(Rotation: Integer);
var
  tH: Integer;
  tW: Integer;
  Lf: TLogFont;
  Tf: TFont;
  Flags: Integer;
  R: TRect;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DrawRotatedText' );{$ENDIF}
  if FCaption = '' then
    Exit;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    with Canvas do
    begin
      Tf := TFont.Create;
      try
        Tf.Assign(CaptionFont);
        GetObject(Tf.Handle, SizeOf(Lf), @Lf);
        Lf.lfEscapement := Rotation * 10;
        Lf.lfOrientation := Rotation * 10;
        Lf.lfOutPrecision := OUT_TT_PRECIS;
        Tf.Handle := CreateFontIndirect(Lf);
        Canvas.Font.Assign(Tf);
      finally
        Tf.Free;
      end;
      R := FCaptionRect;
      tH := ((R.Bottom - R.Top) - Canvas.TextHeight(FCaption)) div 2;
      tW := ((R.Right - R.Left) - Canvas.TextHeight(FCaption)) div 2;
      if FOutlookLook then
      begin
        Dec(tH);
        Dec(tW);
      end;
      case FCaptionPosition of
        dpLeft:
          begin
            if not FIcon.Empty then
              Dec(R.Bottom, FIcon.Height + 2);
            R := Rect(R.Left, R.Bottom, R.Right, R.Top);
            OffsetRect(R, tW, -FOffset);
          end;
        dpTop, dpBottom:
          begin
            OffsetRect(R, FOffset, tH);
            if not FIcon.Empty then
              Inc(R.Left, FIcon.Width + 2);
          end;
        dpRight:
          begin
            if not FIcon.Empty then
              Inc(R.Top, FIcon.Height + 2);
            R := Rect(R.Right, R.Top, R.Left, R.Bottom);
            OffsetRect(R, -tW, FOffset);
          end;
      end;
      Flags := DT_NOPREFIX;
      if FCaptionPosition in [dpTop, dpBottom] then
        Flags := Flags or DT_VCENTER;
      if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
        Flags := Flags or DT_NOCLIP; { bug or feature? }
      DrawText(Canvas, Caption, -1, R, Flags);
    end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DrawRotatedText' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.CreateWindowHandle(const Params: TCreateParams);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateWindowHandle' );{$ENDIF}
  inherited  CreateWindowHandle(Params);
//  with fImage do begin
//    Left := 1;
//    Top := 1;
//    Width := 169;
//    Height := 160;
//    Align := alClient;
//
//    OnMouseEnter := OnClick_ImageMouseEnter;
//    OnMouseLeave := OnClick_ImageMouseLeave;
//  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateWindowHandle' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.CreateWnd;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateWnd' );{$ENDIF}
  inherited CreateWnd;
  SendMessage(Handle, BM_SETCHECK, Integer(fSelected), 0);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateWnd' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetImage_Picture(const Value: TPicture);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Picture' );{$ENDIF}
//  fImage.Picture.Assign(Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Picture' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetImage_AutoSize(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_AutoSize' );{$ENDIF}
//  fImage.AutoSize := Value;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_AutoSize' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetImage_Center(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Center' );{$ENDIF}
//  fImage.Center := Value;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Center' );{$ENDIF}
end;


function TgemCaptionBtnImagePanel.GetChecked: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetChecked' );{$ENDIF}
  Result := fSelected;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetChecked' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetEffectiveCaptionHeight: Integer;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetEffectiveCaptionHeight' );{$ENDIF}
  if FCaptionHeight = 0 then
    Result := GetSystemMetrics(SM_CYCAPTION)
  else
    Result := FCaptionHeight;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetEffectiveCaptionHeight' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetImage_AutoSize: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_AutoSize' );{$ENDIF}
//  result := fImage.AutoSize;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_AutoSize' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetImage_Center: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Center' );{$ENDIF}
//  result := fImage.Center;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Center' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetImage_Picture: TPicture;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Picture' );{$ENDIF}
//  result := fImage.Picture;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Picture' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetImage_Proportional: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Proportional' );{$ENDIF}
//  Result := fImage.Proportional;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Proportional' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetImage_Stretch: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Stretch' );{$ENDIF}
//  result := fImage.Stretch;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Stretch' );{$ENDIF}
end;

function TgemCaptionBtnImagePanel.GetImage_Visible: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Visible' );{$ENDIF}
//  Result := fImage.Visible;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Visible' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'MouseDown' );{$ENDIF}
  inherited MouseDown(Button, Shift, X, Y);

  FMouseDown := True;
  if not PtInRect(FCaptionRect, Point(X, Y)) then
    Exit;

  if FAutoDrag and CanStartDrag then
  begin
    SetZOrder(True);
    FDragging := True;
    ReleaseCapture;
    {.$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
    SetCapture(Handle);
    FAnchorPos := Point(X, Y);
    {.$ELSE}
//    Perform(Winapi.Messages.WM_SYSCOMMAND, Winapi.Messages.SC_DRAGMOVE, 0);
    {.$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'MouseDown' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'MouseMove' );{$ENDIF}
  inherited MouseMove(Shift, X, Y);
  {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
  if FDragging then
    SetBounds(Left + X - FAnchorPos.X, Top + Y - FAnchorPos.Y, Width, Height);
  {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'MouseMove' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'MouseUp' );{$ENDIF}
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
    ReleaseCapture;
    {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
    DoLeaveDrag;
  end;
  FDragging := False;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'MouseUp' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.Paint;
var
  Rotation: Integer;
  R: TRect;
  FlatOffset: Integer;
  AdjustedCaptionHeight: Integer;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Paint' );{$ENDIF}
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  Canvas.Brush.Color := FCaptionColor;
  FBevel := FCaptionOffsetSmall;
  Rotation := 0;

  FlatOffset := Ord(FlatButtons);

  AdjustedCaptionHeight := GetEffectiveCaptionHeight;
  if FOutlookLook then
  begin
    if CaptionPosition = dpLeft then
      AdjustedCaptionHeight := AdjustedCaptionHeight - 3 + FlatOffset
    else
    if CaptionPosition = dpRight then
      AdjustedCaptionHeight := AdjustedCaptionHeight - 4 + FlatOffset
    else
      AdjustedCaptionHeight := AdjustedCaptionHeight - 5 + FlatOffset
  end;

  case FCaptionPosition of
    dpLeft:
      begin
        FCaptionRect := Rect(FBevel, FBevel, AdjustedCaptionHeight + FBevel, ClientHeight - FBevel);
        Rotation := 90;
      end;
    dpTop:
      FCaptionRect := Rect(FBevel, FBevel, ClientWidth - FBevel, AdjustedCaptionHeight + FBevel);
    dpRight:
      begin
        FCaptionRect := Rect(ClientWidth - AdjustedCaptionHeight - FBevel, FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
        Rotation := -90;
      end;
    dpBottom:
      FCaptionRect := Rect(FBevel, ClientHeight - AdjustedCaptionHeight - FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
  end; //case
  Canvas.FillRect(FCaptionRect);
  if not FIcon.Empty then
  begin
    case FCaptionPosition of
      dpRight:
        Canvas.Draw((FCaptionRect.Left + FCaptionRect.Right - FIcon.Width) div 2, FCaptionRect.Top + 1, FIcon);
      dpLeft:
        Canvas.Draw((FCaptionRect.Left + FCaptionRect.Right - FIcon.Width) div 2, FCaptionRect.Bottom - 1 - FIcon.Height, FIcon);
      dpBottom, dpTop:
        Canvas.Draw(FCaptionRect.Left + 1, (FCaptionRect.Top + FCaptionRect.Bottom - FIcon.Height) div 2 , FIcon);
    end;
  end;
  DrawRotatedText(Rotation);
//  DrawButtons;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Paint' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.Resize;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Resize' );{$ENDIF}
  inherited Resize;
  Repaint;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Resize' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetImage_Proportional(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Proportional' );{$ENDIF}
//  fImage.Proportional := (Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Proportional' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetImage_Strech(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Strech' );{$ENDIF}
//  fImage.Stretch := (Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Strech' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetImage_Visible(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Visible' );{$ENDIF}
//  fImage.Visible := (Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Visible' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetResizable(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetResizable' );{$ENDIF}
  if FResizable <> Value then
  begin
    FResizable := Value;
    RecreateWnd;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetResizable' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.Toggle;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Toggle' );{$ENDIF}
  Selected := not fSelected;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Toggle' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.TurnSiblingsOff;
var
  I: Integer;
  Sibling: TControl;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'TurnSiblingsOff' );{$ENDIF}
  if (Parent <> nil) and (GroupIndex <> 0) then
    with Parent do
      for I := 0 to ControlCount - 1 do
      begin
        Sibling := Controls[I];
        if (Sibling <> Self) and (Sibling is TgemCaptionBtnImagePanel) then
          with TgemCaptionBtnImagePanel(Sibling) do
            if GroupIndex = Self.GroupIndex then
            begin
              if Assigned(Action) and
                 (Action is TCustomAction) and
                 TCustomAction(Action).AutoCheck then
                TCustomAction(Action).Checked := False;
              SetChecked(False);
            end;
      end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'TurnSiblingsOff' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetButtons(const Value: TJvCapBtnStyles);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetButtons' );{$ENDIF}
//  FButtons := Value;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetButtons' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetCaption(const Value: string);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaption' );{$ENDIF}
  FCaption := Value;
  inherited Caption := '';
  Invalidate;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaption' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetCaptionColor(const Value: TColor);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaptionColor' );{$ENDIF}
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaptionColor' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetCaptionFont(const Value: TFont);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaptionFont' );{$ENDIF}
  FCaptionFont.Assign(Value);
  Invalidate;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaptionFont' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetCaptionHeight(const Value: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaptionHeight' );{$ENDIF}
  if FCaptionHeight <> Value then
  begin
    FCaptionHeight := Value;
    Invalidate;
    ReAlign;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaptionHeight' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetCaptionPosition(
  const Value: TJvDrawPosition);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaptionPosition' );{$ENDIF}
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    RecreateWnd;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaptionPosition' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetChecked(Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetChecked' );{$ENDIF}
  if fSelected <> Value then
  begin
    fSelected := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(Selected), 0);
    if Value then
      TurnSiblingsOff;
    if not ClicksDisabled then Click;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetChecked' );{$ENDIF}
end;


procedure TgemCaptionBtnImagePanel.SetFlat(const Value: Boolean);
var
  I: TJvCapBtnStyle;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetFlat' );{$ENDIF}
  if FFlat <> Value then
  begin
//    FFlat := Value;
//    for I := Low(FButtonArray) to High(FButtonArray) do
//      FButtonArray[I].Flat := FFlat;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetFlat' );{$ENDIF}
end;

procedure TgemCaptionBtnImagePanel.SetGroupIndex(const Value: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetGroupIndex' );{$ENDIF}
  FGroupIndex := Value;
  if Selected then
    TurnSiblingsOff;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetGroupIndex' );{$ENDIF}
end;

end.


