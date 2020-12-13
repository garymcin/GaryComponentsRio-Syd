unit gemCapPanelBtn;

interface
uses
  Winapi.Windows, Winapi.Messages,

  System.Classes, System.SysUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ActnList,  Vcl.Dialogs, Vcl.Buttons,

  CodeSiteLogging,

  JvImage, JvExtComponent, JvCaptionPanel;

type
  TgemCapPanelBtn = class;

  TCapPnlEventMouseEnter = procedure(sender: TgemCapPanelBtn) of object;
  TCapPnlEventMouseLeave = procedure(sender: TgemCapPanelBtn) of object;

  TgemImPnlBtnState = (gim_MouseOver, gim_Seletected, gim_Normal);
  TButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);
  TJvDrawPosition   = (dpLeft, dpTop, dpRight, dpBottom);
  TJvAutoDragStartEvent = procedure(Sender: TObject; var AllowDrag: Boolean) of object;

//  {$IFDEF RTL230_UP}
//  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
//  {$ENDIF RTL230_UP}


  TgemCapPanelBtn = class(TJvCustomPanel)
    fImage              : TJvImage;
  private
    fOnImage_MouseLeave : TNotifyEvent;
    fOnImage_MouseEnter : TNotifyEvent;
    fOnPnlMouseEnter    : TCapPnlEventMouseEnter;
    fOnPnlMouseLeave    : TCapPnlEventMouseLeave;
    FCaption            : string;
    FCaptionFont        : TFont;
    fCaptionHeight      : integer;
    fCaptionPosition    : TJvDrawPosition;
    FCaptionRect        : TRect;
    FResizable          : Boolean;
    fCaptionColor       : TColor;
    fButtonDownColor    : TColor;
    fMouseOverColor     : TColor;
    fButtonsUpColor     : TColor;
//    fForceUpColor       : TColor;

    FGroupIndex         : Integer;
    FDown               : Boolean;
    FClicksDisabled     : Boolean;
    FAutoDrag           : Boolean;
    FFlat               : Boolean;
    FBevel              : Integer;
    FDragging           : Boolean;
    FEndDrag            : TNotifyEvent;
    FOnStartAutoDrag    : TJvAutoDragStartEvent;
    FOutlookLook        : Boolean;
    FOffset             : Integer;
    FMouseDown          : Boolean;
    FAnchorPos          : TPoint;
    FCaptionOffsetSmall : Integer;
    FCaptionOffsetLarge : Integer;
    FAllowAllUp         : Boolean;
    FMouseInControl     : Boolean;

    procedure SetGroupIndex(const Value: Integer);

    procedure CMPanelMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMPanelMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure OnImageMouseLeaveHandler(Sender: TObject); { TNotifyEvent }
    procedure OnImageMouseEnterHandler(Sender: TObject); { TNotifyEvent }

    function GetImage_Picture: TPicture;
    procedure SetImage_Picture(const Value: TPicture);
    function GetImage_Proportional: Boolean;
    function GetImage_Stretch: Boolean;
    function GetImage_Visible: Boolean;
    procedure SetImage_Proportional(const Value: Boolean);
    procedure SetImage_Strech(const Value: Boolean);
    procedure SetImage_Visible(const Value: Boolean);
    function GetImage_Center: Boolean;
    procedure SetImage_AutoSize(const Value: Boolean);
    procedure SetImage_Center(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCaptionHeight(const Value: Integer);
    procedure SetCaptionPosition(const Value: TJvDrawPosition);
    procedure SetResizable(const Value: Boolean);
    procedure DoCaptionFontChange(Sender: TObject);
    procedure setImageAlign(const Value: TAlign);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageWidth(const Value: Integer);

    procedure DrawRotatedText(Rotation: Integer);
    function getImageAlign: TAlign;
    function GetImage_Height: Integer;
    function GetImage_Width: Integer;
    function GetImage_AutoSize: Boolean;

    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(const Value: Boolean);
    procedure UpdateExclusive;
    procedure UpdateTracking;
//    procedure SetForceUpColor(const Value: TColor);
    procedure SetButtonUpColor(const Value: tcolor);
    procedure SetButtonDownColor(const Value: TColor);
    procedure SetMouseOverColor(const Value: TColor);
  protected
    FState: TButtonState;

    property MouseInControl: Boolean read FMouseInControl;

    function GetEffectiveCaptionHeight: Integer;
    function CanStartDrag: Boolean; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateParams(var Params: TCreateParams); override;
//    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoLeaveDrag; virtual;
    procedure AlignControls(AControl: TControl; var R: TRect); override;
//    procedure WndProc(var message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destory;
    procedure Click; override;
  published
    property Color;

    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Image_Align: TAlign read getImageAlign write setImageAlign default alClient;
    property Image_AutoSize: Boolean read GetImage_AutoSize write  SetImage_AutoSize;
    property Image_Center: Boolean read GetImage_Center write  SetImage_Center;
    property Image_Picture: TPicture read GetImage_Picture write SetImage_Picture;
    property Image_Proportional: Boolean read GetImage_Proportional write SetImage_Proportional;
    property Image_Stretch: Boolean read GetImage_Stretch write SetImage_Strech;
    property Image_Visible: Boolean read GetImage_Visible write SetImage_Visible;
    property Image_Width: Integer read GetImage_Width write SetImageWidth default 50;
    property Image_Height: Integer read GetImage_Height write SetImageHeight default 50;

    property MouseOverColor: TColor read FMouseOverColor write SetMouseOverColor default clSilver;
    property ButtonDownColor : TColor read FButtonDownColor write SetButtonDownColor default clGray;
    property ButtonsUpColor: tcolor read fButtonsUpColor write SetButtonUpColor default clBtnFace;
//    property ForceUpColor: TColor read fForceUpColor write SetForceUpColor default clBtnFace;

    property Down: Boolean read FDown write SetDown default False;
    property Flat: Boolean read FFlat write SetFlat default False;

    property OnImageMouseEnter: TNotifyEvent read fOnImage_MouseEnter write fOnImage_MouseEnter;
    property OnImageMouseLeave: TNotifyEvent read fOnImage_MouseLeave write fOnImage_MouseLeave;
    property OnMouseEnter: TCapPnlEventMouseEnter read fOnPnlMouseEnter write fOnPnlMouseEnter;
    property OnMouseLeave: TCapPnlEventMouseLeave read fOnPnlMouseLeave write fOnPnlMouseLeave;

    property Align;
    property BorderStyle default bsSingle;
    property Caption: string read FCaption write SetCaption;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clActiveCaption;
    property CaptionPosition: TJvDrawPosition read FCaptionPosition write SetCaptionPosition default dpLeft;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight default 15;
    property Cursor;
    property DragCursor;
    property FullRepaint;
    property Locked;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable:Boolean read FResizable write SetResizable default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartAutoDrag: TJvAutoDragStartEvent read FOnStartAutoDrag write FOnStartAutoDrag;
    property OnEndAutoDrag: TNotifyEvent read FEndDrag write FEndDrag;
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



{ TgemCapPanelBtn }


constructor TgemCapPanelBtn.Create(AOwner: TComponent);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Create' );{$ENDIF}
  inherited Create(AOwner);

//  fForceUpColor := clGreen;
  DoubleBuffered := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Size := 10;
  FCaptionFont.Style := [fsBold];
  FCaptionFont.Color := clWhite;
  FCaptionFont.OnChange := DoCaptionFontChange;
  FCaptionPosition := dpBottom;
  FAutoDrag := True;
  FOffset := 8;
  FCaptionColor := clActiveCaption;
  FFlat := False;
  FOutlookLook := false;
  BorderStyle := bsSingle;

  FCaptionOffsetSmall := 2;
  FCaptionOffsetLarge := 3;
  FResizable := True;

  Width := 75;
  Height := 95;
  BevelOuter := bvNone;
  TabOrder := 0;
  CaptionPosition := dpBottom;

  fImage := TJvImage.Create(Self);

//  Color := fButtonUpColor;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Create' );{$ENDIF}
end;


destructor TgemCapPanelBtn.Destory;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Destory' );{$ENDIF}
  FCaptionFont.free;
  Fimage.Free;
  inherited Destroy;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Destory' );{$ENDIF}
end;


procedure TgemCapPanelBtn.CreateParams(var Params: TCreateParams);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateParams' );{$ENDIF}
  inherited CreateParams(Params);
  // from Jedi Unit JcCaptionPanel
  if BorderStyle = bsSingle then
    with Params do
    begin
      if Resizable then
        Style := Style or WS_THICKFRAME
      else
        Style := Style or WS_DLGFRAME;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;

//  old stuff
//  Params.Style := Params.Style or BS_PUSHLIKE  or BS_CHECKBOX;
  Params.WindowClass.style := Params.WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateParams' );{$ENDIF}
end;


procedure TgemCapPanelBtn.CreateWindowHandle(const Params: TCreateParams);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateWindowHandle' );{$ENDIF}
  inherited  CreateWindowHandle(Params);
  with fImage do begin
//    OnMouseEnter := OnImageMouseEnter;
//    OnMouseLeave := OnImageMouseLeave;
    fImage.Parent := Self;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateWindowHandle' );{$ENDIF}
end;


//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]
//procedure TgemCapPanelBtn.CreateWnd;
//begin
//  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateWnd' );{$ENDIF}
//  inherited CreateWnd;
//  showmessage('CreateWnd;');
//  SendMessage(Handle, BM_SETCHECK, Integer(fChecked), 0);
//  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateWnd' );{$ENDIF}
//end;


procedure TgemCapPanelBtn.setImageAlign(const Value: TAlign);
begin
  fImage.Align := value;
end;


procedure TgemCapPanelBtn.SetImageHeight(const Value: Integer);
begin
  fImage.Height := Value;
end;


procedure TgemCapPanelBtn.SetImageWidth(const Value: Integer);
begin
  fImage.Width := Value;
end;


procedure TgemCapPanelBtn.Paint;
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

  FlatOffset := 0;//Ord(FlatButtons);

  AdjustedCaptionHeight := GetEffectiveCaptionHeight;

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

    dpBottom: begin
      FCaptionRect := Rect(FBevel, ClientHeight - AdjustedCaptionHeight - FBevel, ClientWidth - FBevel, ClientHeight - FBevel);
    end;
  end; //case

  Canvas.FillRect(FCaptionRect);
  DrawRotatedText(Rotation);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Paint' );{$ENDIF}
end;


procedure TgemCapPanelBtn.DoCaptionFontChange(Sender: TObject);
begin
  Invalidate;
end;


procedure TgemCapPanelBtn.AlignControls(AControl: TControl; var R: TRect);
begin
  case FCaptionPosition of
    dpLeft:
      R := Rect(GetEffectiveCaptionHeight + FCaptionOffsetSmall, 0, ClientWidth, ClientHeight);
    dpTop:
      R := Rect(0, GetEffectiveCaptionHeight + FCaptionOffsetSmall, ClientWidth, ClientHeight);
    dpRight:
      R := Rect(0, 0, ClientWidth - GetEffectiveCaptionHeight - FCaptionOffsetSmall, ClientHeight);
    dpBottom:
      R := Rect(0, 0, ClientWidth, ClientHeight - GetEffectiveCaptionHeight - FCaptionOffsetSmall);
  end;
  inherited AlignControls(AControl, R);
end;


function TgemCapPanelBtn.CanStartDrag: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CanStartDrag' );{$ENDIF}
  Result := Align = alNone;
  if Assigned(FOnStartAutoDrag) then
    FOnStartAutoDrag(Self, Result);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CanStartDrag' );{$ENDIF}
end;


//procedure TgemCapPanelBtn.WndProc;
//begin
//  inherited;
//  case message.Msg of
//    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
//       showmessage('In WndProc');
//       SetChecked(True);
//       fgemNormalStateColor := Color;
//       Color := fSelectedColor;
//     end;
//   end;
//end;


procedure TgemCapPanelBtn.CMButtonPressed(var Message: TMessage);
var
  Sender: TgemCapPanelBtn;
begin
  if Message.WParam = WPARAM(FGroupIndex) then begin
    showmessage('CMButtonPressed');
    Sender := TgemCapPanelBtn(Message.LParam);
    if Sender <> Self then begin
      if Sender.Down and FDown then begin
        FDown := False;
        Color := fButtonsUpColor;
//        Color := fForceUpColor;
        FState := bsUp;
//        if (Action is TCustomAction) then
//          TCustomAction(Action).Checked := False;
        Invalidate;
//      end
//      else  if fDown then begin
//        fgemNormalStateColor := Color;
//        Color := fSelectedColor;
      end;

      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

{
var
  Sender: TSpeedButton;
begin
  if Message.WParam = WPARAM(FGroupIndex) then
  begin
    Sender := TSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;

}


procedure TgemCapPanelBtn.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg    := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LPARAM(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;


procedure TgemCapPanelBtn.OnImageMouseEnterHandler(Sender: TObject);
begin
//  if ButtonDownColor <> color then begin
////    showmessage('OnImageMouseEnterHandler');
//    Color := MouseOverColor;
//  end;
{ Place your event-handling code here. }
end;


procedure TgemCapPanelBtn.OnImageMouseLeaveHandler(
  Sender: TObject);
begin
//  fButtonUpColor := fForceUpColor;
//  if fButtonDownColor <> color then begin
//    if ButtonUpColor = clBlack then
//      showmessage('Image ButtonUpColor is black');
//    Color := ButtonUpColor;
//  end;
{ Place your event-handling code here. }
end;


procedure TgemCapPanelBtn.CMPanelMouseEnter(var Msg: TMessage);
begin
  inherited;
  if fButtonDownColor <> color then  begin
//    showmessage('CMPanelMouseEnter');
    Color := fMouseOverColor;
  end;
  DoMouseEnter;



//var
//  NeedRepaint: Boolean;
//begin
//  inherited;
//  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
//    be used as a dock client. }
//  NeedRepaint := FFlat and not FMouseInControl and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);
//
//  { Windows XP introduced hot states also for non-flat buttons. }
//  if (NeedRepaint or StyleServices.Enabled) and not (csDesigning in ComponentState) then
//  begin
//    FMouseInControl := True;
//    if Enabled then
//      Repaint;
//  end;
//


end;


procedure TgemCapPanelBtn.CMPanelMouseLeave(var Msg: TMessage);
begin
//  fButtonUpColor := fForceUpColor;
  if fButtonDownColor <> color then begin
//    showmessage('CMPanelMouseLeave');
//    if ButtonUpColor = clBlack then
//      showmessage('Panel: ButtonUpColor is black');
//    Color := fForceUpColor;
    Color := fButtonsUpColor;
  end;
  DoMouseLeave;
  inherited;
end;


procedure TgemCapPanelBtn.Click;
begin
  inherited Click;
  showmessage('Click;');

//  if ButtonDownColor <> color then  begin
////    fgemNormalStateColor := Color;
//    Color := ButtonDownColor;
//  end;
end;

procedure TgemCapPanelBtn.DoLeaveDrag;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoLeaveDrag' );{$ENDIF}
  if Assigned(FEndDrag) then
    FEndDrag(Self);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoLeaveDrag' );{$ENDIF}
end;


procedure TgemCapPanelBtn.DoMouseEnter;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoMouseEnter' );{$ENDIF}
  if fButtonDownColor <> color then  begin
    Color := MouseOverColor;
  end;

  if Assigned(FOnPnlMouseEnter) then
    FOnPnlMouseEnter(Self);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoMouseEnter' );{$ENDIF}
end;


procedure TgemCapPanelBtn.DoMouseLeave;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoMouseLeave' );{$ENDIF}
  if fButtonDownColor <> color then
    Color := fButtonsUpColor;
//    Color := ForceUpColor;

  if Assigned(FOnPnlMouseLeave) then
    FOnPnlMouseLeave(Self);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoMouseLeave' );{$ENDIF}
end;


procedure TgemCapPanelBtn.DrawRotatedText(Rotation: Integer);
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
            R := Rect(R.Left, R.Bottom, R.Right, R.Top);
            OffsetRect(R, tW, -FOffset);
          end;
        dpTop, dpBottom:
          begin
            OffsetRect(R, FOffset, tH);
          end;
        dpRight:
          begin
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


procedure TgemCapPanelBtn.SetImage_Picture(const Value: TPicture);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Picture' );{$ENDIF}
  fImage.Picture.Assign(Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Picture' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetImage_AutoSize(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_AutoSize' );{$ENDIF}
//  fImage_AutoSize := value;
  fImage.AutoSize := Value;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_AutoSize' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetImage_Center(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Center' );{$ENDIF}
  fImage.Center := Value;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Center' );{$ENDIF}
end;


function TgemCapPanelBtn.GetEffectiveCaptionHeight: Integer;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetEffectiveCaptionHeight' );{$ENDIF}
  if FCaptionHeight = 0 then
    Result := GetSystemMetrics(SM_CYCAPTION)
  else
    Result := FCaptionHeight;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetEffectiveCaptionHeight' );{$ENDIF}
end;



function TgemCapPanelBtn.getImageAlign: TAlign;
begin
  Result := fImage.Align;
end;



function TgemCapPanelBtn.GetImage_AutoSize: Boolean;
begin
  result := fImage.AutoSize;
end;


function TgemCapPanelBtn.GetImage_Center: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Center' );{$ENDIF}
  result := fImage.Center;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Center' );{$ENDIF}
end;


function TgemCapPanelBtn.GetImage_Height: Integer;
begin
  result := fImage.Height;
end;


function TgemCapPanelBtn.GetImage_Width: Integer;
begin
  result := fImage.Width;
end;


function TgemCapPanelBtn.GetImage_Picture: TPicture;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Picture' );{$ENDIF}
  result := fImage.Picture;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Picture' );{$ENDIF}
end;


function TgemCapPanelBtn.GetImage_Proportional: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Proportional' );{$ENDIF}
  Result := fImage.Proportional;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Proportional' );{$ENDIF}
end;


function TgemCapPanelBtn.GetImage_Stretch: Boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetImage_Stretch' );{$ENDIF}
  result := fImage.Stretch;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetImage_Stretch' );{$ENDIF}
end;


function TgemCapPanelBtn.GetImage_Visible: Boolean;
begin
  Result := fImage.Visible;
end;


procedure TgemCapPanelBtn.SetImage_Proportional(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Proportional' );{$ENDIF}
  fImage.Proportional := (Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Proportional' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetImage_Strech(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Strech' );{$ENDIF}
  fImage.Stretch := (Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Strech' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetImage_Visible(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetImage_Visible' );{$ENDIF}
  fImage.Visible := (Value);
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetImage_Visible' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetButtonDownColor(const Value: TColor);
begin
  if FButtonDownColor <> Value then begin
    FButtonDownColor := Value;
    if fDown then begin
      Color := Value;
      Invalidate;
    end;
  end;
end;


procedure TgemCapPanelBtn.SetMouseOverColor(const Value: TColor);
begin
  if FMouseOverColor <> Value then begin
    FMouseOverColor := Value;
//    Invalidate;
  end;
end;


procedure TgemCapPanelBtn.SetButtonUpColor(const Value: tcolor);
begin
{  fButtonsUpColor := clBtnFace;}
  if fButtonsUpColor <> Color then begin
    fButtonsUpColor := Value;
    if not fDown then begin
      Color := Value;
      Invalidate;
    end;
  end;
end;


procedure TgemCapPanelBtn.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

//  FMouseDown := True;
//  if not PtInRect(FCaptionRect, Point(X, Y)) then
//    Exit;

//  if FAutoDrag and CanStartDrag then
//  begin
//    SetZOrder(True);
//    FDragging := True;
//    ReleaseCapture;
//    SetCapture(Handle);
//    FAnchorPos := Point(X, Y);
//  end;

  ShowMessage('mouseDown');
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;


procedure TgemCapPanelBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
//  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'MouseMove' );{$ENDIF}
//  inherited MouseMove(Shift, X, Y);
//  {$IFDEF JVCAPTIONPANEL_STD_BEHAVE}
//  if FDragging then
//    SetBounds(Left + X - FAnchorPos.X, Top + Y - FAnchorPos.Y, Width, Height);
//  {$ENDIF JVCAPTIONPANEL_STD_BEHAVE}
//  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'MouseMove' );{$ENDIF}
end;


procedure TgemCapPanelBtn.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  ShowMessage('MouseUp');
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then begin
      { Redraw face in-case mouse is captured }
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else  begin
        if FDown then
          FState := bsExclusive;
        Repaint;
       end;
    if DoClick then
      Click;
    UpdateTracking;
  end;
end;


procedure TgemCapPanelBtn.Resize;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Resize' );{$ENDIF}
  inherited Resize;
  Repaint;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Resize' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetResizable(const Value: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetResizable' );{$ENDIF}
  if FResizable <> Value then
  begin
    FResizable := Value;
    RecreateWnd;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetResizable' );{$ENDIF}
end;


procedure TgemCapPanelBtn.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
    end;
  end;
end;

procedure TgemCapPanelBtn.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;



procedure TgemCapPanelBtn.SetCaption(const Value: string);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaption' );{$ENDIF}
  FCaption := Value;
  inherited Caption := '';
  Invalidate;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaption' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetCaptionColor(const Value: TColor);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaptionColor' );{$ENDIF}
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaptionColor' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetCaptionFont(const Value: TFont);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaptionFont' );{$ENDIF}
  FCaptionFont.Assign(Value);
  Invalidate;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaptionFont' );{$ENDIF}
end;


procedure TgemCapPanelBtn.SetCaptionHeight(const Value: Integer);
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


procedure TgemCapPanelBtn.SetCaptionPosition(
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


procedure TgemCapPanelBtn.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then
    Value := False;

  if Value <> FDown then begin
    if FDown and (not FAllowAllUp) then
      Exit;

    FDown := Value;
    if Value then  begin
      if FState = bsUp then
        Invalidate;
      FState := bsExclusive;
      Color := fButtonDownColor;
    end
    else begin
      FState := bsUp;
      Color := fButtonsUpColor;
//      Color := fForceUpColor;
      Repaint;
    end;
    if Value then
      UpdateExclusive;
  end;
end;


procedure TgemCapPanelBtn.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;


//procedure TgemCapPanelBtn.SetForceUpColor(const Value: TColor);
//begin
//  if Value <> fForceUpColor then  begin
//    if Color <> ButtonDownColor then
//      Color := Value;
//    fForceUpColor := Value;
//  end;
//end;



//procedure TgemCapPanelBtn.WndProc;
//begin
//  inherited;
//  case message.Msg of
//    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
//       showmessage('In WndProc');
//       SetChecked(True);
//       fgemNormalStateColor := Color;
//       Color := fSelectedColor;
//     end;
//   end;
//end;
//

//procedure TgemCapPanelBtn.CNCommand(var Message: TWMCommand);
//begin
//  showmessage('CNCommand');
//
//  case Message.NotifyCode of
//    BN_CLICKED: SetChecked(True);
//
//    WM_LBUTTONDOWN: begin
//      fgemNormalStateColor := Color;
//      Color := fSelectedColor;
//      Toggle;
//    end;
//
//    else inherited;
//  end;
//end;
//

//procedure TgemCapPanelBtn.Toggle;
//begin
//  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Toggle' );{$ENDIF}
//  showmessage('Toggle');
//  Selected := not fChecked;
//  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Toggle' );{$ENDIF}
//end;
//


end.
