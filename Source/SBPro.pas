{------------------------------------------------------------------------------}
{                                                                              }
{  TStatusBarPro v1.80                                                         }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{   
(*
  Gary E McIntosh
   Add word wrap to this component.
   gary@slickrocksoftwaredesign.com
*)
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit SBPro;

interface

uses
  Winapi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes,

  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.Menus, VCL.ComCtrls,
  VCL.ImgList;

type
  TCustomImageList = class(TImageList);

  TImageIndex = type Integer;

  TStatusBarPro = class;

  TWrapRecord=record
    Height:Integer;
    Lines: array of string;
  end;


  //////////////////////////////////////////////////////////////////////////////
  /////////TStatusPanelPro//////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  TStatusPanelPro = class(TCollectionItem)
  private
    FText           : String;
    FHint           : String;
    FImageIndex     : TImageIndex;
    FPopupMenu      : TPopupMenu;
    FWidth          : Integer;
    FMinWidth       : Integer;
    FMaxWidth       : Integer;
    FAutoWidth      : Boolean;
    FAutoSize       : Boolean;
    FCursor         : TCursor;
    FColor          : TColor;                                                     {RAL}
    FParentColor    : Boolean;
    FAlignment      : TAlignment;
    FBevel          : TStatusPanelBevel;
    {$IFDEF COMPILER4_UP}
    FBiDiMode       : TBiDiMode;
    FParentBiDiMode : Boolean;
    {$ENDIF}
    FFont           : TFont;
    FParentFont     : Boolean;
    FIndent         : Integer;
    FStyle          : TStatusPanelStyle;
    FName           : String;
    FTag            : Integer;
    FControl        : TControl;                                                 {VVV}
    FUpdateNeeded   : Boolean;
    FOnCLick        : TNotifyEvent;
    FOnDblClick     : TNotifyEvent;
    FOnContextPopup : TContextPopupEvent;  // ### Luciano Cargnelutti
    procedure SetHint(Value: String);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TStatusPanelBevel);
    procedure SetStyle(Value: TStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetMaxWidth(Value: Integer);
    procedure SetAutoWidth(Value: Boolean);
    procedure SetAutoSize(Value: Boolean);
    procedure SetControl(Value: TControl);                              {VVV}
    procedure SetCursor(Value: TCursor);
    procedure SetColor(Value: TColor);                                  {RAL}
    procedure SetParentColor(Value: Boolean);
    function IsColorStored: Boolean;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    function IsBiDiModeStored: Boolean;
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: Boolean);
    procedure FontChanged(Sender: TObject);
    function IsFontStored: Boolean;
    procedure SetIndent(Value: Integer);
  protected
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); dynamic;  // ### Luciano Cargnelutti
    function GetDisplayName: string; override;
    procedure UpdateControlBounds; virtual;                                {VVV}
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;
    procedure ParentBiDiModeChanged;
    procedure ParentColorChanged;
    procedure ParentFontChanged;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
    property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property Color: TColor read FColor write SetColor stored IsColorStored;     {RAL}
    property Control: TControl read FControl write SetControl;                  {VVV}
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Hint: String read FHint write SetHint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Indent: Integer read FIndent write SetIndent default 0;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property Name: String read FName write FName;
    property Tag: Integer read FTag write FTag default 0;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;    // ### Luciano Cargnelutti
  end;


  //////////////////////////////////////////////////////////////////////////////
  /////////TStatusPanelsPro/////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  TStatusPanelsPro = class(TCollection)
  private
    FStatusBar: TStatusBarPro;
    function GetItem(Index: Integer): TStatusPanelPro;
    procedure SetItem(Index: Integer; Value: TStatusPanelPro);
    function GetByName(const Name: String): TStatusPanelPro;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    property StatusBar: TStatusBarPro read FStatusBar;
  public
    constructor Create(StatusBar: TStatusBarPro);
    function Add: TStatusPanelPro;
    property ByName[const Name: String]: TStatusPanelPro read GetByName;
    property Items[Index: Integer]: TStatusPanelPro read GetItem write SetItem; default;
  end;


  //////////////////////////////////////////////////////////////////////////////
  /////////TStatusBarPro////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  TDrawPanelProEvent = procedure(StatusBar: TStatusBarPro;
                           Panel: TStatusPanelPro; const Rect: TRect) of object;

  TStatusBarPro = class(TWinControl)
  private
    FWordWrap           : Boolean;
    FPanels             : TStatusPanelsPro;
    FCanvas             : TCanvas;
    FCursor             : TCursor;
    FSimpleText         : string;
    FSimplePanel        : Boolean;
    FSizeGrip           : Boolean;
    FUseSystemFont      : Boolean;
    FAutoHint           : Boolean;
    FAutoHintPanelIndex : Integer;	// sancho 2002.09.03
    FOnDrawPanel        : TDrawPanelProEvent;
    FOnHint             : TNotifyEvent;
    FImages             : TCustomImageList;
    FImageChangeLink    : TChangeLink;
    FMousePanel         : TStatusPanelPro;
    procedure DoRightToLeftAlignment(var Str: string; AAlignment: TAlignment;
                                     ARTLAlignment: Boolean);
    function IsFontStored: Boolean;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TCustomImageList);
    procedure SetPanels(Value: TStatusPanelsPro);
    procedure SetSimplePanel(Value: Boolean);
    procedure UpdateSimpleText;
    procedure SetSimpleText(const Value: string);
    procedure SetSizeGrip(Value: Boolean);
    procedure SetAutoHintPanelIndex(Value: Integer);
    procedure SetCursor(Value: TCursor);
    procedure SyncToSystemFont;
    procedure UpdatePanelsWidth;
    procedure UpdatePanel(Index: Integer; Repaint: Boolean);
    procedure UpdatePanels(UpdateRects, UpdateText: Boolean);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMWinIniChange(var Message: TMessage); message CM_WININICHANGE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure SetUseSystemFont(const Value: Boolean);
    function  FindPanelAtPos(X, Y: Integer): TStatusPanelPro;
    procedure UpdateCursor;
    procedure SetWordWrap(const Value: boolean); //Gary E McIntosh
    function WrapText(Canvas: TCanvas; Text: string; const MaxWidth: integer): TWrapRecord;//Gary E McIntosh
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ChangeScale(M, D: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function DoHint: Boolean; virtual;
    procedure DrawPanel(Panel: TStatusPanelPro; const Rect: TRect); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Click; override;
    procedure DblClick; override;
    function GetPopupMenu: TPopupMenu; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    property Canvas: TCanvas read FCanvas;
  published
    property Action;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False;
    property Align default alBottom;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Color default clBtnFace;
    property Cursor read FCursor write SetCursor default crDefault;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font stored IsFontStored;
    property Images: TCustomImageList read FImages write SetImages;
    property Panels: TStatusPanelsPro read FPanels write SetPanels;
    property AutoHintPanelIndex: Integer read FAutoHintPanelIndex write SetAutoHintPanelIndex default 0;  //// sancho 2002.09.03
    property Constraints;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel;
    property SimpleText: string read FSimpleText write SetSimpleText;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default True;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap; //Gary E McIntosh
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDrawPanel: TDrawPanelProEvent read FOnDrawPanel write FOnDrawPanel;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;


//procedure Register;


implementation

uses
  WinAPI.CommCtrl {$IFDEF COMPILER4_UP}, VCL.StdActns {$ENDIF};

const
  SB_SETBKCOLOR  = $2001;  // lParam = bkColor
  MaxPanelCount  = 128;
  SizeGripWidth  = 16;
  InternalIndent = 1;

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TStatusBarPro]);
//end;



////////////////////////////////////////////////////////////////////////////////
{ TStatusPanelPro }
////////////////////////////////////////////////////////////////////////////////

constructor TStatusPanelPro.Create(Collection: TCollection);
begin
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FColor := clBtnFace;
  FWidth := 50;
  FMinWidth := 0;
  FMaxWidth := 10000;
  FBevel := pbLowered;
  FImageIndex := -1;
  {$IFDEF COMPILER4_UP}
  FParentBiDiMode := True;
  {$ENDIF}
  FParentColor := True;
  FParentFont := True;
  FCursor := crDefault;
  inherited Create(Collection);
  {$IFDEF COMPILER4_UP}
  ParentBiDiModeChanged;
  {$ENDIF}
  ParentColorChanged;
  ParentFontChanged;
end;

destructor TStatusPanelPro.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TStatusPanelPro.Assign(Source: TPersistent);
begin
  if Source is TStatusPanel then
  begin
    Text := TStatusPanel(Source).Text;
    Width := TStatusPanel(Source).Width;
    Alignment := TStatusPanel(Source).Alignment;
    Bevel := TStatusPanel(Source).Bevel;
    // ### Luciano Cargnelutti
    // Style := TStatusPanel(Source).Style;
    {$IFDEF COMPILER4_UP}
    BidiMode := TStatusPanelPro(Source).BiDiMode;
    ParentBidiMode := TStatusPanelPro(Source).ParentBiDiMode;
    {$ENDIF}
  end
  else if Source is TStatusPanelPro then
  begin
    Text := TStatusPanelPro(Source).Text;
    AutoSize := TStatusPanelPro(Source).AutoSize;
    AutoWidth := TStatusPanelPro(Source).AutoWidth;
    MinWidth := TStatusPanelPro(Source).MinWidth;
    MaxWidth := TStatusPanelPro(Source).MaxWidth;
    Width := TStatusPanelPro(Source).Width;
    Color := TStatusPanelPro(Source).Color;                             {RAL}
    ParentColor := TStatusPanelPro(Source).ParentColor;
    Cursor := TStatusPanelPro(Source).Cursor;
    Alignment := TStatusPanelPro(Source).Alignment;
    Bevel := TStatusPanelPro(Source).Bevel;
    {$IFDEF COMPILER4_UP}
    BidiMode := TStatusPanelPro(Source).BiDiMode;
    ParentBidiMode := TStatusPanelPro(Source).ParentBiDiMode;
    {$ENDIF}
    Font := TStatusPanelPro(Source).Font;
    ParentFont := TStatusPanelPro(Source).ParentFont;
    Style := TStatusPanelPro(Source).Style;
    Hint := TStatusPanelPro(Source).Hint;
    ImageIndex := TStatusPanelPro(Source).ImageIndex;
    PopupMenu := TStatusPanelPro(Source).PopupMenu;
    OnClick := TStatusPanelPro(Source).OnClick;
    OnDblClick := TStatusPanelPro(Source).OnDblClick;
  end
  else
    inherited Assign(Source);
end;

function TStatusPanelPro.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

function TStatusPanelPro.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

procedure TStatusPanelPro.ParentBiDiModeChanged;
begin
  if FParentBiDiMode and (GetOwner <> nil) then
  begin
    BiDiMode := TStatusPanelsPro(GetOwner).StatusBar.BiDiMode;
    FParentBiDiMode := True;
  end;
end;

procedure TStatusPanelPro.SetBiDiMode(Value: TBiDiMode);
begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Changed(False);
  end;
end;

function TStatusPanelPro.IsBiDiModeStored: Boolean;
begin
  Result := not FParentBiDiMode;
end;

procedure TStatusPanelPro.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    if FParentBiDiMode then
      ParentBiDiModeChanged;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.ParentColorChanged;
begin
  if FParentColor and (GetOwner <> nil) then
  begin
    Color := TStatusPanelsPro(GetOwner).StatusBar.Color;
    FParentColor := True;
  end;
end;

procedure TStatusPanelPro.SetColor(Value : TColor);                     {RAL}
begin                                                                   {RAL}
 if FColor <> Value then                                                {RAL}
  begin                                                                 {RAL}
   FColor := Value;                                                     {RAL}
   FParentColor := False;
   Changed(False);                                                      {RAL}
  end;                                                                  {RAL}
end;                                                                    {RAL}

function TStatusPanelPro.IsColorStored: Boolean;
begin
  Result := not FParentColor;
end;

procedure TStatusPanelPro.SetParentColor(Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if FParentColor then
      ParentColorChanged;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TStatusPanelPro.SetParentFont(Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if FParentFont then
      ParentFontChanged;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.FontChanged(Sender: TObject);
begin
  FParentFont := False;
  Changed(False);
end;

function TStatusPanelPro.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

procedure TStatusPanelPro.ParentFontChanged;
begin
  if FParentFont and (GetOwner <> nil) then
  begin
    FFont.Assign(TStatusPanelsPro(GetOwner).StatusBar.Font);
    FParentFont := True;
  end;
end;

procedure TStatusPanelPro.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    Changed(FAutoWidth);
  end;
end;

function TStatusPanelPro.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then
    Result := inherited GetDisplayName;
  if Name <> '' then
    Result := '<' + Name + '> ' + Result;
end;

procedure TStatusPanelPro.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.SetBevel(Value: TStatusPanelBevel);
begin
  if FBevel <> Value then
  begin
    FBevel := Value;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.SetStyle(Value: TStatusPanelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(FAutoWidth);
  end;
end;

procedure TStatusPanelPro.SetWidth(Value: Integer);
begin
  if not (FAutoSize or FAutoWidth) then
  begin
    if Value < FMinWidth then
      Value := FMinWidth
    else if Value > FMaxWidth then
      Value := FMaxWidth;
    if FWidth <> Value then
    begin
      FWidth := Value;
      Changed(True);
    end;
  end;
end;

procedure TStatusPanelPro.SetMinWidth(Value: Integer);
begin
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    if not FAutoSize and (FWidth < FMinWidth) then
    begin
      FWidth := MinWidth;
      Changed(True);
    end
    else if FAutoWidth then
      Changed(True);
  end;
end;

procedure TStatusPanelPro.SetMaxWidth(Value: Integer);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    if not FAutoSize and (FWidth > FMaxWidth) then
    begin
      FWidth := MaxWidth;
      Changed(True);
    end
    else if FAutoWidth then
      Changed(True);
  end;
end;

procedure TStatusPanelPro.SetAutoWidth(Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    if FAutoWidth then
    begin
      FAutoSize := False;
      Changed(True);
    end;
  end;
end;

procedure TStatusPanelPro.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if FAutoSize then
    begin
      FAutoWidth := False;
      Changed(True);
    end
    else if FWidth < FMinWidth then
    begin
      FWidth := FMinWidth;
      Changed(True);
    end
    else if FWidth > FMaxWidth then
    begin
      FWidth := FMaxWidth;
      Changed(True);
    end
  end;
end;

procedure TStatusPanelPro.SetHint(Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed(False);
  end;
end;

procedure TStatusPanelPro.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(FAutoWidth);
  end;
end;

// ### Luciano Cargnelutti
procedure TStatusPanelPro.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  if Assigned(FOnContextPopup) then
    FOnContextPopup(Self, MousePos, Handled);
end;

procedure TStatusPanelPro.SetPopupMenu(Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if (GetOwner <> nil) and (FPopupMenu <> nil) then
      FPopupMenu.FreeNotification(TStatusPanelsPro(GetOwner).StatusBar);
  end;
end;

procedure TStatusPanelPro.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if GetOwner <> nil then
      TStatusPanelsPro(GetOwner).StatusBar.UpdateCursor;
  end;
end;

procedure TStatusPanelPro.SetControl(Value: TControl);
var
  I: Integer;
begin
  if FControl <> Value then
  begin
    FControl := Value;
    if (GetOwner <> nil) and (FControl <> nil) then
    begin
      with TStatusPanelsPro(GetOwner) do
      begin
        for I := Count - 1 downto 0 do
          if (Items[I].Control = Value) and (Index <> I) then
            Items[I].Control := nil;
        FControl.FreeNotification(StatusBar);
        FControl.Parent := StatusBar;
      end;
      UpdateControlBounds;
    end;
  end;
end;

procedure TStatusPanelPro.UpdateControlBounds;
var
  SB: TStatusBarPro;
  Borders: array[0..2] of Integer;
  Rect: TRect;
begin
  if Assigned(FControl) and (GetOwner <> nil) then
  begin
    SetRect(Rect, 0, 0, 0, 0);
    SB := TStatusPanelsPro(GetOwner).StatusBar;
    if not SB.SimplePanel then
    begin
      SB.Perform(SB_GETRECT, Index, Integer(@Rect));
      FillChar(Borders[0], SizeOf(Borders), 0);
      SB.Perform(SB_GETBORDERS, 0, Integer(@Borders[0]));
      InflateRect(Rect, -Borders[2] div 2, -Borders[1] div 2);
    end;
    if FControl is TWinControl then // Workaround Delphi bug!!!
      SetWindowPos(TWinControl(FControl).Handle, 0, Rect.Left, Rect.Top,
        Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, SWP_NOZORDER)
    else
      FControl.BoundsRect := Rect;
  end;
end;

{ TStatusPanelsPro }

constructor TStatusPanelsPro.Create(StatusBar: TStatusBarPro);
begin
  inherited Create(TStatusPanelPro);
  FStatusBar := StatusBar;
end;

function TStatusPanelsPro.Add: TStatusPanelPro;
begin
  Result := TStatusPanelPro(inherited Add);
end;

function TStatusPanelsPro.GetItem(Index: Integer): TStatusPanelPro;
begin
  Result := TStatusPanelPro(inherited GetItem(Index));
end;

function TStatusPanelsPro.GetByName(const Name: String): TStatusPanelPro;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
    begin
      Result := Items[I];
      Exit;
    end;
end;

function TStatusPanelsPro.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

procedure TStatusPanelsPro.SetItem(Index: Integer; Value: TStatusPanelPro);
begin
  inherited SetItem(Index, Value);
end;

procedure TStatusPanelsPro.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    FStatusBar.UpdatePanel(Item.Index, False)
  else
    FStatusBar.UpdatePanels(True, False);
end;

{ TStatusBarPro }

constructor TStatusBarPro.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanels := TStatusPanelsPro.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque, csAcceptsControls];
  Color := clBtnFace;
  Height := 19;
  Align := alBottom;
  FSizeGrip := True;
  ParentFont := False;
  FUseSystemFont := True;
  SyncToSystemFont;
end;

destructor TStatusBarPro.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  FCanvas.Free;
  FPanels.Free;
  FPanels := nil;
  inherited Destroy;
end;

procedure TStatusBarPro.CreateParams(var Params: TCreateParams);
const
  GripStyles: array[Boolean] of DWORD = (CCS_TOP, SBARS_SIZEGRIP);
begin
  InitCommonControl(ICC_BAR_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, STATUSCLASSNAME);
  with Params do
  begin
    Style := Style or GripStyles[FSizeGrip and
      (Parent is {$IFDEF COMPILER4_UP} TCustomForm {$ELSE} TForm {$ENDIF}) and
      ({$IFDEF COMPILER4_UP} TCustomForm {$ELSE} TForm {$ENDIF} (Parent).BorderStyle
       in [bsSizeable, bsSizeToolWin])];
    WindowClass.style := WindowClass.style and not CS_HREDRAW;
  end;
end;

procedure TStatusBarPro.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, SB_SETBKCOLOR, 0, ColorToRGB(Color));
  UpdatePanels(True, False);
  if FSimpleText <> '' then
    SendMessage(Handle, SB_SETTEXT, 255, Integer(PChar(FSimpleText)));
  if FSimplePanel then
    SendMessage(Handle, SB_SIMPLE, 1, 0);
  UpdateCursor;
end;


function TStatusBarPro.DoHint: Boolean;
begin
  if Assigned(FOnHint) then
  begin
    FOnHint(Self);
    Result := True;
  end
  else
    Result := False;
end;


//==============================================================================
//== Gary E McIOntosh added to do the actual wrapping of the text.  This makes line breaks
//== at a space in the string.
function TStatusBarPro.WrapText(Canvas: TCanvas; Text: string;
                                         const MaxWidth: integer): TWrapRecord;
var
  S         : string;
  CurrWidth : integer;
begin
  SetLength(Result.Lines,0);
  Result.Height := 0;
  // the currwidth needs to be wider than max width for the first go or an
  //  access volation will be thrown becuase the lenght of Reesult.lines is zero.
  //  we will add the lenght of the first line segment to the CurrWidth to
  //  get the CurrWidth longer than the Max Width.
  CurrWidth     := MaxWidth;
  //  Needs a space at the end of the string, but only one space.
   Text := trim(Text);
   Text := Text + ' ';

  repeat
    // get a substring from start of panels string to first space.  Then remove
    // that substring from the panel string
    S := copy(Text, 1, pos(' ', Text) - 1);
    Delete(Text, 1, pos(' ', Text));

    if (Canvas.TextWidth(S + ' ') + CurrWidth) < MaxWidth then begin
      Result.Lines[High(Result.Lines)] := Result.Lines[High(Result.Lines)] + ' ' +S;
      Inc(CurrWidth, Canvas.TextWidth(S + ' '));
    end
    else begin
      if length(Result.Lines) > 0 then
        Inc(Result.Height,Canvas.TextHeight(Result.Lines[High(Result.Lines)]));

      SetLength(Result.Lines,length(Result.Lines) + 1);
      Result.Lines[High(Result.Lines)] := S;
      CurrWidth:=Canvas.TextWidth(S);
    end;
  until length(TrimRight(Text)) = 0;

  Inc(Result.Height, Canvas.TextHeight(Result.Lines[High(Result.Lines)]));
end;


procedure TStatusBarPro.DrawPanel(Panel: TStatusPanelPro; const Rect: TRect);
var
  X, Y, i        : Integer;
  ImageWidth     : Integer;
  Alignment      : TAlignment;
  RightSideImage : Boolean;
  theWordWrap    : TWrapRecord;
begin
  if (Panel.Style = psOwnerDraw) and Assigned(FOnDrawPanel) then
    FOnDrawPanel(Self, Panel, Rect)
  else
  begin
    // Changes alignment according to BiDiMode  =========================
    Alignment := Panel.Alignment;
    if Panel.UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(Alignment);
    RightSideImage := (Alignment = taRightJustify) {$IFDEF COMPILER4_UP} or
      ((Alignment = taCenter) and Panel.UseRightToLeftAlignment) {$ENDIF};
    // Determines image's width
    if (FImages <> nil) and (Panel.ImageIndex >= 0) and
       (Panel.ImageIndex < FImages.Count) then
      ImageWidth := FImages.Width
    else
      ImageWidth := 0;
    // Determines X position ============================================
    case Alignment of
      taLeftJustify: X := Rect.Left + InternalIndent + Panel.Indent;
      taRightJustify: X := Rect.Right - ImageWidth - InternalIndent - Panel.Indent;
    else
      {$IFDEF COMPILER4_UP}
      if Panel.UseRightToLeftAlignment then
        X := Rect.Left + ((Rect.Right - Rect.Left) +
            (ImageWidth + FCanvas.TextWidth(Panel.Text))) div 2 - ImageWidth
      else
      {$ENDIF}
        X := Rect.Left + ((Rect.Right - Rect.Left) -
            (ImageWidth + FCanvas.TextWidth(Panel.Text))) div 2;
    end;
    FCanvas.Brush.Color := Panel.Color; {RAL: Put here so colors can be seen in design mode}
    FCanvas.FillRect(Rect);
    // Draws image  =====================================================
    if ImageWidth > 0 then
    begin
      Y := Rect.Top + ((Rect.Bottom - Rect.Top) - FImages.Height) div 2;
      FImages.Draw(FCanvas, X, Y, Panel.ImageIndex);
      if RightSideImage then
        Dec(X, 2 * InternalIndent)
      else
        Inc(X, FImages.Width + 2 * InternalIndent);
    end;
    // Draws text  ========================================================
    if Panel.Text <> '' then begin
      FCanvas.Font.Assign(Panel.Font);
      if RightSideImage then
        Dec(X, FCanvas.TextWidth(Panel.Text));

      if Panel.UseRightToLeftReading then
        FCanvas.TextFlags := FCanvas.TextFlags or ETO_RTLREADING
      else
        FCanvas.TextFlags := FCanvas.TextFlags and not ETO_RTLREADING;
      // Gary E McIntosh add to allow for wrapping of text.  
      if FWordWrap then begin
        Canvas.FillRect(Rect);

        theWordWrap := WrapText(FCanvas, Panel.Text, Rect.Width);
        y := Rect.Top;
        for i := Low(theWordWrap.Lines) to High(theWordWrap.Lines) do begin
          FCanvas.TextOut(Rect.Left, y, theWordWrap.Lines[i]);
          Inc(y, FCanvas.TextHeight(theWordWrap.Lines[i]));
        end;

      end
      else begin
        Y := Rect.Top + ((Rect.Bottom - Rect.Top) - FCanvas.TextHeight('H')) div 2;
        FCanvas.TextOut(X, Y, Panel.Text);
      end;
    end;
  end;
end;

(* original code
    // Draws text
    if Panel.Text <> '' then
    begin
      FCanvas.Font.Assign(Panel.Font);
      if RightSideImage then
        Dec(X, FCanvas.TextWidth(Panel.Text));
      Y := Rect.Top + ((Rect.Bottom - Rect.Top) - FCanvas.TextHeight('H')) div 2;
      if Panel.UseRightToLeftReading then
        FCanvas.TextFlags := FCanvas.TextFlags or ETO_RTLREADING
      else
        FCanvas.TextFlags := FCanvas.TextFlags and not ETO_RTLREADING;
      FCanvas.TextOut(X, Y, Panel.Text);
    end;

*)


procedure TStatusBarPro.SetImages(Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    ImageListChange(nil);
  end;
end;

procedure TStatusBarPro.ImageListChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Panels.Count-1 do
    if Panels[I].AutoWidth and (Panels[I].ImageIndex >= 0) then
    begin
      UpdatePanels(True, False);
      Exit;
    end;
  Invalidate;
end;

procedure TStatusBarPro.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      Images := nil
    else if Assigned(Panels) then
    begin
      if AComponent is TPopupMenu then
      begin
        for I := 0 to Panels.Count-1 do
          if Panels[I].PopupMenu = AComponent then
            Panels[I].PopupMenu := nil;
      end
      else if AComponent is TControl then
      begin
        for I := 0 to Panels.Count-1 do
          if Panels[I].Control = AComponent then
            Panels[I].Control := nil;
      end;
    end;
  end;
end;

procedure TStatusBarPro.SetPanels(Value: TStatusPanelsPro);
begin
  FPanels.Assign(Value);
end;

procedure TStatusBarPro.SetSimplePanel(Value: Boolean);
var
  I: Integer;
begin
  if FSimplePanel <> Value then
  begin
    FSimplePanel := Value;
    if HandleAllocated then
    begin
      SendMessage(Handle, SB_SIMPLE, Ord(FSimplePanel), 0);
      for I := 0 to Panels.Count - 1 do
        Panels[I].UpdateControlBounds;
    end;
  end;
end;


procedure TStatusBarPro.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    UpdateCursor;
  end;
end;

procedure TStatusBarPro.DoRightToLeftAlignment(var Str: string;
  AAlignment: TAlignment; ARTLAlignment: Boolean);
begin
  if ARTLAlignment then
    ChangeBiDiModeAlignment(AAlignment);
  case AAlignment of
    taCenter: Insert(#9, Str, 1);
    taRightJustify: Insert(#9#9, Str, 1);
  end;
end;

procedure TStatusBarPro.UpdateSimpleText;
const
  RTLReading: array[Boolean] of Longint = (0, SBT_RTLREADING);
begin
  {$IFDEF COMPILER4_UP}
  DoRightToLeftAlignment(FSimpleText, taLeftJustify, UseRightToLeftAlignment);
  {$ENDIF}
  if HandleAllocated then
    SendMessage(Handle, SB_SETTEXT, 255
      {$IFDEF COMPILER4_UP} or RTLREADING[UseRightToLeftReading] {$ENDIF},
      Integer(PChar(FSimpleText)));
end;

procedure TStatusBarPro.SetSimpleText(const Value: string);
begin
  if FSimpleText <> Value then
  begin
    FSimpleText := Value;
    UpdateSimpleText;
  end;
end;

procedure TStatusBarPro.CMBiDiModeChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Panels.Count - 1 do
    if Panels[I].ParentBiDiMode then
      Panels[I].ParentBiDiModeChanged;
  if HandleAllocated then
    if SimplePanel then
      UpdateSimpleText
    else
      UpdatePanels(True, True);
end;

procedure TStatusBarPro.FlipChildren(AllLevels: Boolean);
var
  Loop, FirstWidth, LastWidth: Integer;
  APanels: TStatusPanelsPro;
begin
  if HandleAllocated and
     (not SimplePanel) and (Panels.Count > 0) then
  begin
    { Get the true width of the last panel }
    LastWidth := ClientWidth;
    if SizeGrip then
      Dec(LastWidth, SizeGripWidth);
    FirstWidth := Panels[0].Width;
    for Loop := 0 to Panels.Count - 2 do Dec(LastWidth, Panels[Loop].Width);
    { Flip 'em }
    APanels := TStatusPanelsPro.Create(Self);
    try
      for Loop := 0 to Panels.Count - 1 do with APanels.Add do
        Assign(Self.Panels[Loop]);
      for Loop := 0 to Panels.Count - 1 do
        Panels[Loop].Assign(APanels[Panels.Count - Loop - 1]);
    finally
      APanels.Free;
    end;
    { Set the width of the last panel }
    if Panels.Count > 1 then
    begin
      Panels[Panels.Count-1].Width := FirstWidth;
      Panels[0].Width := LastWidth;
    end;
    UpdatePanels(True, True);
  end;
end;

procedure TStatusBarPro.SetSizeGrip(Value: Boolean);
begin
  if FSizeGrip <> Value then
  begin
    FSizeGrip := Value;
    RecreateWnd;
  end;
end;

procedure TStatusBarPro.SetAutoHintPanelIndex(Value: Integer);
begin
  if (FAutoHintPanelIndex <> Value) and (Value >= 0) and (Value < Panels.Count) then
    FAutoHintPanelIndex := Value;
end;

procedure TStatusBarPro.SyncToSystemFont;
{$IFNDEF COMPILER5_UP}
var
  NonClientMetrics: TNonClientMetrics;
{$ENDIF}
begin
  {$IFNDEF COMPILER5_UP}
  if FUseSystemFont then
  begin
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
      Font.Handle := CreateFontIndirect(NonClientMetrics.lfStatusFont)
  end;
  {$ELSE}
  if FUseSystemFont then
    Font := Screen.HintFont;
  {$ENDIF}
end;

//==============================================================================

procedure TStatusBarPro.UpdatePanel(Index: Integer; Repaint: Boolean);
var
  Flags: Integer;
  S: string;
  PanelRect: TRect;
begin
  if HandleAllocated then
    with Panels[Index] do
    begin
      if not Repaint then
      begin
        FUpdateNeeded := True;
        SendMessage(Handle, SB_GETRECT, Index, Integer(@PanelRect));
        InvalidateRect(Handle, @PanelRect, True);
      end
      else if FUpdateNeeded then
      begin
        FUpdateNeeded := False;
        Flags := 0;
        case Bevel of
          pbNone: Flags := SBT_NOBORDERS;
          pbRaised: Flags := SBT_POPOUT;
        end;
        {$IFDEF COMPILER4_UP}
        if UseRightToLeftReading then Flags := Flags or SBT_RTLREADING;
        {$ENDIF}
        {if Style = psOwnerDraw then} Flags := Flags or SBT_OWNERDRAW;
        S := Text;
        {$IFDEF COMPILER4_UP}
        DoRightToLeftAlignment(S, Alignment, UseRightToLeftAlignment);
        {$ELSE}
        case Alignment of
          taCenter: Insert(#9, S, 1);
          taRightJustify: Insert(#9#9, S, 1);
        end;
        {$ENDIF}
        SendMessage(Handle, SB_SETTEXT, Index or Flags, Integer(PChar(S)));
        UpdateControlBounds;
      end;
    end;
end;


(*
  if HandleAllocated then
    with Panels[Index] do
    begin
      if not Repaint then
      begin
        FUpdateNeeded := True;
        SendMessage(Handle, SB_GETRECT, Index, Integer(@PanelRect));
        InvalidateRect(Handle, @PanelRect, True);
      end
      else if FUpdateNeeded then
      begin
        FUpdateNeeded := False;
        Flags := 0;
        case Bevel of
          pbNone: Flags := SBT_NOBORDERS;
          pbRaised: Flags := SBT_POPOUT;
        end;
        {$IFDEF COMPILER4_UP}
        if UseRightToLeftReading then Flags := Flags or SBT_RTLREADING;
        {$ENDIF}
        {if Style = psOwnerDraw then} Flags := Flags or SBT_OWNERDRAW;
        S := Text;
        {$IFDEF COMPILER4_UP}
        DoRightToLeftAlignment(S, Alignment, UseRightToLeftAlignment);
        {$ELSE}
        case Alignment of
          taCenter: Insert(#9, S, 1);
          taRightJustify: Insert(#9#9, S, 1);
        end;
        {$ENDIF}
        SendMessage(Handle, SB_SETTEXT, Index or Flags, Integer(PChar(S)));
        UpdateControlBounds;
      end;
    end;


*)
//==============================================================================

procedure TStatusBarPro.UpdatePanels(UpdateRects, UpdateText: Boolean);
var
  I, Count, PanelPos: Integer;
  PanelEdges: array[0..MaxPanelCount - 1] of Integer;
begin
  Count := Panels.Count;
  if HandleAllocated then
  begin
    if UpdateRects then
    begin
      if Count > MaxPanelCount then
        Count := MaxPanelCount;
      if Count = 0 then
      begin
        PanelEdges[0] := -1;
        SendMessage(Handle, SB_SETPARTS, 1, Integer(@PanelEdges));
        SendMessage(Handle, SB_SETTEXT, 0, 0);
      end
      else
      begin
        UpdatePanelsWidth;
        PanelPos := 0;
        for I := 0 to Count - 2 do
        begin
          Inc(PanelPos, Panels[I].Width);
          PanelEdges[I] := PanelPos;
        end;
        PanelEdges[Count - 1] := -1;
        SendMessage(Handle, SB_SETPARTS, Count, Integer(@PanelEdges));
      end;
    end;
    for I := 0 to Count - 1 do
      UpdatePanel(I, UpdateText);
  end;
  {$IFDEF COMPILER4_UP}
  if FAutoHintPanelIndex >= Count then
    FAutoHintPanelIndex := 0;
  {$ENDIF}
end;

//==============================================================================

procedure TStatusBarPro.UpdatePanelsWidth;
var
  I, Count: Integer;
  FreeWidth: Integer;
  AutoSizeCount, AutoSizeWidth: Integer;
begin
  Count := Panels.Count;
  if Count > MaxPanelCount then
    Count := MaxPanelCount;
  AutoSizeCount := 0;
  FreeWidth := ClientWidth;
  if SizeGrip then
    Dec(FreeWidth, SizeGripWidth);
  for I := 0 to Count - 1 do
    with Panels[I] do
      if AutoSize then
        Inc(AutoSizeCount)
      else
      begin
        if AutoWidth then
        begin
          Canvas.Font.Assign(Font);
          FWidth := Canvas.TextWidth(Text) + 2 * (Indent + InternalIndent);
          if (FImages <> nil) and (ImageIndex >= 0) and (ImageIndex < FImages.Count) then
            Inc(FWidth, FImages.Width + 2 * InternalIndent);
          if FWidth < MinWidth then
            FWidth := MinWidth
          else if FWidth > MaxWidth then
            FWidth := MaxWidth;
        end;
        Dec(FreeWidth, Width);
      end;
  if AutoSizeCount > 0 then
  begin
    AutoSizeWidth := FreeWidth div AutoSizeCount;
    if AutoSizeWidth < 0 then
      AutoSizeWidth := 0;
    for I := 0 to Count - 1 do
      with Panels[I] do
        if AutoSize then
           FWidth := AutoSizeWidth;
  end;
end;

//==============================================================================


procedure TStatusBarPro.CMWinIniChange(var Message: TMessage);
begin
  inherited;
  if (Message.WParam = 0) or (Message.WParam = SPI_SETNONCLIENTMETRICS) then
    SyncToSystemFont;
end;


procedure TStatusBarPro.CNDrawItem(var Message: TWMDrawItem);
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
      FCanvas.Brush.Color := Color;
      FCanvas.Brush.Style := bsSolid;
      if SizeGrip and (itemID + 1 = DWORD(Panels.Count)) then
        Dec(rcItem.Right, SizeGripWidth);
      DrawPanel(Panels[itemID], rcItem);
    finally
      FCanvas.Handle := 0;
      FCanvas.Unlock;
      RestoreDC(hDC, SaveIndex);
    end;
  end;
  Message.Result := 1;
end;


procedure TStatusBarPro.WMGetTextLength(var Message: TWMGetTextLength);
begin
  Message.Result := Length(FSimpleText);
end;


procedure TStatusBarPro.WMPaint(var Message: TWMPaint);
begin
  UpdatePanels(False, True);
  inherited;
end;


procedure TStatusBarPro.WMSize(var Message: TWMSize);
begin
  { Eat WM_SIZE message to prevent alignment by the control }
  UpdatePanels(True, False);
  {$IFDEF COMPILER4_UP}
  if not (csLoading in ComponentState) then Resize;
  {$ENDIF}
  Repaint;
end;


procedure TStatusBarPro.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if Message.CalcValidRects then
    Message.Result := Message.Result or WVR_REDRAW;
end;


procedure TStatusBarPro.CMHintShow(var Message: TCMHintShow);
begin
  inherited;
  if Assigned(FMousePanel) and (FMousePanel.Hint <> '') then
    Message.HintInfo^.HintStr := FMousePanel.Hint
  else
    Message.HintInfo^.HintStr := Hint;
end;


function TStatusBarPro.FindPanelAtPos(X, Y: Integer): TStatusPanelPro;
var
  Index: Integer;
  PanelRect: TRect;
  Pt: TPoint;
begin
  Result := nil;
  Pt.X := X;
  Pt.Y := Y;
  for Index := 0 to FPanels.Count-1 do
  begin
    if (SendMessage(Handle, SB_GETRECT, Index, Integer(@PanelRect)) <> 0) and
       PtInRect(PanelRect, Pt) then
    begin
      Result := FPanels[Index];
      Break;
    end;
  end;
end;


procedure TStatusBarPro.UpdateCursor;
begin
  if Assigned(FMousePanel) and (FMousePanel.Cursor <> crDefault) then
    inherited Cursor := FMousePanel.Cursor
  else
    inherited Cursor := FCursor;
end;

procedure TStatusBarPro.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  FMousePanel := FindPanelAtPos(X, Y);
  inherited;
end;


procedure TStatusBarPro.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var
  OldPanel: TStatusPanelPro;
begin
  OldPanel := FMousePanel;
  FMousePanel := FindPanelAtPos(X, Y);
  if OldPanel <> FMousePanel then
  begin
    if ShowHint then
      Application.CancelHint;
    UpdateCursor;
  end;
  inherited;
end;


procedure TStatusBarPro.Click;
begin
  if Assigned(FMousePanel) and Assigned(FMousePanel.OnClick) then
    FMousePanel.OnClick(FMousePanel)
  else if Assigned(OnClick) then
    OnClick(Self);
end;


procedure TStatusBarPro.DblClick;
begin
  if Assigned(FMousePanel) and Assigned(FMousePanel.OnDblClick) then
    FMousePanel.OnDblClick(FMousePanel)
  else if Assigned(OnDblClick) then
    OnDblClick(Self);
end;


function TStatusBarPro.GetPopupMenu: TPopupMenu;
begin
  if Assigned(FMousePanel) and Assigned(FMousePanel.PopupMenu) then
  begin
    Result := FMousePanel.PopupMenu;
    {$IFDEF COMPILER4_UP}
    if Result <> nil then Result.BiDiMode := FMousePanel.BiDiMode;
    {$ENDIF}
  end
  else
    Result := PopupMenu;
end;


procedure TStatusBarPro.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FMousePanel) then
    FMousePanel.DoContextPopup(MousePos, Handled);
  if not Handled then
    inherited DoContextPopup(MousePos, Handled);
end;


function TStatusBarPro.IsFontStored: Boolean;
begin
  Result := not FUseSystemFont and not ParentFont and not DesktopFont;
end;


procedure TStatusBarPro.SetUseSystemFont(const Value: Boolean);
begin
  if FUseSystemFont <> Value then
  begin
    FUseSystemFont := Value;
    if Value then
    begin
      if ParentFont then ParentFont := False;
      SyncToSystemFont;
    end;
  end;
end;


procedure TStatusBarPro.SetWordWrap(const Value: boolean);// Added by Gary E McIntosh
begin
  FWordWrap := Value;
  Invalidate;
end;


procedure TStatusBarPro.CMParentFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  if FUseSystemFont and ParentFont then
    FUseSystemFont := False;
  for I := 0 to Panels.Count - 1 do
    if Panels[I].ParentFont then
      Panels[I].ParentFontChanged;
end;


procedure TStatusBarPro.CMColorChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  if HandleAllocated then
    SendMessage(Handle, SB_SETBKCOLOR, 0, ColorToRGB(Color));
  for I := 0 to Panels.Count - 1 do
    if Panels[I].ParentColor then
      Panels[I].ParentColorChanged;
end;


function TStatusBarPro.ExecuteAction(Action: TBasicAction): Boolean;
var
  SingleLineHint: String;
begin
  if AutoHint and (Action is THintAction) and not DoHint then
  begin
    SingleLineHint := StringReplace(THintAction(Action).Hint,
      #13#10, ' ', [rfReplaceAll]);	//sancho 2002.09.03
    if SimplePanel or (Panels.Count = 0) then
      SimpleText := SingleLineHint
    else
      Panels[FAutoHintPanelIndex].Text := SingleLineHint;
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;


procedure TStatusBarPro.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;


procedure TStatusBarPro.CMSysFontChanged(var Message: TMessage);
begin
  inherited;
  SyncToSystemFont;
end;


procedure TStatusBarPro.ChangeScale(M, D: Integer);
begin
  if FUseSystemFont then  // status bar size based on system font size
    ScalingFlags := [sfTop];
  inherited;
end;

end.

