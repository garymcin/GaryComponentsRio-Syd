{*********************************************************}
{*                   OVCDBCL.PAS 4.06                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Orpheus                                    *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C)1995-2002    *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{* Roman Kassebaum                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{.W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

unit GEMOvcDbListControl;
  {-Data-aware column list}

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes, System.UITypes, System.Types,

  Vcl.Controls, Vcl.Buttons, Vcl.Forms, Vcl.DBCtrls, Vcl.Graphics, Vcl.StdCtrls,
  Vcl.Menus,

  Db,

  OvcBase, OvcCmd, OvcConst, OvcData, OvcMisc;//, OvcColor;

type
  THeaderClickEvent =
    procedure(Sender : TObject; Point : TPoint) of object;
    {-event to notify of a mouse click in the header area}
  TIndicatorClickEvent =
    procedure(Sender : TObject; Row : Integer) of object;
    {event to notify of a scroll action}

  TGEMOvcCollectionStreamer = class
  protected {private}
    FCollectionList : TList;
    FOwner          : TComponent;
  protected
  public
    constructor Create(AOwner : TComponent);
    destructor Destroy; override;

    procedure Loaded;
    procedure GetChildren(Proc: TGetChildProc; Root : TComponent);
    procedure Clear;
    function CollectionFromType(Component : TComponent) : TOvcCollection;

    property Owner : TComponent read FOwner;
  end;


  TGEMOvcColors = class(TPersistent)
  protected {private}
    {property variables}
    FBackColor     : TColor;   {background color}
    FTextColor     : TColor;   {text or font color}
    FUseDefault    : Boolean;  {true to use defaults}
    {event variables}
    FOnColorChange : TNotifyEvent;
    {internal variables}
    cDefBackColor  : TColor;   {default background}
    cDefTextColor  : TColor;   {default text color}
    {property methods}
    procedure SetBackColor(Value: TColor);
      {-set the color used for the background}
    procedure SetTextColor(Value: TColor);
      {-set the color used for the foreground}
    procedure SetUseDefault(Value: Boolean);
      {-set the flag to reset colors to parent default values}
    procedure ReadUseDefault(Reader : TReader);
      {-read the UseDefault property. for backward compatibility only}
  protected
    procedure DefineProperties(Filer : TFiler); override;
    procedure DoOnColorChange; dynamic;
      {-notify onwing object that a color has changed}
    procedure ResetToDefaultColors; dynamic;
      {-assign default color values}
  public
    procedure Assign(Source : TPersistent); override;
    constructor Create(FG, BG : TColor); virtual;
    property OnColorChange : TNotifyEvent read FOnColorChange write FOnColorChange;
  published
    property BackColor : TColor read FBackColor write SetBackColor;
    property TextColor : TColor read FTextColor write SetTextColor;
    property UseDefault : Boolean read FUseDefault write SetUseDefault stored False;
  end;


  TGemOvcCustomControl = class(TCustomControl)
  protected {private}
    {property variables}
    FAfterEnter         : TNotifyEvent;
    FAfterExit          : TNotifyEvent;
    FCollectionStreamer : TGEMOvcCollectionStreamer;
    FOnMouseWheel       : TMouseWheelEvent;
//    FLabelInfo          : TOvcLabelInfo;
    FInternal : Boolean; {flag to suppress name generation
                          on collection items}
    {property methods}
    function GetAttachedLabel : TOvcAttachedLabel;
    function GetAbout : string;
    procedure SetAbout(const Value : string);
    {internal methods}
    procedure LabelAttach(Sender : TObject; Value : Boolean);
    procedure LabelChange(Sender : TObject);
    procedure PositionLabel;
    {private message methods}
    procedure OMAssignLabel(var Msg : TMessage); message OM_ASSIGNLABEL;
    procedure OMPositionLabel(var Msg : TMessage); message OM_POSITIONLABEL;
    procedure OMRecordLabelPosition(var Msg : TMessage); message OM_RECORDLABELPOSITION;
    procedure OMAfterEnter(var Msg : TMessage); message OM_AFTERENTER;
    procedure OMAfterExit(var Msg : TMessage); message OM_AFTEREXIT;
    {VCL message methods}
    procedure CMVisibleChanged(var Msg : TMessage); message CM_VISIBLECHANGED;
    {windows message methods}
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg : TMessage); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
  protected
    {descendants can set the value of this variable after calling inherited }
    {create to set the default location and point-of-reference (POR) for the}
    {attached label. if dlpTopLeft, the default location and POR will be at }
    {the top left of the control. if dlpBottomLeft, the default location and}
    {POR will be at the bottom left}
    DefaultLabelPosition : TOvcLabelPosition;

    procedure DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt); dynamic;
    procedure CreateWnd; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function GetChildOwner: TComponent; override;
    procedure Loaded; override;

    property AfterEnter : TNotifyEvent read FAfterEnter write FAfterEnter;
    property AfterExit : TNotifyEvent read FAfterExit write FAfterExit;
    property OnMouseWheel : TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
//    property LabelInfo : TOvcLabelInfo read FLabelInfo write FLabelInfo;
  public
    {Collection streaming hooks:}
    procedure GetChildren(Proc: TGetChildProc; Root : TComponent); override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property AttachedLabel : TOvcAttachedLabel read GetAttachedLabel;
    property CollectionStreamer : TGEMOvcCollectionStreamer read FCollectionStreamer write FCollectionStreamer;
    property Internal : Boolean read FInternal write FInternal;
  published
    property About : string read GetAbout write SetAbout stored False;
  end;



  TGEMOvcDbColumnList = class(TGemOvcCustomControl)//TOvcCustomControlEx)
  protected {private}
    {property variables}
    FActiveRow         : Integer;     {selected item}
    FAutoRowHeight     : Boolean;     {true to handle row height calc}
    FBorderStyle       : TBorderStyle;{border style to use}
    FDataLink          : TFieldDataLink;
    FHeader            : string;      {the column header}
    FHeaderColor       : TGEMOvcColors;  {header line colors}
    FHideSelection     : Boolean;     {true to hide selection}
    FHighlightColors   : TGEMOvcColors;  {highlight colors}
    FIntegralHeight    : Boolean;     {adjust height based on font}
    FLineColor         : TColor;      {color of row divider lines}
    FPageScroll        : Boolean;     {true to scroll like Delphi's grids}
    FRowHeight         : Integer;     {height of one row}
    FRowIndicatorWidth : Integer;     {width of row indicators}
    FScrollBars        : TScrollStyle;{scroll bar style to use}
    FShowHeader        : Boolean;     {true to use the header}
    FShowIndicator     : Boolean;     {true to show row indicators}
    FTextMargin        : Integer;     {indent from left (right)}
    {event variables}
    FOnClickHeader     : THeaderClickEvent;
    FOnIndicatorClick  : TIndicatorClickEvent;
    FOnUserCommand     : TUserCommandEvent;
    {internal/working variables}
    clHDelta           : Integer;     {horizontal scroll delta}
    clIndicators       : TImageList;  {list of indicators}
    clNumRows          : Integer;     {visible rows in window}
    clPainting         : Boolean;     {True when painting}
    {property methods}
    function GetField : TField;
      {-return the associated field object}
    function GetDataField : string;
      {-return the field name}
    function GetDataSource : TDataSource;
      {-return the datasource}
    procedure SetActiveRow(Value : Integer);
      {-set the active row index}
    procedure SetAutoRowHeight(Value : Boolean);
      {-set use of auto row height calculations}
    procedure SetBorderStyle(const Value : TBorderStyle);
      {-set the style used for the border}
    procedure SetDataField(const Value : string);
      {-set the field name}
    procedure SetDataSource(Value : TDataSource);
      {-set the data source}
    procedure SetHeader(const Value : string);
      {-set the header at top of list box}
    procedure SetIntegralHeight(Value : Boolean);
      {-set use of integral font height adjustment}
    procedure SetLineColor(Value : TColor);
      {-set the color used to draw the row divider lines}
    procedure SetRowHeight(Value : Integer);
      {-set height of cell row}
    procedure SetRowIndicatorWidth(Value : Integer);
      {-set the row indicator width}
    procedure SetScrollBars(const Value : TScrollStyle);
      {-set use of vertical and horizontal scroll bars}
    procedure SetShowHeader(Value : Boolean);
      {-set the header at top of list box}
    procedure SetShowIndicator(Value : Boolean);
      {-set the show indicator option}
    procedure SetTextMargin(Value : Integer);
      {-set the text margin}
    {internal methods}
    procedure clAdjustIntegralHeight;
      {-adjust height of the list}
    procedure clAdjustRowHeight;
      {-adjust the row height based on the current font}
    procedure clCalcNumRows;
      {-calculate sizes based on font selection}
    procedure clColorChanged(AColor: TObject);
      {-a color has changed, refresh display}
    procedure clDrawHeader;
      {-draw the header and text area}
    procedure clSetHScrollPos;
      {-set the horizontal scroll position}
    procedure clSetHScrollRange;
      {-set the horizontal scroll range}
    procedure clSetVScrollPos;
      {-set the vertical scroll position}
    procedure clSetVScrollRange;
      {-set the vertical scroll range}
    procedure clInitScrollBarInfo;
      {-setup scroll bar range and initial position}
    procedure clUpdateActive;
      {-update the active record number}
    procedure clUpdateNumRows;
      {-update the number of rows}
    {datalink event handlers}
    procedure clActiveChange(Sender : TObject);
    procedure clDataChange(Sender : TObject);
    procedure clEditingChange(Sender : TObject);
    procedure clUpdateData(Sender : TObject);
    {VCL control messages}
    procedure CMCtl3DChanged(var Msg : TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMGetDataLink(var Msg : TMessage); message CM_GETDATALINK;
    {windows message methods}
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetMinMaxInfo(var Msg : TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMHScroll(var Msg : TWMScroll); message WM_HSCROLL;
    procedure WMKeyDown(var Msg : TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Msg : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseActivate(var Msg : TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCHitTest(var Msg : TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg : TWMScroll); message WM_VSCROLL;
  protected
    procedure ChangeScale(M, D : Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    {event wrappers}
    procedure DoOnClickHeader(Point : TPoint); dynamic;
    procedure DoOnIndicatorClick(Row : Integer); dynamic;
    procedure DoOnUserCommand(Command : Word); dynamic;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure InvalidateItem(Row : Integer);
      {-invalidate the area for this item}
    property ActiveRow : Integer read FActiveRow write SetActiveRow stored False;
    {public properties}
    property Canvas;
    property Field : TField read GetField;
  published
    {properties}
    property AutoRowHeight : Boolean read FAutoRowHeight write SetAutoRowHeight default True;
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property DataField : string read GetDataField write SetDataField;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
    property Header : string read FHeader write SetHeader;
    property HeaderColor : TGEMOvcColors read FHeaderColor write FHeaderColor;
    property HideSelection : Boolean read FHideSelection write FHideSelection default True;
    property HighlightColors : TGEMOvcColors read FHighlightColors write FHighlightColors;
    property IntegralHeight : Boolean read FIntegralHeight write SetIntegralHeight default True;
    property LineColor : TColor  read FLineColor write SetLineColor default clSilver;
    property PageScroll : Boolean read FPageScroll write FPageScroll default False;
    property RowHeight : Integer read FRowHeight write SetRowHeight default 17;
    property RowIndicatorWidth : Integer read FRowIndicatorWidth write SetRowIndicatorWidth default 11;
    property ScrollBars : TScrollStyle read FScrollBars write SetScrollBars default ssVertical;
    property ShowHeader : Boolean read FShowHeader write SetShowHeader default False;
    property ShowIndicator : Boolean read FShowIndicator write SetShowIndicator default True;
    property TextMargin : Integer read FTextMargin write SetTextMargin default 1;
    {inherited properties}
    property Anchors;
    property Constraints;
    property DragKind;
    property Align;
    property Color;
//    property Controller;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    {events}
    property OnClickHeader : THeaderClickEvent read FOnClickHeader write FOnClickHeader;
    property OnIndicatorClick : TIndicatorClickEvent read FOnIndicatorClick write FOnIndicatorClick;
    property OnUserCommand : TUserCommandEvent read FOnUserCommand write FOnUserCommand;
    {inherited events}
    property AfterEnter;
    property AfterExit;
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
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


implementation


{*** TOvcVirtualListBox ***}

procedure TGEMOvcDbColumnList.ChangeScale(M, D : Integer);
begin
  inherited ChangeScale(M, D);

  if M <> D then begin
    {scale row height}
    FRowHeight := MulDiv(FRowHeight, M, D);
    RecreateWnd;
  end;
end;

procedure TGEMOvcDbColumnList.clActiveChange(Sender : TObject);
begin
  clSetHScrollRange;
  clSetVScrollRange;
  Refresh;
end;

procedure TGEMOvcDbColumnList.clAdjustIntegralHeight;
begin
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    if FIntegralHeight then
      if ClientHeight mod FRowHeight <> 0 then
        ClientHeight := (ClientHeight div FRowHeight) * FRowHeight;
end;

procedure TGEMOvcDbColumnList.clAdjustRowHeight;
  {-adjust the row height based on the current font}
var
  DC         : hDC;
  SaveFont   : hFont;
  H          : Integer;
  SysMetrics : TTextMetric;
  Metrics    : TTextMetric;
begin
  if not FAutoRowHeight then
    Exit;

  if csLoading in ComponentState then
    Exit;

  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;

  if NewStyleControls then
    H := GetSystemMetrics(SM_CYBORDER) * 4
  else begin
    H := SysMetrics.tmHeight;
    if H > Metrics.tmHeight then
      H := Metrics.tmHeight;
    H := H div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;

  FRowHeight := Metrics.tmHeight + H;
end;

procedure TGEMOvcDbColumnList.clCalcNumRows;
begin
  if not HandleAllocated then
    Exit;

  clNumRows := (ClientHeight div FRowHeight)-Ord(FShowHeader);
  if clNumRows < 1 then
    clNumRows := 1;
end;

procedure TGEMOvcDbColumnList.clColorChanged(AColor: TObject);
  {-a color has changed, refresh display}
begin
  Refresh;
end;

procedure TGEMOvcDbColumnList.clDataChange(Sender : TObject);
begin
  if clPainting then
    Exit;
  {set the number of managed items}
  clUpdateNumRows;
  {update the active record number}
  clUpdateActive;
  {update the scroll bar position}
  clSetVScrollPos;
  Invalidate;
end;

procedure TGEMOvcDbColumnList.clDrawHeader;
  {-draw the header and text}
var
  R   : TRect;
  Buf : array[0..255] of Char;
  X,Y : Integer;
begin
  {get the header text}
  if FHeader = '' then begin
    if (Field <> nil) then
      StrPCopy(Buf, Field.DisplayLabel)
    else
      Buf[0] := #0;
  end else
    StrPLCopy(Buf, FHeader, 255);

  Canvas.Font := Font;
  Canvas.Brush.Color := FHeaderColor.BackColor;
  Canvas.Font.Color := FHeaderColor.TextColor;

  {draw header text}
  with Canvas do begin
    {if showing indicators, need to paint top/left bit}
    if ShowIndicator then begin
      R := Bounds(0, 0, RowIndicatorWidth, FRowHeight);

      {draw the indicator button face}
      DrawButtonFace(Canvas, R, 1, bsNew, False, False, False);

      {restore right edge}
      R := Bounds(RowIndicatorWidth, 0, Width-RowIndicatorWidth, FRowHeight-1);
      X :=  TextMargin + RowIndicatorWidth;
    end else begin
      R := Bounds(0, 0, Width, FRowHeight-1);
      X := TextMargin;
    end;

    {clear the line}
    Canvas.FillRect(R);

    Y := (FRowHeight - Canvas.TextHeight(GetOrphStr(SCTallLowChars))) div 2;
    if StrLen(Buf) > 0 then
      ExtTextOut(Canvas.Handle, X, Y, {ETO_OPAQUE or }ETO_CLIPPED,
                 @R, Buf, StrLen(Buf), nil);

    {draw border line}
    Pen.Color := clBlack;
    PolyLine([Point(R.Left, R.Bottom), Point(R.Right, R.Bottom)]);

    {draw ctl3d highlight}
    if Ctl3D then begin
      Pen.Color := clBtnHighlight;
      PolyLine([Point(R.Left, R.Bottom-1),
                Point(R.Left, R.Top),
                Point(R.Right, R.Top)]);
    end;
  end;
end;

procedure TGEMOvcDbColumnList.clEditingChange(Sender : TObject);
begin
  clSetHScrollRange;
  clSetVScrollRange;
end;

procedure TGEMOvcDbColumnList.clInitScrollBarInfo;
  {-setup scroll bar range and initial position}
begin
  if not HandleAllocated then
    Exit;

  {initialize scroll bars, if any}
  clSetVScrollRange;
  clSetVScrollPos;
  clSetHScrollRange;
  clSetHScrollPos;
end;

procedure TGEMOvcDbColumnList.clSetHScrollPos;
begin
  if FScrollBars in [ssHorizontal, ssBoth] then
    SetScrollPos(Handle, SB_HORZ, clHDelta, True);
end;

procedure TGEMOvcDbColumnList.clSetHScrollRange;
begin
  if FScrollBars in [ssHorizontal, ssBoth] then begin
    if Field <> nil then
      SetScrollRange(Handle, SB_HORZ, 0, Field.DisplayWidth, False)
    else
      SetScrollRange(Handle, SB_HORZ, 0, 1, False);
  end;
end;

procedure TGEMOvcDbColumnList.clSetVScrollPos;
var
  SIOld, SINew : TScrollInfo;
begin
  if not (FScrollBars in [ssVertical, ssBoth]) then
    Exit;

  if (FDataLink = nil) or not FDataLink.Active then
    Exit;

  if csLoading in ComponentState then
    Exit;

  with FDatalink.DataSet do begin
    SIOld.cbSize := SizeOf(SIOld);
    SIOld.fMask := SIF_ALL;
    GetScrollInfo(Self.Handle, SB_VERT, SIOld);
    SINew := SIOld;
    if IsSequenced then begin
      SINew.nMin := 1;
      SINew.nPage := clNumRows;
      SINew.nMax := Integer(DWORD(RecordCount) + SINew.nPage - 1);
      if State in [dsInactive, dsBrowse, dsEdit] then
        SINew.nPos := RecNo;  {else keep old pos}
    end else begin
      SINew.nMin := 0;
      SINew.nPage := 0;
      SINew.nMax := 4;
      if FDataLink.DataSet.BOF then
        SINew.nPos := 0
      else if FDataLink.DataSet.EOF then
        SINew.nPos := 4
      else
        SINew.nPos := 2;
    end;
    if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
       (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
      SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
  end;
end;

procedure TGEMOvcDbColumnList.clSetVScrollRange;
begin
  if csLoading in ComponentState then
    Exit;
end;

procedure TGEMOvcDbColumnList.clUpdateActive;
begin
  if csLoading in ComponentState then
    Exit;

  if (FDataLink <> nil) and FDataLink.Active then
    FActiveRow := FDataLink.ActiveRecord;
end;

procedure TGEMOvcDbColumnList.clUpdateData(Sender : TObject);
begin
  {}
end;

procedure TGEMOvcDbColumnList.clUpdateNumRows;
  {-update the number of rows}
begin
  if not HandleAllocated then
    Exit;

  clNumRows := ClientHeight div FRowHeight - Ord(FShowHeader);
  if clNumRows < 1 then
    clNumRows := 1;

  if FDataLink <> nil then
    if FDataLink.Active and (FDataLink.RecordCount > 0) then
      FDataLink.BufferCount := clNumRows;
end;

procedure TGEMOvcDbColumnList.CMCtl3DChanged(var Msg : TMessage);
begin
  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;

  inherited;
end;

procedure TGEMOvcDbColumnList.CMFontChanged(var Message: TMessage);
begin
  inherited;

  if (csLoading in ComponentState) or not HandleAllocated then
    Exit;

  {optionally, adjust the row height}
  clAdjustRowHeight;

  if FIntegralHeight then begin
    {determine the number of rows}
    clCalcNumRows;
    {optionally, adjust list height}
    clAdjustIntegralHeight;
  end;

  {determine the number of rows}
  clCalcNumRows;

  {set the scroll bar range}
  clInitScrollBarInfo;
end;

procedure TGEMOvcDbColumnList.CMGetDataLink(var Msg : TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

constructor TGEMOvcDbColumnList.Create(AOwner : TComponent);
var
  Bmp : Vcl.Graphics.TBitmap;
begin
  inherited Create(AOwner);

  if NewStyleControls then
    ControlStyle := ControlStyle + [csClickEvents]
  else
    ControlStyle := ControlStyle + [csClickEvents, csFramed];

  {set default values for inherited persistent properties}
  Color        := clWindow;
  Height       := 150;
  ParentColor  := False;
  TabStop      := True;
  Width        := 100;

  {create indicators object and load bitmaps}
  Bmp := Vcl.Graphics.TBitmap.Create;
  try
    Bmp.Handle := LoadBaseBitmap('ORDBARROW');
    clIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    clIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBaseBitmap('ORDBEDIT');
    clIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBaseBitmap('ORDBINSERT');
    clIndicators.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free;
  end;

  {set default values for new persistent properties}
  FActiveRow      := 0;
  FAutoRowHeight  := True;
  FBorderStyle    := bsSingle;
  FHeader         := '';
  FHideSelection  := True;
  FRowIndicatorWidth := 11;
  FIntegralHeight := True;
  FLineColor      := clSilver;
  FPageScroll     := False;
  FRowHeight      := 17;
  FScrollBars     := ssVertical;
  FShowHeader     := False;
  FShowIndicator  := True;
  FTextMargin     := 1;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;

  FDataLink.OnActiveChange  := clActiveChange;
  FDataLink.OnDataChange    := clDataChange;
  FDataLink.OnEditingChange := clEditingChange;
  FDataLink.OnUpdateData    := clUpdateData;

  {set defaults for internal variables}
  clHDelta := 0;

  {create and initialize color objects}
  FHeaderColor  := TGEMOvcColors.Create(clBtnText, clBtnFace);
  FHeaderColor.OnColorChange := clColorChanged;
  FHighlightColors := TGEMOvcColors.Create(clHighlightText, clHighlight);
  FHighlightColors.OnColorChange := clColorChanged;
end;

procedure TGEMOvcDbColumnList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
    Style := Integer(Style) or ScrollBarStyles[FScrollBars]
                   or BorderStyles[FBorderStyle];

  if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TGEMOvcDbColumnList.CreateWnd;
begin
  inherited CreateWnd;

  {optionally, adjust the row height}
  clAdjustRowHeight;

  if FIntegralHeight then begin
    {determine the number of rows}
    clCalcNumRows;
    {optionally, adjust list height}
    clAdjustIntegralHeight;
  end;

  {determine the number of rows}
  clCalcNumRows;

  {set the number of managed items}
  clUpdateNumRows;

  {set the scroll bars}
  clInitScrollBarInfo;
end;

destructor  TGEMOvcDbColumnList.Destroy;
begin
  {dispose of the color objects}
  FHeaderColor.Free;
  FHeaderColor := nil;
  FHighlightColors.Free;
  FHighlightColors := nil;

  FDataLink.Free;
  FDataLink := nil;

  clIndicators.Free;
  clIndicators := nil;

  inherited Destroy;
end;

procedure TGEMOvcDbColumnList.DoOnClickHeader(Point : TPoint);
begin
  if Assigned(FOnClickHeader) then
    FOnClickHeader(Self, Point);
end;

procedure TGEMOvcDbColumnList.DoOnIndicatorClick(Row : Integer);
  {-perform indicator click notification}
begin
  if not (csDesigning in ComponentState) and Assigned(FOnIndicatorClick) then
    FOnIndicatorClick(Self, Row);
end;

procedure TGEMOvcDbColumnList.DoOnUserCommand(Command : Word);
  {-perform notification of a user command}
begin
  if Assigned(FOnUserCommand) then
    FOnUserCommand(Self, Command);
end;

function TGEMOvcDbColumnList.GetField : TField;
begin
  Result := FDataLink.Field;

  {Result will be nil if the datasource is not active. At design-time}
  {the field information can be obtained if a corresponding field    }
  {component has been added to the form (by using the Fields Editor).}
  if (Result = nil) and (csDesigning in ComponentState) then
  begin
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    begin
      if DataSource.DataSet.Fields.LifeCycles <> [TFieldLifeCycle.lcAutomatic] then
        Result := DataSource.DataSet.FindField(FDataLink.FieldName);
    end;
  end;
end;

function TGEMOvcDbColumnList.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end;

function TGEMOvcDbColumnList.GetDataSource : TDataSource;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TGEMOvcDbColumnList.InvalidateItem(Row : Integer);
  {-invalidate the area for this item}
var
  CR : TRect;
begin
  if (Row >= 0) and (Row < clNumRows) then begin
    CR := Rect(0, (Row+Ord(FShowHeader))*FRowHeight, ClientWidth, 0);
    CR.Bottom := CR.Top+FRowHeight;
    InvalidateRect(Handle, @CR, True);
  end;
end;

procedure TGEMOvcDbColumnList.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TGEMOvcDbColumnList.Paint;
var
  CR          : TRect;
  IR, Clip    : TRect;
  I           : Integer;
  X, Y        : Integer;
  SaveActive  : Integer;
  Left        : Integer;
  Indicator   : Integer;
  P           : PChar;    //SZ FIXME
  Buf         : array[0..255] of Char;
begin
  {get the client area}
  CR := ClientRect;

  {get the cliping region}
  if csDesigning in ComponentState then
    Clip := CR
  else
    GetClipBox(Canvas.Handle, Clip);

  {do we have a header?}
  if FShowHeader then
    if Bool(IntersectRect(IR, Clip, Rect(CR.Left, CR.Top, CR.Right, FRowHeight))) then
      clDrawHeader;

  {set up the proper font and colors}
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if not Enabled and (Color <> clGrayText) then
    Canvas.Font.Color := clGrayText;

  {starting offset for text}
  X := FTextMargin;

  {remember the active row}
  SaveActive := FActiveRow;

  clPainting := True;
  try
    for I := 0 to Pred(clNumRows) do begin
      CR.Top := (I+Ord(FShowHeader))*FRowHeight;
      CR.Bottom := CR.Top+FRowHeight-1;
      if I = Pred(clNumRows) then
        CR.Bottom := ClientHeight;
      Y := CR.Top;

      if Bool(IntersectRect(IR, CR, Clip)) then begin

        {display row indicator if enabled}
        if FShowIndicator then begin
          CR.Left := FRowIndicatorWidth;
          DrawButtonFace(Canvas, Rect(0, CR.Top, FRowIndicatorWidth, CR.Bottom+1),
                         1, bsNew, False, False, False);
          Canvas.Brush.Color := Color;

          if (FDataLink <> nil) and FDataLink.Active and (I = SaveActive) then begin
            Indicator := 0;
            if FDataLink.DataSet <> nil then
              case FDataLink.DataSet.State of
                dsEdit   : Indicator := 1;
                dsInsert : Indicator := 2;
              end;
            clIndicators.BkColor := clBtnFace;
            clIndicators.Draw(Canvas, FRowIndicatorWidth-clIndicators.Width-3,
              (CR.Top + CR.Bottom - clIndicators.Height) shr 1, Indicator);
          end;
        end;

        if (FDataLink <> nil) and FDataLink.Active and (Field <> nil)
            and (I < FDataLink.RecordCount) then begin
          {change active db record}
          FDataLink.ActiveRecord := I;

          {get text to display}
          StrPCopy(Buf, Field.DisplayText);
        end else
          Buf[0] := #0;

        if Field <> nil then begin
          case Field.Alignment of
            taLeftJustify  : Left := X + CR.Left;
            taRightJustify : Left := ClientWidth - Canvas.TextWidth(StrPas(Buf)) - X;
          else
            Left := (ClientWidth - Canvas.TextWidth(StrPas(Buf))) div 2;
          end;
        end else
          Left := X + CR.Left;

        {change colors for active item}
        if (I = ActiveRow) and (Focused or not HideSelection) then begin
          Canvas.Brush.Color := HighlightColors.BackColor;
          Canvas.Font.Color := HighlightColors.TextColor;
        end;

        {adjust display string for horizontal scroll}
        P := @Buf[0];
        if clHDelta > 0 then begin
          if clHDelta < Integer(StrLen(Buf)) then
            P := @Buf[clHDelta]
          else
            P := '';
        end;

        {paint the string}
        Canvas.FillRect(CR);
        ExtTextOut(Canvas.Handle, Left, Y, ETO_CLIPPED, @CR, string(P), StrLen(P), nil);

        {restore colors for active items}
        if I = ActiveRow then begin
          Canvas.Brush.Color := Color;
          if not Enabled and (Color <> clGrayText) then
            Canvas.Font.Color := clGrayText
          else
            Canvas.Font.Color := Font.Color;
        end;
      end;
    end;

  finally
    {restore active record}
    if (FDataLink <> nil) and FDataLink.Active then
      FDataLink.ActiveRecord := SaveActive;

    {clear painting flag}
    clPainting := False;
  end;

  {paint any blank area below the last item}
  CR.Top := FRowHeight * (clNumRows+Ord(FShowHeader));
  if CR.Top < ClientHeight then begin
    CR.Bottom := ClientHeight;
    {clear the area}
    Canvas.FillRect(CR);
  end;

  {draw cell divider lines}
  if FShowIndicator then
    X := FRowIndicatorWidth
  else
    X := 0;
  Y := Ord(FShowHeader)*FRowHeight - 1;
  Canvas.Pen.Color := LineColor;
  if LineColor = clNone then
    Canvas.Pen.Color := Canvas.Brush.Color;
  for I := 0 to Pred(clNumRows)-1 do begin
    Inc(Y, FRowHeight);
    Canvas.PolyLine([Point(X, Y), Point(ClientWidth, Y)]);
  end;
end;

procedure TGEMOvcDbColumnList.SetActiveRow(Value : Integer);
  {-set the currently selected item}
begin
  if (FDataLink = nil) or not FDataLink.Active then
    Exit;

  if Value < 0 then
    Value := 0;
  if Value > Pred(clNumRows) then
    Value := Pred(clNumRows);

  if Value <> FActiveRow then
    FDataLink.DataSet.MoveBy(Value-FActiveRow);
end;

procedure TGEMOvcDbColumnList.SetAutoRowHeight(Value : Boolean);
  {-set use of auto row height calculations}
begin
  if Value <> FAutoRowHeight then begin
    FAutoRowHeight := Value;
    if FAutoRowHeight then
      RecreateWnd;
  end;
end;

procedure TGEMOvcDbColumnList.SetBorderStyle(const Value : TBorderStyle);
  {-set the style used for the border}
begin
  if Value <> FBorderStyle then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TGEMOvcDbColumnList.SetDataField(const Value : string);
begin
  try
    FDataLink.FieldName := Value;
  except
    FDataLink.FieldName := '';
    raise;
  end;
  Refresh;

  if csDesigning in ComponentState then
    RecreateWnd;
end;

procedure TGEMOvcDbColumnList.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  Refresh;
end;

procedure TGEMOvcDbColumnList.SetHeader(const Value : string);
  {-set the header at top of list box}
begin
  if Value <> FHeader then begin
    FHeader := Value;
    Repaint;
  end;
end;

procedure TGEMOvcDbColumnList.SetIntegralHeight(Value : Boolean);
  {-set use of integral font height adjustment}
begin
  if (Value <> FIntegralHeight) then begin
    FIntegralHeight := Value;
    RecreateWnd;
  end;
end;

procedure TGEMOvcDbColumnList.SetLineColor(Value : TColor);
  {-set the color used to draw the row divider lines}
begin
  if Value <> FLineColor then begin
    FLineColor := Value;
    Refresh;
  end;
end;

procedure TGEMOvcDbColumnList.SetRowHeight(Value : Integer);
  {-set height of cell row}
begin
  if (Value <> FRowHeight) and (Value > 0) then begin
    FRowHeight := Value;
    if not (csLoading in ComponentState) then
      FAutoRowHeight := False;
    RecreateWnd;
  end;
end;

procedure TGEMOvcDbColumnList.SetScrollBars(const Value : TScrollStyle);
  {-set use of vertical and horizontal scroll bars}
begin
  if Value <> FScrollBars then begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TGEMOvcDbColumnList.SetShowHeader(Value : Boolean);
  {-set show flag for the header}
begin
  if Value <> FShowHeader then begin
    FShowHeader := Value;
    Refresh;
  end;
end;

procedure TGEMOvcDbColumnList.SetShowIndicator(Value : Boolean);
  {-set the show indicators option}
begin
  if Value <> FShowIndicator then begin
    FShowIndicator := Value;
    if FShowIndicator then begin
      {at design-time, automatically set width}
      if (csDesigning in ComponentState) and (FRowIndicatorWidth = 0) then
        FRowIndicatorWidth := 11;
    end;

    Refresh;
  end;
end;

procedure TGEMOvcDbColumnList.SetRowIndicatorWidth(Value : Integer);
  {-set the row indicator width}
begin
  if (Value <> FRowIndicatorWidth) and (Value >= 0) then begin
    FRowIndicatorWidth := Value;

    {at design-time, automatically toggle the show state}
    if csDesigning in ComponentState then begin
      if FRowIndicatorWidth = 0 then
        FShowIndicator := False
      else
        FShowIndicator := True;
    end;

    Refresh;
  end;
end;

procedure TGEMOvcDbColumnList.SetTextMargin(Value : Integer);
  {-set the text margin}
begin
  if (Value <> FTextMargin) and (Value >= 0) then begin
    FTextMargin := Value;

    Refresh;
  end;
end;

procedure TGEMOvcDbColumnList.WMEraseBkgnd(var Msg : TWMEraseBkGnd);
begin
  {indicate that we have processed this message}
  Msg.Result := 1;
end;

procedure TGEMOvcDbColumnList.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
  inherited;

  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

procedure TGEMOvcDbColumnList.WMGetMinMaxInfo(var Msg : TWMGetMinMaxInfo);
begin
  Msg.MinMaxInfo^.ptMinTrackSize.Y := FRowHeight;
  Msg.Result := 0;
end;

procedure TGEMOvcDbColumnList.WMHScroll(var Msg : TWMHScroll);

  procedure HScrollPrim(Delta : Integer);
  var
    SaveD : Integer;
  begin
    SaveD := clHDelta;
    if Delta < 0 then
      if Abs(Delta) > clHDelta then
        clHDelta := 0
      else
        Inc(clHDelta, Delta)
    else
      if clHDelta+Delta > Field.DisplayWidth then
        clHDelta := Field.DisplayWidth
      else
        Inc(clHDelta, Delta);

    if clHDelta <> SaveD then begin
      clSetHScrollPos;
      Refresh;
    end;
  end;

begin
  if (Field = nil) then
    Exit;

  case Msg.ScrollCode of
    SB_LINERIGHT : HScrollPrim(+1);
    SB_LINELEFT  : HScrollPrim(-1);
    SB_PAGERIGHT : HScrollPrim(+10);
    SB_PAGELEFT  : HScrollPrim(-10);
    SB_THUMBPOSITION, SB_THUMBTRACK :
      if clHDelta <> Msg.Pos then begin
        clHDelta := Msg.Pos;
        clSetHScrollPos;
        Refresh;
      end;
  end;
end;

procedure TGEMOvcDbColumnList.WMKeyDown(var Msg : TWMKeyDown);
var
  Cmd : Word;
begin
  inherited;

  if (Field = nil) then
    Exit;

//  Cmd := Controller.EntryCommands.Translate(TMessage(Msg));
  cmd := ccNone;
  if Cmd <> ccNone then begin
    case Cmd of
      ccLeft :
        if clHDelta > 0 then begin
          Dec(clHDelta);
          clSetHScrollPos;
          Refresh;
        end;
      ccRight :
        if clHDelta < Field.DisplayWidth then begin
          Inc(clHDelta);
          clSetHScrollPos;
          Refresh;
        end;
      ccHome :
        begin
          clHDelta := 0;
          clSetHScrollPos;
          Refresh;
        end;
      ccEnd :
        begin
          clHDelta := Field.DisplayWidth;
          clSetHScrollPos;
          Refresh;
        end;
      ccUp        :
        Perform(WM_VSCROLL, SB_LINEUP, 0);
      ccDown      :
        Perform(WM_VSCROLL, SB_LINEDOWN, 0);
      ccFirstPage :
        Perform(WM_VSCROLL, MAKELONG(SB_THUMBPOSITION, 0), Parent.Handle);
      ccLastPage  :
        Perform(WM_VSCROLL, MAKELONG(SB_THUMBPOSITION, 4), Parent.Handle);
      ccPrevPage  :
        Perform(WM_VSCROLL, SB_PAGEUP, 0);
      ccNextPage  :
        Perform(WM_VSCROLL, SB_PAGEDOWN, 0);
    else
      {do user command notification for user commands}
      if Cmd >= ccUserFirst then
        DoOnUserCommand(Cmd);
    end;

    {indicate that this message was processed}
    Msg.Result := 0;
  end;
end;

procedure TGEMOvcDbColumnList.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;

  {re-draw focused item to erase highlihgt}
  InvalidateItem(ActiveRow);
end;

procedure TGEMOvcDbColumnList.WMLButtonDown(var Msg : TWMLButtonDown);
var
  P   : TPoint;
  Row : Integer;
begin
  inherited;

  {solve problem with minimized modeless dialogs and MDI child windows}
  {that contain virtual ListBox components}
  if not Focused and CanFocus then
    WinApi.Windows.SetFocus(Handle);

  {is this click on the header?}
  if FShowHeader and (Msg.YPos < FRowHeight) then begin
    DoOnClickHeader(Point(Msg.XPos, Msg.YPos));
    Exit;
  end;

  {determine which row the click was in}
  P.X := Msg.Pos.X;
  P.Y := Msg.Pos.Y;
  Row := P.Y div FRowHeight - Ord(FShowHeader);

  if (FDataLink <> nil) and FDataLink.Active and (FDataLink.RecordCount > 0) and
     (Row <> FDataLink.ActiveRecord) then begin
    {scroll the database as necessary}
    FDataLink.DataSet.MoveBy(Row - FDataLink.ActiveRecord);

    {update the active record number}
    clUpdateActive;
  end;

  {see if the click was in an indicator region}
  if FShowIndicator and (P.X <= FRowIndicatorWidth) then
    DoOnIndicatorClick(Row);
end;

procedure TGEMOvcDbColumnList.WMMouseActivate(var Msg : TWMMouseActivate);
begin
  if (csDesigning in ComponentState) or (GetFocus = Handle) then
    inherited
  else begin
//    if Controller.ErrorPending then
//      Msg.Result := MA_NOACTIVATEANDEAT
//    else
      Msg.Result := MA_ACTIVATE;
  end;
end;

procedure TGEMOvcDbColumnList.WMNCHitTest(var Msg : TWMNCHitTest);
begin
  if csDesigning in ComponentState then
    {don't call inherited so we can bypass vcl's attempt}
    {to trap the mouse hit}
    DefaultHandler(Msg)
  else
    inherited;
end;

procedure TGEMOvcDbColumnList.WMSetFocus(var Msg : TWMSetFocus);
begin
  inherited;

  {re-draw focused item to erase highlihgt}
  InvalidateItem(ActiveRow);
end;

procedure TGEMOvcDbColumnList.WMSize(var Msg : TWMSize);
begin
  inherited;

  {optionally, adjust list height}
  clAdjustIntegralHeight;
  {determine the number of rows}
  clCalcNumRows;

  {set the number of managed items}
  clUpdateNumRows;

  {update the active record number}
  clUpdateActive;

  {set the scroll bar range}
  clInitScrollBarInfo;
end;

procedure TGEMOvcDbColumnList.WMVScroll(var Msg : TWMVScroll);
var
  SI : TScrollInfo;
begin
  if (FDataLink <> nil) and FDataLink.Active then with FDataLink do begin
    case Msg.ScrollCode of
      SB_LINEUP        :
        if FPageScroll then
          DataSet.MoveBy(-FActiveRow-1)
        else
          DataSet.MoveBy(-1);
      SB_LINEDOWN      :
        if FPageScroll then
          DataSet.MoveBy(clNumRows-FActiveRow)
        else
          DataSet.MoveBy(+1);
      SB_PAGEUP        :
        DataSet.MoveBy(-clNumRows);
      SB_PAGEDOWN      :
        DataSet.MoveBy(+clNumRows);
      SB_THUMBPOSITION :
        begin
          if DataSet.IsSequenced then begin
            SI.cbSize := SizeOf(SI);
            SI.fMask := SIF_ALL;
            GetScrollInfo(Self.Handle, SB_VERT, SI);
            if SI.nTrackPos <= 1 then
              DataSet.First
            else if SI.nTrackPos >= DataSet.RecordCount then
              DataSet.Last
            else
              DataSet.RecNo := SI.nTrackPos;
          end else
          case Msg.Pos of
            0: DataSet.First;
            1: DataSet.MoveBy(-clNumRows);
            2: Exit;
            3: DataSet.MoveBy(clNumRows);
            4: DataSet.Last;
          end;
        end;
      SB_BOTTOM : DataSet.Last;
      SB_TOP    : DataSet.First;
    end;

    {update the active record number}
    clUpdateActive;
  end;

  Update;
end;

function TGEMOvcDbColumnList.ExecuteAction(Action : TBasicAction) : Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TGEMOvcDbColumnList.UpdateAction(Action : TBasicAction) : Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;



{*** TGemOvcCustomControl ***}
procedure TGemOvcCustomControl.CMVisibleChanged(var Msg : TMessage);
begin
  inherited;

  if csLoading in ComponentState then
    Exit;

//  if LabelInfo.Visible then
//    AttachedLabel.Visible := Visible;
end;

constructor TGemOvcCustomControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  DefaultLabelPosition := lpTopLeft;

//  FLabelInfo := TOvcLabelInfo.Create;
//  FLabelInfo.OnChange := LabelChange;
//  FLabelInfo.OnAttach := LabelAttach;
end;

procedure TGemOvcCustomControl.CreateWnd;
begin
  inherited CreateWnd;
end;

destructor TGemOvcCustomControl.Destroy;
begin
//  FLabelInfo.Visible := False;
//  FLabelInfo.Free;
//  FLabelInfo := nil;
  FCollectionStreamer.Free;
  FCollectionStreamer := nil;
  inherited Destroy;
end;

procedure TGemOvcCustomControl.DoOnMouseWheel(Shift : TShiftState;
                                 Delta, XPos, YPos : SmallInt);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, Delta, XPos, YPos);
end;

function TGemOvcCustomControl.GetAttachedLabel : TOvcAttachedLabel;
begin
//  if not FLabelInfo.Visible then
//    raise Exception.Create(GetOrphStr(SCLabelNotAttached));
//
//  Result := FLabelInfo.ALabel;
end;

function TGemOvcCustomControl.GetAbout : string;
begin
  Result := 'Ver 1.0 beta';//OrVersionStr;
end;

procedure TGemOvcCustomControl.LabelAttach(Sender : TObject; Value : Boolean);
var
  PF : TWinControl;
  S  : string;
begin
//  if (csLoading in ComponentState) then
//    Exit;
//
//  PF := GetImmediateParentForm(Self);
//  if Value then begin
//    if Assigned(PF) then begin
//      FLabelInfo.ALabel.Free;
//      FLabelInfo.ALabel := TOvcAttachedLabel.CreateEx(PF, Self);
//      FLabelInfo.ALabel.Parent := Parent;
//
//      S := GenerateComponentName(PF, Name + 'Label');
//      FLabelInfo.ALabel.Name := S;
//      FLabelInfo.ALabel.Caption := S;
//
//      FLabelInfo.SetOffsets(0, 0);
//      PositionLabel;
//      FLabelInfo.ALabel.BringToFront;
//
//      {force auto size}
//      FLabelInfo.ALabel.AutoSize := True;
//    end;
//  end else begin
//    if Assigned(PF) then begin
//      FLabelInfo.ALabel.Free;
//      FLabelInfo.ALabel := nil;
//    end;
//  end;
end;

procedure TGemOvcCustomControl.LabelChange(Sender : TObject);
begin
  if not (csLoading in ComponentState) then
    PositionLabel;
end;

procedure TGemOvcCustomControl.Notification(AComponent : TComponent; Operation : TOperation);
var
  PF : TWinControl;
begin
  inherited Notification(AComponent, Operation);

//  if Operation = opRemove then
//    if Assigned(FLabelInfo) and (AComponent = FLabelInfo.ALabel) then begin
//      PF := GetImmediateParentForm(Self);
//      if Assigned(PF) and not (csDestroying in PF.ComponentState) then begin
//        FLabelInfo.FVisible := False;
//        FLabelInfo.ALabel := nil;
//      end;
//    end;
end;

procedure TGemOvcCustomControl.OMAfterEnter(var Msg : TMessage);
begin
  if Assigned(FAfterEnter) then
    FAfterEnter(Self);
end;

procedure TGemOvcCustomControl.OMAfterExit(var Msg : TMessage);
begin
  if Assigned(FAfterExit) then
    FAfterExit(Self);
end;

procedure TGemOvcCustomControl.OMAssignLabel(var Msg : TMessage);
begin
//  FLabelInfo.ALabel := TOvcAttachedLabel(Msg.lParam);
end;

procedure TGemOvcCustomControl.OMPositionLabel(var Msg : TMessage);
const
  DX : Integer = 0;
  DY : Integer = 0;
begin
//  if FLabelInfo.Visible and
//     Assigned(FLabelInfo.ALabel) and
//     (FLabelInfo.ALabel.Parent <> nil) and
//     not (csLoading in ComponentState) then begin
//    if DefaultLabelPosition = lpTopLeft then begin
//      DX := FLabelInfo.ALabel.Left - Left;
//      DY := FLabelInfo.ALabel.Top + FLabelInfo.ALabel.Height - Top;
//    end else begin
//      DX := FLabelInfo.ALabel.Left - Left;
//      DY := FLabelInfo.ALabel.Top - Top - Height;
//    end;
//    if (DX <> FLabelInfo.OffsetX) or (DY <> FLabelInfo.OffsetY) then
//      PositionLabel;
//  end;
end;

procedure TGemOvcCustomControl.OMRecordLabelPosition(var Msg : TMessage);
begin
//  if Assigned(FLabelInfo.ALabel) and
//     (FLabelInfo.ALabel.Parent <> nil) then begin
//    {if the label was cut and then pasted, this will complete the re-attachment}
//    FLabelInfo.FVisible := True;
//
//    if DefaultLabelPosition = lpTopLeft then
//      FLabelInfo.SetOffsets(FLabelInfo.ALabel.Left - Left,
//        FLabelInfo.ALabel.Top + FLabelInfo.ALabel.Height - Top)
//    else
//      FLabelInfo.SetOffsets(FLabelInfo.ALabel.Left - Left,
//        FLabelInfo.ALabel.Top - Top - Height);
//  end;
end;

procedure TGemOvcCustomControl.PositionLabel;
begin
//  if FLabelInfo.Visible and Assigned(FLabelInfo.ALabel) and
//                           (FLabelInfo.ALabel.Parent <> nil) and
//                           not (csLoading in ComponentState) then begin
//
//    if DefaultLabelPosition = lpTopLeft then begin
//      FLabelInfo.ALabel.SetBounds(Left + FLabelInfo.OffsetX,
//                         FLabelInfo.OffsetY - FLabelInfo.ALabel.Height + Top,
//                         FLabelInfo.ALabel.Width, FLabelInfo.ALabel.Height);
//    end else begin
//      FLabelInfo.ALabel.SetBounds(Left + FLabelInfo.OffsetX,
//                         FLabelInfo.OffsetY + Top + Height,
//                         FLabelInfo.ALabel.Width, FLabelInfo.ALabel.Height);
//    end;
//  end;
end;

procedure TGemOvcCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if HandleAllocated then
    PostMessage(Handle, OM_POSITIONLABEL, 0, 0);
end;

procedure TGemOvcCustomControl.SetAbout(const Value : string);
begin
end;

procedure TGemOvcCustomControl.WMKillFocus(var Msg : TWMKillFocus);
begin
  inherited;

  PostMessage(Handle, OM_AFTEREXIT, 0, 0);
end;

procedure TGemOvcCustomControl.WMMouseWheel(var Msg : TMessage);
begin
  with Msg do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)) {fwKeys},
                   HIWORD(wParam) {zDelta},
                   LOWORD(lParam) {xPos},   HIWORD(lParam) {yPos});
end;

procedure TGemOvcCustomControl.WMSetFocus(var Msg : TWMSetFocus);
begin
  inherited;

  PostMessage(Handle, OM_AFTERENTER, 0, 0);
end;


{Logic for streaming collections of sub-components}

function TGemOvcCustomControl.GetChildOwner: TComponent;
begin
  if Assigned(FCollectionStreamer) then
    Result := FCollectionStreamer.Owner
  else
    Result := inherited GetChildOwner;
end;

procedure TGemOvcCustomControl.GetChildren(Proc: TGetChildProc; Root : TComponent);
begin
  if Assigned(FCollectionStreamer) then
    CollectionStreamer.GetChildren(Proc, Root)
  else
    inherited GetChildren(Proc, Root);
end;

procedure TGemOvcCustomControl.Loaded;
begin
  if Assigned(FCollectionStreamer) then
    FCollectionStreamer.Loaded;
  inherited Loaded;
end;


{ TGEMOvcColors }

procedure TGEMOvcColors.Assign(Source: TPersistent);
var
  C : TGEMOvcColors absolute Source;
begin
  if (Source <> nil) and (Source is TGEMOvcColors) then begin
    BackColor  := C.BackColor;
    TextColor  := C.TextColor;
  end else
    inherited Assign(Source);
end;

constructor TGEMOvcColors.Create(FG, BG: TColor);
begin
  inherited Create;

  cDefBackColor := BG;
  cDefTextColor := FG;
  FUseDefault   := True;

  {initialize to these colors}
  ResetToDefaultColors;
end;

procedure TGEMOvcColors.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  {define a UseDefault property for compatibility with eariler versions}
  Filer.DefineProperty('UseDefault', ReadUseDefault, nil, False);
end;

procedure TGEMOvcColors.DoOnColorChange;
begin
  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

procedure TGEMOvcColors.ReadUseDefault(Reader: TReader);
begin
  {read property and discard it}
  Reader.ReadBoolean;
end;

procedure TGEMOvcColors.ResetToDefaultColors;
  {-obtain default color values}
begin
  FBackColor := cDefBackColor;
  FTextColor := cDefTextColor;
end;

procedure TGEMOvcColors.SetBackColor(Value: TColor);
  {-set the color used for the background}
begin
  if Value <> FBackColor then begin
    if Value <> cDefBackColor then
      FUseDefault := False;
    FBackColor := Value;
    DoOnColorChange;
  end;
end;

procedure TGEMOvcColors.SetTextColor(Value: TColor);
begin
  if Value <> FTextColor then begin
    if Value <> cDefTextColor then
      FUseDefault := False;
    FTextColor := Value;
    DoOnColorChange;
  end;
end;

procedure TGEMOvcColors.SetUseDefault(Value: Boolean);
  {-set the flag to reset colors to parent default values}
begin
  FUseDefault := Value;
  if FUseDefault then begin
    ResetToDefaultColors;  {assign default values}
    DoOnColorChange;
  end;
end;

{ TGEMOvcCollectionStreamer }

procedure TGEMOvcCollectionStreamer.Clear;
var
  I : Integer;
begin
  for I := 0 to pred(FCollectionList.Count) do
    TOvcCollection(FCollectionList[I]).Clear;
end;

function TGEMOvcCollectionStreamer.CollectionFromType(
  Component: TComponent): TOvcCollection;
var
  I : Integer;
begin
  for I := 0 to pred(FCollectionList.Count) do
    if Component is TOvcCollection(FCollectionList[I]).ItemClass then begin
      Result := TOvcCollection(FCollectionList[I]);
      exit;
    end;
  raise Exception.Create(GetOrphStr(SCCollectionNotFound));
end;

constructor TGEMOvcCollectionStreamer.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FCollectionList := TList.Create;
end;

destructor TGEMOvcCollectionStreamer.Destroy;
begin
  FCollectionList.Free;
  FCollectionList := nil;

  inherited Destroy;
end;

procedure TGEMOvcCollectionStreamer.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I,J: Integer;
begin
  for I := 0 to pred(FCollectionList.Count) do
    with TOvcCollection(FCollectionList[I]) do
      if Stored then
        for J := 0 to Count - 1 do
          Proc(Item[J]);
end;

procedure TGEMOvcCollectionStreamer.Loaded;
var
  I : Integer;
begin
  for I := 0 to pred(FCollectionList.Count) do
    TOvcCollection(FCollectionList[I]).Loaded;
end;

end.
