unit ImageBtn;

interface
{.$Define USE_CODESITE}

uses
  Winapi.Windows, WinApi.Messages, Winapi.ShellAPI, WinApi.ShlObj,
  WinApi.KnownFolders, WinApi.URLMon, Winapi.ActiveX,


  System.StrUtils, System.Classes, System.SysUtils, System.Variants,
  System.Notification, system.UITypes, System.Win.Registry,


  VCL.Graphics, VCL.Controls, VCL.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Dialogs, VCL.Forms;

  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdSocks, IdFTP,
  IdUserPassProvider, IdCustomTransparentProxy, IdFTPCommon, IdHTTP, IdDICT,
  IdComponent, IdIntercept, IdIOHandler, IdIOHandlerStream, IdCookieManager,
  IdBaseComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,

  ColorButton,

  JvExExtCtrls, JvExtComponent, JvLabel

//{$IFDEF USE_CODESITE}
//  , CodeSiteLogging
//{$ENDIF}
  ;

type
  TImageBtn = class(TCustomPanel)
    fImage: TImage;
    fDrawing: Boolean; override;
  private
    function GetPicture: TPicture;
    function GetProportional: Boolean;
    function GetStretch: Boolean;
    function GetTransparent: Boolean;
    procedure SetPicture(const Value: TPicture);
    procedure SetProportional(const Value: Boolean);
    procedure SetStretch(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure PictureChanged(Sender: TObject);  override;
  protected
    procedure CreateWindowHandle(const Params: TCreateParams);
    function DoPaletteChange: Boolean; override;
  public
  	constructor Create(Aowner: TComponent);  override;
    destructor Destroy; override;
    property DockManager;
  published
//    FPicture: TPicture;
//    FOnProgress: TProgressEvent;
//    FOnFindGraphicClass: TFindGraphicClassEvent;
//    FStretch: Boolean;
//    FCenter: Boolean;
//    FIncrementalDisplay: Boolean;
//    FTransparent: Boolean;
//    FDrawing: Boolean;
//    FProportional: Boolean;
  //for image
    property Picture: TPicture read GetPicture write SetPicture;
    property Proportional: Boolean read GetProportional write SetProportional default false;
    property Stretch: Boolean read GetStretch write SetStretch default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Visible;
    property OnClick;
  // end for image

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    property StyleName;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

  end;




implementation


constructor TImageBtn.Create(Aowner: TComponent);
begin
  Inherited create(Aowner);
  Width := 73;
  Height := 73;
  BevelOuter := bvNone;
  TabOrder := 0;

  fImage := TImage.Create(self);
end; {End create}


destructor TImageBtn.Destroy;
begin
  fImage.Free;
  inherited;
end;


function TImageBtn.DoPaletteChange: Boolean;
begin
  inherited;
end;


function TImageBtn.GetPicture: TPicture;
begin
  result := fImage.Picture;
end;


procedure TImageBtn.SetPicture(const Value: TPicture);
begin
  fImage.Picture := Value;
end;


function TImageBtn.GetProportional: Boolean;
begin
  Result := fImage.Proportional;
end;


procedure TImageBtn.SetProportional(const Value: Boolean);
begin
  if fImage.Proportional <> Value then
  begin
    fImage.Proportional := Value;
    PictureChanged(Self);
  end;
end;


procedure TImageBtn.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  inherited;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkEdit(Observers) then
      TLinkObservers.EditLinkModified(Observers);

  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
	SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := fImage.Transparent;
    D := DestRect;
    if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
       (D.Right >= Width) and (D.Bottom >= Height) then
      ControlStyle := ControlStyle + [csOpaque]
    else  // picture might not cover entire clientrect
      ControlStyle := ControlStyle - [csOpaque];
    if DoPaletteChange and fDrawing then Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkIsEditing(Observers) then
      TLinkObservers.EditLinkUpdate(Observers);
end;



function TImageBtn.GetStretch: Boolean;
begin

end;

function TImageBtn.GetTransparent: Boolean;
begin

end;

procedure TImageBtn.SetStretch(const Value: Boolean);
begin

end;

procedure TImageBtn.SetTransparent(const Value: Boolean);
begin

end;

procedure TImageBtn.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  with fImage do begin
    Left := 1;
    Top := 1;
    Width := 71;
    Height := 71;
    Align := alClient;
    ExplicitLeft := 8;
    ExplicitTop := 8;
    ExplicitWidth := 105;
    ExplicitHeight := 105;
  end;

end;

end.
