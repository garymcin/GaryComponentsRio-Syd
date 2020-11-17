unit URLLabel;

interface

uses
  Winapi.ShellAPI, Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Classes, System.UITypes,

   Vcl.StdCtrls, Vcl.Graphics, Vcl.ExtCtrls;

type
  TGEMUrlLabel = class(TCustomLabel)
  private
    { Private declarations }
  protected
    FCaption         : string;
    FHighlightColor  : TColor;
    FURL             : string;
    FUseVisitedColor : Boolean;
    FVisitedColor    : TColor;

    {internal variables}
    urlTimer         : TTimer;
    urlFontColor     : TColor;

    {property methods}
    function GetAbout : string;
    function GetUnderlineURL: Boolean;
    procedure SetAbout(const Value : string);
    procedure SetCaption(const Value : string);
    procedure SetHighlightColor(const Value : TColor);
    procedure SetUnderlineURL(Value: Boolean);
    procedure SetURL(const Value : string);
    procedure SetVisitedColor(const Value : TColor);

    {internal methods}
    procedure TimerEvent(Sender : TObject);

    procedure Loaded; override;

  protected
    procedure MouseMove(Shift : TShiftState; X, Y : Integer);
      override;

  public
    procedure Click; override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  published
    property About : string
      read GetAbout write SetAbout stored False;
    property Caption : string
      read FCaption write SetCaption;
    property HighlightColor : TColor
      read FHighlightColor write SetHighlightColor
      default clRed;
    property UnderlineURL: Boolean
      read GetUnderlineURL write SetUnderlineURL
      stored False;
    property URL : string
      read FURL write SetURL;
    property UseVisitedColor : Boolean
      read FUseVisitedColor write FUseVisitedColor
      default False;
    property VisitedColor : TColor
      read FVisitedColor write SetVisitedColor
      stored FUseVisitedColor
      default clBlack;

    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property DragKind;
    {$ENDIF}
    property Align;
    property Alignment;
    property AutoSize;
    property Color;
    property Cursor default crHandPoint;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent default False;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

//procedure Register;

implementation
uses
  System.Types;

const
  BadColor = $02000000;

{*** TGEMUrlLabel ***}

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMUrlLabel]);
//end;
//
procedure TGEMUrlLabel.Loaded;
begin
  inherited Loaded;

//  Font.Style := Font.Style + [fsUnderline];
  urlFontColor := BadColor;
end;

procedure TGEMUrlLabel.Click;
var
  Buf : array[0..1023] of Char;
begin
  if URL > '' then begin
    StrPLCopy(Buf, URL, Length(Buf)-1);
    if ShellExecute(0, 'open', Buf, '', '', SW_SHOWNORMAL) <= 32 then
      MessageBeep(0);
  end;

  inherited Click;

  {change color to visited color if enabled}
  if FUseVisitedColor then
    urlFontColor := FVisitedColor;
end;

constructor TGEMUrlLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FHighlightColor := clRed;
  Cursor := crHandPoint;
  Font.Style := Font.Style + [fsUnderline];
end;

destructor TGEMUrlLabel.Destroy;
begin
  if Assigned(urlTimer) then begin
    urlTimer.Free;
    urlTimer := nil;
  end;

  inherited Destroy;
end;

function TGEMUrlLabel.GetAbout : string;
begin
  //Result := OrVersionStr;
end;

function TGEMUrlLabel.GetUnderlineURL: Boolean;
begin
  result := fsUnderline in Font.Style;
end;

procedure TGEMUrlLabel.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if PtInRect(ClientRect, Point(X, Y)) then begin
    if not Assigned(urlTimer) then begin
      {save current font color}
      if urlFontColor = BadColor then
        urlFontColor := Font.Color;
      Font.Color := FHighlightColor;
      urlTimer := TTimer.Create(Self);
      urlTimer.Interval := 100;
      urlTimer.OnTimer := TimerEvent;
      urlTimer.Enabled := True;
    end;
  end;
end;

procedure TGEMUrlLabel.SetAbout(const Value : string);
begin
end;

procedure TGEMUrlLabel.SetCaption(const Value : string);
begin
  FCaption := Value;
  if FCaption > '' then
    inherited Caption := FCaption
  else
    inherited Caption := URL;
end;

procedure TGEMUrlLabel.SetHighlightColor(const Value: TColor);
begin
  if Value = clNone then
    FHighlightColor := Font.Color
  else
    FHighlightColor := Value;

  {reset stored color}
  urlFontColor := BadColor;
end;

{ - added}
procedure TGEMUrlLabel.SetUnderlineURL(Value: Boolean);
begin
  if Value then
    Font.Style := Font.Style + [fsUnderline]
  else
    Font.Style := Font.Style - [fsUnderline];
end;

procedure TGEMUrlLabel.SetURL(const Value : string);
begin
  FURL := Value;
  if FCaption = '' then
    inherited Caption := URL;
end;

procedure TGEMUrlLabel.SetVisitedColor(const Value : TColor);
begin
  if Value = clNone then
    FVisitedColor := Font.Color
  else
    FVisitedColor := Value;

  {reset stored color}
  urlFontColor := BadColor;
end;

procedure TGEMUrlLabel.TimerEvent(Sender : TObject);
var
  Pt : TPoint;
begin
  GetCursorPos(Pt);
  Pt := ScreentoClient(Pt);
  if not PtInRect(ClientRect, Pt) then begin
    urlTimer.Free;
    urlTimer := nil;
    Font.Color := urlFontColor;
    Repaint;
  end;
end;

end.
