unit DateLabel;

interface

uses
  Winapi.ShellAPI, Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Classes, System.UITypes,

  Vcl.StdCtrls, Vcl.Graphics, Vcl.ExtCtrls;

type
  TDateMasks = (df_None, df_dmy, df_ddmmyy, df_dddSdSofSmmmSyyyy, df_ddddSdSofSmmmmSyyyy,
                 df_ddddd, df_dddddd, df_c);
  TTimeMasks = (tf_None, tf_hmsz, tf_ffmmsszzz, tf_t, tf_tt);

  TGEMDateLabel = class(TCustomLabel)
  private
    procedure setDateTime(const Value: Extended);
    procedure SetDateMask(const Value: TDateMasks);
    procedure SetTimeMask(const Value: TTimeMasks);
    procedure SetPrefixStr(const Value: string);
    { Private declarations }
  protected
    FCaption         : string;
    FHighlightColor  : TColor;
    fDateMask        : TDateMasks;
    fTimeMask        : TTimeMasks;
    fPrefixStr       : string;
    {internal variables}

     fDateTime: Extended;

    {property methods}
    function GetAbout : string;
//    function GetUnderlineURL: Boolean;
    procedure SetAbout(const Value : string);
    procedure SetCaption;//(const Value : string);
    procedure SetHighlightColor(const Value : TColor);
    procedure SetVisitedColor(const Value : TColor);

    {internal methods}
    procedure Loaded; override;

  protected

  public
    procedure Click;
      override;
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;

    property Caption : string read FCaption;// write SetCaption;

  published
    property About : string
      read GetAbout write SetAbout stored False;
    property HighlightColor : TColor
      read FHighlightColor write SetHighlightColor
      default clRed;

    property DateTime: Extended read fDateTime write setDateTime;
    property DateMask: TDateMasks read fDateMask write SetDateMask;
    property TimeMask: TTimeMasks read fTimeMask write SetTimeMask;
    property PrefixStr: string read fPrefixStr write SetPrefixStr;

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

const
  cDateFormat: array[df_None..df_c] of string  = (' ', 'd/m/y', 'dd/mm/yy', 'ddd d of mmm yyyy',
                                                 'dddd d of mmmm yyyy', 'ddddd',
                                                 'dddddd', 'c');
  cTimeFormat: array[tf_None..tf_tt] of string  = (' ', 'h:m:s.z', 'hh:mm:ss.zzz', 't',
                                                 'tt');
  c_UnassignedDate = -693594;

//procedure Register;

implementation
uses
  System.Types;

const
  BadColor = $02000000;

{*** TGEMUrlLabel ***}

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMDateLabel]);
//end;


procedure TGEMDateLabel.Loaded;
begin
  inherited Loaded;

//  Font.Style := Font.Style + [fsUnderline];
//  urlFontColor := BadColor;
end;

procedure TGEMDateLabel.Click;
//var
//  Buf : array[0..1023] of Char;
begin
//  if URL > '' then begin
//    StrPLCopy(Buf, URL, Length(Buf)-1);
//    if ShellExecute(0, 'open', Buf, '', '', SW_SHOWNORMAL) <= 32 then
//      MessageBeep(0);
//  end;

  inherited Click;

  {change color to visited color if enabled}
//  if FUseVisitedColor then
//    urlFontColor := FVisitedColor;
end;

constructor TGEMDateLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FHighlightColor := clRed;
  Cursor := crHandPoint;
  Font.Style := Font.Style + [fsUnderline];
end;

destructor TGEMDateLabel.Destroy;
begin
//  if Assigned(urlTimer) then begin
//    urlTimer.Free;
//    urlTimer := nil;
//  end;

  inherited Destroy;
end;

function TGEMDateLabel.GetAbout : string;
begin
  //Result := OrVersionStr;
end;

//function TGEMDateLabel.GetUnderlineURL: Boolean;
//begin
//  result := fsUnderline in Font.Style;
//end;

//procedure TGEMDateLabel.MouseMove(Shift : TShiftState; X, Y : Integer);
//begin
//  inherited MouseMove(Shift, X, Y);
//
//  if PtInRect(ClientRect, Point(X, Y)) then begin
//    if not Assigned(urlTimer) then begin
//      {save current font color}
//      if urlFontColor = BadColor then
//        urlFontColor := Font.Color;
//      Font.Color := FHighlightColor;
//      urlTimer := TTimer.Create(Self);
//      urlTimer.Interval := 100;
//      urlTimer.OnTimer := TimerEvent;
//      urlTimer.Enabled := True;
//    end;
//  end;
//end;

procedure TGEMDateLabel.SetAbout(const Value : string);
begin
end;

procedure TGEMDateLabel.SetDateMask(const Value: TDateMasks);
begin
  fDateMask := Value;
  SetCaption;
end;

procedure TGEMDateLabel.SetCaption;//(const Value : string);
var
  aCaption: string;
begin
  if fDateTime = c_UnassignedDate then begin
    inherited Caption := FCaption;
  end
  else begin
    aCaption := fPrefixStr+' '+FormatDateTime(cDateFormat[fDateMask]+' '+cTimeFormat[fTimeMask]  , fDateTime);
    aCaption := trim(aCaption);
    inherited Caption := aCaption;
  end;
//
//
//  FCaption := Value;
//  if FCaption > '' then
//    inherited Caption := FCaption
//  else
//    inherited Caption := URL;
end;


procedure TGEMDateLabel.setDateTime(const Value: extended);
begin
  fDateTime := Value;
  SetCaption;
end;


procedure TGEMDateLabel.SetHighlightColor(const Value: TColor);
begin
//  if Value = clNone then
//    FHighlightColor := Font.Color
//  else
//    FHighlightColor := Value;
//
//  {reset stored color}
//  urlFontColor := BadColor;
end;

procedure TGEMDateLabel.SetPrefixStr(const Value: string);
begin
  fPrefixStr := Value;
  SetCaption;
end;

procedure TGEMDateLabel.SetTimeMask(const Value: TTimeMasks);
begin
  fTimeMask := Value;
  SetCaption;
end;

{ - added}
//procedure TGEMDateLabel.SetUnderlineURL(Value: Boolean);
//begin
//  if Value then
//    Font.Style := Font.Style + [fsUnderline]
//  else
//    Font.Style := Font.Style - [fsUnderline];
//end;

//procedure TGEMDateLabel.SetURL(const Value : string);
//begin
//  FURL := Value;
//  if FCaption = '' then
//    inherited Caption := URL;
//end;

procedure TGEMDateLabel.SetVisitedColor(const Value : TColor);
begin
//  if Value = clNone then
//    FVisitedColor := Font.Color
//  else
//    FVisitedColor := Value;
//
//  {reset stored color}
//  urlFontColor := BadColor;
end;

//procedure TGEMDateLabel.TimerEvent(Sender : TObject);
//var
//  Pt : TPoint;
//begin
//  GetCursorPos(Pt);
//  Pt := ScreentoClient(Pt);
//  if not PtInRect(ClientRect, Pt) then begin
//    urlTimer.Free;
//    urlTimer := nil;
//    Font.Color := urlFontColor;
//    Repaint;
//  end;
//end;
end.
