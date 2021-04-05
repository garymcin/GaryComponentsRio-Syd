unit Calendar;

interface

uses
  Winapi.Windows, Winapi.Messages,

  {System.SysUtils, System.Variants,} System.Classes,

  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Graphics;

type
//  TGEMMonthCalendar = class(TMonthCalendar)
//
//  end;



  TCalendarForm = class(TPanel)
    OkButton: TButton;
    CancelBtn: TButton;
    DateFieldCalendar: TMonthCalendar;

    procedure CancelBtnClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
    fDate: TDate;
    FOnClick_OKButton: TNotifyEvent;
    FOnClick_CancelButton: TNotifyEvent;

    procedure SetDate(const Value: TDate);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick_OKButton: TNotifyEvent read FOnClick_OKButton write FOnClick_OKButton;
    property OnClick_CancelButton: TNotifyEvent read FOnClick_CancelButton write FOnClick_CancelButton;
    property Date: TDate read fDate write SetDate;
    property Visible;
  end;

//var
//  CalendarForm: TCalendarForm;

implementation

{.$R *.dfm}

constructor TCalendarForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Visible := False;
  Left := 39;
  Top := 24;
  Width := 231;
  Height := 193;
  BevelOuter := bvNone;
  TabOrder := 0;

  GrpPanel := TPanel.Create(Self);
  GrpPanel.Parent := Self;

  DateFieldCalendar := TMonthCalendar.Create(Self);
  DateFieldCalendar.Parent := Self;

  OkButton := TButton.Create(Self);
  OkButton.Parent := GrpPanel;

  CancelBtn := TButton.Create(Self);
  CancelBtn.Parent := GrpPanel;
end;


destructor TCalendarForm.Destroy;
begin
  inherited;
end;


procedure TCalendarForm.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  with DateFieldCalendar do begin
    Left := 1;
    Top := 1;
    Width := 229;
    Height := 162;
    Align := alClient;
    Date := 42426.167505752320000000;
    TabOrder := 0;
    //ExplicitLeft := 0;
    //ExplicitTop := 0;
    //ExplicitWidth := 225;
    //ExplicitHeight := 160;
  end;

  with GrpPanel do begin
    Left := 1;
    Top := 163;
    Width := 229;
    Height := 29;
    Align := alBottom;
    Color := clMoneyGreen;
    ParentBackground := False;
    TabOrder := 1;
    //ExplicitLeft := 0;
    //ExplicitWidth := 238;
  end;

  with OkButton do begin
    Left := 4;
    Top := 2;
    Width := 75;
    Height := 25;
    Caption := 'Ok';
    TabOrder := 0;
    OnClick := OkButtonClick;
  end;

  with CancelBtn do begin
    Left := 82;
    Top := 2;
    Width := 75;
    Height := 25;
    Caption := 'Cancel';
    TabOrder := 1;
    OnClick := CancelBtnClick;
  end;
end;


procedure TCalendarForm.CancelBtnClick(Sender: TObject);
{ Transfers the CancelButton OnClick event to the outside world. }
begin
  if assigned(FOnClick_CancelButton) then
    FOnClick_CancelButton(Self); { Substitute Self for subcomponent's Sender. }
end; { Click_OKButtonTransfer }


procedure TCalendarForm.OkButtonClick(Sender: TObject);
{ Transfers the OKButton OnClick event to the outside world. }
begin
  if assigned(FOnClick_OKButton) then
    FOnClick_OKButton(Self);  { Substitute Self for subcomponent's Sender. }
end;


procedure TCalendarForm.SetDate(const Value: TDate);
begin
  fDate := Value;
end;

end.
