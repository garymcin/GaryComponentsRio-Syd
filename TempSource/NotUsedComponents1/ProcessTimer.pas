unit ProcessTimer;

interface

uses
  Winapi.Windows,

  System.SysUtils, System.Classes;


type
  TState = (stStarted, stStopped);
  TStateChangeEvent = procedure(sender: TObject; State: TState) of object;

  TGEMProcessTimer = class(TComponent)
  private
    { Private declarations }
    FStartTime,
    FStopTime:  DWord;

    FOnStart,
    FOnStop: TNotifyEvent;

    FOnStateChanged: TStateChangeEvent;
    FState: TState;
  protected
    { Protected declarations }
    function GetElpasedTime: string; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);
    //destructor Destroy;

    procedure Start;
    procedure Stop;

    property StartTime: DWord read FStartTime;
    property StopTime: DWord read FStopTime;
    property ElapsedTime: string read GetElpasedTime;
    property State: TState read FState;
  published
    { Published declarations }
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnStateChange: TStateChangeEvent read FOnStateChanged
                                              write FOnStateChanged;

  end;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TGEMProcessTimer]);
end;


{ TProcessTimer }

constructor TGEMProcessTimer.Create(AOwner: TComponent);
begin
  FState := stStopped;
end;


function TGEMProcessTimer.GetElpasedTime: string;
var
  s: string;
begin
  Result := s +' milliseonds or '+ Format('%.2f Seconds', [(FStopTime - FStartTime)/ 1000]);
end;


procedure TGEMProcessTimer.Start;
begin
  FState := stStarted;

  if Assigned(OnStart) then
    OnStop(Self);

  if Assigned(OnStateChange) then
    OnStateChange(Self, State);

  FStartTime := GetTickCount;
end;


procedure TGEMProcessTimer.Stop;
begin
  FStopTime := GetTickCount;
  FState := stStopped;

  if Assigned(OnStop) then
    OnStop(Self);

  if Assigned(OnStateChange) then
    OnStateChange(Self, State);
end;


end.
