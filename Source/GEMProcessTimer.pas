unit GEMProcessTimer;

interface

uses
  Winapi.Windows,

  System.SysUtils, System.Classes, System.Diagnostics,

  GEMComponentsGlobal;


type
  TState = (stStarted, stStopped);
  TStateChangeEvent = procedure(sender: TObject; State: TState) of object;

  TGEMProcessTimer = class(TComponent)
  private
    { Private declarations }
    FStartTime,
    FStopTime       :  DWord;

    FOnStart,
    FOnStop         : TNotifyEvent;

    FOnStateChanged : TStateChangeEvent;
    FState          : TState;
    FVersion        : string;
    fMsg            : string;
    fStopWatch      : tStopWatch;
    fStopWatchElapsedTime: int64;

    function SetVersion: string;
  protected
    { Protected declarations }
    function GetElpasedTime: string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    //destructor Destroy;

    procedure Start;
    procedure Stop;

    property StartTime: DWord read FStartTime;
    property StopTime: DWord read FStopTime;
    property ElapsedTime: string read GetElpasedTime;
    property State: TState read FState;
  published
    { Published declarations }
    property Msg: string read fMsg write fMsg;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnStateChange: TStateChangeEvent read FOnStateChanged
                                              write FOnStateChanged;
    property Version: string read SetVersion;

  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TGEMProcessTimer]);
end;


{ TProcessTimer }

constructor TGEMProcessTimer.Create(AOwner: TComponent);
begin
  inherited;
  FState := stStopped;
  fMsg := '';
end;


function TGEMProcessTimer.GetElpasedTime: string;
begin
  if fMsg = '' then
    Result := IntToStr(fStopWatchElapsedTime) +' milliseonds or '+
                     Format('%.16f Seconds', [(FStopTime - FStartTime)/ System.SysUtils.MSecsPerSec])
  else
    Result := fMsg+ '--'+IntToStr(fStopWatchElapsedTime) +' milliseonds or '+
                     Format('%.16f Seconds', [(FStopTime - FStartTime)/ System.SysUtils.MSecsPerSec]);
//  if fMsg = '' then
//    Result := IntToStr(FStopTime - FStartTime) +' milliseonds or '+
//                     Format('%.16f Seconds', [(FStopTime - FStartTime)/ 1000])
//  else
//    Result := fMsg+ '--'+IntToStr(FStopTime - FStartTime) +' milliseonds or '+
//                     Format('%.16f Seconds', [(FStopTime - FStartTime)/ 1000]);
end;


function TGEMProcessTimer.SetVersion: string;
begin
  FVersion := '1.0';
  result := '1.0';
end;


procedure TGEMProcessTimer.Start;
begin
  FState := stStarted;

  if Assigned(OnStart) then
    OnStart(Self);

  if Assigned(OnStateChange) then
    OnStateChange(Self, State);

  FStartTime := GetTickCount64;
  fStopWatch := TStopwatch.StartNew;
end;


procedure TGEMProcessTimer.Stop;
begin
  FStopTime := GetTickCount64;
  fStopWatchElapsedTime := fStopWatch.ElapsedMilliseconds;
  FState := stStopped;

  if Assigned(OnStop) then
    OnStop(Self);

  if Assigned(OnStateChange) then
    OnStateChange(Self, State);
end;

end.
