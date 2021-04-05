unit Unit22;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, OverbyteIcsWndControl,
  OverbyteIcsWSocket, Vcl.ExtCtrls, JvExExtCtrls, JvExtComponent, JvPanel,
  JvShape;

type

  TTrafficLight = record

  end;

  TForm22 = class(TForm)
    tac: TPanel;
    DisplayMemo: TMemo;
    jvpnl_TrafficLights: TJvPanel;
    WSocket1: TWSocket;
    lbl1: TLabel;
    lbl2: TLabel;
    TrafficServerHostEdit: TEdit;
    TrafficServerPortEdit: TEdit;
    btn1: TButton;
    DisconnectButton: TButton;
    jvshp_RedLight: TJvShape;
    jvshp_YellowLight: TJvShape;
    jvshp_GreenLight: TJvShape;
    procedure btn1Click(Sender: TObject);
    procedure WSocket1SessionConnected(Sender: TObject; ErrCode: Word);
    procedure WSocket1DataAvailable(Sender: TObject; ErrCode: Word);
  private
    { Private declarations }
    trafficLightState: TTrafficLightState;
    procedure Display(Msg : String);
  public
    { Public declarations }
  end;

var
  Form22: TForm22;

implementation

{$R *.dfm}



procedure TForm22.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


procedure TForm22.btn1Click(Sender: TObject);
begin
  WSocket1.LineMode := TRUE;
  WSocket1.LineEnd  := #13#10;
  WSocket1.Proto    := 'tcp';
  WSocket1.Port     := TrafficServerPortEdit.Text;
  WSocket1.Addr     := TrafficServerHostEdit.Text;
  WSocket1.Connect;
end;


procedure TForm22.WSocket1DataAvailable(Sender: TObject; ErrCode: Word);
begin
  RcvBuf := WSocket1.ReceiveStr;

 { Remove end of line marker }
  if Length(RcvBuf) > 1 then
      SetLength(RcvBuf, Length(RcvBuf) - 2);
  Display('Received: ''' + RcvBuf + '''');
  { Remove unused blanks }
  RcvBuf := Trim(RcvBuf);
  { Split command and parameters }
  I := Pos(' ', RcvBuf);
  if I > 0 then begin
      Command := Copy(RcvBuf, 1, I - 1);
      Params  := Trim(Copy(RcvBuf, I + 1, Length(RcvBuf)));
  end
  else begin
      Command := RcvBuf;
      Params  := '';
  end;

  if CompareText(Command, 'STATE') = 0 then
    TrafficLight1.State := TTrafficLightState(StrToInt(Params))
  else if CompareText(Command, 'WELCOME') = 0 then begin
    Display('Welcome received.');
    WSocket1.SendStr('STATE ' + IntToStr(Ord(TrafficLight1.State)) + #13#10);
  end
  else if CompareText(Command, 'SORRY') = 0 then
    Display('Server is too busy.')
  else
    Display('** Unknown command received **');
end;


procedure TForm22.WSocket1SessionConnected(Sender: TObject; ErrCode: Word);
begin
  WSocket1.OnSessionConnected:
  if Error <> 0 then
    Display('Can''t connect to traffic server. Error #' + IntToStr(Error))
  else
    Display('Connected to traffic server.');
end;

end.
