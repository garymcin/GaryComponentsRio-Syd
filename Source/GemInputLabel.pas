unit GemInputLabel;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Classes,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms;

type
  TGEMInputButton = class(TButton)
  private
    procedure WmChar (var Msg: TWMChar);
      message wm_Char;
  end;



  TGEMInputLabel = class(TLabel)
  private
    { Private declarations }
    MyEdit: TEdit;
    //MyEdit: TInputEdit;
    procedure WMLButtonDown (var Msg: TMessage);
      message wm_LButtonDown;
  protected
    { Protected declarations }
    procedure EditChange (Sender: TObject);
    procedure EditExit (Sender: TObject);
  public
    { Public declarations }
    constructor Create (AOwner: TComponent); override;
  published
    { Published declarations }
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMInputLabel]);
//  RegisterComponents('Gary"s Stuff', [TGEMInputButton]);
//end;


{ TInputButton }

procedure TGEMInputButton.WmChar(var Msg: TWMChar);
var
  Temp: String;
begin
  if Char (Msg.CharCode) = #8 then
  begin
    Temp := Caption;
    Delete (Temp, Length (Temp), 1);
    Caption := Temp;
  end
  else
    Caption := Caption + Char (Msg.CharCode);
end;


{ TInputLabel }

constructor TGEMInputLabel.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);

  //MyEdit          := TInputEdit.Create (AOwner);
  MyEdit          := TEdit.Create (AOwner);
  MyEdit.Parent   := AOwner as TForm;
  MyEdit.Width    := 0;
  MyEdit.Height   := 0;
  MyEdit.TabStop  := False;
  MyEdit.OnChange := EditChange;
  MyEdit.OnExit   := EditExit;
end;


procedure TGEMInputLabel.EditChange(Sender: TObject);
begin
  Caption := MyEdit.Text;
  Invalidate;
  Update;
  (Owner as TForm).Canvas.DrawFocusRect (BoundsRect);
end;


procedure TGEMInputLabel.EditExit(Sender: TObject);
begin
  (Owner as TForm).Invalidate;
end;


procedure TGEMInputLabel.WMLButtonDown(var Msg: TMessage);
begin
  MyEdit.SetFocus;
  MyEdit.Text := Caption;
  (Owner as TForm).Canvas.DrawFocusRect (BoundsRect);
end;


begin
  RegisterClasses ([TEdit]);
end.
