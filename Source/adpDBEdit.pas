unit adpDBEdit;


interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.Graphics, Windows, Messages,
  VCL.Forms, Vcl.Mask, Vcl.DBCtrls;

type
  TadpDBEdit = class(TDBEdit)
  private
    FOldBackColor : TColor;
    FColorOnEnter : TColor;
    FAlignment: TAlignment;
    FTabOnEnter: boolean;
    procedure SetAlignment(const Value: TAlignment);

    property OldBackColor : TColor read FOldBackColor write FOldBackColor;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;

    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property Alignment : TAlignment read FAlignment write SetAlignment;
    property ColorOnEnter :TColor read FColorOnEnter write FColorOnEnter;
    property TabOnEnter : boolean read FTabOnEnter write FTabOnEnter;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TadpDBEdit]);
//end;

{ TadpDBEdit }
procedure TadpDBEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);

  with Params do
    Style := Style or Alignments[FAlignment];
end; (*CreateParams*)


constructor TadpDBEdit.Create(AOwner: TComponent);
begin
  inherited;

  FColorOnEnter := Color;
  Alignment := taLeftJustify;
end; (*Create*)


procedure TadpDBEdit.DoEnter;
begin
  OldBackColor := Color;
  Color := ColorOnEnter;

  inherited;
end;(*DoEnter*)


procedure TadpDBEdit.DoExit;
begin
  Color := OldBackColor;

  inherited;
end; (*DoExit*)


procedure TadpDBEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end; (*SetAlignment*)


procedure TadpDBEdit.KeyPress(var Key: Char);
begin

{
  if TabOnEnter AND (Owner is TWinControl) then begin
    If Key = #13 Then Begin
      If HiWord(GetKeyState(VK_SHIFT)) <> 0 then
        SelectNext(Owner as TWinControl,False,True)
      else
        SelectNext(Owner as TWinControl,True,True) ;
      //Key := #0
    end;
  end;
 }

  if TabOnEnter AND (Owner is TWinControl) then
  begin
    if Key = Char(VK_RETURN) then
    begin
     if HiWord(GetKeyState(VK_SHIFT)) <> 0 then
        PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 1, 0)
     else
        PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 0, 0);
    end;
  end;
  inherited KeyPress(Key);
end; (*KeyPress*)

end.
