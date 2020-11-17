unit adpEdit;


{
Article: 

Advanced editing - TadpEdit component

http://delphi.about.com/library/weekly/aa120603a.htm

Full source code of a TadpEdit Delphi component, an extension
to the standard TEdit control with properties like: ColorOnEnter
(changes the background color of the control when it receives the
input focus), Alignment (determines how the text in the edit component
is aligned), and TabOnEnter (allows the edit control to react on
the Enter key press as if the Tab key was pressed - sending the
focus to the next control in the tab order).
}

interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.Graphics, Windows, Messages,
  VCL.Forms;

type
  TadpEdit = class(TEdit)
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
//  RegisterComponents('Gary"s Stuff', [TadpEdit]);
//end;

{ TadpEdit }
procedure TadpEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);

  with Params do
    Style := Style or Alignments[FAlignment];
end; (*CreateParams*)

constructor TadpEdit.Create(AOwner: TComponent);
begin
  inherited;

  FColorOnEnter := Color;
  Alignment := taLeftJustify;
end; (*Create*)


procedure TadpEdit.DoEnter;
begin
  OldBackColor := Color;
  Color := ColorOnEnter;

  inherited;
end;(*DoEnter*)

procedure TadpEdit.DoExit;
begin
  Color := OldBackColor;

  inherited;
end; (*DoExit*)


procedure TadpEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end; (*SetAlignment*)


procedure TadpEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

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
end; (*KeyPress*)

end.
