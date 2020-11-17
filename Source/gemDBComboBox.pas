unit gemDBComboBox;


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
  VCL.Forms,  DB, VCL.DbCtrls, VCL.ComCtrls;

type
  TgemDBComboBox = class(TDBComboBox)
  private
    FOldBackColor : TColor;
    FColorOnEnter : TColor;
    FTabOnEnter: boolean;
    property OldBackColor : TColor read FOldBackColor write FOldBackColor;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property ColorOnEnter :TColor read FColorOnEnter write FColorOnEnter;
    property TabOnEnter : boolean read FTabOnEnter write FTabOnEnter;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TgemDBComboBox]);
//end;

{ TgemDBComboBox }

constructor TgemDBComboBox.Create(AOwner: TComponent);
begin
  inherited;

  FColorOnEnter := Color;
end; (*Create*)


procedure TgemDBComboBox.DoEnter;
begin
  OldBackColor := Color;
  Color := ColorOnEnter;

  inherited;
end;(*DoEnter*)


procedure TgemDBComboBox.DoExit;
begin
  Color := OldBackColor;

  inherited;
end; (*DoExit*)


procedure TgemDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  {
  if TabOnEnter AND (Owner is TWinControl) then begin
    If Key = #13 Then Begin
      If HiWord(GetKeyState(VK_SHIFT)) <> 0 then
        SelectNext(Owner as TWinControl,False,True)
      else
        SelectNext(Owner as TWinControl,True,True) ;
      Key := #0
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

end; (*KeyPress*)

end.
