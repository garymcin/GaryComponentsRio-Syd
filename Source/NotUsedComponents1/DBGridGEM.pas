unit DBGridGEM;

interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.Graphics, Windows, Messages,
  VCL.Forms, VCL.Grids, VCL.DBGrids;

type
  TDBGridGEM = class(TDBGrid)
  private
    fGridInfoFile : string;
    //FColorOnEnter : TColor;
    //FAlignment: TAlignment;
    //FTabOnEnter: boolean;
    //procedure SetAlignment(const Value: TAlignment);

    //property OldBackColor : TColor read FOldBackColor write FOldBackColor;
  protected
    //procedure DoEnter; override;
    //procedure DoExit; override;
    //procedure KeyPress(var Key: Char); override;

    //procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property GridInfoFile : string read fGridInfoFile write fGridInfoFile;
    //property ColorOnEnter :TColor read FColorOnEnter write FColorOnEnter;
    //property TabOnEnter : boolean read FTabOnEnter write FTabOnEnter;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TDBGridGEM]);
end;



constructor TDBGridGEM.Create(AOwner: TComponent);
begin
  inherited;

  fGridInfoFile := '';
  //Alignment := taLeftJustify;
end; (*Create*)

end.
