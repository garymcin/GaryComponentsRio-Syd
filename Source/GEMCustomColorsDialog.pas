unit GEMCustomColorsDialog;

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  Vcl.ExtCtrls, Vcl.Buttons,

  JvExExtCtrls, JvExtComponent, JvPanel, JvExControls, JvLabel,

  GEMColorsUnit, CnColorGrid;

type
  TgemColorDialog = class (TComponent)
  private
  { Private declarations }
    fColorName: string;
    fSelectedColor: tColor;
  protected
  { Protected declarations }
  public
  { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
  { Published declarations }
    property SelectedColor: tColor read fSelectedColor write fSelectedColor;
    property ColorName: string read fColorName write fColorName;
  end;



  TGEMColorsDialog = class(TForm)
    panel_Bottom: TJvPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    panel_BackPanel: TJvPanel;
    jvlbl_ColorLabel: TJvLabel;
    cncolorgrid_ColorSelection: TCnColorGrid;
    procedure FormCreate(Sender: TObject);
    procedure cncolorgrid_ColorSelectionSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure cncolorgrid_ColorSelectionMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    fColor: tcolor;
    fColorName: string;
    procedure CellCoordinatesFromMouse(X,Y: Integer; out gX,gY: Integer);
  public
    { Public declarations }
    property SelectedColor: tcolor read fColor;
    property ColorName: string read fColorName write fColorName;
  end;

var
  GEMColorsDialog: TGEMColorsDialog;

implementation
uses
  {Global,} GEMUseFullRoutines;
{$R *.dfm}
const
  theBevelWidth = 2;

procedure TGEMColorsDialog.CellCoordinatesFromMouse(X,Y: Integer; out gX,gY: Integer);
begin
  gX := Trunc((X - 2)/(cncolorgrid_ColorSelection.DefaultColWidth +cncolorgrid_ColorSelection.GridLineWidth));
  gY :=Trunc((Y - 2)/(cncolorgrid_ColorSelection.DefaultRowHeight +cncolorgrid_ColorSelection.GridLineWidth));
end;


procedure TGEMColorsDialog.cncolorgrid_ColorSelectionMouseMove(Sender: TObject;
                                             Shift: TShiftState; X, Y: Integer);
var
  bColor: Tcolor;
  CellX, CellY: Integer;
begin
  CellCoordinatesFromMouse(X ,Y ,CellX, CellY);
  bColor := GemColors[TGemColorNames((10 * CellY) + CellX)].Color;

  Caption := string(GetGEMColorName(bColor));
end;


procedure TGEMColorsDialog.cncolorgrid_ColorSelectionSelectCell(
                  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  cncolorgrid_ColorSelection.OnMouseMove := nil;
  Caption := string(GetGEMColorName(panel_Bottom.Color));
  fColor := GemColors[TGemColorNames((10 * ARow) + ACol)].Color;
  panel_Bottom.Color := fColor;
  BitBtn1.Enabled := true;
  jvlbl_ColorLabel.Caption := string(GetGEMColorName(fColor));
  jvlbl_ColorLabel.Font.Color := GetContrastColor(fColor);
  fColorName := jvlbl_ColorLabel.Caption;
  jvlbl_ColorLabel.Repaint;
end;


procedure TGEMColorsDialog.FormActivate(Sender: TObject);
begin
  cncolorgrid_ColorSelection.OnMouseMove := cncolorgrid_ColorSelectionMouseMove;
end;


procedure TGEMColorsDialog.FormCreate(Sender: TObject);
var
  i: TGemColorNames;
begin
  for i := Low(GemColors) to High(GemColors) do
    cncolorgrid_ColorSelection.CustomColors.Add(IntToStr(GemColors[i].Color));
end;


{ TGEMColorDialog }

constructor TGEMColorDialog.Create(AOwner: TComponent);
begin
  inherited;
  fSelectedColor := clBlack;
end;


destructor TGEMColorDialog.Destroy;
begin

  inherited;
end;


function TGEMColorDialog.Execute: Boolean;
begin
  GEMColorsDialog := TGEMColorsDialog.Create(Self);
  try
    if GEMColorsDialog.ShowModal = mrOK then
    begin
      result := True;
      fSelectedColor := GEMColorsDialog.SelectedColor;
      fColorName     := GEMColorsDialog.ColorName;
    end
    else
      result := False;
  finally
    FreeAndNil(GEMColorsDialog);
  end;
end;


end.
