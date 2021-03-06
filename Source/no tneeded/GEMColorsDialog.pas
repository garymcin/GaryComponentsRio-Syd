unit CustomColorsDialog;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  Vcl.ExtCtrls, Vcl.Buttons,

  JvExExtCtrls, JvExtComponent, JvPanel, JvExControls, JvLabel,

  GEMColorsUnit, CnColorGrid;

type
  TGEMColorDialog = class(TComponent)
  private
    fSelectedColor: tColor;
  published
    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
    function Execute: Boolean;
  public
    property SelectedColor: tcolor read fSelectedColor default clBlue;
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
  private
    { Private declarations }
    fSelectedColor: tcolor;
    fSelected: Boolean;
    procedure CellCoordinatesFromMouse(X,Y: Integer; out gX,gY: Integer);
  public
    { Public declarations }
    property SelectedColor: tcolor read fSelectedColor;
    property Selected: Boolean read fSelected;
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
  if not fSelected then
  begin
    panel_Bottom.Color := bColor;
  end;

  jvlbl_ColorLabel.Caption := string(GetGEMColorName(bColor));
  jvlbl_ColorLabel.Font.Color := GetContrastColor(bColor);
  jvlbl_ColorLabel.Repaint;
end;


procedure TGEMColorsDialog.cncolorgrid_ColorSelectionSelectCell(
                  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  cncolorgrid_ColorSelection.OnMouseMove := nil;
  Caption := string(GetGEMColorName(panel_Bottom.Color));
  fSelectedColor := GemColors[TGemColorNames((10 * ARow) + ACol)].Color;
  fSelected := True;
end;


procedure TGEMColorsDialog.FormCreate(Sender: TObject);
var
  i: TGemColorNames;
begin
  fSelected := false;
  cncolorgrid_ColorSelection.OnMouseMove := cncolorgrid_ColorSelectionMouseMove;
  for i := Low(GemColors) to High(GemColors) do
    cncolorgrid_ColorSelection.CustomColors.Add(IntToStr(GemColors[i].Color));
end;


{ TGEMColorDialog }

constructor TGEMColorDialog.Create(AOwner: TComponent);
begin
  inherited;
  fSelectedColor := clBlack;
end;


//destructor TGEMColorDialog.Destroy;
//begin
//
//  inherited;
//end;


function TGEMColorDialog.Execute: Boolean;
begin
  var GEMColorsDialog := TGEMColorsDialog.Create(Self);
  try
    if GEMColorsDialog.ShowModal = mrOK then
    begin
      result := True;
      fSelectedColor := GEMColorsDialog.SelectedColor;
    end
    else
      result := False;
  finally
    FreeAndNil(GEMColorsDialog);
  end;
end;


end.
