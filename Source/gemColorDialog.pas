unit gemColorDialog;

interface

uses
  WinApi.Windows, WinApi.Messages,

  System.SysUtils, System.Classes,

  vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Grids,

  JvExControls, JvLabel, JvExExtCtrls, JvExtComponent, JvPanel,

  CnColorGrid;


type
  TgemColorDialog = class (TComponent)
  private
  { Private declarations }
    fTitle: string;
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
    property Title: string read fTitle write fTitle;
  end;


type
  TColorsDialog = class(TForm)
    panel_Bottom: TJvPanel;
    jvlbl_ColorLabel: TJvLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    panel_BackPanel: TJvPanel;
    cncolorgrid_ColorSelection: TCnColorGrid;
    procedure cncolorgrid_ColorSelectionMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure cncolorgrid_ColorSelectionSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fReturnColor: Boolean;
    fColor: tcolor;
    procedure CellCoordinatesFromMouse(X,Y: Integer; out gX,gY: Integer);
  public
    { Public declarations }
    property Color: tColor read fColor;
//    property ReturnColor: Boolean read fReturnColor;
//    function Execute: Boolean;
  end;

var
  ColorsDialog: TColorsDialog;

implementation
uses
  GEMColorsUnit, GEMUseFullRoutines;
{$R *.dfm}
const
  theBevelWidth = 2;



{ TgemColorDialog }

constructor TgemColorDialog.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TgemColorDialog.Destroy;
begin

  inherited;
end;

function TgemColorDialog.Execute: Boolean;
var
  ColorDialog: TColorsDialog;
begin
  ColorDialog := TColorsDialog.create(Self);
  try
    if ColorDialog.ShowModal = mrOk then begin
      fSelectedColor := ColorDialog.Color;
      result := true;
    end
    else
      result := false;
  finally
    FreeAndNil(ColorDialog);
  end;
end;


{ TColorsDialog }

procedure TColorsDialog.CellCoordinatesFromMouse(X, Y: Integer; out gX,
  gY: Integer);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CellCoordinatesFromMouse' );{$ENDIF}

  gX := Trunc((X - 2)/(cncolorgrid_ColorSelection.DefaultColWidth  + cncolorgrid_ColorSelection.GridLineWidth));
  gY := Trunc((Y - 2)/(cncolorgrid_ColorSelection.DefaultRowHeight + cncolorgrid_ColorSelection.GridLineWidth));

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CellCoordinatesFromMouse' );{$ENDIF}
end;


procedure TColorsDialog.cncolorgrid_ColorSelectionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  bColor: Tcolor;
  CellX, CellY: Integer;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'cncolorgrid_ColorSelectionMouseMove' );{$ENDIF}

  CellCoordinatesFromMouse(X ,Y ,CellX, CellY);
  bColor := GemColors[TGemColorNames((10 * CellY) + CellX)].Color;
  panel_Bottom.Color := bColor;

  jvlbl_ColorLabel.Caption := string(GetGEMColorName(bColor));
  jvlbl_ColorLabel.Font.Color := GetContrastColor(bColor);
  jvlbl_ColorLabel.Repaint;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'cncolorgrid_ColorSelectionMouseMove' );{$ENDIF}
end;


procedure TColorsDialog.cncolorgrid_ColorSelectionSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'cncolorgrid_ColorSelectionSelectCell' );{$ENDIF}

  cncolorgrid_ColorSelection.OnMouseMove := nil;
  ColorsDialog.Caption := string(GetGEMColorName(panel_Bottom.Color));
  fColor := GemColors[TGemColorNames((10 * ARow) + ACol)].Color;
//  fReturnColor := true;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'cncolorgrid_ColorSelectionSelectCell' );{$ENDIF}
end;


procedure TColorsDialog.FormActivate(Sender: TObject);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'FormActivate' );{$ENDIF}
//  showmessage('color dialog FormActivate');


  cncolorgrid_ColorSelection.OnMouseMove := cncolorgrid_ColorSelectionMouseMove;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'FormActivate' );{$ENDIF}
end;


procedure TColorsDialog.FormCreate(Sender: TObject);
var
  i: TGemColorNames;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'FormCreate' );{$ENDIF}
//  showmessage('color dialog create');
  for i := Low(GemColors) to High(GemColors) do
    cncolorgrid_ColorSelection.CustomColors.Add(IntToStr(GemColors[i].Color));

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'FormCreate' );{$ENDIF}
end;


end.
