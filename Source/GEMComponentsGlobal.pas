unit GEMComponentsGlobal;

interface

type

  tGEMComponents = (gemGEMDBGrid, gemGEMDBGridEdit, gemSelColVisGrid,
                    gemColorButtonSpecial, gemColorPanelBtnSpecial, gemGEMColorBtnSpecial,
                    gemGEMnxTable, gemGEMnxQuery, gemAdpMRU, gemGEMProcessTimer,
                    gemGEMArrow, gemUpdater, gemCapPanelBtn);

  TGEMComponentsVersion = record
//    fGEMComponent: tGEMComponents;
    fVersionNum: string;
    FileName: string;
    ComponentName: string;
//    function GetVersion(aComponent: TGEMComponents): string;
  private
    procedure SetVersionNum(const Value: string; aGEMComponent: tGEMComponents);
  public
    property VersionNum: string read fVersionNum write SetVersionNum;
//    property GEMComponent: tGEMComponents read fGEMComponent write fGEMComponent;
  end;


//  TComponentVersioning = class
//  private
//    TheComponenetsVersion: array of TGEMComponentsVersion;
//  public
//
//    //property ComponentVersion: TGEMComponentsVersion  read F write Set;
//  end;
const
  Company = 'GEM Components: ';
  VersionGEMDBGrid = Company + '1.1';        //yes
  VersionGEMDBGridEdit = Company + '2.0';    //yes
  VersionSelColVisGrid = Company + '1.0';
  VersionColorButtonSpecial = Company + '1.0';
  VersionColorPanelBtnSpecial = Company + '1.0';  // yes
  VersionGEMColorBtnSpecial = Company + '1.1';   //yes
  VersionGEMnxTable = Company + '1.0';
  VersionGEMnxQuery = Company + '1.0';   //yes
  VersionAdpMRU = Company + '1.2';
  VersionGEMProcessTimer = Company + '1.0';
  VersionGEMArrow = Company + '1.1';
  VersionUpdater = 'Updater Ver: 1.0.1';
  VersionCapPanelBtn = '1.0.0';

//  var
//    fComponents: array[gemGEMDBGrid..gemUpdater] of TGEMComponentsVersion;

//function GetComponentVersion(aComponent: tGEMComponents): fComponents;
function ComponentVersion(aComponent: tGEMComponents): string;

implementation


function ComponentVersion(aComponent: tGEMComponents): string;
begin
  case aComponent of
    gemGEMDBGrid: begin
      Result:=   VersionGEMDBGrid;        //yes
;
    end;

    gemGEMDBGridEdit: begin
      Result:= VersionGEMDBGridEdit;
    end;

    gemSelColVisGrid: begin
       Result:= VersionSelColVisGrid;
   end;

    gemColorButtonSpecial: begin
      Result:= VersionColorButtonSpecial;
    end;

    gemColorPanelBtnSpecial: begin
      Result:= VersionColorPanelBtnSpecial;
    end;

    gemGEMColorBtnSpecial: begin
      Result:= VersionGEMColorBtnSpecial;
    end;

    gemGEMnxTable: begin
      Result:= VersionGEMnxTable;
    end;

    gemGEMnxQuery: begin
      Result:= VersionGEMnxQuery;
    end;

    gemAdpMRU: begin
      Result:= VersionAdpMRU;
    end;

    gemGEMProcessTimer: begin
      Result:= VersionGEMProcessTimer;
    end;

    gemGEMArrow: begin
      Result:= VersionGEMArrow;
    end;

    gemUpdater: begin
      Result:= VersionUpdater;
    end;

    gemCapPanelBtn: begin
      Result:= VersionCapPanelBtn;
    end;
  end;
end;



//const
//  AboutFindFile in 'Source\AboutFindFile.pas',
//  adpDBDateTimePicker in 'Source\adpDBDateTimePicker.pas',
//  adpDBEdit in 'Source\adpDBEdit.pas',
//  adpMRU in 'Source\adpMRU.pas',
//  ColorButton in 'Source\ColorButton.pas',
//  FindFile in 'Source\FindFile.pas',
//  GEMadpDBCheckBox in 'Source\GEMadpDBCheckBox.pas',
//  gemArrow in 'Source\gemArrow.pas',
//  gemCustomButton in 'Source\gemCustomButton.pas',
//  GEMDBLabel in 'Source\GEMDBLabel.pas',
//  GEMDBMaskEdit in 'Source\GEMDBMaskEdit.pas',
//  gemResistor in 'Source\gemResistor.pas',
//  GwWebBrowserTamed in 'Source\GwWebBrowserTamed.pas',
//  GwWebBrowserTamedReg in 'Source\GwWebBrowserTamedReg.pas',
//  PJEnvVars in 'Source\PJEnvVars.pas',
//  Preview in 'Source\Preview.pas',
//  SBPro in 'Source\SBPro.pas',
//  TGEMListBx in 'Source\TGEMListBx.pas',
//  gemDBLookupComboBox in 'Source\gemDBLookupComboBox.pas',
//  gemDBComboBox in 'Source\gemDBComboBox.pas',
//  adpEdit in 'Source\adpEdit.pas',
//  URLLabel in 'Source\URLLabel.pas',
//  MemoBar in 'Source\MemoBar.pas',
//  GarysComponentsReg in 'Source\GarysComponentsReg.pas',
//  AutoCompletEditReg in 'Source\AutoCompletEditReg.pas',
//  ExtentedDBGrid in 'Source\ExtentedDBGrid.pas',
//  GEMProcessTimer in 'Source\GEMProcessTimer.pas',
//  GemInputLabel in 'Source\GemInputLabel.pas',
//  DBControlLists in 'Source\DBControlLists.pas',
//  adpInstanceControl in 'Source\adpInstanceControl.pas',
//  MouseDBGrid in 'Source\MouseDBGrid.pas',
//  uAutoComplete in 'Source\uAutoComplete.pas',
//  Mydbp in 'Source\Mydbp.pas',
//  WindowMng in 'Source\WindowMng.pas' {/u_TEnhDBGrid in 'Source\u_TEnhDBGrid.pas';},
//  GEMComponentsGlobal in 'GEMComponentsGlobal.pas';


{ TGEMComponentsVersion }

procedure TGEMComponentsVersion.SetVersionNum(const Value: string);
begin
  fVersionNum := Value;
end;

end.
