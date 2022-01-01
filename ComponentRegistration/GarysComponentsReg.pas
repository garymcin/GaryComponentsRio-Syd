th unit GarysComponentsReg;

interface


procedure Register;

implementation
uses
 System.Classes,
 GEMUpdaterPanel, GemMRUList, SBPro, gemResistor, ModListnxQuery, ModListnxTable,
 GEMProcessTimer, gemArrow, SelColVisGrid, ColorPanelBtn, Preview, ColorButton,
 gemCustomButton, adpMRU, GEMDBLabel, URLLabel, FindFile, adpDBEdit, GemDBGrid,
 gemDBComboBox, gemDBLookupComboBox, PJVersionInfo, GEMTrafficLight,
 GEMTrafficLight2, uAutoComplete,  gemCapPanelBtn, GEMCustomColorsDialog, GEMCustomBtns;


procedure Register;
begin
{$IF CompilerVersion = 35.0}
  var VerType := 'Garys Stuff Delphi 11';
{$IFEND}
{$IF CompilerVersion = 34.0}
  var VerType := 'Garys Stuff Syd';
{$IFEND}
{$IF CompilerVersion = 33.0}
  var VerType := 'Garys Stuff Rio';
{$IFEND}
  RegisterComponents(VerType, [TGEMAppUpdater, TGemMruList, TStatusBarPro,
                               TgemResistor, TGEMNxQuery, TGEMNxTable,
                               TGEMProcessTimer, TgemArrow, TColorButton,
                               TColorPanelSpecial, TPrintPreview, TThumbnailPreview,
                               TPaperPreview, TgemShapeBtn, TadpMRU, TGEMDBLabel,
                               TGEMUrlLabel, TFindFile, TGemDBGrid, TadpDBEdit,
                               TgemDBComboBox, TgemDBLookupComboBox, TPJVersionInfo,
                               TGEMColorButton, TGEMTrafficLight, TGEMTrafficLight2,
                               TgemCapPanelBtn, TAutoCompleteEdit, TGEMDBGridColumnsIniStorage,
                               TGEMShape, TGEMShape2, TGEMColorDialog,
                               TGEMAddredssColorButton, TSaveDBGridColProps]);
end;

end.
