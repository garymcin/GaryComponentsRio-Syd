 unit GarysComponentsReg;

interface


procedure Register;

implementation
uses
 System.Classes,
 GEMUpdaterPanel, GemMRUList, SBPro, gemResistor, ModListnxQuery, ModListnxTable,
 GEMProcessTimer, gemArrow, SelColVisGrid, ColorPanelBtn, Preview, ColorButton,
 gemCustomButton, adpMRU, GEMDBLabel, URLLabel, FindFile, adpDBEdit, GemDBGrid,
 gemDBComboBox, gemDBLookupComboBox, PJVersionInfo, GEMColorButton, GEMTrafficLight,
 GEMTrafficLight2, uAutoComplete,  gemCapPanelBtn, gemColorDialog
 {$IF CompilerVersion = 34.0}, CnColorGrid {$EndIF};


procedure Register;
begin
{$IF CompilerVersion = 34.0}
  var VerType := 'Garys Stuff Syd';
{$IFEND}
{$IF CompilerVersion = 33.0}
  var VerType := 'Garys Stuff Rio';
{$IFEND}
  RegisterComponents(VerType, [TGEMAppUpdater, TGemMruList, TStatusBarPro,
                                     TgemResistor, TGEMNxQuery, TGEMNxTable, TColorButton,
                                     TGEMProcessTimer, TgemArrow, TSelColVisGridBtn,
                                     TColorPanelSpecial, TPrintPreview, TThumbnailPreview,
                                     TPaperPreview, TgemShapeBtn, TadpMRU, TGEMDBLabel,
                                     TGEMUrlLabel, TFindFile, TGemDBGrid, TadpDBEdit,
                                     TgemDBComboBox, TgemDBLookupComboBox, TPJVersionInfo,
                                     TGEMColorButton, TGEMTrafficLight, TGEMTrafficLight2,
                                     TgemCapPanelBtn, TAutoCompleteEdit,
                                     TGEMShape, TGEMShape2, TgemColorDialog
                                     {$IF CompilerVersion = 34.0}, TCnColorGrid,
                                     TCnColorGridPanel {$EndIF}]);
end;

end.
