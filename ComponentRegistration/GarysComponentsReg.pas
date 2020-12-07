unit GarysComponentsReg;

interface


procedure Register;

implementation
uses
  System.Classes,

 GEMUpdaterPanel, GemMRUList, SBPro, gemResistor, ModListnxQuery, ModListnxTable,
 GEMProcessTimer, gemArrow, SelColVisGrid, ColorPanelBtn, Preview, ColorButton,
 gemCustomButton, adpMRU, GEMDBLabel, URLLabel, FindFile, adpDBEdit, GemDBGrid,
 gemDBComboBox, gemDBLookupComboBox, PJVersionInfo, GEMColorButton, cnColorGrid,
 gemPanelImageBtn;


//  System.Classes, AboutFindFile, adpDBDateTimePicker,  adpEdit,
//  adpInstanceControl, GEMCalendar, ColorButton, ColorButtonSpecial,
//  ,  DBTabControl, ExtentedDBGrid,
//  GEMadpDBCheckBox, GEMDBGridExtended, SelColVisGrid,
//  GEMDBMaskEdit, GemInputLabel,
//  ModListnxQuery, ModListnxTable, TGEMListBx, WindowMng, DateLabel,
//  URLLabel, ;

procedure Register;
begin
  RegisterComponents('Garys Stuff', [TGEMAppUpdater, TGemMruList, TStatusBarPro,
                                     TgemResistor, TGEMNxQuery, TGEMNxTable, TColorButton,
                                     TGEMProcessTimer, TgemArrow, TSelColVisGridBtn,
                                     TColorPanelSpecial, TPrintPreview, TThumbnailPreview,
                                     TPaperPreview, TgemShapeBtn, TadpMRU, TGEMDBLabel,
                                     TGEMUrlLabel, TFindFile, TGemDBGrid, TadpDBEdit,
                                     TgemDBComboBox, TgemDBLookupComboBox, TPJVersionInfo,
                                     TGEMColorButton, TCnColorGrid, TgemCaptionBtnImagePanel]);



//  RegisterComponents('Garys Stuff', [TGEMFindFile, TadpDBDateTimePicker,
//                                     TadpEdit, TadpInstanceControl,
//                                     TColorButton, TColorButtonSpecial,
//                                     TDBTabControl, TExtendedDBGrid, TGEMadpDBCheckBox,
//
//                                      TGEMDBGridExtended, TGEMInputLabel,
//                                     TGEMInputButton,  ,
//                                     TGEMDBMaskEdit
//
//                                     , TGEMListBox, TWindowManager,
//                                     TGEMDateLabel, TGEMUrlLabel, {, TEnhDBGrid,
//                                     TGEMDBGridExtendedNew}]);
end;

end.
