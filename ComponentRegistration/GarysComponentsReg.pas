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
 GEMTrafficLight2, uAutoComplete,  gemCapPanelBtn {$IFDEF VER330}, cnColorGrid{$ENDIF};


procedure Register;
begin
{$IFDEF VER340}
  RegisterComponents('Garys Stuff Syd', [TGEMAppUpdater, TGemMruList, TStatusBarPro,
                                     TgemResistor, TGEMNxQuery, TGEMNxTable, TColorButton,
                                     TGEMProcessTimer, TgemArrow, TSelColVisGridBtn,
                                     TColorPanelSpecial, TPrintPreview, TThumbnailPreview,
                                     TPaperPreview, TgemShapeBtn, TadpMRU, TGEMDBLabel,
                                     TGEMUrlLabel, TFindFile, TGemDBGrid, TadpDBEdit,
                                     TgemDBComboBox, TgemDBLookupComboBox, TPJVersionInfo,
                                     TGEMColorButton, TGEMTrafficLight, TGEMTrafficLight2, TgemCapPanelBtn,
                                     TAutoCompleteEdit, TGEMShape, TGEMShape2]);
{$ELSEIF VER330 }
  RegisterComponents('Garys Stuff Rio', [TGEMAppUpdater, TGemMruList, TStatusBarPro,
                                     TgemResistor, TGEMNxQuery, TGEMNxTable, TColorButton,
                                     TGEMProcessTimer, TgemArrow, TSelColVisGridBtn,
                                     TColorPanelSpecial, TPrintPreview, TThumbnailPreview,
                                     TPaperPreview, TgemShapeBtn, TadpMRU, TGEMDBLabel,
                                     TGEMUrlLabel, TFindFile, TGemDBGrid, TadpDBEdit,
                                     TgemDBComboBox, TgemDBLookupComboBox, TPJVersionInfo,
                                     TGEMColorButton, TGEMTrafficLight, TGEMTrafficLight2,
                                     TCnColorGrid, TgemCapPanelBtn, TAutoCompleteEdit,
                                     TGEMShape, TGEMShape2]);
{$ENDIF}

end;

end.
