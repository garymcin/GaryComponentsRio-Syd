unit GarysComponentsReg;

interface


procedure Register;

implementation
uses
  System.Classes,

 GEMUpdaterPanel, GemMRUList, SBPro, gemResistor, ModListnxQuery, ModListnxTable,
 GEMProcessTimer, gemArrow, SelColVisGrid, ColorPanelBtn, Preview, ColorButton,
 gemCustomButton, adpMRU, GEMDBLabel, URLLabel, FindFile, adpDBEdit, GemDBGrid,
 gemDBComboBox, gemDBLookupComboBox, PJVersionInfo, GEMColorButton,
{$IFDEF VER340}
  cnColorGrid,
{$ENDIF}
 gemPanelImageBtn, gemCapPanelBtn;


procedure Register;
begin
  RegisterComponents('Garys Stuff', [TGEMAppUpdater, TGemMruList, TStatusBarPro,
                                     TgemResistor, TGEMNxQuery, TGEMNxTable, TColorButton,
                                     TGEMProcessTimer, TgemArrow, TSelColVisGridBtn,
                                     TColorPanelSpecial, TPrintPreview, TThumbnailPreview,
                                     TPaperPreview, TgemShapeBtn, TadpMRU, TGEMDBLabel,
                                     TGEMUrlLabel, TFindFile, TGemDBGrid, TadpDBEdit,
                                     TgemDBComboBox, TgemDBLookupComboBox, TPJVersionInfo,
                                     TGEMColorButton,
                                   {$IFDEF VER340}
                                     TCnColorGrid,
                                   {$ENDIF}
                                     TgemCaptionBtnImagePanel, TgemCapPanelBtn]);


end;

end.
