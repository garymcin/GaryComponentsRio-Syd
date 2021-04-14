unit GEMComponentsGlobal;

interface

type
  tGEMComponents = (com_GEMAppUpdater, com_GemMruList, com_StatusBarPro,
                    com_gemResistor, com_GEMNxQuery, com_GEMNxTable,
                    com_ColorButton, com_GEMProcessTimer, com_gemArrow,
                    com_SelColVisGridBtn, com_ColorPanelSpecial, com_PrintPreview,
                    com_ThumbnailPreview, com_PaperPreview, com_gemShapeBtn,
                    com_adpMRU, com_GEMDBLabel, com_GEMUrlLabel,
                    com_FindFile, com_GemDBGrid, com_adpDBEdit,
                    com_gemDBComboBox, com_gemDBLookupComboBox, com_PJVersionInfo,
                    com_GEMColorButton, com_GEMTrafficLight, com_GEMTrafficLight2,
                    com_gemCapPanelBtn, com_AutoCompleteEdit, com_GEMShape);



type
  TGEMVerStrRec = record
    Version: String;
    ComponentName: string;
  end;


  TGEMComponentsVersion = class
    fGEMComponent: tGEMComponents;
//    fVersionNum: string;
//    FileName: string;
//    fComponentName: string;
  private
    procedure SetVersionInfo(const Value: TGEMVerStrRec);
    function GetVersionInfo: TGEMVerStrRec;
    function GetComponentName: string;
    function GetVersionNum: string;
  public
    property Version: string read GetVersionNum;
    property Component: string read GetComponentName;
    property VersionInfo: TGEMVerStrRec read GetVersionInfo write SetVersionInfo;
  end;

//function ComponentVersion(aComponent: tGEMComponents): string;

implementation

const
  GEMVerInfo : array [tGEMComponents] of TGEMVerStrRec = (
    (Version: '1.4.1'; ComponentName: 'GEMAppUpdater'),
    (Version: '1.0.0'; ComponentName: 'GemMruList'),
    (Version: '1.0.0'; ComponentName: 'StatusBarPro'),
    (Version: '1.0.0'; ComponentName: 'gemResistor'),
    (Version: '1.0.0'; ComponentName: 'GEMNxQuery'),
    (Version: '1.0.0'; ComponentName: 'GEMNxTable'),
    (Version: '1.0.0'; ComponentName: 'ColorButton'),
    (Version: '1.0.0'; ComponentName: 'GEMProcessTimer'),
    (Version: '1.0.0'; ComponentName: 'gemArrow'),
    (Version: '1.0.0'; ComponentName: 'SelColVisGridBt'),
    (Version: '1.0.0'; ComponentName: 'ColorPanelSpecia'),
    (Version: '1.0.0'; ComponentName: 'PrintPreview'),
    (Version: '1.0.0'; ComponentName: 'ThumbnailPreview'),
    (Version: '1.0.0'; ComponentName: 'PaperPreview'),
    (Version: '1.0.0'; ComponentName: 'gemShapeBt'),
    (Version: '1.0.0'; ComponentName: 'adpMRU'),
    (Version: '1.0.0'; ComponentName: 'GEMDBLabel'),
    (Version: '1.0.0'; ComponentName: 'GEMUrlLabel'),
    (Version: '1.0.0'; ComponentName: 'FindFile'),
    (Version: '1.0.0'; ComponentName: 'GemDBGrid'),
    (Version: '1.0.0'; ComponentName: 'adpDBEdit'),
    (Version: '1.0.0'; ComponentName: 'gemDBComboBox'),
    (Version: '1.0.0'; ComponentName: 'gemDBLookupComboBox'),
    (Version: '1.0.0'; ComponentName: 'PJVersionInfo'),
    (Version: '1.0.0'; ComponentName: 'GEMColorButton'),
    (Version: '1.3.0'; ComponentName: 'GEMTrafficLight'),
    (Version: '1.3.0'; ComponentName: 'GEMTrafficLight2'),
    (Version: '1.0.0'; ComponentName: 'gemCapPanelBtn'),
    (Version: '1.0.0'; ComponentName: 'AutoCompleteEdit'),
    (Version: '1.0.0'; ComponentName: 'GEMShape')
  );


{ TGEMComponentsVersion }

function TGEMComponentsVersion.GetComponentName: string;
begin
  Result := GEMVerInfo[fGEMComponent].ComponentName
end;

function TGEMComponentsVersion.GetVersionInfo: TGEMVerStrRec;
begin
   Result := GEMVerInfo[fGEMComponent];
end;


function TGEMComponentsVersion.GetVersionNum: string;
begin
  Result := GEMVerInfo[fGEMComponent].Version;
end;

procedure TGEMComponentsVersion.SetVersionInfo(const Value: TGEMVerStrRec);
begin
//  fVersionNum := Value;
end;


end.
