unit adpMRU;

{

  TadpMRU

  Full source code of a TadpMRU component, a non-visual
  component which simplifies implementing a "Most Recently Used"
  file list in a menu (or a popup menu). The TadpMRU component
  allows for quick selection of a file that was recently accessed
  (opened) in an application.

  How to use:

  http://delphi.about.com/library/weekly/aa112503a.htm

  .............................................
  Modified by Gary McIntosh to use ini files and have multiple sections using ini
  files.  To move last menu item clicked to top of the menu list and all others
  down one position.


  ..............................................
  Zarko Gajic, BSCS
  About Guide to Delphi Programming
  http://delphi.about.com
  how to advertise: http://delphi.about.com/library/bladvertise.htm
  free newsletter: http://delphi.about.com/library/blnewsletter.htm
  forum: http://forums.about.com/ab-delphi/start/
  ..............................................
}


interface

{$REGION 'Example program using component'}
(*
unit Unit28;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, EsBase, EsMnuBtn,
  Vcl.Menus, adpMRU, GemMruList, Vcl.ComCtrls, JvExComCtrls, JvRegistryTreeview,
  JvComponentBase, JvAppStorage, JvAppRegistryStorage;

type
  TForm28 = class(TForm)
    adpmr_1: TadpMRU;
    pm1: TPopupMenu;
    mniMRUList1: TMenuItem;
    mniClose1: TMenuItem;
    btnMen_1: TEsMenuButton;
    edt_1: TEdit;
    btn_1: TButton;
    edt_2: TEdit;
    procedure btn_1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure adpmr_1Click(Sender: TObject; const FileName: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form28: TForm28;

implementation

{$R *.dfm}

procedure TForm28.adpmr_1Click(Sender: TObject; const FileName: string);
begin
  edt_2.Text := FileName;
end;

procedure TForm28.btn_1Click(Sender: TObject);
begin
  adpmr_1.AddItem('Men',edt_1.text, true);
  edt_1.text := '';
end;


procedure TForm28.FormCreate(Sender: TObject);
begin
  adpmr_1.IniFilePath := 'C:\Users\Gary\Documents\TestIni\Test.ini';
  ShowMessage(adpmr_1.StatusMsg);
end;

procedure TForm28.FormShow(Sender: TObject);
begin
  adpmr_1.LoadIniFile('Men');
end;

end.
=======================



*)
{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.IniFiles, System.Classes, System.Win.Registry,

  VCL.Menus, VCL.Dialogs,

  GEMComponentsGlobal;

type
  //  must have admin set to use most of these
  TJvRegKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers,
    hkPerformanceData, hkCurrentConfig, hkDynData);
  TJvRegKeys = set of TJvRegKey;

  TMRUClickEvent = procedure(Sender: TObject; const FileName: String) of object;
//  TMRUFileNameChange = procedure(Sender: TObject; const FileName: String) of object;
  TMRUStoreType  = (mstIni, mstReg, mstNone);

  TadpMRU = class(TComponent)
  private
    FRegHKEY           : HKEY;
    FItems             : TStringList;
    FMaxItems          : cardinal;
    FShowFullPath      : boolean;
    FRegistryPath      : string;
    FIniFilePath       : string;
    FParentMenuItem    : TMenuItem;
    FOnClick           : TMRUClickEvent;
//    fOnFileNameChange  : TMRUFileNameChange;
    FUseIniReg         : TMRUStoreType;
    fGroupIndex        : byte;
    fRadioItem         : boolean;
    FVersion           : string;
    fSectionIniNameReg : string;
    fStatusMsg         : string;

    procedure SetMaxItems(const Value: cardinal);
    procedure SetShowFullPath(const Value: boolean);
    procedure SetRegistryPath(const Value: string);
    procedure SetParentMenuItem(const Value: TMenuItem);

    procedure LoadMRU;
    procedure SaveMRU;

    procedure ItemsChange(Sender: TObject);
    procedure ClearParentMenu;
    procedure SetIniFilePath(const Value: string);
    function GetVersion: string;
    procedure SetSectionIniNameReg(const Value: string);
    function GetTheItems: TStringList;
    procedure SetVersion(const Value: string);
    procedure SetUseIniReg(const Value: TMRUStoreType);

//    procedure FileNameChange(Sender: TObject; const FileName: String);
    procedure SetUseIniFile(const Value: TMRUStoreType);
    function GetRegRoot: TJvRegKey;
    procedure SetRegRoot(const Value: TJvRegKey);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoClick(Sender: TObject);
//    procedure DoIniFileNameChange(Sender: TObject; const FileName: String);
    function IniPathExists: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadIniFile(Section: string);
    procedure AddItem(const aSection, aValue: string; aResetIFNewSection: Boolean = false);
    function RemoveItem(const FileName : string) : boolean;
    procedure SetupNewSection(const aSection: string);
    function GetSectionNames: TStrings;
  published
    property RegRoot: TJvRegKey read GetRegRoot write SetRegRoot default hkCurrentUser;
    property StatusMsg: string read fStatusMsg;
    property IniFilePath: string read FIniFilePath write SetIniFilePath;
    property GroupIndex: byte read fGroupIndex write fGroupIndex default 0;
    property MaxItems: cardinal read FMaxItems write SetMaxItems default 7;
    property ShowFullPath: boolean read FShowFullPath write SetShowFullPath default True;
    property RegistryPath: string read FRegistryPath write SetRegistryPath;
    property ParentMenuItem: TMenuItem read FParentMenuItem write SetParentMenuItem;
    property RadioItem: boolean read fRadioItem write fRadioItem default False;
    property UseIniReg: TMRUStoreType read FUseIniReg write SetUseIniFile;// default True;
    property Version: string read GetVersion write SetVersion;
    property StartingSection: string read fSectionIniNameReg write SetSectionIniNameReg;
    property TheItems: TStringList read GetTheItems;
    property OnClick: TMRUClickEvent read FOnClick write FOnClick;
//    property OnFileNameChange: TMRUFileNameChange read fOnFileNameChange write fOnFileNameChange;
  end;

//procedure Register;

implementation

type
  TMRUMenuItem = class(TMenuItem); //to be able to recognize MRU menu item when deleting


  // procedure Register;
  // begin
  // RegisterComponents('Gary"s Stuff', [TadpMRU]);
  // end;

  { TadpMRU }

constructor TadpMRU.Create(AOwner: TComponent);
begin
  inherited;
  FRegHKEY := HKEY_CURRENT_USER;
  FParentMenuItem := nil;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;

  FShowFullPath := True;
  fVersion := VersionAdpMRU;
end; (*Create*)


procedure TadpMRU.Loaded;
begin
  inherited;

  if not(csDesigning in ComponentState) then
    case fUseIniReg of
      mstIni: if IniPathExists then
                LoadMRU;

      mstReg: if FRegistryPath <> '' then
                LoadMRU;
    end;
end; (* Loaded *)


destructor TadpMRU.Destroy;
begin
  if not(csDesigning in ComponentState) then
    SaveMRU;

  FItems.OnChange := nil;
  FItems.Free;

  inherited;
end; (* Destroy *)


procedure TadpMRU.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FParentMenuItem) then
    FParentMenuItem := nil;
end; (* Notification *)


procedure TadpMRU.AddItem(const aSection, aValue: string; aResetIFNewSection: boolean = false);
begin
  if aValue <> '' then
  begin
    Trim(aSection);
    if aSection <> '' then
    begin
      if (fSectionIniNameReg <> aSection) and aResetIFNewSection and (UseIniReg  = mstIni)then
      begin
        SaveMRU;
        fSectionIniNameReg := aSection;
        LoadMRU;
      end
      else
      begin
        fSectionIniNameReg := aSection;
      end;
    end;
    FItems.BeginUpdate;
    try
      if FItems.IndexOf(aValue) > -1 then
        FItems.Delete(FItems.IndexOf(aValue));
      FItems.Insert(0, aValue);

      while Cardinal(FItems.Count) > MaxItems do
        FItems.Delete(MaxItems);
    finally
      FItems.EndUpdate;
      ItemsChange(Self);
    end;
  end;
end; (*AddItem*)


function TadpMRU.RemoveItem(const FileName: string): boolean;
begin
  if FItems.IndexOf(FileName) > -1 then
  begin
    FItems.Delete(FItems.IndexOf(FileName));
    Result := True;
  end
  else
    Result := False;
end; (*RemoveItem*)


procedure TadpMRU.SetIniFilePath(const Value: string);
begin
  if (FIniFilePath <> Value) then
  begin
    FIniFilePath := Value;
    LoadMRU;
  end;
end; (*SetIniFilePath*)


procedure TadpMRU.SetUseIniFile(const Value: TMRUStoreType);
begin
  FUseIniReg := Value;
end;


procedure TadpMRU.SetUseIniReg(const Value: TMRUStoreType);
begin
  if (FUseIniReg <> Value) then
  begin
    FUseIniReg := Value;
    ItemsChange(Self);
  end;
end;(*SetUseIniFile*)


procedure TadpMRU.SetMaxItems(const Value: cardinal);
begin
  if Value <> FMaxItems then
  begin
    if Value < 1 then
      FMaxItems := 1
    else
      if Value > Cardinal(MaxInt) then
        FMaxItems := MaxInt - 1
      else
      begin
        FMaxItems := Value;
        FItems.BeginUpdate;
        try
          while Cardinal(FItems.Count) > MaxItems do
            FItems.Delete(FItems.Count - 1);
        finally
          FItems.EndUpdate;
        end;
      end;
  end;
  ItemsChange(Self);
end; (* SetMaxItems *)


procedure TadpMRU.SetRegistryPath(const Value: string);
begin
  if FRegistryPath <> Value then
  begin
    FRegistryPath := Value;
    LoadMRU;
  end;
end;


function TadpMRU.GetRegRoot: TJvRegKey;
begin
  Result := TJvRegKey(FRegHKEY - HKEY_CLASSES_ROOT);
end;


procedure TadpMRU.SetRegRoot(const Value: TJvRegKey);
begin
  if Value <> RegRoot then
    FRegHKEY := HKEY_CLASSES_ROOT + Longword(Ord(Value));
end;


procedure TadpMRU.SetSectionIniNameReg(const Value: string);
begin
  if (fSectionIniNameReg <> Value) then
  begin
    SaveMRU;
    fSectionIniNameReg := Value;
    LoadMRU;
    ItemsChange(Self);
  end;
end;


procedure TadpMRU.SetShowFullPath(const Value: boolean);
begin
  if FShowFullPath <> Value then
  begin
    FShowFullPath := Value;
    ItemsChange(Self);
  end;
end;


procedure TadpMRU.SetupNewSection(const aSection: string);
begin
  if (fSectionIniNameReg <> aSection) then
  begin
    SaveMRU;
    fSectionIniNameReg := aSection;
    LoadMRU;
    ItemsChange(Self);
  end;
end;


procedure TadpMRU.SetVersion(const Value: string);
begin
  FVersion := VersionAdpMRU;
end;


function TadpMRU.GetSectionNames: TStrings;
begin
  Result := nil;
  if (UseIniReg  = mstIni) and FileExists(FIniFilePath) then
    with TIniFile.Create(FIniFilePath) do
      try
        ReadSections(result);
      finally
        Free;
      end;
end;


function TadpMRU.GetTheItems: TStringList;
begin
  Result :=  FItems;
end;


function TadpMRU.GetVersion: string;
begin
  Result := VersionAdpMRU; // VersionAdpMRU;
end;


procedure TadpMRU.LoadIniFile(Section: string);
begin
  fSectionIniNameReg := Section;
  LoadMRU;
end;


procedure TadpMRU.LoadMRU;
var
  i: cardinal;
  s: string;
begin
  case UseIniReg of
    mstIni:
    begin
      if IniPathExists then
        with TIniFile.Create(FIniFilePath) do
        begin
          FItems.BeginUpdate;
          FItems.Clear;
          try
            for i := 1 to FMaxItems do
              if ValueExists(fSectionIniNameReg, fSectionIniNameReg + IntToStr(i))
              then
              begin
                s := ReadString(fSectionIniNameReg, fSectionIniNameReg +
                  IntToStr(i), 'None');
                FItems.Add(s);
              end;
          finally
            FItems.EndUpdate;
            Free;
          end;
        end;
    end;

    mstReg:
    begin
      with TRegistry.Create(KEY_ALL_ACCESS OR KEY_WOW64_64KEY) do
        try
          RootKey := FRegHKEY;
          if OpenKey(FRegistryPath, false) then
          begin
            FItems.BeginUpdate;
            FItems.Clear;
            try
              for i := 1 to FMaxItems do
                if ValueExists(fSectionIniNameReg + IntToStr(i)) then
                  FItems.Add(ReadString(fSectionIniNameReg + IntToStr(i)));
            finally
              FItems.EndUpdate;
            end;
            CloseKey;
          end;
        finally
          Free;
        end;
    end;
  end;
end; (* LoadMRU *)


procedure TadpMRU.SaveMRU;
var
  i: integer;
  reg: TRegistry;
  OpenResult: boolean;
  ///================================
  procedure SaveToReg;
  var
    i: Integer;
    reg: TRegistry;
    OpenResult: boolean;
  begin
    reg := TRegistry.Create(KEY_ALL_ACCESS OR KEY_WOW64_64KEY);
    try
      reg.RootKey  := FRegHKEY;

      openResult := reg.OpenKey(FRegistryPath, True);

      if not openResult = True then
      begin
        fStatusMsg := 'Unable to Open key! Exiting.';
        Exit();
      end
      else
      begin
        // delete old mru
        i := 1;
        while reg.ValueExists(fSectionIniNameReg + IntToStr(i)) do
        begin
          reg.DeleteValue(fSectionIniNameReg + IntToStr(i));
          Inc(i);
        end;

        // write new mru
        for i := 0 to -1 + FItems.Count do
          reg.WriteString(fSectionIniNameReg + IntToStr(i + 1), FItems[i]);
        reg.CloseKey;
      end;
    finally
      reg.Free;
    end;
  end;
  //================================
begin
  if not IniPathExists then
    Exit;

  case UseIniReg of
    mstIni: begin
      with TIniFile.Create(FIniFilePath) do
        try
          EraseSection(fSectionIniNameReg);
          for i := 0 to -1 + FItems.Count do
            WriteString(fSectionIniNameReg, fSectionIniNameReg + IntToStr(i + 1),
              FItems[i]);
        finally
          Free;
        end;
    end;

    mstReg: begin
      SaveToReg;
    end;
  end;
end; (* SaveMRU *)


function TadpMRU.IniPathExists: Boolean;
begin
  var IniPath: string := ExtractFilePath(FIniFilePath);
  Result := DirectoryExists(IniPath);

  if result then
    fStatusMsg := 'Ini File Path OK'
  else
    fStatusMsg := 'Error: Ini File Path Does Not Exist.';
end;


procedure TadpMRU.ItemsChange(Sender: TObject);
var
  i: integer;
  NewMenuItem: TMenuItem;
  FileName: String;
begin
  if ParentMenuItem <> nil then
  begin
    ClearParentMenu;
    for i := 0 to -1 + FItems.Count do
    begin
      if ShowFullPath then
        FileName := StringReplace(FItems[I], '&', '&&', [rfReplaceAll, rfIgnoreCase])
      else
        FileName := StringReplace(ExtractFileName(FItems[i]), '&', '&&', [rfReplaceAll, rfIgnoreCase]);

      NewMenuItem := TMRUMenuItem.Create(Self);
      NewMenuItem.GroupIndex := fGroupIndex;
      if fRadioItem then
        NewMenuItem.RadioItem := True;
      NewMenuItem.Caption := Format('%s', [FileName]);
      NewMenuItem.Tag := i;
      NewMenuItem.OnClick := DoClick;
      ParentMenuItem.Add(NewMenuItem);
    end;
  end;
end; (* ItemsChange *)


procedure TadpMRU.ClearParentMenu;
var
  i: integer;
begin
  if Assigned(ParentMenuItem) then
    for i := -1 + ParentMenuItem.Count downto 0 do
      if ParentMenuItem.Items[i] is TMRUMenuItem then
        ParentMenuItem.Delete(i);
end; 

(* ClearParentMenu *)


procedure TadpMRU.DoClick(Sender: TObject);
var
  ListIndex: Integer;
  S: string;
begin
  if Assigned(FOnClick) and (Sender is TMRUMenuItem) then
  begin
    FOnClick(Self, FItems[TMRUMenuItem(Sender).Tag]);
    TMRUMenuItem(Sender).checked := True;
//    fCheckedMenuItem := TMRUMenuItem(Sender).Tag;

    s := StringReplace(TMRUMenuItem(Sender).Caption, '&', '',[rfReplaceAll, rfIgnoreCase]);
    ListIndex := FItems.IndexOf(s);
    // ShowMessage(TMRUMenuItem(Sender).Caption+'  '+IntToStr(ListIndex));
    if ListIndex > -1 then
    begin
      FItems.BeginUpdate;
      FItems.Move(ListIndex, 0);
      ItemsChange(Self);
      FItems.EndUpdate;
    end;
  end;
end;  (* DoClick *)


procedure TadpMRU.SetParentMenuItem(const Value: TMenuItem);
begin
  if FParentMenuItem <> Value then
  begin
    ClearParentMenu;
    FParentMenuItem := Value;
    ItemsChange(Self);
  end;
end; (* SetParentMenuItem *)

(* adpMRU.pas *)

end.
