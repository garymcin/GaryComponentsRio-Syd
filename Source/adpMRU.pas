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

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.IniFiles, System.Classes, System.Win.Registry,

  VCL.Menus, VCL.Dialogs,

  GEMComponentsGlobal;

type

  TMRUClickEvent = procedure(Sender: TObject; const FileName: String) of object;

  TadpMRU = class(TComponent)
  private
    FItems: TStringList;
    FMaxItems: cardinal;
    FShowFullPath: boolean;
    FRegistryPath: string;
    FIniFilePath: string;
    FParentMenuItem: TMenuItem;
    FOnClick: TMRUClickEvent;
    FUseIniFile: boolean;
    fGroupIndex: byte;
    fRadioItem: boolean;
    FVersion: string;
    fSectionIniNameReg: string;
    fIniFilePathExists: boolean;
    fStatusMsg: string;
    procedure SetMaxItems(const Value: cardinal);
    procedure SetUseIniFile(const Value: boolean);
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
    function SetIniFilePathExists: boolean;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(const aSection, aFileName: string;
      aResetIFNewSection: boolean = false);
    function RemoveItem(const FileName: string): boolean;
    procedure SetupNewSection(const aSection: string);
    function GetSectionNames: TStrings;
//    function IniFilePathExists: boolean;
  published
    property StatusMsg: string read fStatusMsg;
    property DoesIniFilePathExists: boolean read SetIniFilePathExists;
    property IniFilePath: string read FIniFilePath write SetIniFilePath;
    property GroupIndex: byte read fGroupIndex write fGroupIndex;
    property MaxItems: cardinal read FMaxItems write SetMaxItems default 7;
    property ShowFullPath: boolean read FShowFullPath write SetShowFullPath
      default True;
    property RegistryPath: string read FRegistryPath write SetRegistryPath;
    property ParentMenuItem: TMenuItem read FParentMenuItem
      write SetParentMenuItem;
    property RadioItem: boolean read fRadioItem write fRadioItem default false;
    property UseIniFile: boolean read FUseIniFile write SetUseIniFile;
    // default True;
    property Version: string read GetVersion write SetVersion;
    property StartingSection: string read fSectionIniNameReg
      write SetSectionIniNameReg;
    property TheItems: TStringList read GetTheItems;

    property OnClick: TMRUClickEvent read FOnClick write FOnClick;
  end;

  // procedure Register;

implementation

type
  TMRUMenuItem = class(TMenuItem);
  // to be able to recognize MRU menu item when deleting


  // procedure Register;
  // begin
  // RegisterComponents('Gary"s Stuff', [TadpMRU]);
  // end;

  { TadpMRU }

constructor TadpMRU.Create(AOwner: TComponent);
begin
  inherited;
  FParentMenuItem := nil;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;

  FShowFullPath := True;
  FVersion := VersionAdpMRU;
  SetIniFilePathExists;
end; (* Create *)

procedure TadpMRU.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    if FUseIniFile then
    begin
      if SetIniFilePathExists then
        LoadMRU;
    end
    else
    begin
      if FRegistryPath <> '' then
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


function TadpMRU.SetIniFilePathExists: boolean;
begin
  if DirectoryExists(FIniFilePath) then
  begin
    fIniFilePathExists := True;
    result := True;
    fStatusMsg := 'Ini File Path OK';
  end
  else
  begin
    fIniFilePathExists := false;
    Result := False;
    fStatusMsg := 'Error: Getting Ini File Path.'
  end;
end;


procedure TadpMRU.AddItem(const aSection, aFileName: string;
  aResetIFNewSection: boolean = false);
begin
  if aFileName <> '' then
  begin
    Trim(aSection);
    if aSection <> '' then
    begin
      if (fSectionIniNameReg <> aSection) and aResetIFNewSection and FUseIniFile
      then
      begin
        // showmessage('reset '+aSection+ '  '+ fSectionIniNameReg);
        SaveMRU;
        fSectionIniNameReg := aSection;
        LoadMRU;
      end
      else
      begin
        fSectionIniNameReg := aSection;
        // showmessage('not reset '+fSectionIniNameReg);
      end;
    end;
    FItems.BeginUpdate;
    // showmessage('Stuff '+fSectionIniNameReg+'   '+aFileName);

    try
      if FItems.IndexOf(aFileName) > -1 then
        FItems.Delete(FItems.IndexOf(aFileName));
      FItems.Insert(0, aFileName);

      while cardinal(FItems.Count) > MaxItems do
        FItems.Delete(MaxItems);
    finally
      FItems.EndUpdate;
      ItemsChange(Self);
    end;
  end;
end; (* AddItem *)

function TadpMRU.RemoveItem(const FileName: string): boolean;
begin
  if FItems.IndexOf(FileName) > -1 then
  begin
    FItems.Delete(FItems.IndexOf(FileName));
    Result := True;
  end
  else
    Result := false;
end; (* RemoveItem *)


// procedure TadpMRU.SetHint(const Value: string);
// begin
// if Value <> FHint then
// fHint := Value;
// end;

procedure TadpMRU.SetIniFilePath(const Value: string);
begin
  if (FIniFilePath <> Value) then
  begin
    FIniFilePath := Value;
    LoadMRU;
  end;
  SetIniFilePathExists;
end;

(* SetIniFilePath *)

procedure TadpMRU.SetUseIniFile(const Value: boolean);
begin
  if (FUseIniFile <> Value) then
  begin
    FUseIniFile := Value;
    ItemsChange(Self);
  end;
end;

(* SetUseIniFile *)

procedure TadpMRU.SetMaxItems(const Value: cardinal);
begin
  if Value <> FMaxItems then
  begin
    if Value < 1 then
      FMaxItems := 1
    else if Value > cardinal(MaxInt) then
      FMaxItems := MaxInt - 1
    else
    begin
      FMaxItems := Value;
      FItems.BeginUpdate;
      try
        while cardinal(FItems.Count) > MaxItems do
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
end; (* SetRegistryPath *)


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


// procedure TadpMRU.SetTheItems(const Value: TStringList);
// begin
// FItems := Value;
// end;
//

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
  if FUseIniFile and FileExists(FIniFilePath) then
    with TIniFile.Create(FIniFilePath) do
      try
        ReadSections(Result);
      finally
        Free;
      end;
end;


function TadpMRU.GetTheItems: TStringList;
begin
  Result := FItems;
end;


function TadpMRU.GetVersion: string;
begin
  Result := VersionAdpMRU; // VersionAdpMRU;
end;

(* SetShowFullPath *)

procedure TadpMRU.LoadMRU;
var
  i: cardinal;
  s: string;
begin
  if not fIniFilePathExists then
    Exit;

  if FUseIniFile then
  begin
    if FileExists(FIniFilePath) then
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
  end
  else
  begin
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
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
end; (* LoadMRU *)


procedure TadpMRU.SaveMRU;
var
  i: integer;
begin
  if not fIniFilePathExists then
    Exit;

  if FUseIniFile then
  begin
    with TIniFile.Create(FIniFilePath) do
      try
        EraseSection(fSectionIniNameReg);
        for i := 0 to -1 + FItems.Count do
          WriteString(fSectionIniNameReg, fSectionIniNameReg + IntToStr(i + 1),
            FItems[i]);
      finally
        Free;
      end;
  end
  else
  begin
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey(FRegistryPath, True) then
        begin
          // delete old mru
          i := 1;
          while ValueExists(fSectionIniNameReg + IntToStr(i)) do
          begin
            DeleteValue(fSectionIniNameReg + IntToStr(i));
            Inc(i);
          end;

          // write new mru
          for i := 0 to -1 + FItems.Count do
            WriteString(fSectionIniNameReg + IntToStr(i + 1), FItems[i]);
          CloseKey;
        end;
      finally
        Free;
      end;
  end;
end; (* SaveMRU *)


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
        FileName := StringReplace(FItems[i], '&', '&&',
          [rfReplaceAll, rfIgnoreCase])
      else
        FileName := StringReplace(ExtractFileName(FItems[i]), '&', '&&',
          [rfReplaceAll, rfIgnoreCase]);

      NewMenuItem := TMRUMenuItem.Create(Self);
      NewMenuItem.GroupIndex := fGroupIndex;
      if fRadioItem then
        NewMenuItem.RadioItem := True;
      NewMenuItem.Caption := Format('%s', [FileName]);
      // if fHint <> '' then
      // NewMenuItem.Hint := fHint;
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
end; (* ClearParentMenu *)


procedure TadpMRU.DoClick(Sender: TObject);
var
  ListIndex: integer;
  s: string;
begin
  if Assigned(FOnClick) and (Sender is TMRUMenuItem) then
  begin
    FOnClick(Self, FItems[TMRUMenuItem(Sender).Tag]);
    TMRUMenuItem(Sender).checked := True;
    // fCheckedMenuItem := TMRUMenuItem(Sender).Tag;

    s := StringReplace(TMRUMenuItem(Sender).Caption, '&', '',
      [rfReplaceAll, rfIgnoreCase]);
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
end; (* DoClick *)


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
