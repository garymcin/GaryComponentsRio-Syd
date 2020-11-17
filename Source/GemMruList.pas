unit GemMruList;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes, System.UITypes,

  Vcl.Controls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Themes;


type
  tUseStringListType = (uslt_NameValue, uslt_Name, uslt_Value);

  TClickMRUItemEvent = procedure(Sender:TObject; aIndexItem:Integer; aName, aValue: string) of object;
  TGEMScrollEvent = procedure(Sender: TObject; const Msg: TWMScroll; var DontScroll: Boolean) of object;

  tGemMruList = class(TCustomListBox)
  strict private
    class constructor Create;
    class destructor Destroy;
  private
    { Private declarations }
    fMruStringList: TStringList;
    fMruListFile: string;
    fMaxLenghtList: integer;
    fStringListType: tUseStringListType;
    FOnClick: TClickMRUItemEvent;

    FScrollBars: System.UITypes.TScrollStyle;
    FOnHorizontalScroll: TGEMScrollEvent;
    FOnVerticalScroll: TGEMScrollEvent;

    function GetValueFItemStr(const cItemStr: string): string;
    procedure PopLastIfMoreMax;
    function GetListOnNames: TStringList;
    function GetMruListFile: string;
    procedure SetMruListFile(const Value: string);
    procedure SetMaxLenghtList(const Value: Integer);
    procedure SetScrollBars(const Value: System.UITypes.TScrollStyle);
    property Items;

  protected
    procedure Loaded; override;

    property ScrollBars: System.UITypes.TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property OnVerticalScroll: TGEMScrollEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TGEMScrollEvent read FOnHorizontalScroll write FOnHorizontalScroll;

  public
    { Public declarations }
    destructor Destroy; override;

    procedure Add(aName, aValue: string);
    function Count: Integer;
    function GetValue(const cIndex: Integer): string;  //works
    function GetName(const cIndes: Integer): string;
    function GetIndexFName(const cName: string): Integer;
    procedure DeleteItem(const cSection, cValue: string); overload;
    procedure DeleteItem(const cIndex: Integer); overload;
    procedure DeleteItem(const cName: string); overload;
    procedure DeleteAllItemNoValue;
    procedure ResetListBox;
    procedure Click; override;

    property ListOfNameValue: TStringList read fMruStringList;
    property ListOfNames: TStringList read GetListOnNames;
  published
    procedure LoadMRUList(fFileName: string);
    property MaxNumItems: Integer read fMaxLenghtList write SetMaxLenghtList;
    property MruListFile: string read GetMruListFile write SetMruListFile;
    property StringListType: tUseStringListType read fStringListType write fStringListType;
    property OnClick: TClickMRUItemEvent read FOnClick write FOnClick;

    property Style;
    property AutoComplete;
    property AutoCompleteDelay;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Touch;
    property Visible;
    property StyleElements;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation
const
  cMaxLenListNum = 15;
  cSectionValueSeparator = '=';

//=============================================================================
//==================== constructor and destructor =============================
//=============================================================================

class constructor tGemMruList.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(tGemMruList, TListBoxStyleHook);
end;


class destructor tGemMruList.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(tGemMruList, TListBoxStyleHook);
end;


procedure tGemMruList.Loaded;
begin
  inherited;
  fMruStringList := TStringList.Create;

  fMruStringList.Clear;
  fMruStringList.Sorted := False;
  fMruStringList.Duplicates := dupIgnore;

  fMruListFile := 'NONE';
end;


destructor tGemMruList.Destroy;
begin
  var fPath := ExtractFilePath(fMruListFile);
  if DirectoryExists(fPath) then
    fMruStringList.SaveToFile(fMruListFile);
  FreeAndNil(fMruStringList);
  inherited Destroy;
end;

//=============================================================================
//=================End constructor and destructor =============================
//=============================================================================

procedure tGemMruList.LoadMRUList(fFileName: string);
begin
  if fMruStringList = nil then
    Exit;

  SetMruListFile(fFileName);
end;


function tGemMruList.GetValueFItemStr(const cItemStr: string): string;
begin
  try
    var j := Pos(cSectionValueSeparator, cItemStr);
    var subStr := copy(cItemStr, 1, j); // this gets the section + separator from cItemStr

    Result := StringReplace(cItemStr, subStr, '', [rfIgnoreCase]);
//    j := Pos(subStr, cItemStr) + Length(subStr);
//    k := Length(cItemStr);
//    Result := Copy(cItemStr, j, k-1);
  except
    Result := 'Error';
  end;
end;


procedure tGemMruList.Click;
begin
  inherited;
  if Assigned(FOnClick) then begin
    var fItemIndex := GetItemIndex;
    FOnClick(Self, fItemIndex, fMruStringList.Names[fItemIndex],
             fMruStringList.ValueFromIndex[fItemIndex]);
  end;
end;


function tGemMruList.Count: Integer;
begin
  Result := fMruStringList.Count;
end;


function tGemMruList.GetValue(const cIndex: Integer): string;
begin
  if cIndex <= fMruStringList.Count - 1 then begin
    try
      Result := GetValueFItemStr(fMruStringList.ValueFromIndex[cIndex]);
    except
      result := '';
    end;
  end;
end;


function tGemMruList.GetIndexFName(const cName: string): Integer;
begin
  result := fMruStringList.IndexOfName(cName);
end;


function tGemMruList.GetListOnNames: TStringList;
begin
  result := nil;
  for var i := 0 to fMruStringList.Count - 1 do
    Result.Insert(0, fMruStringList.Names[i]);
end;


function tGemMruList.GetMruListFile: string;
begin
  if fMruListFile = '' then
    Result := 'NONE'
  else
    Result := fMruListFile;
end;


function tGemMruList.GetName(const cIndes: Integer): string;
begin
  Result := fMruStringList.Names[cIndes];
end;


procedure tGemMruList.PopLastIfMoreMax;
begin
  if fMruStringList.Count > cMaxLenListNum then
    fMruStringList.Delete(fMruStringList.Count-1);
end;


procedure tGemMruList.ResetListBox;
begin
  Items.clear;
  case fStringListType of
    uslt_NameValue: Items.Assign(fMruStringList);

    uslt_Name: for var index := 0 to fMruStringList.count -1 do
                 Items.Add(fMruStringList.Names[Index]);

    uslt_Value: for var index := 0 to fMruStringList.count -1 do
                 Items.Add(fMruStringList.ValueFromIndex[Index]);
  end;
end;


procedure tGemMruList.SetMaxLenghtList(const Value: Integer);
begin
  fMaxLenghtList := Value;
  if cMaxLenListNum > fMaxLenghtList then
    fMaxLenghtList := cMaxLenListNum;
end;


procedure tGemMruList.SetMruListFile(const Value: string);
begin
  if fMruStringList = nil then
    Exit;
  fMruListFile := Value;
  if FileExists(fMruListFile) then begin
    try
      fMruStringList.LoadFromFile(fMruListFile);
      Items.Assign(fMruStringList);
    except
      on E : Exception do
        ShowMessage(E.ClassName+' error raised, with message : '+E.Message);
    end;
  end;
end;


procedure tGemMruList.SetScrollBars(const Value: System.UITypes.TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;


procedure tGemMruList.Add(aName, aValue: string);
begin
  aName := Trim(aName);
  aValue := Trim(aValue);
  if (aName <> '') and (aValue <> '') then begin
    DeleteItem(aName);
    fMruStringList.Insert(0, aName + cSectionValueSeparator + aValue);
    PopLastIfMoreMax;
    ResetListBox;
  end;
end;


procedure tGemMruList.DeleteItem(const cIndex: Integer);
begin
  if cIndex <= fMruStringList.Count - 1 then
    fMruStringList.Delete(cIndex);
  ResetListBox;
end;


procedure tGemMruList.DeleteItem(const cSection, cValue: string);
var
  vIndex: Integer;
begin
  if fMruStringList.find(cSection + cSectionValueSeparator + cValue, vIndex) then
    fMruStringList.Delete(vIndex);
  ResetListBox;
end;


procedure tGemMruList.DeleteItem(const cName: string);
begin
  var vIndex := fMruStringList.IndexOfName(cName);
  if vIndex <> -1 then
    fMruStringList.Delete(vIndex);

  ResetListBox;
end;


procedure tGemMruList.DeleteAllItemNoValue;
begin
  var vIndex := 0;
  while vIndex <= fMruStringList.Count - 1 do
    if Pos(cSectionValueSeparator, fMruStringList[vIndex]) = Length(fMruStringList[vIndex]) then
      fMruStringList.Delete(vIndex)
    else
      Inc(vIndex);
end;

end.

