unit uAutoComplete;

interface

uses
  WinApi.Windows, WinApi.ActiveX, WinApi.Messages,

  System.SysUtils, System.Classes, System.Win.ComObj, System.StrUtils,

  VCL.Controls, VCL.stdctrls, VCL.Forms, Vcl.Dialogs;


const
  IID_IAutoComplete: TGUID = '{00bb2762-6a77-11d0-a535-00c04fd7d062}';
  IID_IAutoComplete2: TGUID = '{EAC04BC0-3791-11d2-BB95-0060977B464C}';
  CLSID_IAutoComplete: TGUID = '{00BB2763-6A77-11D0-A535-00C04FD7D062}';

  IID_IACList: TGUID = '{77A130B0-94FD-11D0-A544-00C04FD7d062}';
  IID_IACList2: TGUID = '{470141a0-5186-11d2-bbb6-0060977b464c}';

  CLSID_ACLHistory: TGUID = '{00BB2764-6A77-11D0-A535-00C04FD7D062}';
  CLSID_ACListISF: TGUID = '{03C036F1-A186-11D0-824A-00AA005B4383}';
  CLSID_ACLMRU: TGUID = '{6756a641-de71-11d0-831b-00aa005b4383}';

type

  IACList = interface(IUnknown)
  ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand : POLESTR) : HResult; stdcall;
  end;

const
  //options for IACList2
  ACLO_NONE = 0;          // don't enumerate anything
  ACLO_CURRENTDIR = 1;    // enumerate current directory
  ACLO_MYCOMPUTER = 2;    // enumerate MyComputer
  ACLO_DESKTOP = 4;       // enumerate Desktop Folder
  ACLO_FAVORITES = 8;     // enumerate Favorites Folder
  ACLO_FILESYSONLY = 16;  // enumerate only the file system

type
  IACList2 = interface(IACList)
  ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(var pdwFlag: DWORD): HResult; stdcall;
  end;

  IAutoComplete = interface(IUnknown)
  ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit: HWND; const punkACL: IUnknown;
      pwszRegKeyPath, pwszQuickComplete: POLESTR): HResult; stdcall;
    function Enable(fEnable: BOOL): HResult; stdcall;
  end;

const
  //options for IAutoComplete2
  ACO_NONE = 0;
  ACO_AUTOSUGGEST = $1;
  ACO_AUTOAPPEND = $2;
  ACO_SEARCH = $4;
  ACO_FILTERPREFIXES = $8;
  ACO_USETAB = $10;
  ACO_UPDOWNKEYDROPSLIST = $20;
  ACO_RTLREADING = $40;

type
  IAutoComplete2 = interface(IAutoComplete)
  ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

  TEnumString = class(TInterfacedObject, IEnumString)
  private
    FStrings: TStringList;
    FCurrIndex: integer;
  public
    //IEnumString
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
    //VCL
    constructor Create;
    destructor Destroy;override;
  end;

  TAutoCompleteStrings = class(TStringList)
  private
  public
  end;

  TACOption = (acAutoAppend, acAutoSuggest, acUseArrowKey);
  TACOptions = set of TACOption;

  TACSource = (acsList, acsHistory, acsMRU, acsShell);

  TAutoCompleteEdit = class(TEdit)
  private
    FACList: TStringList;
    //FEnumString: IEnumString;
    FAutoComplete: IAutoComplete;
    FACEnabled: boolean;
    FACOptions: TACOptions;
    FACSource: TACSource;
    //fListBox: TListBox;
    function  GetACStrings: TStringList;
    procedure SetACEnabled(const Value: boolean);
    procedure SetACOptions(const Value: TACOptions);
    procedure SetACSource(const Value: TACSource);
    procedure SetACStrings(const Value: TStringList);
    procedure KeyPress(var Key: Char); override;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ACEnabled: boolean read FACEnabled write SetACEnabled;
    property ACOptions: TACOptions read FACOptions write SetACOptions;
    property ACSource: TACSource read FACSource write SetACSource;
    property ACStrings: TStringList read GetACStrings write SetACStrings;
  end;

implementation

{ IUnknownInt }

function TEnumString.Clone(out enm: IEnumString): HResult;
begin
  Result := E_NOTIMPL;
  Pointer(enm) := nil;
end;

constructor TEnumString.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FCurrIndex := 0;
end;

destructor TEnumString.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TEnumString.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
  //wStr: WideString;
  wStr: String;
begin
  I := 0;
  while (I < celt) and (FCurrIndex < FStrings.Count) do
  begin
    wStr := FStrings[FCurrIndex];
    TPointerList(elt)[I] := CoTaskMemAlloc(2 * (Length(wStr) + 1));
    StringToWideChar(wStr, TPointerList(elt)[I], 2 * (Length(wStr) + 1));
    Inc(I);
    Inc(FCurrIndex);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := I;
  if I = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumString.Reset: HResult;
begin
  FCurrIndex := 0;
  Result := S_OK;
end;

function TEnumString.Skip(celt: Integer): HResult;
begin
  if (FCurrIndex + celt) <= FStrings.Count then
  begin
    Inc(FCurrIndex, celt);
    Result := S_OK;
  end
  else
  begin
    FCurrIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

{ TACEdit }

constructor TAutoCompleteEdit.Create(AOwner: TComponent);
begin
  inherited;
  FACList := TStringList.Create;
  FACList.Sort;
  //FEnumString := FACList;
  FACEnabled := True;
  FACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];
end;


procedure TAutoCompleteEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

  if Key <> Char(VK_SPACE) then begin
    // create the listbox and fill it
    // Search the sting list for a match
    if PosEx('j', Text,1) > 0 then  begin

    end;

  end;

//  if TabOnEnter AND (Owner is TWinControl) then
//  begin
//    if Key = Char(VK_RETURN) then
//    begin
//     if HiWord(GetKeyState(VK_SHIFT)) <> 0 then
//        PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 1, 0)
//     else
//        PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 0, 0);
//    end;
//  end;
end; (*KeyPress*)


procedure TAutoCompleteEdit.CreateWnd;
var
  Dummy: IUnknown;
  Strings: IEnumString;
begin
  inherited;
  if HandleAllocated then
  begin
    try
      Dummy := CreateComObject(CLSID_IAutoComplete);
      if (Dummy <> nil) and
         (Dummy.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK) then
      begin
        case FACSource of
          acsHistory: Strings := CreateComObject(CLSID_ACLHistory) as
            IEnumString;
          acsMRU: Strings := CreateComObject(CLSID_ACLMRU) as
            IEnumString;
          acsShell: Strings := CreateComObject(CLSID_ACListISF) as
            IEnumString;
        else
          //Strings := FACList as IEnumString;
        end;
        if S_OK = FAutoComplete.Init(Handle, Strings, nil, nil) then
        begin
          SetACEnabled(FACEnabled);
          SetACOptions(FACOptions);
        end;
      end;
    except
      //CLSID_IAutoComplete is not available
    end;
  end;
end;


procedure TAutoCompleteEdit.DestroyWnd;
begin
  if (FAutoComplete <> nil) then
  begin
    FAutoComplete.Enable(False);
    FAutoComplete := nil;
  end;
  inherited;
end;


function TAutoCompleteEdit.GetACStrings: TStringList;
begin
  //Result := FACList.FStrings;
end;


procedure TAutoCompleteEdit.SetACEnabled(const Value: Boolean);
begin
  if (FAutoComplete <> nil) then
  begin
    FAutoComplete.Enable(FACEnabled);
  end;
  FACEnabled := Value;
end;


procedure TAutoCompleteEdit.SetACOptions(const Value: TACOptions);
const
  Options : array[TACOption] of integer = (ACO_AUTOAPPEND,
                                           ACO_AUTOSUGGEST,
                                           ACO_UPDOWNKEYDROPSLIST);
var
  Option:TACOption;
  Opt: DWORD;
  AC2: IAutoComplete2;
begin
  if (FAutoComplete <> nil) then
  begin
    if S_OK = FAutoComplete.QueryInterface(IID_IAutoComplete2, AC2) then
    begin
      Opt := ACO_NONE;
      for Option := Low(Options) to High(Options) do
      begin
        if (Option in FACOptions) then
          Opt := Opt or DWORD(Options[Option]);
      end;
      AC2.SetOptions(Opt);
    end;
  end;
  FACOptions := Value;
end;


procedure TAutoCompleteEdit.SetACSource(const Value: TACSource);
begin
  if FACSource <> Value then
  begin
    FACSource := Value;
    RecreateWnd;
  end;
end;


procedure TAutoCompleteEdit.SetACStrings(const Value: TStringList);
begin
//  if Value <> FACList.FStrings then
//    FACList.FStrings.Assign(Value);
end;

end.
