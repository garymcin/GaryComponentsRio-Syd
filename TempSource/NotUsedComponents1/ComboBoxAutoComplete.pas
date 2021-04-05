unit ComboBoxAutoComplete;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Classes, System.StrUtils,

  Vcl.Controls, Vcl.StdCtrls;


type
  TACType = (act_StartBegin_CaseSenitive, act_NStartBegin_NCaseSenitive,
             act_StartBegin, act_CaseSenitive);

  TGEM_ComboBoxAutoC = class(TCustomComboBox)
  private
    AutoCItemIndex:integer;
    FAutoCItems: TStringList;
    fUseAutoComplete:boolean;
    fAutoCType: TACType;
    procedure FilterItems;
    procedure StoredItemsChange(Sender: TObject);
    procedure SetStoredItems(const Value: TStringList);
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure CloseUp; override;
    procedure Click; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
//    procedure InitACCombo;
//    procedure AddACList(aItem: string);
//    procedure AddACStrings(aStrings: TStrings);
//    procedure ClearACList;
//    function ACListCount: Integer;
    property AutoC_Items: TStringList read FAutoCItems write SetStoredItems;
    property AutoCType: TACType read fAutoCType write fAutoCType default act_StartBegin_CaseSenitive;
    property Align;
//    property AutoComplete default True;
//    property AutoCompleteDelay default 500;
//    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
 end;

  procedure register;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TGEM_ComboBoxAutoC]);
end;


{ TGEM_ComboBoxAutoC ==========================================================}

constructor TGEM_ComboBoxAutoC.Create(AOwner: TComponent);
begin
  inherited;
  fUseAutoComplete := False;
  FAutoCItems := TStringList.Create;

  FAutoCItems.OnChange := nil;
  AutoComplete := False;
  FAutoCItems.OnChange := StoredItemsChange;
  fUseAutoComplete := true;
  AutoCItemIndex := -1;
  Text := '';
end;


destructor TGEM_ComboBoxAutoC.Destroy;
begin
  FAutoCItems.Free;
  inherited;
end;


procedure TGEM_ComboBoxAutoC.Click;
begin
  if fUseAutoComplete then begin
    if AutoCItemIndex <> -1 then
      ItemIndex := AutoCItemIndex;
    AutoCItemIndex := -1;
  end;
  inherited;
end;


procedure TGEM_ComboBoxAutoC.CloseUp;
var
  x: string;
begin
  if fUseAutoComplete then begin
    if (items.count = 1) and (ItemIndex = 0) then
      text := items[ItemIndex]
    else
      if ((text <> '') and (itemindex <> -1) and (text <> items[ItemIndex])) or ((text = '') and (ItemIndex = 0)) then begin
        AutoCItemIndex := ItemIndex;
        x := text;
        ItemIndex := items.indexof(text);
        if ItemIndex = -1 then
          text := x;
      end
      else
        AutoCItemIndex := -1;
  end;
  inherited;
end;


procedure TGEM_ComboBoxAutoC.CNCommand(var AMessage: TWMCommand);
begin
  // we have to process everything from our ancestor
  inherited;
//   if we received the CBN_EDITUPDATE notification
  if (AMessage.NotifyCode = CBN_EDITUPDATE) and fUseAutoComplete then begin
    // fill the items with the matches
    FilterItems;
  end;
end;


procedure TGEM_ComboBoxAutoC.FilterItems;
var
  I: Integer;
  Selection: TSelection;
begin
  // store the current combo edit selection
  SendMessage(Handle, CB_GETEDITSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));

  // begin with the items update
  Items.BeginUpdate;
  try
    // if the combo edit is not empty, then clear the items and search through the FStoredItems
    if Text <> '' then begin
      Items.Clear;
      for I := 0 to FAutoCItems.Count - 1 do begin
        case fAutoCType of
          act_StartBegin_CaseSenitive  : if (AnsiStartsStr(Text, FAutoCItems[I])) then
                                          Items.Add(FAutoCItems[I]);

          act_NStartBegin_NCaseSenitive: if (Pos( uppercase(Text), uppercase(FAutoCItems[I]))>0) then
                                           Items.Add(FAutoCItems[I]);

          act_StartBegin               : if (AnsiStartsStr( uppercase(Text), uppercase(FAutoCItems[I]))) then
                                          Items.Add(FAutoCItems[I]);

          act_CaseSenitive             : if (Pos( Text, FAutoCItems[I]) > 0) then
                                          Items.Add(FAutoCItems[I]);
        end;
      end;
    end
    else begin
      // else the combo edit is empty, so then we'll use all what we have in the FStoredItems
      Items.Assign(FAutoCItems);
    end;
  finally
    // finish the items update
    Items.EndUpdate;
  end;
  // and restore the last combo edit selection

  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos, Selection.EndPos));
end;


//procedure TGEM_ComboBoxAutoC.InitACCombo;
//begin
//    FAutoCItems.OnChange:=nil;
//    FAutoCItems.Assign(Items);
//    AutoComplete := False;
//    FAutoCItems.OnChange := StoredItemsChange;
//    fUseAutoComplete:=true;
//    AutoCItemIndex := -1;
//end;
//

procedure TGEM_ComboBoxAutoC.KeyPress(var Key: Char);
// combo dropdown must be done in keypress, if its done on CBN_EDITUPDATE it messes up whole message processing mumbo-jumbo
begin
  inherited;
  if fUseAutoComplete and not (ord(key) in [13,27]) then begin
//    if (AutoC_Items.Count > 0) and (Items.Count = 0) then
//      Items.Assign(AutoC_Items);
    if (items.Count <> 0) and not DroppedDown then
      SendMessage(Handle, CB_SHOWDROPDOWN, 1, 0)   // something matched -> dropdown combo to display results
  end;
end;


procedure TGEM_ComboBoxAutoC.SetStoredItems(const Value: TStringList);
begin
//  if Assigned(FAutoCItems) then
//    FAutoCItems.Assign(Value)
//  else
  FAutoCItems := Value;
end;


procedure TGEM_ComboBoxAutoC.StoredItemsChange(Sender: TObject);
begin
//  if Assigned(FAutoCItems) then
  FilterItems;
end;

end.
