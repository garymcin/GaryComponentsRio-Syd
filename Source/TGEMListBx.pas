unit TGEMListBx;
interface
uses
  System.Classes, System.StrUtils,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics;

type
  TSearchType = (fBegin, fAnyWhere, fEnd);

  TGEMTStrings = class(TStringList)
    protected
      fMaxLines: word;
    private
    published
      property MaxLines: word read fMaxLines write fMaxLines;
      function AddAndCheckNumLines(const S: string): Integer; virtual;
      procedure AddAtIndexAndCheckNumLines(const S: string; TheIndex: integer); virtual;
  end;
  {TGEMTStrings}


  TGEMListBox = class(TListBox)
  protected
  private
    fSearchType     : TSearchType;
    fCaseSensitive  : boolean;
    fSearchString   : string;
    fMaxNumberLines : integer;
    fMess           : string;
    FSearchIndex    : integer;

    procedure SetMaxLines(Value: integer);
    procedure SLChange(aSender: TObject);
    procedure SetSLItems(const Value: TGEMTStrings);
  public
    function GetCount: Integer; override;
  published
    Itemsgem: TGEMTStrings;

    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property ItemIndex;
    property Color;
    property Count: Integer Read GetCount;
    property Font;
    property SearchString: string read fSearchString write fSearchString;
    property SearchCaseSenitive: boolean read fCaseSensitive write fCaseSensitive default False;
    property Visible;
    property OnClick;
    property CustomHint;
    property OnContextPopup;
    property MaxNumberLines: integer read fMaxNumberLines write SetMaxLines Default 0;
    property SearchType: TSearchType read fSearchType write fSearchType Default fAnyWhere;
    property Mess: string read fMess write fMess;
    property Items: TGEMTStrings read Itemsgem write SetSLItems;
    function CompareSearchString(SearchString: String): integer;
    function CompareSerachStringNext: boolean;
    function CompareSearchStringFromIndex(Searchstring: string; StartIndex: Integer): integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end {TGEMListBox};



implementation
uses
  SysUtils;

{  TGEMTStrings  }

procedure TGEMTStrings.AddAtIndexAndCheckNumLines(const S: string; theIndex: Integer);
begin
  Insert(TheIndex, s);
  while (Count > 0) and (Count > fMaxLines) do
    Delete(Count-1);

end;


function TGEMTStrings.AddAndCheckNumLines(const S: string): Integer;
begin
  result := -1;

  Add(s);
  while (Count > 0) and (Count > fMaxLines) do
    Delete(0);
end;

{  TGEMListBox  }

function TGEMListBox.CompareSearchStringFromIndex(Searchstring: string; StartIndex: Integer): integer;
var
  UCS: string;
begin
  result := -1;
  if not (Count > 0) then
    exit;
  if not FCaseSensitive then
    UCS := UpperCase(SearchString);

  ItemIndex := StartIndex;
  FSearchIndex := ItemIndex;
  case fSearchType of
    fBegin:
      if not fCaseSensitive then
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiStartsStr(UpperCase(Itemsgem[FSearchIndex]), UCS)) do begin
          Inc(FSearchIndex);
        end
      else
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiStartsStr(Itemsgem[FSearchIndex], SearchString)) do begin
          Inc(FSearchIndex);
        end;
    fAnyWhere:
      if not fCaseSensitive then
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiContainsStr(UpperCase(Itemsgem[FSearchIndex]), UCS)) do begin
          Inc(FSearchIndex);
        end
      else
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiContainsStr(Itemsgem[FSearchIndex], SearchString)) do begin
          Inc(FSearchIndex);
        end;
    fEnd:
      if not fCaseSensitive then
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiEndsStr(UpperCase(Itemsgem[FSearchIndex]), UCS)) do begin
          Inc(FSearchIndex);
        end
      else
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiEndsStr(Itemsgem[FSearchIndex], SearchString)) do begin
          Inc(FSearchIndex);
        end;
  end;
  if FSearchIndex in[0..Itemsgem.Count-1] then
    result := FSearchIndex;
  ItemIndex := FSearchIndex;
end;


function TGEMListBox.CompareSerachStringNext: boolean;
var
  UCS: string;
begin
  result := false;
  if not (Count > 0) then
    exit;
  if not FCaseSensitive then
    UCS := UpperCase(SearchString);

  FSearchIndex := ItemIndex + 1;
  case fSearchType of
    fBegin:
      if not fCaseSensitive then
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiStartsStr(UpperCase(Itemsgem[FSearchIndex]), UCS)) do begin
          Inc(FSearchIndex);
        end
      else
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiStartsStr(Itemsgem[FSearchIndex], SearchString)) do begin
          Inc(FSearchIndex);
        end;
    fAnyWhere:
      if not fCaseSensitive then
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiContainsStr(UpperCase(Itemsgem[FSearchIndex]), UCS)) do begin
          Inc(FSearchIndex);
        end
      else
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiContainsStr(Itemsgem[FSearchIndex], SearchString)) do begin
          Inc(FSearchIndex);
        end;
    fEnd:
      if not fCaseSensitive then
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiEndsStr(UpperCase(Itemsgem[FSearchIndex]), UCS)) do begin
          Inc(FSearchIndex);
        end
      else
        while (FSearchIndex <= Itemsgem.Count-1) and (not AnsiEndsStr(Itemsgem[FSearchIndex], SearchString)) do begin
          Inc(FSearchIndex);
        end;
  end;
  if FSearchIndex in[0..Itemsgem.Count-1] then
    result := true;
  ItemIndex := FSearchIndex;
end;


function TGEMListBox.CompareSearchString(SearchString: String): integer;
var
  index: Integer;
  UCs: string;
begin
  result := -1;
  if not (Count > 0) then
    exit;

  index := 0;
  //index := Itemsgem.Count-1;
  fSearchString := SearchString;
  if not FCaseSensitive then
    UCS := UpperCase(SearchString);
  case fSearchType of
    fBegin:
      if not fCaseSensitive then
        while (index <= Itemsgem.Count-1) and (not AnsiStartsStr(UpperCase(Itemsgem[index]), UCS)) do begin
          Inc(index);
        end
      else
        while (index <= Itemsgem.Count-1) and (not AnsiStartsStr(Itemsgem[index], SearchString)) do begin
          Inc(index);
        end;

    fAnyWhere:
      if not fCaseSensitive then
        while (index <= Itemsgem.Count-1) and (not AnsiContainsStr(UpperCase(Itemsgem[index]), UCS)) do begin
          Inc(index);
        end
      else
        while (index <= Itemsgem.Count-1) and (not AnsiContainsStr(Itemsgem[index], SearchString)) do begin
          Inc(index);
        end;

    fEnd:
      if not fCaseSensitive then
        while (index <= Itemsgem.Count-1) and (not AnsiEndsStr(UpperCase(Itemsgem[index]), UCS)) do begin
          Inc(index);
        end
      else
        while (index <= Itemsgem.Count-1) and (not AnsiEndsStr(Itemsgem[index], SearchString)) do begin
          Inc(index);
        end;
  end;
  if index in[0..Itemsgem.Count-1] then
    result := index;
  ItemIndex := index;
end;


constructor TGEMListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Itemsgem := TGEMTStrings.Create;
  Itemsgem.OnChange:= SLChange;
end {Create};


destructor TGEMListBox.Destroy;
begin
  FreeAndNil(Itemsgem);
  inherited;
end;


function TGEMListBox.GetCount: Integer;
begin
  result := Items.GetCount;
  inherited;
end;


procedure TGEMListBox.SetMaxLines(Value: integer);
begin
  Itemsgem.MaxLines := value;
  fMaxNumberLines := value;
end;


procedure TGEMListBox.SetSLItems(const Value: TGEMTStrings);
begin
  Itemsgem.Assign(Value);
end;


procedure TGEMListBox.SLChange(aSender: TObject);
begin
  // This line updates the original string repository with the changes made to SL
  TListBox(Self).Items := Itemsgem;
end;


end.
