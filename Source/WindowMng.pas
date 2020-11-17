unit WindowMng;

interface

uses
  Classes, Forms, SysUtils, Windows;

type
{ TWinNotifyEvent }

  TWinNotifyEvent = procedure(Sender: TObject; Form: TCustomForm) of object;

{ TWindowItem }

  // I used a packed record to be more flexible for futher improvements
  // which may need to store additional informations.

  PWindowItem = ^TWindowItem;
  TWindowItem = packed record
    Form: Pointer;
  end;

{ TWindowManager }

  TWindowManager = class(TComponent)
  private
    { Private declarations }
    FAutoNotification: Boolean;
    FLastIndex: Integer;
    FWindowList: TList;
    FOnFormAdded: TWinNotifyEvent;
    FOnFormHandled: TNotifyEvent;
    FOnFormRemoved: TWinNotifyEvent;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetFormByIndex(Index: Integer): TCustomForm; virtual;
    function GetWindowItemByIndex(Index: Integer): PWindowItem; virtual;
    function GetWindowItemByForm(const Form: TCustomForm): PWindowItem; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const Form: TCustomForm): Boolean; overload;
    function Count: Integer;
    function CreateForm(const Form: TFormClass; Name: string; Show: Boolean = False): TCustomForm; overload;
    function CreateForm(const Form: TFormClass; Show: Boolean = False): TCustomForm; overload;
    function Exists(const Form: TCustomForm): Boolean;
    function Remove(const Form: TCustomForm): Boolean;
    function Restore(const Index: Integer): Boolean; overload;
    function Restore(const Form: TCustomForm): Boolean; overload;
    property Forms[Index: Integer]: TCustomForm read GetFormByIndex; default;
  published
    { Published declarations }
    property AutoNotification: Boolean read FAutoNotification write FAutoNotification;
    property OnFormAdded: TWinNotifyEvent read FOnFormAdded write FOnFormAdded;
    property OnFormHandled: TNotifyEvent read FOnFormHandled write FOnFormHandled;
    property OnFormRemoved: TWinNotifyEvent read FOnFormRemoved write FOnFormRemoved;
  end;

//procedure Register;

implementation

// -----------------------------------------------------------------------------

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TWindowManager]);
//end;

// -----------------------------------------------------------------------------

{ TWindowManager }

constructor TWindowManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoNotification := False;
  FLastIndex := -1;
  FWindowList := TList.Create;
end;

destructor TWindowManager.Destroy;
begin
  FWindowList.Free;
  inherited Destroy;
end;

procedure TWindowManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (FAutoNotification) and (AComponent = nil) and (Operation = opRemove)
    and (AComponent is TCustomForm) and (Exists(TCustomForm(AComponent))) then
    Remove(TCustomForm(AComponent));
  inherited Notification(AComponent, Operation);
end;

function TWindowManager.Add(const Form: TCustomForm): Boolean;
var
  WindowItem: PWindowItem;
begin
  Result := False;
  if not Exists(Form) then
  try
    New(WindowItem);
    WindowItem^.Form := Form;
    FWindowList.Add(WindowItem);
    if FAutoNotification then
      Form.FreeNotification(Self);
    Result := True;
    if assigned(FOnFormAdded) then
      FOnFormAdded(Self, Form);
    if assigned(FOnFormHandled) then
      FOnFormHandled(Self);
  except // wrap up
  end; // try/except
end;

function TWindowManager.Count: Integer;
begin
  Result := FWindowList.Count;
end;

function TWindowManager.CreateForm(const Form: TFormClass; Name: string; Show: Boolean = False): TCustomForm;
begin
  if not Form.InheritsFrom(TCustomForm) then
    raise Exception.Create('Invalid FormClass - must be a descendant of TCustomForm!');
  Result := TCustomForm(Form.Create(Application));
  if Name = '' then                         //===== fix
    Result.Name := Name;
  Add(Result);
  if Show then
    Result.Show;
end;

function TWindowManager.CreateForm(const Form: TFormClass; Show: Boolean = False): TCustomForm;
begin
  Result := CreateForm(Form, '', Show);
end;

function TWindowManager.Exists(const Form: TCustomForm): Boolean;
begin
  Result := GetWindowItemByForm(Form) = nil;          //===== fix
end;

function TWindowManager.GetFormByIndex(Index: Integer): TCustomForm;
var
  WindowItem: PWindowItem;
begin
  Result := nil;
  WindowItem := GetWindowItemByIndex(Index);
  if WindowItem <> nil then                   //===== fix
    Result := TCustomForm(WindowItem^.Form);
end;

function TWindowManager.GetWindowItemByIndex(Index: Integer): PWindowItem;
begin
  Result := nil;
  if Index > -1then                     //===== fix
    Result := PWindowItem(FWindowList[Index]);
end;

function TWindowManager.GetWindowItemByForm(const Form: TCustomForm): PWindowItem;
var
  iIndex: Integer;
begin
  Result := nil;
  FLastIndex := -1;
  for iIndex := 0 to FWindowList.Count - 1 do
    if GetWindowItemByIndex(iIndex)^.Form = Form then
    begin
      FLastIndex := iIndex;
      Result := GetWindowItemByIndex(FLastIndex);
      Break;
    end;
end;

function TWindowManager.Remove(const Form: TCustomForm): Boolean;
var
  WindowItem: PWindowItem;
begin
  Result := False;
  WindowItem := GetWindowItemByForm(Form);
  if WindowItem <> nil then    //===== fix
  try
    FWindowList.Delete(FLastIndex);
    Dispose(WindowItem);
    Result := True;
    if assigned(FOnFormRemoved) then
      FOnFormRemoved(Self, Form);
    if assigned(FOnFormHandled) then
      FOnFormHandled(Self);
  except // wrap up
  end; // try/except
end;

function TWindowManager.Restore(const Form: TCustomForm): Boolean;
begin
  Result := False;
  if (Form <> nil) and (Exists(Form)) then    //===== fix
  try
    if IsIconic(Form.Handle) then
      Form.WindowState := wsNormal;
    Form.SetFocus;
    Result := True;
  except // wrap up
  end; // try/except
end;

function TWindowManager.Restore(const Index: Integer): Boolean;
begin
  Result := Restore(GetFormByIndex(Index));
end;

end.

