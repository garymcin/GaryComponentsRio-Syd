unit GEMosmMarkers;

interface
uses
  System.SysUtils, System.Classes, FMX.Controls, System.Types, System.StrUtils,

  GEMWebOSMapsConst;

type

  TMarker = class(TCollectionItem)
  private
    FWebOSMaps : TControl;
    FLatitude  : double;
    FDraggable : boolean;
    FTitle     : String;
    FLongitude : double;
    FIcon      : String;
    FVisible   : boolean;
    FTag       : integer;
    procedure SetDraggable(const Value: boolean);
    procedure SetIcon(const Value: String);
    procedure SetLatitude(const Value: double);
    procedure SetLongitude(const Value: double);
    procedure SetTitle(const Value: String);
    procedure SetVisible(const Value: boolean);
    procedure SetTag(const Value: integer);
  protected
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  published
    property Visible : boolean read FVisible write SetVisible;
    property Latitude : double read FLatitude write SetLatitude;
    property Longitude : double read FLongitude write SetLongitude;
    property Title : String read FTitle write SetTitle;
    property Icon : String read FIcon write SetIcon;
    property Draggable : boolean read FDraggable write SetDraggable;
    property Tag: integer read FTag write SetTag default 0;
  end;

  TMarkers = class(TCollection)
  private
    FWebOSMaps : TControl;
    function GetItem(Index : integer) : TMarker;
    procedure SetItem(Index : integer; Value : TMarker);
  protected
    function GetOwner : TPersistent; override;
    procedure Update(Item : TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AWebOSMaps : TControl);
    function Add(Latitude,Longitude:Double; Title,Icon :string; Draggable,Visible:Boolean): TMarker; overload;
    function Add(Latitude,Longitude:Double; Icon: string): TMarker; overload;
    function Add: TMarker; overload;
    property Items[index : integer] : TMarker read GetItem write SetItem; default;
  end;


implementation

{ TMarkers }

function TMarkers.Add(Latitude, Longitude: Double; Icon: string): TMarker;
begin

end;

function TMarkers.Add(Latitude, Longitude: Double; Title, Icon: string;
  Draggable, Visible: Boolean): TMarker;
begin

end;

function TMarkers.Add: TMarker;
begin

end;

constructor TMarkers.Create(AWebOSMaps: TControl);
begin

end;

function TMarkers.GetItem(Index: integer): TMarker;
begin

end;

function TMarkers.GetOwner: TPersistent;
begin

end;

procedure TMarkers.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;

end;

procedure TMarkers.SetItem(Index: integer; Value: TMarker);
begin

end;

procedure TMarkers.Update(Item: TCollectionItem);
begin
  inherited;

end;

{
}

{ TMarker }

procedure TMarker.Assign(Source: TPersistent);
var
  MarkerSource : TMarker;
begin
  if (Source is TMarker) then begin
    MarkerSource          := TMarker(Source);
    FLatitude             := MarkerSource.FLatitude;
    FLongitude            := MarkerSource.FLongitude;
    FDraggable            := MarkerSource.FDraggable;
    FTitle                := MarkerSource.FTitle;
    FIcon                 := MarkerSource.FIcon;
    FVisible              := MarkerSource.FVisible;
    FTag                  := MarkerSource.FTag;
    Changed(True);
  end
  else
    inherited;
end;

constructor TMarker.Create(Collection: TCollection);
begin
  inherited;
  FLatitude             := 0;
  FLongitude            := 0;
  FDraggable            := True;
  FTitle                := 'Marker'+inttostr(Index);
  FIcon                 := '';
  FVisible              := True;
  FTag                  := 0;
end;


destructor TMarker.Destroy;
begin
  inherited;
end;


procedure TMarker.SetDraggable(const Value: boolean);
begin
  FDraggable := Value;
//  if (Value) then
//    (FWebOSmaps as TTMSFMXWebOSMaps).ExecJScript('if ('+inttostr(Index)+'<allmarkerdrag.length) allmarkerdrag['+inttostr(Index)+'] = true;')
//  else
//    (FWebOSmaps as TTMSFMXWebOSMaps).ExecJScript('if ('+inttostr(Index)+'<allmarkerdrag.length) allmarkerdrag['+inttostr(Index)+'] = false');
end;

procedure TMarker.SetIcon(const Value: String);
begin

end;

procedure TMarker.SetLatitude(const Value: double);
begin

end;

procedure TMarker.SetLongitude(const Value: double);
begin

end;

procedure TMarker.SetTag(const Value: integer);
begin

end;

procedure TMarker.SetTitle(const Value: String);
begin

end;

procedure TMarker.SetVisible(const Value: boolean);
begin

end;

end.
