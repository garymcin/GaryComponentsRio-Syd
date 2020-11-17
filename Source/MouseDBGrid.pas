unit MouseDBGrid;

interface

uses
Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
Grids, DBGrids;

type
  TMouseDBGrid = class(TDBGrid)
  private
  { Private declarations }
    pDragAndDrop : Boolean; //is Drag/Drop now
    pOnBeforeMouseUp : TMouseEvent;
    pOnBeforeMouseDown : TMouseEvent;
    pOnAfterMouseUp : TMouseEvent;
    pOnAfterMouseDown : TMouseEvent;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    protected
  { Protected declarations }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    public
  { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure StartDragAndDrop(Var Message : TMessage);virtual;

    property GridState : TGridState Read FGridState Write FGridState;
  published
  { Published declarations }
    property DragAndDrop : Boolean Read pDragAndDrop Write pDragAndDrop default False;
    property OnBeforeMouseUp : TMouseEvent Read pOnBeforeMouseUp Write pOnBeforeMouseUp;
    property OnBeforeMouseDown : TMouseEvent Read pOnBeforeMouseDown Write pOnBeforeMouseDown;
    property OnAfterMouseUp : TMouseEvent Read pOnAfterMouseUp Write pOnAfterMouseUp;
    property OnAfterMouseDown : TMouseEvent Read pOnAfterMouseDown Write pOnAfterMouseDown;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TMouseDBGrid]);
end; (* Register *)

//=============================================================================

constructor TMouseDBGrid.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  pDragAndDrop:=False;
  pOnBeforeMouseUp :=Nil;
  pOnAfterMouseUp :=Nil;
  pOnBeforeMouseDown:=Nil;
  pOnAfterMouseDown :=Nil;
End;


procedure TMouseDBGrid.WMLButtonDown(var Message: TMessage);
Begin
  inherited;
  If (DragMode=dmManual) And DragAndDrop Then
    StartDragAndDrop(Message);
End;


procedure TMouseDBGrid.StartDragAndDrop(Var Message : TMessage);
Begin
  BeginDrag(False);
End;


procedure TMouseDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
Begin
  If Assigned(pOnBeforeMouseUp) Then
    pOnBeforeMouseUp(Self,Button,Shift,X,Y);

  inherited MouseUp(Button,Shift,X,Y);

  If Assigned(pOnAfterMouseUp) Then
    pOnAfterMouseUp(Self,Button,Shift,X,Y);
End;


procedure TMouseDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
Begin
  If Assigned(pOnBeforeMouseDown) Then
    pOnBeforeMouseDown(Self,Button,Shift,X,Y);

  inherited MouseDown(Button,Shift,X,Y);

  If Assigned(pOnAfterMouseDown) Then
    pOnAfterMouseDown(Self,Button,Shift,X,Y);
End;


end.

