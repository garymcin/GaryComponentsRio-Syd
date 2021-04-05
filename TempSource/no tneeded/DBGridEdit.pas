unit DBGridEdit;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.DBCtrls, Vcl.DBGrids,
  Vcl.StdCtrls, Vcl.ExtCtrls;


type
  // this is  a TCheckBox itercepter class.  It adds two more items to the checkbox
  //   class.  It also overrides the click event
  TGEMCheckBox = class(TCheckBox)
  private
    fDBColumn: TColumn;
    fGridComponentName: string;
    procedure SetfDBColumn(Value: TColumn);
  public
    procedure Click; override;
  published
    property DBColumn : TColumn read fDBColumn write SetfDBColumn;
    property GridComponentName: string read fGridComponentName write fGridComponentName;
  end;

  // this record holds the array of check boxes that will be placed on the panel
  //  on the form
  TDataGridCheckBoxArray = record
  private
    fCount: Integer;
    fParent: TWinControl;
    fCreateRun: Boolean;
    fPosX, fPosY: integer;
  public
    DataGridCheckBoxArray: array of TGEMCheckBox;
    constructor Create(TheParent: TWinControl; StartX, StartY: integer);
    procedure DestroyAllCheckBoxes;
    procedure MakeACheckBox;
    property CreateRun: Boolean read fCreateRun;
    property Count: Integer read fCount;
    property PosY: Integer read fPosY;
  end;


  TDBGridEditForm = class(TForm)
    Panel: TPanel;
    CloseBtn: TButton;
    Label1: TLabel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    DataGridCheckBoxs : TDataGridCheckBoxArray;
    fXPos, fYPos: Integer;
    fGridComponentName: string;
    ftheColumns: TDBGridColumns;
    //ftheDBGrid: TDBGrid;
    ftheHeight: Integer;
  public
    { Public declarations }
    procedure CreateDataGridFieldVisible;

    property theColumns: TDBGridColumns read ftheColumns write ftheColumns;
    property GridComponentName: string read fGridComponentName write fGridComponentName;
    property Xpos: Integer read fXPos write fXPos;
    property Ypos: Integer read fYPos write fYPos;
    property theHieght: Integer read ftheHeight write ftheHeight;
  end;



var
  DBGridEditForm: TDBGridEditForm;

implementation

{$R *.dfm}

procedure TDBGridEditForm.CreateDataGridFieldVisible;
var
  j: Integer;
const
  left = 5;
  top = 10;
begin
  DataGridCheckBoxs.Create(Panel, 8, 15);

  // create the checkboxes
  for j := 0 to ftheColumns.Count - 1 do begin
    DataGridCheckBoxs.MakeACheckBox();
    try
      DataGridCheckBoxs.DataGridCheckBoxArray[j].DBColumn := ftheColumns.Items[j];
      DataGridCheckBoxs.DataGridCheckBoxArray[j].GridComponentName := fGridComponentName;
      DataGridCheckBoxs.DataGridCheckBoxArray[j].Visible := true;
    except
      ShowMessage('In except');
    end;
  end;
  Panel.Height := DataGridCheckBoxs.PosY + 5;

  ClientHeight := Panel.Height + CloseBtn.Height;
  CloseBtn.Top := Panel.Height ;
end;


procedure TDBGridEditForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;


procedure TDBGridEditForm.FormDestroy(Sender: TObject);
begin
  DataGridCheckBoxs.DestroyAllCheckBoxes;
end;


procedure TDBGridEditForm.FormShow(Sender: TObject);
begin
  CreateDataGridFieldVisible;
end;

{ TCheckBox ===================================================================}
{ TCheckBox ===================================================================}
{ TCheckBox ===================================================================}

procedure TGEMCheckBox.Click;
begin
  if fGridComponentName <> '' then
    fDBColumn.Visible := Checked;
  inherited;
end;

procedure TGEMCheckBox.SetfDBColumn(Value: TColumn);
begin
  fDBColumn := Value;
  //Caption := fDBColumn.FieldName;
  Caption := fDBColumn.Title.Caption;
  Checked := fDBColumn.Visible;
end;


{ DataGridCheckBoxArray =======================================================}
{ DataGridCheckBoxArray =======================================================}
{ DataGridCheckBoxArray =======================================================}

constructor TDataGridCheckBoxArray.Create(TheParent: TWinControl; StartX, StartY: Integer);
begin
  if fCount > 0 then
    DestroyAllCheckBoxes;
  // this is the number of checkboxes created.
  fCount := 0;
  //  need the parent the checkbox is place on or the checkbox will not be visible.
  fParent := TheParent;
  fCreateRun := True;
  // the start of the check box placement.  fPosx is held constant and the fPosY
  //  is incremented by the heigth of the check box created.  This forms a
  //  column of check boxes
  fPosX := StartX;
  fPosY := StartY;
end;


procedure TDataGridCheckBoxArray.DestroyAllCheckBoxes;
// this has to be in all forms destroy method, unless you like memory leaks
var
  cnt: Word;
begin
  //ShowMessage(IntToStr(High(DataGridCheckBoxArray)));
  if fCount > 0 then
    for cnt := Low(DataGridCheckBoxArray) to High(DataGridCheckBoxArray) do
      FreeAndNil(DataGridCheckBoxArray[cnt]);
  fCount := 0;
  fCreateRun := False;
end;


procedure TDataGridCheckBoxArray.MakeACheckBox;
begin
  if fCreateRun then  begin
    SetLength(DataGridCheckBoxArray, fCount + 1);
    DataGridCheckBoxArray[fCount] := TGEMCheckBox.Create(nil) ;
    DataGridCheckBoxArray[fCount].Parent := fParent;

    DataGridCheckBoxArray[fCount].Left := fPosX;
    DataGridCheckBoxArray[fCount].Top := fPosY;

    Inc(fPosY, DataGridCheckBoxArray[fCount].Height);
    Inc(fCount);
  end;
end;

end.
