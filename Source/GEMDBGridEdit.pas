unit GEMDBGridEdit;

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.DBCtrls, Vcl.DBGrids,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.ComCtrls, Vcl.CheckLst,

  Data.DB, GEMComponentsGlobal;



type

  TGEMDBGridEditForm = class(TForm)
    Panel: TPanel;
    CloseBtn: TButton;
    CheckListBox1: TCheckListBox;
//    fVersion: string;
    procedure CloseBtnClick(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
  private
    { Private declarations }
    fGridComponentName: string;
    ftheColumns: TDBGridColumns;
    function SetVersion: string;
  public
    { Public declarations }
    procedure CreateDataGridFieldVisible;

    property theColumns: TDBGridColumns read ftheColumns write ftheColumns;
    property GridComponentName: string read fGridComponentName write fGridComponentName;
    property Version: string read SetVersion;
  end;



var
  GEMDBGridEditForm: TGEMDBGridEditForm;

implementation

{$R *.dfm}

procedure TGEMDBGridEditForm.CreateDataGridFieldVisible;
var
  i: Byte;
const
  left = 5;
  top = 10;
begin
  CheckListBox1.Clear;
  for i := 0 to ftheColumns.Count - 1 do begin
    CheckListBox1.Items.add(ftheColumns[i].FieldName);
    CheckListBox1.Checked[i] := ftheColumns[i].Visible;;
  end;
end;


function TGEMDBGridEditForm.SetVersion: string;
begin
//  Result := VersionGEMDBGridEdit;
end;

procedure TGEMDBGridEditForm.CheckListBox1ClickCheck(Sender: TObject);
begin
  if CheckListBox1.Count > 0 then begin
    ftheColumns[CheckListBox1.ItemIndex].Visible := CheckListBox1.Checked[CheckListBox1.ItemIndex];
  end;
end;

procedure TGEMDBGridEditForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;


end.
