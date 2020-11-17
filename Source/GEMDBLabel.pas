unit GEMDBLabel;


{

Article:

Advanced editing - TadpEdit component

http://delphi.about.com/library/weekly/aa120603a.htm

Full source code of a TadpEdit Delphi component, an extension
to the standard TEdit control with properties like: ColorOnEnter
(changes the background color of the control when it receives the
input focus), Alignment (determines how the text in the edit component
is aligned), and TabOnEnter (allows the edit control to react on
the Enter key press as if the Tab key was pressed - sending the
focus to the next control in the tab order).
}

interface

uses
  Winapi.Windows, Winapi.Messages,

  System.SysUtils, System.Classes,

  VCL.Controls, VCL.StdCtrls, VCL.Graphics, VCL.Forms, VCL.DbCtrls,
  VCL.ComCtrls,

  DB;

type
  TCloseActions = (dtnChange, dtnCloseUp);

  TGEMDBLabel = class(TLabel)
    private
      {private}
      fCloseAction: TCloseActions;
	  	fDataLink: TFieldDataLink; // we link only one field

      function  GetDataField: string;
      function  GetDataSource: TDataSource;
      //function  GetReadOnly: boolean;
      procedure SetDataField(const Value: string);
      procedure SetDataSource(const Value: TDataSource);
  		procedure DataChange(Sender: TObject);
	  	//procedure UpdateData(Sender: TObject);
		  procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    protected
      {protected}
  		procedure Loaded; override;
    public
      {public}
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;
    published
      {published}
      property CloseAction: TCloseActions read FCloseAction write FCloseAction;
      property DataField: string read GetDataField write SetDataField;
      property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMDBLabel]);
//end;

{==============================================================================}
{ TDBLabel }
{==============================================================================}

procedure TGEMDBLabel.CNNotify(var Message: TWMNotify);
begin
(*
  with Message, NMHdr^ do
  begin
    case Code of

    end;
  end
	with Message, NMHdr^ do
  begin
		case code of
			DTN_DATETIMECHANGE:
        if CloseAction = dtnChange then UpdateData(self); // the system time has changed
      DTN_CLOSEUP:
        if CloseAction = dtnCloseUp then UpdateData(Self);
		end; //...case
	end; //...with
  *)
end;


constructor TGEMDBLabel.Create(Owner: TComponent);
begin
  inherited;
  fDataLink:= TFieldDataLink.Create;
  fDataLink.Control:= self;
	fDataLink.OnDataChange:= DataChange;
	//fDataLink.OnUpdateData:= UpdateData;
end;


procedure TGEMDBLabel.DataChange(Sender: TObject);
//var
//  DT: String;
begin
  if Assigned(fDataLink.Field) then
    Caption:= fDataLink.Field.AsString;
  //else
  //  DT:= Now;
  //Caption := DT;
  //if Kind = dtkTime then
  //  Time:= DT
  //else
  //  Date:= DT;
end;


destructor TGEMDBLabel.Destroy;
begin
	fDataLink.OnDataChange:= nil;
	fDataLink.OnUpdateData:= nil;
  fDataLink.Free;
  fDataLink:= nil;

  inherited;
end;


function TGEMDBLabel.GetDataField: string;
begin
  Result:= fDataLink.FieldName;
end;


function TGEMDBLabel.GetDataSource: TDataSource;
begin
  Result:=  fDataLink.DataSource;
end;


procedure TGEMDBLabel.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then
    DataChange(self);
end;


procedure TGEMDBLabel.SetDataField(const Value: string);
begin
  fDataLink.FieldName:= Value;
end;


procedure TGEMDBLabel.SetDataSource(const Value: TDataSource);
begin
  if not (fDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    fDataLink.DataSource:= Value;
  if Value <> nil then
    Value.FreeNotification(self);
end;

(*
procedure TGEMDBLabel.UpdateData(Sender: TObject);
begin
  try
    fDataLink.OnDataChange := nil;
    fDataLink.DataSet.Edit;
    fDataLink.Field.AsString := Caption;
  finally
    fDataLink.OnDataChange := DataChange;
  end;
end;
*)

end.
