unit GEMDBMaskEdit;


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
  SysUtils, Classes, VCL.Mask, VCL.Controls, VCL.StdCtrls, VCL.Graphics, Windows, Messages,
  VCL.Forms, DB, VCL.DbCtrls, VCL.ComCtrls;

type
  TCloseActions = (dtnChange, dtnCloseUp);

  TGEMDBMaskEdit = class(TMaskEdit)
    private
      {private}
      fCloseAction: TCloseActions;
	  	fDataLink: TFieldDataLink; // we link only one field

      FOldBackColor : TColor;
      FColorOnEnter : TColor;
      FAlignment: TAlignment;
      FTabOnEnter: boolean;
      procedure SetAlignment(const Value: TAlignment);

      function  GetDataField: string;
      function  GetDataSource: TDataSource;
      //function  GetReadOnly: boolean;
      procedure SetDataField(const Value: string);
      procedure SetDataSource(const Value: TDataSource);
  		procedure DataChange(Sender: TObject);
	  	//procedure UpdateData(Sender: TObject);
		  procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
      property OldBackColor : TColor read FOldBackColor write FOldBackColor;
    protected
      {protected}
      procedure DoEnter; override;
      procedure DoExit; override;
      procedure KeyPress(var Key: Char); override;

      procedure CreateParams(var Params: TCreateParams); override;
  		procedure Loaded; override;
    public
      {public}
      constructor Create(Owner: TComponent); override;
      destructor Destroy; override;
    published
      {published}
      property Alignment : TAlignment read FAlignment write SetAlignment;
      property ColorOnEnter :TColor read FColorOnEnter write FColorOnEnter;
      property TabOnEnter : boolean read FTabOnEnter write FTabOnEnter;
      property CloseAction: TCloseActions read FCloseAction write FCloseAction;
      property DataField: string read GetDataField write SetDataField;
      property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMDBMaskEdit]);
//end;

{==============================================================================}
{ TDBLabel }
{==============================================================================}

procedure TGEMDBMaskEdit.CNNotify(var Message: TWMNotify);
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


constructor TGEMDBMaskEdit.Create(Owner: TComponent);
begin
  inherited;
  fDataLink:= TFieldDataLink.Create;
  fDataLink.Control:= self;
	fDataLink.OnDataChange:= DataChange;
	//fDataLink.OnUpdateData:= UpdateData;
end;


procedure TGEMDBMaskEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);

  with Params do
    Style := Style or Alignments[FAlignment];
end;

procedure TGEMDBMaskEdit.DataChange(Sender: TObject);
//var
//  DT: String;
begin
(*
    case fDataLink.Field.DataType of
      ftUnknown:;
      ftString:;
      ftSmallint:;
      ftInteger:;
      ftWord:;
      ftBoolean:;
      ftFloat:;
      ftCurrency:;
      ftBCD:;
      ftDate:;
      ftTime:;
      ftDateTime:;
      ftBytes:;
      ftVarBytes:;
      ftAutoInc:;
      ftBlob:;
      ftMemo:;
      ftGraphic:;
      ftFmtMemo:;
      ftParadoxOle:;
      ftDBaseOle:;
      ftTypedBinary:;
      ftCursor:;
      ftFixedChar:;
      ftWideString:;
      ftLargeint:;
      ftADT:;
      ftArray:;
      ftReference:;
      ftDataSet:;
      ftOraBlob:;
      ftOraClob:;
      ftVariant:;
      ftInterface:;
      ftIDispatch:;
      ftGuid:;
      ftTimeStamp:;
      ftFMTBcd:;
      ftFixedWideChar:;
      ftWideMemo:;
      ftOraTimeStamp:;
      ftOraInterval:;
      ftLongWord:;
      ftShortint:;
      ftByte:;
      ftExtended:;
      ftConnection:;
      ftParams:;
      ftStream:;
    end;
*)


  if Assigned(fDataLink.Field) then
    Text:= fDataLink.Field.AsString;
  //else
  //  DT:= Now;
  //Caption := DT;
  //if Kind = dtkData then
  //  Time:= DT
  //else
  //  Date:= DT;
end;


destructor TGEMDBMaskEdit.Destroy;
begin
	fDataLink.OnDataChange:= nil;
	fDataLink.OnUpdateData:= nil;
  fDataLink.Free;
  fDataLink:= nil;

  inherited;
end;


procedure TGEMDBMaskEdit.DoEnter;
begin
  OldBackColor := Color;
  Color := ColorOnEnter;

  inherited;
end;

procedure TGEMDBMaskEdit.DoExit;
begin
  Color := OldBackColor;

  inherited;
end;

function TGEMDBMaskEdit.GetDataField: string;
begin
  Result:= fDataLink.FieldName;
end;


function TGEMDBMaskEdit.GetDataSource: TDataSource;
begin
  Result:=  fDataLink.DataSource;
end;


procedure TGEMDBMaskEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
{
  if TabOnEnter AND (Owner is TWinControl) then begin
    If Key = #13 Then Begin
      If HiWord(GetKeyState(VK_SHIFT)) <> 0 then
        SelectNext(Owner as TWinControl,False,True)
      else
        SelectNext(Owner as TWinControl,True,True) ;
      //Key := #0
    end;
  end;
 }

  if TabOnEnter AND (Owner is TWinControl) then
  begin
    if Key = Char(VK_RETURN) then
    begin
     if HiWord(GetKeyState(VK_SHIFT)) <> 0 then
        PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 1, 0)
     else
        PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 0, 0);
    end;
  end;
end;

procedure TGEMDBMaskEdit.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then
    DataChange(self);
end;


procedure TGEMDBMaskEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TGEMDBMaskEdit.SetDataField(const Value: string);
begin
  fDataLink.FieldName:= Value;
end;


procedure TGEMDBMaskEdit.SetDataSource(const Value: TDataSource);
begin
  if not (fDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    fDataLink.DataSource:= Value;
  if Value <> nil then
    Value.FreeNotification(self);
end;

(*
procedure TGEMDBMaskEdit.UpdateData(Sender: TObject);
begin
  try
    fDataLink.OnDataChange := nil;
    fDataLink.DataSet.Edit;
    fDataLink.Field.AsString := Text;
  finally
    fDataLink.OnDataChange := DataChange;
  end;
end;
*)
end.
