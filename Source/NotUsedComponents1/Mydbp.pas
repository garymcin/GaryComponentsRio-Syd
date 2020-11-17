unit Mydbp;

interface

uses
  WinApi.Windows, WinApi.Messages, WinTypes, WinProcs,

  System.SysUtils, System.Classes,

  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ExtCtrls, Vcl.DBCtrls,

  Data.DB;

type
  TDBPanel = class(TCustomPanel)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: String;
    function GetDataSource: TDataSource;
    procedure SetDataField(Const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure DataChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TdataSource read GetDataSource write SetDataSource;
end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TDBPanel]);
end;

function TDBPanel.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

function TDBPanel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBPanel.SetDataField(Const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TDBPanel.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBPanel.DataChange(Sender: TObject);
begin
  if FDataLink.Field = nil then
    Caption := ''
  else
    Caption := FDataLink.Field.AsString;
end;

constructor TDBPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
end;

destructor TDBPanel.Destroy;
begin
  FDataLink.Free;
  FDataLink.OnDataChange := nil;
  inherited Destroy;
end;

end.

