unit ModListnxTable;

interface

{$REGION 'DOC'}{
This unit adds OnModifiedField event handler.  It adds a string list of all fields that
have been modified. This is added to tnxtable.

Using this component as a substitute for TnxTable allows you to check the
ModifiedFieldList property in the Before or After Post methods.  The event
OnModifiedField is triggered each time a field value is modified.  It passes
the TField in as the parameter.  Using this event you can capture each field as
it is modified in real-time.

From:
Delphi TFields -- What Changed?
By Steve Zimmelman
}
{$ENDREGION}

uses
  System.SysUtils, System.Classes,

  Data.DB, nxdb, GEMComponentsGlobal;

type
  TMyFieldModEvent = Procedure(AField:TField) of Object ;

  TGEMNxTable = Class(TNxTable)
  Private
     FModifiedList : TStringList ;
     FFieldModEvent: TMyFieldModEvent ;
     Procedure AddToList(AFld:TField);
  Protected
    Procedure DataEvent(Event: TDataEvent; Info: NativeInt);Override ;
    Procedure DoBeforeEdit ; Override ;
    function GetVersion: string;
  Public
     Constructor Create(AOwner : TComponent); Override ;
     Destructor Destroy ; Override ;
     Property ModifiedFieldList : TStringList Read FModifiedList ;
  Published
    Property OnModifiedField : TMyFieldModEvent
                                      Read FFieldModEvent Write FFieldModEvent;
    property Version: string read GetVersion;
  End;

//procedure Register;


implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMNxTable]);
//end;

{ TGEMNxTable }

constructor TGEMNxTable.Create(AOwner: TComponent);
begin
  inherited;
  FModifiedList := TStringList.Create;
end;


destructor TGEMNxTable.Destroy;
begin
  FModifiedList.Free;
  inherited;
end;


function TGEMNxTable.GetVersion: string;
begin
  Result := VersionGEMnxTable;
end;


procedure TGEMNxTable.AddToList(AFld: TField);
Var
  i : Integer ;
Begin
  i := FModifiedList.IndexOf(AFld.FieldName);
  If (i < 0) Then
    FModifiedList.AddObject(AFld.FieldName,AFld);
end;


procedure TGEMNxTable.DataEvent(Event: TDataEvent; Info: NativeInt);
Var
  Fld : TField ;
begin
  inherited;
  Fld := TField(Info);
  Case Event Of
    deFieldChange: Begin
      If (Fld <> Nil) Then Begin
        AddToList(Fld);
        If Assigned(FFieldModEvent) Then
          FFieldModEvent(Fld);
      End;
    End;
  End;
end;

procedure TGEMNxTable.DoBeforeEdit;
begin
  // Clear old values
  FModifiedList.Clear ;
  Inherited DoBeforeEdit ;
end;

end.
