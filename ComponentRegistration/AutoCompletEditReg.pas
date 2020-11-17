unit AutoCompletEditReg;

interface

uses
  uAutoComplete;

procedure Register;

implementation

uses
  Classes;

procedure Register;
begin
  RegisterComponents('Garys Stuff', [TAutoCompleteEdit]);
end;

end.
