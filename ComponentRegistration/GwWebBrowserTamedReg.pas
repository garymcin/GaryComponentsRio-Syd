unit GwWebBrowserTamedReg;

interface

procedure Register;

implementation
uses
    System.Classes,
    GwWebBrowserTamed;

{$R .\GwWebBrowserTamed.dcr}

procedure Register;
Begin
  RegisterComponents('Wideman', [TGwWebBrowserTamed]);
End;
end.
