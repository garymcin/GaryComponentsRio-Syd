unit uRegister;

interface

uses
  WinApi.Windows,

  System.Win.Registry,

  Vcl.Dialogs;

type
  TRegValueType = (rvtString, rvtInteger, rvtDateTime, rvtBool,
                   rvtExpString, rvtFloat, rvtDate);


function ReadRegister(aApp, aSubKeyStr: string; aKeyType: TRegValueType): variant;

procedure InsertToReg(aApp, aSubKeyStr: string; aSubKeyValue: Variant;
                      aAskToCreateKey: boolean; aKeyType: TRegValueType);


implementation

const
  cRootKey = HKEY_LOCAL_MACHINE;
  cCompanyName = 'Software\SlickRockSoftwareDesign\';


{Function to return the contents of a determined registry}
function ReadRegister(aApp, aSubKeyStr: string; aKeyType: TRegValueType): variant;
var
  reg        : TRegistry;
  openResult : Boolean;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := cRootKey;
    openResult := reg.OpenKey(cCompanyName + aApp+'\', True);
//    openResult := reg.OpenKey(aKey, True);

    if OpenResult then
      case aKeyType of
        rvtString: result := reg.ReadString(aSubKeyStr);

        rvtInteger: result := reg.ReadInteger(aSubKeyStr);

        rvtDateTime: result := reg.ReadDateTime(aSubKeyStr);

        rvtBool: result := reg.ReadBool(aSubKeyStr);

        rvtFloat: result := reg.ReadFloat(aSubKeyStr);

        rvtDate: result := reg.ReadDate(aSubKeyStr);

      end
    else
      MessageDlg('Updater Key not found!', mtError, mbOKCancel, 0);

  finally
    reg.CloseKey();
  end;
end;


procedure InsertToReg(aApp, aSubKeyStr: string; aSubKeyValue: Variant;
                      aAskToCreateKey: boolean; aKeyType: TRegValueType);
var
  reg        : TRegistry;
  openResult : Boolean;
  vKey       : string;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := cRootKey;
    vKey := cCompanyName + aApp+'\';
//    vSubKey := aSubKeyStr + '\';

//    openResult := reg.OpenKey(vKey, True);

    if (not reg.KeyExists(vKey)) then begin
        MessageDlg('Key not found! Created now.',
                              mtInformation, mbOKCancel, 0);
      end;
    reg.Access := KEY_WRITE;
    openResult := reg.OpenKey(vKey, True);

    if not openResult = True then
      begin
        MessageDlg('Unable to create key! Exiting.',
                    mtError, mbOKCancel, 0);
        Exit();
      end;

    {checking if the values exist and inserting when neccesary}

    case aKeyType of
      rvtString: reg.WriteString(aSubKeyStr, aSubKeyValue);

      rvtInteger: reg.WriteInteger(aSubKeyStr, aSubKeyValue);

      rvtDateTime: reg.WriteDateTime(aSubKeyStr, aSubKeyValue);

      rvtBool: reg.WriteBool(aSubKeyStr, aSubKeyValue);

      rvtExpString: reg.WriteExpandString(aSubKeyStr, aSubKeyValue);

      rvtFloat: reg.WriteFloat(aSubKeyStr, aSubKeyValue);

      rvtDate: reg.WriteDate(aSubKeyStr, aSubKeyValue);

    end;

  finally
    reg.CloseKey();
  end;
end;

end.
