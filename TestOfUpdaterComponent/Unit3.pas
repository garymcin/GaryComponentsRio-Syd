unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ActnMan,
  Vcl.ActnColorMaps, JvExStdCtrls, JvBehaviorLabel, JvExControls, JvLabel,
  JvAutoComplete, ColorButton, GEMUpdaterPanel, System.win.registry,

  CodeSiteLogging, CodeSiteMessage, nxllComponent, nxdb;

type
  TForm3 = class(TForm)
    gmpdtr_1: TGEMAppUpdater;
    btn_1: TButton;
    lst_1: TListBox;
    lbl_1: TLabel;
    btn_2: TButton;
    lbl_2: TLabel;
    procedure gmpdtr_1Click(Sender: TObject);
    procedure btn_1Click(Sender: TObject);
    procedure btn_2Click(Sender: TObject);
  private
    { Private declarations }
    fLocationNameOfUpdaterApp : string;
    function GetAppVersionStr(aNumPlaces: Byte): string;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

const
//  cRootKey               = HKEY_CURRENT_USER;
  cRootKey                 = HKEY_LOCAL_MACHINE;
  cRegistryCompanyName     = 'SOFTWARE\\SlickRockSoftwareDesign\\';
  cRegistryCompanyName32   = 'SOFTWARE\\WOW6432Node\\SlickRockSoftwareDesign\\';



procedure TForm3.gmpdtr_1Click(Sender: TObject);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'gmpdtr_1Click' );{$ENDIF}

  ShowMessage('In app on click');

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'gmpdtr_1Click' );{$ENDIF}
end;


procedure TForm3.btn_1Click(Sender: TObject);
var
  reg: TRegistry;
  vKey: string;
  openResult: Boolean;
//const
//  regLoc = 'Computer\HKEY_LOCAL_MACHINE\SOFTWARE\SRSDUpdater\InstallDir';
begin
//  fLocationNameOfUpdaterApp := ReadRegister('SRSDUpdater', 'AppLocation', rvtString);
//openResult := reg.OpenKey('Software\MyCompanyName\MyApplication\',True);

  reg := TRegistry.Create;//(KEY_WRITE OR KEY_WOW64_64KEY);
//  try
//    vKey := cRegistryCompanyName + 'SRSDUpdater\';
//    reg.RootKey := cRootKey;
//
//    reg.Access := KEY_WRITE;
//
//    openResult :=  reg.OpenKey(vKey, true);
//    if not openResult then begin
//      ShowMessage('A. did not create key');
//      if not openResult then begin
//        ShowMessage('B. did not create key');
//      end;
//      Exit;
//    end;
//
//    if not reg.KeyExists('AppLocation') then begin
//      reg.WriteString('AppLocation', 'C:\Program Files\SlickRockSoftwareDesign\Spending\Spending.exe');
//    end;
//

  reg := TRegistry.Create;//(KEY_WRITE OR KEY_WOW64_64KEY);
  try
    vKey := cRegistryCompanyName + 'SRSDUpdater\';
    reg.RootKey := cRootKey;
    reg.Access := KEY_READ;
    openResult :=  reg.OpenKey(vKey, false);
    if not openResult then begin
      ShowMessage('Did not find key');
      Exit;
    end;
    if Reg.ValueExists('AppLocation') then begin
      fLocationNameOfUpdaterApp := reg.ReadString('AppLocation');
      lbl_2.Caption := fLocationNameOfUpdaterApp;
    end
    else begin
      ShowMessage('Did not find Update Location');
    end;
  finally
    FreeAndNil(reg);
  end;

end;

procedure TForm3.btn_2Click(Sender: TObject);
var
  urlpath: string;
begin
  urlpath := 'http://www.slickrocksoftwaredesign.com/SpendingInstall/spendingInstall.exe';
  urlpath := StringReplace(urlpath, '/', '\', [rfReplaceAll]);
  urlpath := ExtractFileName(urlpath);
  lbl_1.Caption := urlpath;
end;

function TForm3.GetAppVersionStr(aNumPlaces: Byte): string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetAppVersionStr' );{$ENDIF}

  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  case aNumPlaces of
    3: begin
      Result := Format('%d.%d.%d',
        [LongRec(FixedPtr.dwFileVersionMS).Hi,   //major
         LongRec(FixedPtr.dwFileVersionMS).Lo,   //minor
         LongRec(FixedPtr.dwFileVersionLS).Hi]); //release
    end;
    4: begin
      Result := Format('%d.%d.%d.%d',
        [LongRec(FixedPtr.dwFileVersionMS).Hi,  //major
         LongRec(FixedPtr.dwFileVersionMS).Lo,  //minor
         LongRec(FixedPtr.dwFileVersionLS).Hi,  //release
         LongRec(FixedPtr.dwFileVersionLS).Lo]); //build

    end;
    else begin
      Result := Format('%d.%d.%d.%d',
        [LongRec(FixedPtr.dwFileVersionMS).Hi,    //major
         LongRec(FixedPtr.dwFileVersionMS).Lo]);  //minor
    end;
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetAppVersionStr' );{$ENDIF}
end;


end.




