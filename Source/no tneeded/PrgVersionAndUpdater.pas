unit PrgVersionAndUpdater;

interface

{$DEFINE USE_CODESITE}
{.$DEFINE DEBUG_STUFF}


uses
  WinAPI.Shellapi, winApi.Windows, WinApi.URLMon,

  System.SysUtils, System.Variants, System.Classes, System.StrUtils,

  Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtActns,

  IdHTTP, IdComponent;//, Global;

type
  TUpdateType = (udNotAssessed, udNotNeeded, udNeeded, udNeededReady, udUserProgramNewer,
                 udUpdateCheckError, udNeededNotReady, udError);

//  TFTPStatusStuff = (fResolving, fConnecting, fConnected, fDisconnecting,
//                     fFTPDisconnected, fftpTransfer, fftpReady, fftpAborted);

  TWebUpdate = class(TThread)
    private
      function ReadVersionFile: boolean;
      function DownLoadVersionFile: boolean;
      function DownLoadInstallFile: boolean;
      procedure SendMessage(TextColor, BackGroundColor: tColor; aMessage, aVersionStr: string);
    protected
//      FTPUpdateStatus: TFTPStatusStuff;
      TheUpdateInstallFileName: string;
      NewPrgVersionLabel      : string;

      procedure Execute; override;
      function DoDownload(aUrlPathFileName, alocalPathFile: string): boolean;
      function CheckAndGetUpdates: TUpdateType;
    public

      SBar                     : TAppUpdateStatusBar; // use a status bar to out put thread info
      SBarPanel,
      sBarUpdateStrPanel       : integer;  // panel to use for display of status

      LocalVersionPathAndFile,
      UrlVersionPathAndFile,

      UrlUpdatePathAndFile,
      LocalUpdatePathAndFile   : string;

      OldPrgVersion            : string;
      NumPlacesInVersionNum    : byte;
      TheHandle                : integer;

      function CheckNeedForUpDate: TUpdateType;

      property ReturnValue;
  end;


implementation

uses
  GEM_Updater;

const
  WebUrl = 'http://www.slickrocksoftwaredesign.com/SpendingInstall/';


procedure TWebUpdate.Execute;
begin
  inherited;
  try
    ReturnValue := Ord(CheckAndGetUpdates);
  finally

  end;
end;


function TWebUpdate.DoDownload(aUrlPathFileName, alocalPathFile: string):Boolean;
var
  HTTPURL : TIdHTTP;
  MS: TMemoryStream;
begin
  HTTPURL := TIdHTTP.Create(nil);
  MS := TMemoryStream.Create;
  try
    try
      HTTPURL.Get(aUrlPathFileName, MS);
      MS.SaveToFile(alocalPathFile);
      result := True;
    except
      result := false;
    end;
  finally
    FreeAndNil(HTTPURL);
    FreeAndNil(MS);
  end;
 end;


procedure TWebUpdate.SendMessage(TextColor, BackGroundColor: tColor; aMessage, aVersionStr: string);
begin
  SBar.Panels[SBarPanel].Color := BackGroundColor;
  SBar.Panels[SBarPanel].Font.Color := TextColor;
  SBar.Panels[SBarPanel].Font.Size := 8;
  SBar.Panels[SBarPanel].Text := aMessage;
  SBar.Panels[sBarUpdateStrPanel].Text := aVersionStr;
end;


function TWebUpdate.CheckAndGetUpdates: TUpdateType;
begin
//(fNotAssessed, fNotNeeded, fNeeded, fNeededReady, fUserProgramNewer, fUpdateCheckError, fNeededNotReady);
  result := udNotAssessed;
  SendMessage(clBlack, clYellow, 'Checking for Updates', 'a.');
  if DownLoadVersionFile then begin
    SendMessage(clBlack, clYellow, 'Downloading Version File', 'b.');
    if ReadVersionFile then begin
      SendMessage(clBlack, clYellow, 'Reading Version File', 'c.');
      case CheckNeedForUpDate of
        udNeeded: begin
          result := udNeededNotReady;
          SendMessage(clBlack, clYellow, 'Getting Update Ready: '+ NewPrgVersionLabel, NewPrgVersionLabel);
          // next we download the install file
          if DownLoadInstallFile then begin
            SendMessage(clBlack, clYellow, 'Update Available: ' + NewPrgVersionLabel, NewPrgVersionLabel);
            result := udNeededReady;
            //UpdateStatus := fNeeded;
          end;
        end;
        // all 3 below stop update checking
        udNotNeeded: begin
          //UpdateStatus := fNotNeeded;
          result := udNotNeeded;
          SendMessage(clBlack, clLime, 'No Updates Available', '');
        end;
        udUserProgramNewer: begin
          //UpdateStatus := fUserProgramNewer;
          result := udUserProgramNewer;
          SendMessage(clYellow, clRed, 'User Program newer than update', '');
        end;
        udUpdateCheckError: begin
          //UpdateStatus := fUpdateCheckError;
          result := udError;
          SendMessage(clYellow, clRed, 'Error determining need for update', '');
        end;
      end;
    end // end read version file
    else begin  //  error reading version file
      result := udError;
      SendMessage(clYellow, clRed, 'Error Reading Version File', '');
    end;
  end // end download version file
  else begin // error downloading version file
    result := udError;
    SendMessage(clYellow, clRed, 'Version File Download ERROR', '');
  end;
end;


function TWebUpdate.DownLoadInstallFile: boolean;
begin
  result := true;
  if DoDownload(UrlUpdatePathAndFile, LocalUpdatePathAndFile) then
    SendMessage(clBlack, clYellow, 'Installer File Downloaded', '')
  else begin
    SendMessage(clYellow, clRed, 'ERROR Installer File', '');
    result := false;
  end;
end;


function TWebUpdate.DownLoadVersionFile: boolean;
begin
  if DoDownload(UrlVersionPathAndFile, LocalVersionPathAndFile) then
    result := true
  else begin
    result := false;
  end
end;


function TWebUpdate.CheckNeedForUpDate: TUpdateType;
var
  UserMajor, UserDb, UserMinor, s: string;
  WebMajor, WebDb, WebMinor: String;
begin
  WebDb := '0';
  UserDb := '0';
{$REGION 'DOC'}
  //  This routine compares the version number of the users current program with
  //  the version number found in the version file on the web.  The number of
  //  places of the version number is 2 (1.2) or 3 (6.21.5).
  {$ENDREGION}
  try
    WebMajor := LeftStr(NewPrgVersionLabel, Pos('.', NewPrgVersionLabel)-1);
    s :=  NewPrgVersionLabel;
    delete(s, 1, Pos('.', NewPrgVersionLabel));

    if NumPlacesInVersionNum = 3 then  begin
      WebDb := LeftStr(s, Pos('.', s)-1);
      delete(s, 1, Pos('.', s));
    end;
    WebMinor := s;

    //  Version nums from the program
    UserMajor := LeftStr(OldPrgVersion, Pos('.', OldPrgVersion)-1);
    s :=  OldPrgVersion;
    delete(s, 1, Pos('.', OldPrgVersion));

    if NumPlacesInVersionNum = 3 then  begin
      UserDb := LeftStr(s, Pos('.', s)-1);
      delete(s, 1, Pos('.', s));
    end;
    UserMinor := s;
    // test need for update
    result := udNotNeeded;
    // major version first
    if StrToInt(WebMajor) > StrToInt(UserMajor) then begin
      result := udNeeded;
      exit;
    end;

    if StrToInt(WebMajor) < StrToInt(UserMajor) then begin
      result := udUserProgramNewer;
      exit;
    end;

    if (StrToInt(WebDb) > StrToInt(UserDb)) and (NumPlacesInVersionNum = 3) then begin
      result := udNeeded;
      exit;
    end;

    if (StrToInt(WebDb) < StrToInt(UserDb)) and (NumPlacesInVersionNum = 3) then begin
      result := udUserProgramNewer;
      exit;
    end;

    if StrToInt(WebMinor) > StrToInt(UserMinor) then begin
      result := udNeeded;
      exit;
    end;

    if StrToInt(WebMinor) < StrToInt(UserMinor) then begin
      result := udUserProgramNewer;
      exit;
    end;
  except
    result := udUpdateCheckError;
  end;
end;


function TWebUpdate.ReadVersionFile: boolean;
var
  myFile : TextFile;
begin
  try
    AssignFile(myFile, LocalVersionPathAndFile);
    Reset(myFile);
      ReadLn(myFile, NewPrgVersionLabel);
      ReadLN(MyFile, TheUpdateInstallFileName);
    CloseFile(myFile);
    result := true;
  except
    result := false;
  end;

end;

end.








