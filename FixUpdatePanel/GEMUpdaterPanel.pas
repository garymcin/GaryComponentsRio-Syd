unit GEMUpdaterPanel;

interface
{.$Define USE_CODESITE}

uses
  Winapi.Windows, WinApi.Messages, Winapi.ShellAPI, WinApi.ShlObj,
  WinApi.KnownFolders, WinApi.URLMon, Winapi.ActiveX,


  System.StrUtils, System.Classes, System.SysUtils, System.Variants,
  System.Notification, system.UITypes, System.Win.Registry,


  VCL.Graphics, VCL.Controls, VCL.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Dialogs,

  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdSocks, IdFTP,
  IdUserPassProvider, IdCustomTransparentProxy, IdFTPCommon, IdHTTP,
  IdComponent, IdIntercept, IdIOHandler, IdIOHandlerStream, IdCookieManager,
  IdBaseComponent, IdTCPConnection, IdTCPClient ,IdDICT,
  IdExplicitTLSClientServerBase,

  ColorButton,

  JvExExtCtrls, JvExtComponent, JvLabel;


type
  TRegValueType = (rvtString, rvtInteger, rvtDateTime, rvtBool,
                   rvtExpString, rvtFloat, rvtDate);

  TUpdateStatus = (udAccessing, udNotAssessed, udNotNeeded, udNeeded, udUserProgramNewer,
                   udUpdateCheckError, udNeededNotReady, udError, udRunningUpdater, udUpdateFileDownloaded);

  TAlertType = (atError, atOpeningDB, atDbOpened, atClosingDB, atUpdate, atNone);

  TWebDownLoadType = (wdt_HTTP, wdt_FTP);

  tSlashType = (st_Win, st_Web);

  tUpdaterVersionType = (udvt_AppOptionFile, udvt_Product);

  tStatusEvent = procedure(ResponseValue: TUpdateStatus) of Object;

  tEXEVersionType = (evt_ProductName, evt_ProductVersion, evt_FileVersion);

  tCheckVersionDownloadUpdateFile = (cvdu_JustCheckVersion, cvdu_DownloadInstallFile);

  TEXEVersionData = record
    CompanyName,
    FileVersion,
    ProductName,
    ProductVersion : string;
  end;


  TUrlFileName = class(Tobject)
  private
    fUrlFileName: string;
  public
    property Caption: string read fUrlFileName write fUrlFileName;
  end;

  { TWebUpdate ===================================}
  TWebUpdate = class(TThread)
    private
      idftp_FTP                : TIdFTP;
      idHndlrstck              : TIdIOHandlerStack;
      fTotalSize               : Integer;
      fWorkDone                : Integer;
      fBackGColor              : tColor;
      fFTPmsg                  : string;
      fTransferType            : TIDFTPTransferType;

      fBackGroundColor         : tColor;
      fColorNoUpdates          : tColor;
      fColorGetReadyUpdates    : tColor;
      fColorErrorGetUpdates    : tColor;
      fColorUserAppNewer       : tColor;

      fMessage,
      fVersionStr,
      fUrlFileName             : string;

      fFTPHost                 : string;
      fUserName                : string;
      fPassWord                : string;
      fFTPPort                 : Integer;

      fTransferVersonType      : TIDFTPTransferType;
      fTransferInstallType     : TIDFTPTransferType;
      fWebDownLoadType         : TWebDownLoadType;

      fLocalVersionPathAndFile : string;
      fUrlVersionPathAndFile   : string;
      fLocalUpdatePathAndFile  : string;
      fOldPrgVersion           : string;
      fInstallAppName          : string;
      fPathFileLocalInstall    : string;

      fNumPlacesInVersionNum   : byte;

      //there can be either checking of versions or checking of versions and download of update file
      fDownLoadUpdateFile      : tCheckVersionDownloadUpdateFile;

      fcb_1,
      fcb_2                    : TJvLabel;     // use a labels to out put thread info and display to user
      fcb_3                    : TUrlFileName; //use an object to get the url file name

      function ReadVersionFile    : boolean;
      function DownLoadVersionFile: boolean;
      function DownLoadInstallFile: boolean;

      procedure SendMessage(BackGroundColor: tColor; aMessage, aVersionStr, aUrlFileName: string);
      procedure idftp_FTPStatus(ASender: TObject; const AStatus: TIdStatus;
                                const AStatusText: string);
      procedure idftp_FTPWork(ASender: TObject; AWorkMode: TWorkMode;
                              AWorkCount: Int64);
      procedure idftp_FTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
                                   AWorkCountMax: Int64);
      procedure idftp_FTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
      procedure DoProgress;
    protected
      TheUpdateInstallFileName: string;
      NewPrgVersionLabel      : string;

      procedure Execute; override;
      function DoDownload(aUrlPathFileName, alocalPathFile: string): boolean;
      function CheckAndGetUpdates: TUpdateStatus;
    public
      function CheckNeedForUpDate: TUpdateStatus;

      property ReturnValue;

      //there can be either checking of versions or checking of versions and download of update file
      property DownLoadUpdateFile: tCheckVersionDownloadUpdateFile
                             read fDownLoadUpdateFile write fDownLoadUpdateFile;

      // FTP stuff
      property FTPPort : Integer read fFTPPort  write fFTPPort;
      property FTPHost : string  read fFTPHost  write fFTPHost;
      property PassWord: string  read fPassWord write fPassWord;
      property UserName: string  read fUserName write fUserName;

      property TransferVersonType : TIDFTPTransferType read fTransferVersonType  write fTransferVersonType;
      property TransferInstallType: TIDFTPTransferType read fTransferInstallType write fTransferInstallType;
      property WebDownLoadType    : TWebDownLoadType   read fWebDownLoadType     write fWebDownLoadType;

      property LocalVersionPathAndFile: string read fLocalVersionPathAndFile write fLocalVersionPathAndFile;
      property UrlVersionPathAndFile  : string read fUrlVersionPathAndFile   write fUrlVersionPathAndFile;
      property LocalUpdatePathAndFile : string read fLocalUpdatePathAndFile  write fLocalUpdatePathAndFile;
      property PathFileLocalInstall   : string read fPathFileLocalInstall    write fPathFileLocalInstall;

      property OldPrgVersion : string read fOldPrgVersion  write fOldPrgVersion;
      property InstallAppName: string read fInstallAppName write fInstallAppName;

      property ColorNoUpdates      : TColor read fColorNoUpdates       write fColorNoUpdates;
      property ColorGetReadyUpdates: TColor read fColorGetReadyUpdates write fColorGetReadyUpdates;
      property ColorErrorGetUpdates: TColor read fColorErrorGetUpdates write fColorErrorGetUpdates;
      property ColorUserAppNewer   : TColor read fColorUserAppNewer    write fColorUserAppNewer;

      property NumPlacesInVersionNum: Byte read fNumPlacesInVersionNum write fNumPlacesInVersionNum;

      property cb_1: TJvLabel     read fcb_1 write fcb_1;
      property cb_2: TJvLabel     read fcb_2 write fcb_2;
      property cb_3: TUrlFileName read fcb_3 write fcb_3;
    end;




  { TGEMAppUpdater ===============================}

  TGEMAppUpdater = class(TCustomGridPanel)
    UpdateBtn  : TColorButton;
    Status1Lbl : TJvLabel;
    Status1Lb2 : TJvLabel;
    Status1Lb3 : TUrlFileName;
  private
  { Private declarations }
    fPathFileLocalInstall: string;
    NotificationCenter   : TNotificationCenter;

    // updater stuff ============================
    fAppsToClose              : TStringList;
    fVersionFileName          : string; // name of Version file both on local and web site
    fLocalInstallPath         : string; // Local path to the install and version files
    fUrlInstallPath           : string; // Web path to the version file
    fUpdaterCaption           : string; // Caption for installer.
    fIconFileLocation         : string; // Location of app to be updated icon
    fFTPHost                  : string;
    fFTPUserName              : string;
    fFTPPassWord              : string;
    fWebDownLoadType          : TWebDownLoadType; // is it ftp or http
    fFTPPort                  : Integer;
    fFTPFolder                : string;
    fTransferVersonType       : TIDFTPTransferType;  // is if ascii or binary
    fTransferInstallType      : TIDFTPTransferType;  // is if ascii or binary

// these next lines may need to be removed
    fPathAndNameOfUpdaterApp: string; // Location of updater app
//    fPathAndUpdaterName         : string; // the path to the installer and the file name of the installer.
    fCanRunUpdate               : Boolean;
    fAppHnd                     : HWnd;
    fPlacesVersionNum           : byte;

    fColorNoUpdates,
    fColorGetReadyUpdates,
    fColorErrorGetUpdates,
    fColorUserAppNewer        : TColor;
    fUpdateVersionType        : tUpdaterVersionType;
    fDownLoadUpdateFile       : Boolean;
    // end updater stuff ========================

    fProgramInfo              : TEXEVersionData;
    fGemUpdaterVersion        : string;
    fUpdateString             : string;
//    fUpDaterPathType          : tUpDaterLocationType;
    fUpdateStatus             : TUpdateStatus;

    fEnabledBtn               : Boolean;

    procedure Click_UpdateBtnHandler(Sender: TObject); {Tnotify event}
    procedure SetCaption_UpdateButton(newValue: TCaption);
    function GetCaption_UpdateButton: TCaption;
    procedure SetAppsToClose(const Value: TStringList);
    function GetVersionStr: string;
    function GetVersionInfo(AIdent: tEXEVersionType; aVersionPlaces: byte): String;
  protected
  { Protected declarations }
    procedure SetEnabled(Value: Boolean); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;

    // updater stuff ===========================================================
    function AppUpdateInfoThread(Lbll, Lbl2: TJvLabel; Lbl3: TUrlFileName;
                  aTypeOfChecking: tCheckVersionDownloadUpdateFile): TWebUpdate;
    procedure UpDateThreadTerminated(Sender:TObject);
    procedure DisplayNotification(aNotification: TAlertType; aMsg: string);
    procedure RunUpdater;
    function CheckUpdaterIsReady: Boolean;

    // end updater stuff =======================================================
  public
  { Public declarations }
    fOnStatusChange: tStatusEvent;
    procedure StatusChange(const ResponseValue: TUpdateStatus);
    function StatusOfUpdate: TWebUpdate;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property UpdateStatus:TUpdateStatus read fUpdateStatus;
//    property InstallerFileName: string read fStatus3 write fStatus3;
  published
  { Published properties and events }
    // updater stuff ===========================================================
    property AppsToClose: TStringList read fAppsToClose write SetAppsToClose;
    property CanRunUpdate: Boolean read fCanRunUpdate;
    property VersionNumPlaces: Byte read fPlacesVersionNum write fPlacesVersionNum;
    property VersionFileName: string read fVersionFileName write fVersionFileName; // name of Version file both on local and web site
    property LocalInstallPath: string read fLocalInstallPath write fLocalInstallPath; // Local path to the install and version files
    property UrlInstallPath: string read fUrlInstallPath write fUrlInstallPath; // Web path to the install and version files
    property UpdaterCaption: string read fUpdaterCaption write fUpdaterCaption; // Caption for installer.
    property IconFileLocation: string read fIconFileLocation write fIconFileLocation; // Location of app to be updated icon

    property AppVersionStr: string read GetVersionStr;

    property ColorNoUpdates: TColor read fColorNoUpdates write fColorNoUpdates;
    property ColorGetReadyUpdates: TColor read fColorGetReadyUpdates
                                          write fColorGetReadyUpdates;
    property ColorErrorGetUpdates: TColor read fColorErrorGetUpdates
                                          write fColorErrorGetUpdates;
    property ColorUserAppNewer: TColor read fColorUserAppNewer
                                       write fColorUserAppNewer;
    property UpdateVersionType: tUpdaterVersionType read fUpdateVersionType write fUpdateVersionType;
    // end updater stuff =======================================================
    property ftpHost: string read fFTPHost write fFTPHost;
    property ftpUserName: string read fFTPUserName write fFTPUserName;
    property ftpPassWord: string read fFTPPassWord write fFTPPassWord;
    property ftpFolder : string read fFTPFolder write fFTPFolder;
    property DownLoadType: TWebDownLoadType read fWebDownLoadType
                                            write fWebDownLoadType default wdt_HTTP;
    property ftpPort: Integer read fFTPPort write fFTPPort Default 21;
    property Caption_UpdateButton: TCaption read GetCaption_UpdateButton write SetCaption_UpdateButton;
    property GEMUpdaterVersion: string read fGemUpdaterVersion;
    property UpdaterApplicationLocName: string read fPathAndNameOfUpdaterApp write fPathAndNameOfUpdaterApp;
//    property UpDaterPathLocationType: tUpDaterLocationType read fUpDaterPathType write fUpDaterPathType;     //SetUpDaterPathType;
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Constraints;
    property ControlCollection;
    property Ctl3D;
    property DoubleBuffered;
    property Enabled;
    property EnabledBtn: Boolean read fEnabledBtn write SetEnabled;
    property ExpandStyle;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property TransferVersionType: TIDFTPTransferType read  fTransferVersonType
                                                     write fTransferVersonType;
    property TransferInstallType: TIDFTPTransferType read  fTransferInstallType
                                                     write fTransferInstallType;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStatusChange: tStatusEvent read fOnStatusChange
                                          write fOnStatusChange;
  end;



//procedure Register;

implementation

const
  cGEMComponentVersion  = 'SRSD Updater Component: 2.3.0';

  cUpdateBtnCheckUpdate = 'Check for Update';     // step 1 check if there is an update
  cUpdateFileDownLoad   = 'Download Update?'; //Download the update installer
  cUpdateBtnUpdateReady = 'Install Update';       // Start the update.

  cUpdaterCloseAppsFile = 'GEMTmpCloseApps.txt';
  cUpdaterInfo          = 'GEMTmpUpdateInfo.txt';

  cExeVersionInfo : array[evt_ProductName..evt_FileVersion] of string =
                               ('ProductName', 'ProductVersion', 'FileVersion');

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMAppUpdater]);
//end;

// Global stuff ================================================================
// Global stuff ================================================================
// Global stuff ================================================================

function GetFontColorFromBackGroundColor(aABGColor: TColor): TColor;
var
  ADouble: Double;
  R, G, B: Byte;
begin

{$REGION 'DOC'}
(*
https://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
Do you mean brightness? Perceived brightness? Luminance?

Luminance (standard for certain colour spaces): (0.2126*R + 0.7152*G + 0.0722*B) [1]
Luminance (perceived option 1): (0.299*R + 0.587*G + 0.114*B) [2]
Luminance (perceived option 2, slower to calculate):
       sqrt( 0.241*R^2 + 0.691*G^2 + 0.068*B^2 ) → sqrt( 0.299*R^2 + 0.587*G^2 + 0.114*B^2 )
                 (thanks to @MatthewHerbst) [3]
*)
{$ENDREGION}

  if aABGColor <= 0 then begin
    Result := clWhite;
    Exit; // *** EXIT RIGHT HERE ***
  end;

  if aABGColor = clWhite then begin
    Result := clBlack;
    Exit; // *** EXIT RIGHT HERE ***
  end;

  // Get RGB from Color
  R := GetRValue(aABGColor);
  G := GetGValue(aABGColor);
  B := GetBValue(aABGColor);

  // Counting the perceptive luminance - human eye favors green color...
  ADouble := 1 - ( 0.299 * R + 0.587 * G + 0.114 * B)/255;

  if (ADouble < 0.5) then
    Result := clBlack  // bright colors - black font
  else
    Result := clWhite;  // dark colors - white font
end;


function getWinSpecialFolder(aFOLDERID : TGuid ; IncludeBackSlash: boolean) : string; overload;
var
  vSpecialPath : PWideChar;
begin
  if SHGetKnownFolderPath(aFOLDERID, KF_FLAG_DEFAULT, 0, vSpecialPath) = S_OK then
  begin
    try
      Result := System.SysUtils.IncludeTrailingPathDelimiter(vSpecialPath);
    finally
      if vSpecialPath <> nil then
        CoTaskMemFree(vSpecialPath);
    end;
  end
  else
    Result := '';
end;


function GetCorrectedSlashes(const aPath, aFile: string; aSlashType: tSlashType): string;
var
  fPath: string;
  fFile: string;
  fChar: Char;
begin
  // Delete any trialing forward or backward slash
  fPath := aPath;
  fChar := aPath[Length(aPath)];
  if (fChar = '/') or (fChar = '\') then
    Delete(fPath, Length(aPath), 1);

  fFile := aFile;
  fChar := aFile[1];
  if (fChar = '/') or (fChar = '\') then
    Delete(fFile, 1, 1);

  case aSlashType of
    st_Win: Result := fPath + '\' + fFile;

    st_Web: Result := fPath + '/' + fFile;

    else Result := '';
  end;

end;

{ TGEMAppUpdater ==============================================================}
{ TGEMAppUpdater ==============================================================}
{ TGEMAppUpdater ==============================================================}

constructor TGEMAppUpdater.Create(AOwner: TComponent);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Create' );{$ENDIF}

  inherited Create(AOwner);

  fAppsToClose := TStringList.Create;
  NotificationCenter := TNotificationCenter.Create(Self);

  ColumnCollection.Add;     //Default is 2 columns.  Need 3.
  RowCollection[1].Destroy; // default is 2 rows.  only need one.

  UpdateBtn             := TColorButton.Create(Self);
  Status1Lbl            := TJvLabel.Create(Self);
  Status1Lb2            := TJvLabel.Create(Self);
  Status1Lb3            := TUrlFileName.Create;

  fColorNoUpdates       := clLime;
  fColorGetReadyUpdates := clYellow;
  fColorErrorGetUpdates := clRed;
  fColorUserAppNewer    := clPurple;
  fFTPPort              := 21;
  fWebDownLoadType      := wdt_HTTP;
//  fDownLoadUpdateFile   := false;

  StatusChange(udNotAssessed);

{$REGION 'DOC'}
  {
  Terms:
    A. Clients program -- this is the program the user has installed on their
    computer for thier use.  Any update ability is NOT the main use of the Clients
    program.

    B. Upater component -- This is the Component the programer placed into the
    Clients program to aid in updating the Clients program.  The component checks
    a web site for a Version file that has a version number of the latest version
    of the Clients program.  The component will check the version of the Clients
    program against the version in the version file found on the web site.

    The version file also contains the web site location of the Clients Program
    installer.  If an update is available, the component will download the install
    file to a local location the programmer placed in the component. The component
    will write a text file so the SRSDAppUpdater will know where the installer is
    located.

    C.SRSDAppUpdater -- This is the app the Updater Component runs to update
    the Clients program.  The  The component has allready downloaded the install
    program to the Clients computer and has written a file for the update will
    know what file to run.

  There are two ways this component will operate. One is where the SRSDAppUpdater
  has been installed on the clients computer as a stand alone program.  The other
  is where the SRSDAppUpdater has been installed with the Clients program and in
  the same folder as the clients program.

  The next lines are used to determine whick updater mode the component will use.
  If the SRSDAppUpdater has been install on the computer a registry entry will
  have been created showing the location of the SRSDAppUpdater exe.  If there is
  no registry value, then the component assumes the programmer has installed the
  SRSDAppUpdater with the Clients program.
  }

{$ENDREGION}

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Create' );{$ENDIF}
end;


destructor TGEMAppUpdater.Destroy;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Destroy' );{$ENDIF}

  fAppsToClose.Free;
  Status1Lb3.Free;
  inherited;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Destroy' );{$ENDIF}
end;


function TGEMAppUpdater.StatusOfUpdate: TWebUpdate;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'UpdateStatus' );{$ENDIF}
  fCanRunUpdate := CheckUpdaterIsReady;
  UpdateBtn.Enabled := fEnabledBtn;
  if fCanRunUpdate then
    result := AppUpdateInfoThread(Status1Lbl, Status1Lb2, Status1Lb3, cvdu_JustCheckVersion)
  else
    result := nil;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'UpdateStatus' );{$ENDIF}
end;


procedure TGEMAppUpdater.SetEnabled(Value: Boolean);
begin
  fEnabledBtn := Value;
  UpdateBtn.Enabled := Value;
end;


procedure TGEMAppUpdater.CreateWindowHandle(const Params: TCreateParams);
{ Calls inherited CreateWindowHandle and initializes subcomponents. }
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CreateWindowHandle' );{$ENDIF}
  inherited CreateWindowHandle(Params);

  fProgramInfo.ProductName    := GetVersionInfo(evt_ProductName, 0);
  fProgramInfo.ProductVersion := GetVersionInfo(evt_ProductVersion, fPlacesVersionNum);
  fProgramInfo.FileVersion    := GetVersionInfo(evt_FileVersion, fPlacesVersionNum);

  with UpdateBtn do begin
    Left      := 0;
    Top       := 0;
    Width     := 75;
    Height    := 25;
    Parent    := Self;
    Align     := alClient;
//    Caption   := 'OK';
    Font.Color:= GetFontColorFromBackGroundColor(BackColor);
    TabOrder  := 0;
    OnClick   := Click_UpdateBtnHandler;
    Caption   := cUpdateBtnCheckUpdate;
    BackColor := fColorNoUpdates;
  end;  { UpdateBtn }

  with Status1Lbl do begin
    Left       := 4;
    Top        := 4;
    Width      := 75;
    Height     := 42;
    Parent     := Self;
    Align      := alClient;
//    Caption    := 'Update Info 1';
    TabOrder   := 1;
    WordWrap   := True;
    Caption    := fProgramInfo.ProductName;
    FrameColor := fColorNoUpdates;
  end;

  with Status1Lb2 do begin
    Left       := 81;
    Top        := 4;
    Width      := 77;
    Height     := 42;
    Parent     := Self;
    Align      := alClient;
    Caption    := 'Update Info 2';
    TabOrder   := 2;
    WordWrap   := True;

    case UpdateVersionType of
      udvt_AppOptionFile: Caption := 'Prg Version: ' + fProgramInfo.FileVersion;  //GetAppVersionStr(fPlacesVersionNum);

      udvt_Product:       Caption := 'Prg Version: ' + fProgramInfo.ProductVersion;
    end;
    FrameColor := fColorNoUpdates;
  end;

  while ColumnCollection.Count < 3 do
    ColumnCollection.Add;

  ColumnCollection.BeginUpdate;
    ColumnCollection[0].SizeStyle := ssPercent;
    ColumnCollection[1].SizeStyle := ssPercent;
    ColumnCollection[2].SizeStyle := ssPercent;

    ColumnCollection[0].Value := 19.00;
    ColumnCollection[1].Value := 41.00;
    ColumnCollection[2].Value := 40.00;
  ColumnCollection.EndUpdate;

  ControlCollection.AddControl(UpdateBtn, 0,0);
  ControlCollection.AddControl(Status1Lbl, 1,0);
  ControlCollection.AddControl(Status1Lb2, 2,0);

  Padding.Left   := 1;
  Padding.Top    := 1;
  Padding.Right  := 1;
  Padding.Bottom := 2;

  Caption := '';

  fCanRunUpdate      := CheckUpdaterIsReady;
  fGemUpdaterVersion := cUpdaterVersion;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CreateWindowHandle' );{$ENDIF}
end;      { CreateWindowHandle }


procedure TGEMAppUpdater.DisplayNotification(aNotification: TAlertType; aMsg: string);
var
  MyNotification: TNotification;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DisplayNotification' );{$ENDIF}

  MyNotification := NotificationCenter.CreateNotification;
  try
    MyNotification.Name := 'SRSD Updater: ';// + ;
    MyNotification.Title := 'Update check for: ' + fProgramInfo.ProductName;
    MyNotification.AlertBody := aMsg;

    NotificationCenter.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DisplayNotification' );{$ENDIF}
end;


function TGEMAppUpdater.AppUpdateInfoThread(Lbll, Lbl2: TJvLabel; Lbl3: TUrlFileName;
                  aTypeOfChecking: tCheckVersionDownloadUpdateFile): TWebUpdate;
var
  AppUpdateInformationThread : TWebUpdate;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'AppUpdateInfoThread' );{$ENDIF}
  StatusChange(udAccessing);
  UpdateBtn.Enabled := false;
  AppUpdateInformationThread                        := TWebUpdate.Create(true);
  AppUpdateInformationThread.FreeOnTerminate        := true;


  AppUpdateInformationThread.DownLoadUpdateFile     := aTypeOfChecking;
  // labels for display of update status
  AppUpdateInformationThread.cb_1                   := Lbll;
  AppUpdateInformationThread.cb_2                   := Lbl2;
  AppUpdateInformationThread.cb_3                   := Lbl3;

  AppUpdateInformationThread.TransferVersonType     := fTransferVersonType;
  AppUpdateInformationThread.TransferInstallType    := fTransferInstallType;

  AppUpdateInformationThread.ColorNoUpdates         := fColorNoUpdates;
  AppUpdateInformationThread.ColorGetReadyUpdates   := fColorGetReadyUpdates;
  AppUpdateInformationThread.ColorErrorGetUpdates   := fColorErrorGetUpdates;
  AppUpdateInformationThread.ColorUserAppNewer      := fColorUserAppNewer;

  case UpdateVersionType of
    udvt_AppOptionFile: AppUpdateInformationThread.OldPrgVersion := fProgramInfo.FileVersion;//GetAppVersionStr(fPlacesVersionNum);

    udvt_Product:       AppUpdateInformationThread.OldPrgVersion := fProgramInfo.ProductVersion;
  end;

  AppUpdateInformationThread.NumPlacesInVersionNum   := fPlacesVersionNum;

  AppUpdateInformationThread.FTPHost                := fFTPHost;
  AppUpdateInformationThread.PassWord               := fFTPPassWord;
  AppUpdateInformationThread.UserName               := fFTPUserName;
  AppUpdateInformationThread.fWebDownLoadType       := fWebDownLoadType;
  AppUpdateInformationThread.FTPPort                := fFTPPort;

  AppUpdateInformationThread.PathFileLocalInstall   := fPathFileLocalInstall;
  AppUpdateInformationThread.LocalVersionPathAndFile:= GetCorrectedSlashes(fLocalInstallPath, fVersionFileName, st_Win);//fLocalInstallPath + '\' + fVersionFileName;
  AppUpdateInformationThread.UrlVersionPathAndFile  := GetCorrectedSlashes(fUrlInstallPath, fVersionFileName, st_Web);  //fUrlInstallPath + '/' + fVersionFileName;
  AppUpdateInformationThread.LocalUpdatePathAndFile := trim(GetCorrectedSlashes(fLocalInstallPath, '\ ', st_Win));      // fLocalInstallPath + '\';// + fInstallAppName;

  AppUpdateInformationThread.OnTerminate            := UpDateThreadTerminated;
  AppUpdateInformationThread.Start;

  Result := AppUpdateInformationThread;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'AppUpdateInfoThread' );{$ENDIF}
end;


procedure TGEMAppUpdater.UpDateThreadTerminated(Sender: TObject);
var
  aReturnValue: TUpdateStatus;
  s: string;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'UpDateThreadTerminated' );{$ENDIF}

  aReturnValue  := TUpdateStatus((Sender as TWebUpdate).returnValue);
//  fUpdateStatus := aReturnValue;

  UpdateBtn.Caption     := cUpdateBtnCheckUpdate;
  UpdateBtn.BackColor   := fColorNoUpdates;
  UpdateBtn.Font.Color  := GetFontColorFromBackGroundColor(fColorNoUpdates);

  case aReturnValue of
    udNotAssessed: s := 'Update needs not Assessed!';
    udNotNeeded:   s := 'App up todate';

//    udNeeded:      s := 'Update needed!';

    udUpdateFileDownloaded: begin // before this was to say that the installer was downloaded.
      UpdateBtn.BackColor  := fColorGetReadyUpdates;
      UpdateBtn.Font.Color := GetFontColorFromBackGroundColor(fColorGetReadyUpdates);
      UpdateBtn.Caption    := cUpdateBtnUpdateReady;

      Status1Lbl.FrameColor:= fColorGetReadyUpdates;
      Status1Lbl.Caption   := cUpdateBtnUpdateReady;

      Status1Lb2.FrameColor:= fColorGetReadyUpdates;
      s                    := Status1Lb2.Caption;
      fUpdateString        := s;
      fUpdateString        := StringReplace(fUpdateString, '/', '\', [rfReplaceAll]);
      fUpdateString        := ExtractFileName(fUpdateString);
    end;

    udUserProgramNewer : s := 'Update checked. User program newer than update';
    udUpdateCheckError : s := 'Error checking for updates';
    udNeededNotReady   : s := 'Up date needed, but not ready';
    udError            : s := 'Error checking for updates';

    udNeeded: begin
      UpdateBtn.BackColor  := fColorGetReadyUpdates;
      UpdateBtn.Font.Color := GetFontColorFromBackGroundColor(fColorGetReadyUpdates);
      UpdateBtn.Caption    := cUpdateFileDownLoad;
      Status1Lbl.FrameColor:= fColorGetReadyUpdates;
      Status1Lbl.Caption   := cUpdateFileDownLoad;

      Status1Lb2.FrameColor:= fColorGetReadyUpdates;
    end;
  end;
  UpdateBtn.Enabled := fEnabledBtn;
  StatusChange(aReturnValue);

  DisplayNotification(atUpdate, s);
  Status1Lb2.Caption := s + ': ' + FormatDateTime('mm/dd/yyyy hh:nn', Now);//DateTimeToStr(Now);

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'UpDateThreadTerminated' );{$ENDIF}
end;



function TGEMAppUpdater.GetVersionInfo(aIdent: tEXEVersionType; aVersionPlaces: byte): String;
  //=====================================
  function OccurrencesOfChar(const ContentString: string; const CharToCount: char): integer;
{$REGION 'Reference'}
{
https://stackoverflow.com/questions/15294501/how-to-count-number-of-occurrences-of-a-certain-char-in-string

You can use this simple function:

function OccurrencesOfChar(const S: string; const C: char): integer;
var
  i: Integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      inc(result);
end;
shareimprove this answer
edited Mar 8 '13 at 13:02
answered Mar 8 '13 at 12:57

Andreas Rejbrand
72.4k6208302
Can't I do it in a single line? – user1556433 Mar 8 '13 at 12:58
11
@NareshKumar: Yes, of course: OccurrencesOfChar(myString, ',') – Andreas Rejbrand Mar 8 '13 at 12:58
2
+1 but chr is a poor name since it already has meaning. I suggest C – David Heffernan Mar 8 '13 at 13:02
@David: Very good point. – Andreas Rejbrand Mar 8 '13 at 13:02
@AndreasRejbrand - Thanks Andreas – user1556433 Mar 8 '13 at 13:12
}
{$ENDREGION}
  var
    C: Char;
  begin
    result := 0;
    for C in ContentString do
      if C = CharToCount then
        Inc(result);
  end;

  //====================================

  function GetVersionStr(aSubStr, aStr: string; aOccurrence: Integer): string;
  var
    fOffSet, Places: Integer;
    C: Char;
    count: Integer;
  begin
    // if 3 periods then 4 place version
    // if 2 periods then 3 place version
    Places := aVersionPlaces;

   {$IFDEF USE_CODESITE}CodeSite.SendMsg( Format('B. Version: '+ aStr+'  aVersionPlaces: %d  aOffset: %d  aOccurence: %d',[aVersionPlaces, Places, aOccurrence]) );{$ENDIF}
    if Places > (aOccurrence + 1) then begin   // less version separators than (version places - 1)
      Result := 'Error Getting Version';
      Exit;
    end;


    if Places = (aOccurrence + 1) then begin  // same num (separators + 1) as version places. use aStr
      result := aStr;
      Exit;
    end;

    if Places < (aOccurrence + 1) then begin // Less or equal num separators than version places.
      count := 0;
      fOffSet := 0;
      for C in aStr do begin
        inc(count);
        if (C = aSubStr) then
          inc(fOffSet);
        {$IFDEF USE_CODESITE}CodeSite.SendMsg( Format('C. Version: '+ aStr+' Char: '+ C+' Count: %d  aOffset: %d  fOffset: %d',[Count, Places, fOffset]) );{$ENDIF}
        if fOffSet >= Places then
          Break;
      end;
     {$IFDEF USE_CODESITE}CodeSite.SendMsg( Format('D. Version: '+ aStr+' Count: %d  To: %d',[Count, Length(Result) - count + 1]) );{$ENDIF}
      Delete(aStr, count, Length(aStr) - count + 1);
      Result := aStr;
      exit;
    end;

    result := 'Error';
  end;

  //=====================================

  function FinalVersionStr(aInVersion: string): string;
  var
    fCount      : Integer;
  begin
    fCount := OccurrencesOfChar(aInVersion, '.');
    result := GetVersionStr('.', aInVersion, fCount);
  end;

  //=====================================

{$REGION 'Reference'}
{
https://stackoverflow.com/questions/1717844/how-to-determine-delphi-application-version
Modified by: Gary McIntosh
Hard work by:
answered Jun 20 '12 at 8:32
Jiri Krivanek
}
{$ENDREGION}

type
  TLang = packed record
    Lng, Page: WORD;
  end;

  TLangs = array [0 .. 10000] of TLang;

  PLangs = ^TLangs;

var
  BLngs: PLangs;
  BLngsCnt: Cardinal;
  BLangId: String;
  RM: TMemoryStream;
  RS: TResourceStream;
  BP: PChar;
  BL: Cardinal;
  BId: String;

begin
  // Assume error
  Result := '';

  RM := TMemoryStream.Create;
  try
    // Load the version resource into memory
    RS := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
    try
      RM.CopyFrom(RS, RS.Size);
    finally
      FreeAndNil(RS);
    end;

    // Extract the translations list
    if not VerQueryValue(RM.Memory, '\\VarFileInfo\\Translation', Pointer(BLngs), BL) then
      Exit; // Failed to parse the translations table
    BLngsCnt := BL div sizeof(TLang);
    if BLngsCnt <= 0 then
      Exit; // No translations available

    // Use the first translation from the table (in most cases will be OK)
    with BLngs[0] do
      BLangId := IntToHex(Lng, 4) + IntToHex(Page, 4);

    // Extract field by parameter
    BId := '\\StringFileInfo\\' + BLangId + '\\' + cExeVersionInfo[aIdent];
    if not VerQueryValue(RM.Memory, PChar(BId), Pointer(BP), BL) then
      Exit; // No such field

    // Prepare result
    case aIdent of
      evt_ProductName: Result := BP;

      evt_ProductVersion,
      evt_FileVersion: Result := FinalVersionStr(BP);

      else Result := BP
    end;

  finally
    FreeAndNil(RM);
  end;
end;


function TGEMAppUpdater.GetVersionStr: string;
begin
  case UpdateVersionType of
    udvt_AppOptionFile: Result := fProgramInfo.FileVersion;//GetAppVersionStr(fPlacesVersionNum);

    udvt_Product      : Result := fProgramInfo.ProductVersion;
  end;
//  result := GetAppVersionStr(fPlacesVersionNum);
end;


function TGEMAppUpdater.GetCaption_UpdateButton: TCaption;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'GetCaption_UpdateButton' );{$ENDIF}

  result := UpdateBtn.Caption;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'GetCaption_UpdateButton' );{$ENDIF}
end;


function TGEMAppUpdater.CheckUpdaterIsReady: Boolean;
var
  fMsg : string;
  fMsg2: string;
begin
  Result := True;
  fMsg := '';
{$REGION 'Old shit'}
  // first check if updater program is istalled
//  case  fUpDaterPathType of
//    udpt_UpdaterInstallLoc: begin // use what programmer entered for component property. Means that updater was installed separatly for application
//      fLocationNameOfUpdaterApp := fLocationNameOfUpdaterApp + cDaterFile;
//    {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'CheckUpdaterIsReady  fUpDaterPathType: udpt_UpdateInstall'  );{$ENDIF}
//    end;
//
//    udpt_AppInstallLoc    : begin // Means that the updater is installed in the same location as the updater app.
//      fLocationNameOfUpdaterApp := ExtractFileDir(ParamStr(0));
//    {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'CheckUpdaterIsReady  udpt_AppInstall: ' + ExtractFileDir(ParamStr(0)) );{$ENDIF}
//      fLocationNameOfUpdaterApp := fLocationNameOfUpdaterApp + cDaterFile;
//    end;
//  end;

{$ENDREGION}
    {$IFDEF USE_CODESITE}CodeSite.SendMsg( 'CheckUpdaterIsReady: ' + fPathAndNameOfUpdaterApp );{$ENDIF}


  Result               :=  FileExists(fPathAndNameOfUpdaterApp);
  if not Result then begin
    fMsg := 'SRSDUpdater Not Found';
    fMsg2 := 'Install '+ ExtractFileName(fPathAndNameOfUpdaterApp);
  end;


  if fUrlInstallPath = '' then begin
    fMsg := 'Local Install Location Blank';
    fMsg2 := 'Correct Install Location';
    Result := False;
  end;


  if Result then begin
    case fWebDownLoadType of
      wdt_HTTP:  begin
        if fUrlInstallPath = '' then begin
          fMsg := 'Web Install Location Blank';
          fMsg2 := 'Correct Web Install Location';
          Result := False;
        end;
      end;

      wdt_FTP: begin
        fMsg2 := 'Correct Missing Value';
        if fFTPHost = '' then
          fMsg := 'Ftp Host name blank'
        else
          if fFTPUserName = '' then
            fMsg := 'FTP User Name Blank'
          else
            if fFTPPassWord = '' then
              fMsg := 'FTP Passord Name Blank'
            else
              if fFTPFolder = '' then
                fMsg := 'FTP Folder Blank';
        Result :=  fMsg = '';
      end;
    end;
  end;

  if Result then begin
    fMsg2 := 'Correct Version File Name';
    Result := fVersionFileName <> '';
    if not Result then begin
      fMsg := 'Version File Name Blank';
    end;
  end;

  if not Result then begin
    UpdateBtn.Enabled := False;
    UpdateBtn.BackColor := fColorErrorGetUpdates;
    UpdateBtn.Font.Color := GetFontColorFromBackGroundColor(fColorErrorGetUpdates);
    UpdateBtn.Caption := 'Updater Error';

    Status1Lb2.FrameColor := fColorErrorGetUpdates;
    Status1Lb2.Caption := fMsg;

    Status1Lbl.FrameColor := fColorErrorGetUpdates;
    Status1Lbl.Caption := fMsg2;
  end;

end;


procedure TGEMAppUpdater.Click_UpdateBtnHandler(Sender: TObject);
var
  vUpdaterDataPath: string;

  //====================

  Function SetupUpdaterAppOK: boolean;
  var
    StuffToSave: TStringList;
  begin
    {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Click_UpdateBtnHandler/SetupUpdaterAppOK' );{$ENDIF}

    Result := True;
    StuffToSave := TStringList.Create;
    try
      // get updater data path.  this should be in the program data folder
      vUpdaterDataPath := getWinSpecialFolder(FOLDERID_ProgramData, false) + cUpdaterDataPath;
//      vUpdaterDataPath := getWinSpecialFolder(CSIDL_COMMON_APPDATA, false) +  cUpdaterDataPath;

      if not DirectoryExists(vUpdaterDataPath) then
        ForceDirectories(vUpdaterDataPath);

      if DirectoryExists(vUpdaterDataPath) then  begin
        AppsToClose.SaveToFile(vUpdaterDataPath + cUpdaterCloseAppsFile);

        StuffToSave.Add('ProductName|' + fProgramInfo.ProductName);

        StuffToSave.Add('InstallApp|'  + GetCorrectedSlashes(fLocalInstallPath, Status1Lb3.Caption, st_Win));// fLocalInstallPath + Statuslbl3.caption);
        StuffToSave.Add('IconFile|'    + fIconFileLocation);
        StuffToSave.Add('VersionInfo|' + AnsiLeftStr(Status1Lb2.caption, Pos(':',  Status1Lb2.caption) -1));
        StuffToSave.Add('UpdaterPath|' + fPathAndNameOfUpdaterApp); // used for debugging

        StuffToSave.SaveToFile(vUpdaterDataPath + cUpdaterInfo);
      end
      else
        Result := False;
    finally
      FreeAndNil(StuffToSave);
    end;

    {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Click_UpdateBtnHandler/SetupUpdaterAppOK' );{$ENDIF}
  end;

  //====================

begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Click_UpdateBtnHandler' );{$ENDIF}

  //  cvdu_JustCheckVersion, cvdu_CheckDownload
//  if UpdateBtn.InstallFileName = cUpdateBtnUpdateReady then begin
  if UpdateBtn.Caption = cUpdateBtnUpdateReady then begin
    {$IFDEF DEBUG}
      showmessage('1128 StatusLabel: '+AppInstallName.InstallFileName);
    {$ENDIF}
    if SetupUpdaterAppOK then begin
      StatusChange(udRunningUpdater);
      RunUpdater;
    end
    else begin
      MessageDlg('Code 1135 -- Could Not Find Temp Dir. Call Support!', mtError, [mbOK], 0);
      StatusChange(udError);
    end;
    Exit;
  end;

  if UpdateBtn.Caption = cUpdateBtnCheckUpdate then begin
    AppUpdateInfoThread(Status1Lbl, Status1Lb2, Status1Lb3, cvdu_JustCheckVersion);
    UpdateBtn.Font.Color := GetFontColorFromBackGroundColor(UpdateBtn.BackColor);
    end;
//  else begin
//    MessageDlg('Error: No updates available.', mtInformation, [mbOK], 0);
//    StatusChange(udError);
//  end;

  if UpdateBtn.Caption = cUpdateFileDownLoad then begin
  {$IFDEF DEBUG}
    showmessage(AppInstallName.InstallFileName);
  {$ENDIF}
    AppUpdateInfoThread(Status1Lbl, Status1Lb2, AppInstallName, cvdu_DownloadInstallFile);
    UpdateBtn.Font.Color := GetFontColorFromBackGroundColor(UpdateBtn.BackColor);

  end;


  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Click_UpdateBtnHandler' );{$ENDIF}
end;


procedure TGEMAppUpdater.RunUpdater;
var
  SEInfo: TShellExecuteInfo;
  ExecuteFile: string;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'RunUpdater' );{$ENDIF}


  DisplayNotification(atUpdate, 'Update App Selected');

  if MessageDlg('Update '+ fProgramInfo.ProductName +'?', mtWarning, [mbYes, mbNo],0) = mrYes then  begin
    FillChar(SEInfo, SizeOf(SEInfo), 0) ;
    SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
    ExecuteFile := fPathAndNameOfUpdaterApp;
    with SEInfo do begin
      Wnd    := fAppHnd;
      lpFile := PChar(ExecuteFile) ;
      lpVerb := PWideChar('Open');

      case UpdateVersionType of
        udvt_AppOptionFile: lpParameters := PWideChar(fProgramInfo.FileVersion +
                                            ' ' + Status1Lb2.Caption);

        udvt_Product      : lpParameters := PWideChar(fProgramInfo.ProductVersion +
                                            ' ' + Status1Lb2.Caption);
      end;
      nShow := SW_SHOWNORMAL;
    end;
    ShellExecuteEx(@SEInfo);
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'RunUpdater' );{$ENDIF}
end;


procedure TGEMAppUpdater.SetAppsToClose(const Value: TStringList);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetAppsToClose' );{$ENDIF}

  if Assigned(fAppsToClose) then
    fAppsToClose.Assign(Value);

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetAppsToClose' );{$ENDIF}
end;


procedure TGEMAppUpdater.SetCaption_UpdateButton(newValue: TCaption);
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'SetCaption_UpdateButton' );{$ENDIF}

  UpdateBtn.Caption := newValue;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'SetCaption_UpdateButton' );{$ENDIF}
end;


//procedure TGEMAppUpdater.SetUpDaterPathType(const Value: tUpDaterLocationType);
//begin
//  fUpDaterPathType := Value;
//end;


procedure TGEMAppUpdater.StatusChange(const ResponseValue: TUpdateStatus);
begin
  fUpdateStatus := ResponseValue;
  if Assigned(OnStatusChange) then // tests if the event is assigned
      OnStatusChange(ResponseValue); // calls the event.
end;


{ TWebUpdate ==================================================================}
{ TWebUpdate ==================================================================}
{ TWebUpdate ==================================================================}

procedure TWebUpdate.Execute;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'Execute' );{$ENDIF}

  inherited;
  if fWebDownLoadType = wdt_FTP then begin
    idHndlrstck := TIdIOHandlerStack.create(nil);
    idftp_FTP := TIdFTP.Create(Nil);
    idHndlrstck.Port := fFTPPort;
    idHndlrstck.Destination := ':'+ IntToStr(fFTPPort);
    idHndlrstck.ReadTimeout := 60000;
    idftp_FTP.IOHandler := idHndlrstck;

    idftp_FTP.Host               := fFTPHost;
    idftp_FTP.Username           := fUserName;
    idftp_FTP.Password           := fPassword;
    idftp_FTP.OnStatus           := idftp_FTPStatus;
    idftp_FTP.OnWork             := idftp_FTPWork;
    idftp_FTP.OnWorkBegin        := idftp_FTPWorkBegin;
    idftp_FTP.OnWorkEnd          := idftp_FTPWorkEnd;
    idftp_FTP.Port               := fFTPPort;
    idftp_FTP.UseHost            := true;
    idftp_FTP.UseMLIS            := true;
    idftp_FTP.UseTLS             := utNoTLSSupport;

    idftp_FTP.Connect;
    try
      ReturnValue := Ord(CheckAndGetUpdates);
    finally
      idftp_FTP.Disconnect;
      FreeAndNil(idHndlrstck);
      FreeAndNil(idftp_FTP);
    end;

  end
  else begin
    try
      ReturnValue := Ord(CheckAndGetUpdates);
    finally
    end;
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'Execute' );{$ENDIF}
end;


procedure TWebUpdate.idftp_FTPStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
var
  Clr: TColor;
  msg: string;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'idftp_FTPStatus' );{$ENDIF}

  msg := AStatusText;
  case AStatus of
    hsStatusText:
      Clr :=  clBlue ;//ApplicationConfig.LogColors.Colors['hsStatusText'];
    hsResolving:
      Clr :=  clYellow;//ApplicationConfig.LogColors.Colors['hsResolving'];
    hsConnecting:
      Clr :=  clNavy ;//ApplicationConfig.LogColors.Colors['hsConnecting'];
    hsDisconnecting:
      Clr :=  clMaroon ;//ApplicationConfig.LogColors.Colors['hsDisconnecting'];
    hsConnected:
      Clr :=  clGreen  ;//ApplicationConfig.LogColors.Colors['hsConnected'];
    hsDisconnected:
      Clr :=  clLime;// ApplicationConfig.LogColors.Colors['hsDisconnected'];
    ftpTransfer:
      Clr :=   clRed; //ApplicationConfig.LogColors.Colors['ftpTransfer'];
//    ftpReady:
//      Clr :=   clCream;//  ApplicationConfig.LogColors.Colors['ftpReady'];
    ftpAborted:
      Clr :=   clYellow;//ApplicationConfig.LogColors.Colors['ftpAborted'];
  else begin
      msg := 'working';
      Clr := clBtnFace;//ApplicationConfig.LogColors.Colors['Default'];
    end;
  end;
  fBackGColor := Clr;
  fFTPmsg := msg;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'idftp_FTPStatus' );{$ENDIF}
end;

procedure TWebUpdate.idftp_FTPWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
//  TheAvg: Integer;
  fMsg: string;
begin
  fMsg :=  'Working:  '+ IntToStr(AWorkCount);//+'/'+IntToStr(fTotalSize);
  SendMessage(fBackGColor, fMsg, fOldPrgVersion+'-->'+NewPrgVersionLabel, fInstallAppName);
end;


procedure TWebUpdate.idftp_FTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
var
  fMsg: string;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'idftp_FTPWorkBegin' );{$ENDIF}

  fTotalSize := AWorkCountMax;
  fWorkDone := 0;
  fMsg := 'BeginWork: ' + IntToStr(fWorkDone);
  SendMessage(fBackGColor, fMsg, fOldPrgVersion+'-->'+NewPrgVersionLabel, fInstallAppName);

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'idftp_FTPWorkBegin' );{$ENDIF}
end;


procedure TWebUpdate.idftp_FTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  SendMessage(clLime, 'Work end', fOldPrgVersion+'-->'+NewPrgVersionLabel, fInstallAppName);
end;


procedure TWebUpdate.DoProgress;
begin
  fcb_1.caption    := fMessage;
  fcb_1.FrameColor := fBackGroundColor;
  fcb_1.font.size  := 8;

  fcb_2.FrameColor := fBackGroundColor;
  fcb_2.Caption    := fVersionStr;
  fcb_3.Caption    := fUrlFileName;
end;


procedure TWebUpdate.SendMessage(BackGroundColor: tColor; aMessage,
                                 aVersionStr, aUrlFileName: string);
begin
  fMessage := aMessage;
  fBackGroundColor := BackGroundColor;
  fVersionStr := aVersionStr;
  fUrlFileName := aUrlFileName;

  Synchronize(DoProgress);
end;


function TWebUpdate.CheckAndGetUpdates: TUpdateStatus;

  //===========================

  procedure CaseCheckNeedForUpdate;
  begin
    case CheckNeedForUpDate of

      udNeeded: begin
        result := udNeededNotReady;
        // next we download the install file
        SendMessage(fColorGetReadyUpdates, 'Update Available: ' +
                    NewPrgVersionLabel, fOldPrgVersion+'-->'+NewPrgVersionLabel, fInstallAppName);
        case DownLoadUpdateFile of
          cvdu_JustCheckVersion: result := udNeeded;

          cvdu_DownloadInstallFile: begin
            if DownLoadInstallFile then
              result := udUpdateFileDownloaded
            else
              result := udError;
          end;
        end;
      end;

      // all 3 below stop update checking
      udNotNeeded: begin
        result := udNotNeeded;
      end;

      udUserProgramNewer: begin
        result := udUserProgramNewer;
      end;

      else begin
        result := udError;
      end;
    end;
  end;

  //===========================

begin
  result := udNotAssessed;
  if DownLoadVersionFile then begin
    if ReadVersionFile then begin
      CaseCheckNeedForUpdate;
    end
    else
      result := udError;
  end
  else
    result := udError;
end;


function TWebUpdate.CheckNeedForUpDate: TUpdateStatus;
var
  UserMajor, UserDb, UserMinor, s: string;
  WebMajor, WebDb, WebMinor: String;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'CheckNeedForUpDate' );{$ENDIF}

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

    if fNumPlacesInVersionNum = 3 then  begin
      WebDb := LeftStr(s, Pos('.', s)-1);
      delete(s, 1, Pos('.', s));
    end;
    WebMinor := s;

    //  Version nums from the program
    UserMajor := LeftStr(fOldPrgVersion, Pos('.', fOldPrgVersion)-1);
    s :=  fOldPrgVersion;
    delete(s, 1, Pos('.', fOldPrgVersion));

    if fNumPlacesInVersionNum = 3 then  begin
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
      SendMessage( fColorUserAppNewer, 'User Program newer than update', '', '');
      exit;
    end;

    if (StrToInt(WebDb) > StrToInt(UserDb)) and (fNumPlacesInVersionNum = 3) then begin
      result := udNeeded;
      exit;
    end;

    if (StrToInt(WebDb) < StrToInt(UserDb)) and (fNumPlacesInVersionNum = 3) then begin
      result := udUserProgramNewer;
      SendMessage( fColorUserAppNewer, 'User Program newer than update', '', '');
      exit;
    end;

    if StrToInt(WebMinor) > StrToInt(UserMinor) then begin
      result := udNeeded;
      exit;
    end;

    if StrToInt(WebMinor) < StrToInt(UserMinor) then begin
      result := udUserProgramNewer;
      SendMessage( fColorUserAppNewer, 'User Program newer than update', '', '');
      exit;
    end;

    SendMessage( fColorNoUpdates, 'No Updates Available', '', '');
  except
    SendMessage( fColorErrorGetUpdates, 'Error determining need for update', '', '');
    result := udError; //udUpdateCheckError;
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'CheckNeedForUpDate' );{$ENDIF}
end;


function TWebUpdate.DoDownload(aUrlPathFileName, alocalPathFile: string): boolean;
var
  s: string;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DoDownload' );{$ENDIF}
  result := false;
  case fWebDownLoadType of
    wdt_HTTP: begin
      try
        if FileExists(alocalPathFile) then
          DeleteFile(alocalPathFile);
        Result := UrlDownloadToFile(nil, PChar(aUrlPathFileName), PChar(alocalPathFile), 0, nil) = 0;
      except
        Result := False;
      end;
    end;

    wdt_FTP: begin
      Result := True;
      try
        if FileExists(alocalPathFile) then
          DeleteFile(alocalPathFile);
        s := ExtractFileExt(alocalPathFile);
        idftp_FTP.TransferType := fTransferType;
        idftp_FTP.Get(aUrlPathFileName, alocalPathFile, true, idftp_FTP.ResumeSupported);
      except
        Result := False;
      end;
    end;
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DoDownload' );{$ENDIF}
end;


function TWebUpdate.DownLoadInstallFile: boolean;
begin
  SendMessage(fColorGetReadyUpdates, 'Getting Update Ready: '+
              NewPrgVersionLabel, fOldPrgVersion+'-->'+NewPrgVersionLabel, fInstallAppName);
  result := true;
  fTransferType := fTransferInstallType;
  if DoDownload(TheUpdateInstallFileName, fLocalUpdatePathAndFile + fInstallAppName) then begin
    SendMessage(clYellow, 'Installer File Downloaded', '', '');
    fPathFileLocalInstall := fLocalUpdatePathAndFile + fInstallAppName;
  end
  else begin
    SendMessage(clRed, '1713- ERROR Installer File', '', '');
    fPathFileLocalInstall := 'ERROR';
    result := false;
  end;
end;


function TWebUpdate.DownLoadVersionFile: boolean;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'DownLoadVersionFile' );{$ENDIF}
  fTransferType := fTransferVersonType;
  if DoDownload(fUrlVersionPathAndFile, fLocalVersionPathAndFile) then begin
    SendMessage(fColorGetReadyUpdates, 'Downloading Version File', 'b.', '');
    result := true;
  end
  else begin
    SendMessage(fColorErrorGetUpdates, 'Version File Download ERROR', '', '');
    result := false;
  end;

  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'DownLoadVersionFile' );{$ENDIF}
end;


function TWebUpdate.ReadVersionFile: boolean;
var
  myFile : TextFile;
begin
  {$IFDEF USE_CODESITE}CodeSite.EnterMethod( Self, 'ReadVersionFile' );{$ENDIF}
  SendMessage(fColorGetReadyUpdates, 'Reading Version File', 'c.', '');
  try
    AssignFile(myFile, fLocalVersionPathAndFile);
    Reset(myFile);
      ReadLn(myFile, NewPrgVersionLabel);
      ReadLN(MyFile, TheUpdateInstallFileName);
    CloseFile(myFile);
    result := true;
   fInstallAppName := StringReplace(TheUpdateInstallFileName, '/', '\', [rfReplaceAll]);
   fInstallAppName := ExtractFileName(fInstallAppName);
  except
    SendMessage(fColorErrorGetUpdates, 'Error Reading Version File', '', '');
    result := false;
  end;
  {$IFDEF USE_CODESITE}CodeSite.ExitMethod( Self, 'ReadVersionFile' );{$ENDIF}
end;

end.




