unit GwWebBrowserTamed;

{ Descendant of TWebBrowser which exposes the DLCONTROL
ambient property as a Delphi set property.

Version history
----------------
2011-02-01 GW Original

Graham Wideman (gwideman)
}

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.OleCtrls
  , SHDocVw
  , Vcl.StdCtrls
  , MSHTML
  , Mshtmdid
  ;

type
  //--------------------------------------
  //  Ambient DL Ctl
  //--------------------------------------

  TAmbientDLCtl = (
  { DLCTL_DLIMAGES                    = $00000010; }  dlcDownloadImages,
  { DLCTL_VIDEOS                      = $00000020; }  dlcPlayVideos,
  { DLCTL_BGSOUNDS                    = $00000040; }  dlcPlayBkgSounds,
  { DLCTL_NO_SCRIPTS                  = $00000080; }  dlcNoScripts,
  { DLCTL_NO_JAVA                     = $00000100; }  dlcNoJava,
  { DLCTL_NO_RUNACTIVEXCTLS           = $00000200; }  dlcNoActiveXRun,
  { DLCTL_NO_DLACTIVEXCTLS            = $00000400; }  dlcNoActiveXDownload,
  { DLCTL_DOWNLOADONLY                = $00000800; }  dlcDownloadOnlyNoDisplay,
  { DLCTL_NO_FRAMEDOWNLOAD            = $00001000; }  dlcNoFrameDownload,
  { DLCTL_RESYNCHRONIZE               = $00002000; }  dlcResynchronizeIgnoreCache,
  { DLCTL_PRAGMA_NO_CACHE             = $00004000; }  dlcPragmaNoProxyCache,
  { DLCTL_NO_BEHAVIORS                = $00008000; }  dlcNoBehaviors,
  { DLCTL_NO_METACHARSET              = $00010000; }  dlcNoMetaCharSet,
  { DLCTL_URL_ENCODING_DISABLE_UTF8   = $00020000; }  dlcURLEncodeDisableUTF8,
  { DLCTL_URL_ENCODING_ENABLE_UTF8    = $00040000; }  dlcURLEncodeEnableUTF8,
  { DLCTL_FORCEOFFLINE                = $10000000; }  dlcForceOffline,
  { DLCTL_NO_CLIENTPULL               = $20000000; }  dlcNoClientPullMETARefresh,
  { DLCTL_SILENT                      = $40000000; }  dlcSilentNoUI,
  { DLCTL_OFFLINEIFNOTCONNECTED       = $80000000; }  dlcOfflineIfNotConnected
  { DLCTL_OFFLINE                     = DLCTL_OFFLINEIFNOTCONNECTED; }
  );

  TAmbientDLCtlSet = set of TAmbientDLCtl;
Const
  dlcBegin = dlcDownloadImages;
  dlcEnd   = dlcOfflineIfNotConnected;

Type
  //--------------------------------------
  TGwWebBrowserTamed = class(TWebBrowser, IDispatch {+ others from ancestor })
  //--------------------------------------
  protected
    { IDispatch }
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   CreateWnd; override;

    //---------------------------------
    // Ambient DL Ctl
    //---------------------------------
  private
    FAmbientDLCtlSet      : TAmbientDLCtlSet;
    fAmbientDLCtlsPending : boolean;
    procedure setAmbientDLCtlSet(AAmbientDLCtls: TAmbientDLCtlSet);
    procedure AmbientDLCtlSetSendToWB;
  public
    // AmbientDLCtlsPending: diagnostic flag
    // True  = AmbientDLCtlSetSendToWB executed, told browser to get new values
    // False = Browser got DLCtl values via IDispatch.Invoke
    property AmbientDLCtlsPending: boolean read fAmbientDLCtlsPending;
  published
    property AmbientDLCtlSet: TAmbientDLCtlSet read FAmbientDLCtlSet write setAmbientDLCtlSet;

  public  //--------- Utility -----------
    class function AmbientDLCtlSetToBitFields(AAmbientDLCtlSet: TAmbientDLCtlSet): integer;
  end;

//=========================================
           implementation
//=========================================

uses
  Winapi.ActiveX
  ;

Const
  AmbientDLCtl_default = [
  { DLCTL_DLIMAGES                    }  // dlcDownloadImages,
  { DLCTL_VIDEOS                      }  // dlcPlayVideos,
  { DLCTL_BGSOUNDS                    }  // dlcPlayBkgSounds,
  { DLCTL_NO_SCRIPTS                  }  dlcNoScripts,
  { DLCTL_NO_JAVA                     }  dlcNoJava,
  { DLCTL_NO_RUNACTIVEXCTLS           }  dlcNoActiveXRun,
  { DLCTL_NO_DLACTIVEXCTLS            }  dlcNoActiveXDownload,
  { DLCTL_DOWNLOADONLY                }  // dlcDownloadOnlyNoDisplay,
  { DLCTL_NO_FRAMEDOWNLOAD            }  dlcNoFrameDownload,
  { DLCTL_RESYNCHRONIZE               }  // dlcResynchronizeIgnoreCache,
  { DLCTL_PRAGMA_NO_CACHE             }  // dlcPragmaNoProxyCache,
  { DLCTL_NO_BEHAVIORS                }  dlcNoBehaviors,
  { DLCTL_NO_METACHARSET              }  dlcNoMetaCharSet,
  { DLCTL_URL_ENCODING_DISABLE_UTF8   }  // dlcURLEncodeDisableUTF8,
  { DLCTL_URL_ENCODING_ENABLE_UTF8    }  // dlcURLEncodeEnableUTF8,
  { DLCTL_FORCEOFFLINE                }  // dlcForceOffline,
  { DLCTL_NO_CLIENTPULL               }  dlcNoClientPullMETARefresh // ,
  { DLCTL_SILENT                      }  // dlcSilentNoUI,
  { DLCTL_OFFLINEIFNOTCONNECTED       }  // dlcOfflineIfNotConnected
  { DLCTL_OFFLINE  (redundant)        }
  ];


{ TGwWebBrowserTamed }
//-------------------------------------
constructor TGwWebBrowserTamed.Create(AOwner: TComponent);
//-------------------------------------
begin
  inherited;
  // Set default value here, will be overwritten by streaming
  // Do not send to WebBrowser control yet
  FAmbientDLCtlSet := AmbientDLCtl_default;
end;

//-------------------------------------
procedure TGwWebBrowserTamed.CreateWnd;
//-------------------------------------
begin
  inherited;
  // Send whatever settings we have now, which might be from Create (default),
  // or from streaming of design-time settings.
  AmbientDLCtlSetSendToWB;
end;

//-------------------------------------
function TGwWebBrowserTamed.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
//-------------------------------------
var
  AmbientControlFlags: Integer;
begin
  if ( (DispId = DISPID_AMBIENT_DLCONTROL) OR (DispId = DISPID_AMBIENT_USERMODE) ) AND
     (Flags and DISPATCH_PROPERTYGET <> 0) AND
     (VarResult <> nil) then
  Begin
    Result := S_OK;
    case DispID of
      DISPID_AMBIENT_DLCONTROL:
      begin
        AmbientControlFlags   := AmbientDLCtlSetToBitFields(AmbientDLCtlSet);
        fAmbientDLCtlsPending := false;
        Result := S_OK;
        PVariant(VarResult)^ := AmbientControlFlags;
      end;
      DISPID_AMBIENT_USERMODE:
        PVariant(VarResult)^ := WordBool(True);
    else
      Result := DISP_E_MEMBERNOTFOUND;
    end;
  End else
  Begin
    Result := inherited Invoke(DispID, IID, LocaleID, Flags, Params, VarResult, ExcepInfo, ArgErr);
  End;
end;

//-------------------------------------
procedure TGwWebBrowserTamed.setAmbientDLCtlSet(AAmbientDLCtls: TAmbientDLCtlSet);
//-------------------------------------
begin
  FAmbientDLCtlSet  := AAmbientDLCtls;
  AmbientDLCtlSetSendToWB;
end;

//-------------------------------------
procedure TGwWebBrowserTamed.AmbientDLCtlSetSendToWB;
//-------------------------------------
var
  Control: IOleControl;
begin
  //--------- only push to control if not designing ---------
  if not (csDesigning in ComponentState) then
  Begin
    fAmbientDLCtlsPending  := true;
    if Assigned(DefaultInterface) then
    Begin
      Control := DefaultInterface as IOleControl;
      if assigned(Control) then
        Control.OnAmbientPropertyChange(DISPID_AMBIENT_DLCONTROL); //call the OnAmbientPropertyChange event
    end;
  end;
end;

//-------------------------------------
class function TGwWebBrowserTamed.AmbientDLCtlSetToBitFields(AAmbientDLCtlSet: TAmbientDLCtlSet): integer;
//-------------------------------------
  procedure SetOneCtl(AAmbientDLCtl: TAmbientDLCtl; ADLCTL: integer);
  Begin
    if AAmbientDLCtl in AAmbientDLCtlSet then Result := Result OR ADLCTL;
  End;
begin
  Result := 0;
  SetOneCtl(dlcDownloadImages                 , DLCTL_DLIMAGES                   );
  SetOneCtl(dlcPlayVideos                     , DLCTL_VIDEOS                     );
  SetOneCtl(dlcPlayBkgSounds                  , DLCTL_BGSOUNDS                   );
  SetOneCtl(dlcNoScripts                      , DLCTL_NO_SCRIPTS                 );
  SetOneCtl(dlcNoJava                         , DLCTL_NO_JAVA                    );
  SetOneCtl(dlcNoActiveXRun                   , DLCTL_NO_RUNACTIVEXCTLS          );
  SetOneCtl(dlcNoActiveXDownload              , DLCTL_NO_DLACTIVEXCTLS           );
  SetOneCtl(dlcDownloadOnlyNoDisplay          , DLCTL_DOWNLOADONLY               );
  SetOneCtl(dlcNoFrameDownload                , DLCTL_NO_FRAMEDOWNLOAD           );
  SetOneCtl(dlcResynchronizeIgnoreCache       , DLCTL_RESYNCHRONIZE              );
  SetOneCtl(dlcPragmaNoProxyCache             , DLCTL_PRAGMA_NO_CACHE            );
  SetOneCtl(dlcNoBehaviors                    , DLCTL_NO_BEHAVIORS               );
  SetOneCtl(dlcNoMetaCharSet                  , DLCTL_NO_METACHARSET             );
  SetOneCtl(dlcURLEncodeDisableUTF8           , DLCTL_URL_ENCODING_DISABLE_UTF8  );
  SetOneCtl(dlcURLEncodeEnableUTF8            , DLCTL_URL_ENCODING_ENABLE_UTF8   );
  SetOneCtl(dlcForceOffline                   , DLCTL_FORCEOFFLINE               );
  SetOneCtl(dlcNoClientPullMETARefresh        , DLCTL_NO_CLIENTPULL              );
  SetOneCtl(dlcSilentNoUI                     , DLCTL_SILENT                     );
  SetOneCtl(dlcOfflineIfNotConnected          , integer(DLCTL_OFFLINEIFNOTCONNECTED)   );
end;



end.
