unit GEMosmWebBrowser;

interface
{$SCOPEDENUMS ON}
uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Messaging,

  FMX.Types, FMX.Controls, FMX.Graphics, FMX.WebBrowser, FMX.GEMTMSWebOSMapsConst;

type

  TMapOptions = class(TPersistent)
  private
    fLat: Double;
    fLong: Double;
  public
//    constructor Create(AOwner: TComponent);
//    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Lat: Double read fLat write fLat;
    property Long: Double read fLong write fLong;
  end;


  TGEMosmWebBrowser = class(TCustomWebBrowser)
  private
    fMapOtions: TMapOptions;
  published
    property EnableCaching default True;
    property Align;
    property Anchors;
    property Height;
    property Size;
    property Margins;
    property Position;
    property URL;
    property Visible default True;
    property CanFocus default True;
    property Width;
    property OnDidStartLoad;
    property OnDidFinishLoad;
    property OnDidFailLoadWithError;
    property OnShouldStartLoadWithRequest;
  public
    property MapOptions: TMapOptions read fMapOtions write fMapOtions;
  end;

procedure Register;

implementation
uses
  System.Types,
{$IFDEF ANDROID}
  FMX.WebBrowser.Android,
{$ENDIF ANDROID}
{$IF DEFINED(IOS) OR DEFINED(MACOS)}
  FMX.WebBrowser.Cocoa,
{$ENDIF}
{$IFDEF MSWINDOWS}
  FMX.WebBrowser.Win,
{$ENDIF MSWINDOWS}
  FMX.Platform, FMX.Forms;

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TGEMosmWebBrowser]);
end;

//=============================================================================
{ TMapOptions }

procedure TMapOptions.Assign(Source: TPersistent);
begin
  if Source is TMapOptions then begin
    Self.fLat := Lat;
    Self.fLong := Long;
  end
  else
    inherited;
end;


end.
