unit GemOsmWebB;

interface

uses
  System.SysUtils, System.Classes,

  Vcl.Controls, Vcl.OleCtrls, mshtml, Vcl.Imaging.pngimage,

  SHDocVw, GwWebBrowserTamed, GEMosmMarkers;

type
  TMapOptions = class(TPersistent)
  private
    fLat: Double;
    fLong: Double;
  public
//    constructor Create(AOwner: TComponent);
//    destructor Destroy; override;
//    procedure Assign(Source: TPersistent); override;
  published
    property Lat: Double read fLat write fLat;
    property Long: Double read fLong write fLong;
  end;


  TGEMosmWebBrowser = class(TGwWebBrowserTamed)
  private
    fMapOtions: TMapOptions;
    fURL: string;
    fMarkers: TMarkers;
    bLaunchFinish:Boolean;
//    FOnWebOSMapsError: TWebOSMapsErrorEvent;
    function GetURL: string;
    procedure SetMarkers(const Value: TMarkers);
    function ExecJScript(const Script: string):Boolean;
  published
//    property EnableCaching default True;
    property Align;
    property Anchors;
    property Height;
//    property Size;
    property Margins;
    property Markers : TMarkers read FMarkers write SetMarkers;

//    property Position;
    property URL: string read GetURL write fURL;
    property Visible default True;
//    property CanFocus default True;
    property Width;
//    property OnDidStartLoad;
//    property OnDidFinishLoad;
//    property OnDidFailLoadWithError;
//    property OnShouldStartLoadWithRequest;
  public
    property MapOptions: TMapOptions read fMapOtions write fMapOtions;
  end;

procedure Register;

implementation
uses
  System.Types;


procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TGEMosmWebBrowser]);
end;

{ TGEMosmWebBrowser }

function TGEMosmWebBrowser.ExecJScript(const Script: string): Boolean;
begin
  Result:=False;
  //Fix: only allow ExecJScript after the map has finished loading
  //to avoid JS errors
  if not bLaunchFinish then
    Exit;

//  {$IFNDEF FMXLIB}
//  if HTMLWindow2<>nil then
//  begin
//    try
//      HTMLWindow2.ExecScript(Script, JAVASCRIPT);
//      Result := True;
//    except
//      if Assigned(FOnWebOSMapsError) then
//        FOnWebOSMapsError(Self, etJavascriptError);
//    end;
//  end;
//  {$ENDIF}
//  {$IFDEF FMXLIB}
//  Result := ExecuteJavascript(Script) <> '';
//  {$ENDIF}
end;

function TGEMosmWebBrowser.GetURL: string;
begin
  Result := fURL;
end;

procedure TGEMosmWebBrowser.SetMarkers(const Value: TMarkers);
begin
  FMarkers := Value;
end;


end.
