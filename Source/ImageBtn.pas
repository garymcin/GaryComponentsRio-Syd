unit ImageBtn;

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
  IdUserPassProvider, IdCustomTransparentProxy, IdFTPCommon, IdHTTP, IdDICT,
  IdComponent, IdIntercept, IdIOHandler, IdIOHandlerStream, IdCookieManager,
  IdBaseComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,

  ColorButton,

  JvExExtCtrls, JvExtComponent, JvLabel

{$IFDEF USE_CODESITE}
  , CodeSiteLogging
{$ENDIF}
  ;

type
  TImageBtn = object(TPanel)
    fImage: TImage;
  private
  protected
  public


//      Left := 1;
//      Top := 1;
//      Width := 71;
//      Height := 71;
//      Align := alClient;
//      ExplicitLeft := 8;
//      ExplicitTop := 8;
//      ExplicitWidth := 105;
//      ExplicitHeight := 105;
    end
  	constructor Create(Aowner: TComponent;
  published

  end




implementation


constructor TImageBtn.Create(Aowner: TComponent;
begin
  Inherited create(Aowner);
  Width := 73;
  Height := 73;
  BevelOuter := bvNone;
  TabOrder := 0;

  fImage := TImage.Create(self);
end; {End create}

end.
