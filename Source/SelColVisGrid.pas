(*$IMPORTEDDATA ON*)
unit SelColVisGrid;

{$S-,W-,R-,H+,X+}
{$C PRELOAD}

{$HPPEMIT '#ifndef _WIN64'}
{$HPPEMIT '#pragma link "dwmapi.lib"'}
{$HPPEMIT '#endif //_WIN64'}

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,

  System.Classes, System.Contnrs, System.Generics.Collections, System.UITypes,
  System.SysUtils,

  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.DBCtrls, Vcl.DBGrids, Vcl.Grids,
  Vcl.ExtCtrls, VCL.Graphics, Vcl.Themes, Vcl.Imglist, VCL.Dialogs,

  Data.DB, DBControlLists,

  GEMDBGridEdit, GEMComponentsGlobal;

type

//  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
//  TButtonState  = (bsUp, bsDisabled, bsDown, bsExclusive);
//  TButtonStyle  = (bsAutoDetect, bsWin31, bsNew);
//  TNumGlyphs    = 1..4;

  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;


  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;


  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FPaintOnGlass: Boolean;
    FThemeDetails: TThemedElementDetails;
    FThemesEnabled: Boolean;
    FOnChange: TNotifyEvent;
    FThemeTextColor: Boolean;

    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; Flags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean; BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSelColVisGridBtn = class;

  TSelColVisGridBtActionLink = class(TControlActionLink)
  protected
    FClient    : TSelColVisGridBtn;
    FImageIndex: Integer;
    procedure AssignClient(AClient: TObject); override;
    function IsImageIndexLinked: Boolean; override;
    function IsGlyphLinked(Index:  System.UITypes.TImageIndex): Boolean;
    procedure SetImageIndex(Value: Integer); override;
  public
    constructor Create(AClient: TObject); override;
  end;

  TErrorEvent  = procedure(Sender: TObject; const aStrParam: string) of object;
  TStatusEvent = procedure(Sender: TObject; const aStrParam: string) of object;

  TSelColVisGridBtn = class(TCustomButton)
  private
    { Private declarations }
    FCanvas               : TCanvas;
    FGlyph                : TObject;
    FStyle                : TButtonStyle;
    FKind                 : TBitBtnKind;
    FLayout               : TButtonLayout;

    //FPathName             : string;
    FFileName             : string;
    FSaveGridLayout       : Boolean;

    FSpacing              : Integer;
    FMargin               : Integer;
    IsFocused             : Boolean;
    FModifiedGlyph        : Boolean;
    FMouseInControl       : Boolean;

    FBackBeforeHoverColor : TColor;

    fDBGrid               : TDBGrid;
    fDBNavigator          : TDBNavigator;
    FBackColor            : TColor;
    FForeColor            : TColor;
    FHoverColor           : TColor;

    ColSelEditor          : TGEMDBGridEditForm;

    fSectionName          : string;

    fOnError              : TErrorEvent;
    fOnStatus             : TStatusEvent;

    procedure ClickButton(Sender: TObject);
    procedure GlyphChanged(Sender: TObject);
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetStyle(Value: TButtonStyle);
    function GetGlyph: TBitmap;
    function IsCustom: Boolean;
    procedure SetGlyph(const Value: TBitmap);
    procedure GEMInternalCopyImage(Image: TBitmap; ImageList: TCustomImageList;
                                   Index: Integer);
//    function GetKind: TBitBtnKind;
    procedure SetKind(const Value: TBitBtnKind);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetFileName(const Value: string);
    procedure SetSaveGridlayout(const Value: Boolean);
    procedure SetDBNavigator(const Value: TDBNavigator);
    procedure SetDBGrid(const Value: TDBGrid);
    function SetVersion: string;

    property BackBeforeHoverColor : TColor read FBackBeforeHoverColor write FBackBeforeHoverColor;

    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure CMGridVisiblityChange(var message: TCMControlChange); message CM_VISIBLECHANGED;
    //procedure CMGridEnabledChange(var message: TCMControlChange); message CM_ENABLEDCHANGED;
    //CM_VISIBLECHANGED
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WmBeforeShow(var Msg: TMessage); message WM_CREATE;
    procedure WmBeforeClose(var Msg: TMessage); message WM_DESTROY;


    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;
    function  GetPalette: HPALETTE; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
    procedure CreateHandle; override;
    function  GetActionLinkClass: TControlActionLinkClass; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
    procedure UpdateStyleElements; override;
    procedure SaveColumnsLayout(const Section: string);
    procedure RestoreColumnsLayout(const Section: string);
    function  ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
    function  WordPosition(const N: Integer; const S: string;
                          const WordDelims: TSysCharSet): Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;
    procedure ErrorMsg(const msg: string);
    procedure StatusMsg(const msg: string);
    property  BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property  ForeColor: TColor read FForeColor write SetForeColor default clBtnText;
    property  HoverColor: TColor read FHoverColor write SetHoverColor default clBtnFace;
    //property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustom;
    property  OnClick;
    property  Kind: TBitBtnKind read fKind write SetKind default bkCustom;
    property  NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
  published
    { Published declarations }
    property Action;
    property Align;
    property Anchors;
    property FileName       : string read FFileName write SetFileName;
    property DoubleBuffered default True;
    property Enabled default True;
    property Glyph          : TBitmap read GetGlyph write SetGlyph stored isCustom;
    property Layout         : TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin         : Integer read FMargin write SetMargin default -1;
    property Navigator      : TDBNavigator read fDBNavigator write SetDBNavigator;
    property SaveGridLayout : Boolean read FSaveGridLayout write SetSaveGridlayout default False;
    property SelColDBGrid   : TDBGrid read fDBGrid write SetDBGrid;
    property ShowHint;
    property Spacing        : Integer read FSpacing write SetSpacing default 4;
    property Style          : TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Visible default True;
    property Version        : string read SetVersion;
    property OnError        : TErrorEvent read fOnError write fOnError;
    property OnStatus       : TStatusEvent read fOnStatus write fOnStatus;
  end;


implementation

uses
  Winapi.UxTheme, Winapi.DwmApi,

  System.Types, System.Math, System.IniFiles,

  Vcl.Consts, Vcl.ActnList, Vcl.Forms,

  GemINI;


//var
//  BitBtnResNames: array[TBitBtnKind] of PChar = (
//    nil, 'BBOK', 'BBCANCEL', 'BBHELP', 'BBYES', 'BBNO', 'BBCLOSE',
//    'BBABORT', 'BBRETRY', 'BBIGNORE', 'BBALL');
//
//  BitBtnModalResults: array[TBitBtnKind] of TModalResult = (
//    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
//    mrAll);
//
//  BitBtnGlyphs: array[TBitBtnKind] of TBitmap;



function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
begin
  if Component <> nil then begin
    Result := Component.Name;
    // only works with 2005 and greater
    F := GetParentForm(TControl(Component), false);
    if F <> nil then
      Result := Format('%s\%s',[F.ClassName, Result])
    else begin
      if TControl(Component).Parent <> nil then
        Result := Format('%s\%s',[TControl(Component).Parent.Name, Result]);
    end;
  end
  else
    result := '';
end;


//=============================================================================
//[SecurityPermission(SecurityAction.Assert, UnmanagedCode=True)]
//function GetBitBtnGlyph(Kind: TBitBtnKind): TBitmap;
//begin
//  if BitBtnGlyphs[Kind] = nil then
//  begin
//    BitBtnGlyphs[Kind] := TBitmap.Create;
//    BitBtnGlyphs[Kind].LoadFromResourceName(HInstance, BitBtnResNames[Kind]);
//  end;
//  Result := BitBtnGlyphs[Kind];
//end;
//

{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;


destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;


function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;


function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;


procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;


{ TGlyphCache ================================================================}

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;


destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;


function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := TGlyphList(GlyphLists[I]);
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;


procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;


function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;


var
  GlyphCache: TGlyphCache = nil;
  ButtonCount: Integer = 0;


{ TSelColVisGrid ==============================================================}
{ TSelColVisGrid ==============================================================}
{ TSelColVisGrid ==============================================================}

constructor TSelColVisGridBtn.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;

  inherited Create(AOwner);

  FCanvas        := TCanvas.Create;
  FStyle         := bsAutoDetect;
  FKind          := bkCustom;
  FLayout        := blGlyphLeft;
  FSpacing       := 4;
  FMargin        := -1;
  ControlStyle   := ControlStyle + [csReflector, csPaintBlackOpaqueOnGlass];
  DoubleBuffered := True;

  Glyph.LoadFromResourceName(HInstance, 'ARROWDOWN');
  FFileName := '';

  Caption := '';
  OnClick := ClickButton;
end;


destructor TSelColVisGridBtn.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;
  inherited Destroy;
end;


procedure TSelColVisGridBtn.CreateHandle;
var
  State: TButtonState;
begin
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  inherited CreateHandle;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;


procedure TSelColVisGridBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or BS_OWNERDRAW;
end;


procedure TSelColVisGridBtn.WmBeforeShow(var Msg: TMessage);
begin
  StatusMsg('In Before Show');
  fSectionName := GetDefaultSection(self);//Name+'\'+fDBGrid.Name;
//  if  fSectionName = '' then
//    fSectionName := Name+'\'+fDBGrid.Name;
//  fSectionName := Parent.ClassName+'\'+fDBGrid.Name;
  StatusMsg('Section Name = '+ fSectionName);

  if Assigned(fDBGrid) and FSaveGridLayout then begin
    if FileExists(Filename) then
      try
        RestoreColumnsLayout(fSectionName);
      except
        on err:exception do
          ErrorMsg('Could NOT fine Storage Ini file.')
      end;
  end;
end;


procedure TSelColVisGridBtn.SaveColumnsLayout(const Section: string);
var
  I: Integer;
  SectionName: string;
  fIniFile   : TIniFile;
begin
  StatusMsg('Save Columns Layout');

  if Section <> '' then
    SectionName := Section
  else begin
    ErrorMsg('Could not save Column layout = '+Section);
    Exit;
  end;

  fIniFile := TIniFile.Create(fFileName);
  try
    try
      fIniFile.EraseSection(SectionName);
      for I := 0 to fDBGrid.Columns.Count - 1 do
        fIniFile.WriteString(SectionName, format('%s.%s', [fDBGrid.Name, fDBGrid.Columns.Items[I].FieldName]),
          Format('%d,%d', [fDBGrid.Columns.Items[I].Index, fDBGrid.Columns.Items[I].Width]));
    finally
      FreeAndNil(fIniFile);
    end;
  except
    ErrorMsg('Storage Ini file could not be Created')
  end;
end;


procedure TSelColVisGridBtn.WmBeforeClose(var Msg: TMessage);
begin
  StatusMsg('In Before Close');
  if Assigned(fDBGrid) and FSaveGridLayout then begin
    if FileName <> '' then  begin
      try
        SaveColumnsLayout(fSectionName);
      except
        on err:exception do
          ErrorMsg('Could not Save Col Save File');
      end;
    end
    else
      ErrorMsg('Storage file not found.')
  end;
end;


procedure TSelColVisGridBtn.ErrorMsg(const msg: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, msg);
end;


procedure TSelColVisGridBtn.StatusMsg(const msg: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, msg);
end;


function TSelColVisGridBtn.ExtractWord(N: Integer; const S: string;
                                       const WordDelims: TSysCharSet): string;

{$REGION 'SOURCE'}
// Next two routines
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJCLUtils.pas, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Andreas Hausladen
  Ralf Kaiser
  Vladimir Gaitanoff
  Dejoy den

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

-----------------------------------------------------------------------------}
{$ENDREGION}
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
    begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;


function TSelColVisGridBtn.WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin
    { skip over delimiters }
    while (I <= Length(S)) and CharInSet(S[I], WordDelims) do
      Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then
      Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;


procedure TSelColVisGridBtn.RestoreColumnsLayout(const Section: string);

  function MyBoolToStr(aBool: Boolean): string;
  begin
    if aBool then
      Result := 'true'
    else
      Result := 'false';
  end;

const
  Delims = [' ', ','];
type
  TColumnInfo = record
    Column: TColumn;
    EndIndex: Integer;
  end;
  TColumnArray = array of TColumnInfo;
var
  I, J: Integer;
  SectionName,
  S          : string;
  ColumnArray: TColumnArray;
  fIniFile   : TIniFile;
begin
  StatusMsg('in RestoreColumnsLayout');
  if Section <> '' then
    SectionName := Section
  else begin
    ErrorMsg('Not Section Name found.');
    Exit;
  end;

  StatusMsg(SectionName+ ' ==============================');

  fIniFile := TIniFile.Create(fFileName);
  try
    try
    {$REGION 'DOC'}
      {
       This is taken from Jedi's dbgrid routines

       The ColumnArray are objects from the actual grid. When a value is changed
       in array, the change is reflected in the DbGrid.  Since there can not
       be columns in the grid with the same index value. Placing a column in the
       array with the same index a another columns will cause that column's
       index to be incremented. And then the next column that may have the
       increment columns index value.
      }
    {$ENDREGION}
      SetLength(ColumnArray, fDBGrid.Columns.Count);
      for I := 0 to fDBGrid.Columns.Count - 1 do begin
        S := fIniFile.ReadString(SectionName, format('%s.%s', [fDBGrid.Name,
                                 fDBGrid.Columns.Items[I].FieldName]),
                                 '');
        StatusMsg('Section Data: ' + S + ' ==========================');

        ColumnArray[I].Column   := fDBGrid.Columns.Items[I];
        ColumnArray[I].EndIndex := fDBGrid.Columns.Items[I].Index;
//        StatusMsg(Format('   ColunmName -- Column/Index: %s -- %s/%s',
//                  [fDBGrid.Columns.Items[I].FieldName, IntToStr(i),
//                  IntToStr(ColumnArray[I].EndIndex)]));
        if S <> '' then begin
          ColumnArray[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
                                     ColumnArray[I].EndIndex);
          S := ExtractWord(2, S, Delims);
          fDBGrid.Columns.Items[I].Width := StrToIntDef(S,
                                                fDBGrid.Columns.Items[I].Width);
          fDBGrid.Columns.Items[I].Visible := (S <> '-1');

//          StatusMsg(Format('S <> '''' Field -- Width/Visible: %s -- %s/%s  Index = %s',
//                    [fDBGrid.Columns.Items[I].FieldName ,
//                    IntToStr(fDBGrid.Columns.Items[I].Width),
//                    MyBoolToStr(fDBGrid.Columns.Items[I].Visible),
//                    IntToStr(ColumnArray[I].EndIndex)]));
        end;
//        else
//          StatusMsg(Format('S = '''' Field -- Width/Visible: %s -- %s/%s  Index = %s',
//                    [fDBGrid.Columns.Items[I].FieldName ,
//                    IntToStr(fDBGrid.Columns.Items[I].Width),
//                    MyBoolToStr(fDBGrid.Columns.Items[I].Visible),
//                    IntToStr(ColumnArray[I].EndIndex)]));

      end;

      StatusMsg('======================');

      for I := 0 to fDBGrid.Columns.Count - 1 do begin
        StatusMsg('FieldName -- I = '+fDBGrid.Columns.Items[I].FieldName+'  '+
                  IntToStr(I));
        for J := 0 to fDBGrid.Columns.Count - 1 do begin
          if ColumnArray[J].EndIndex = I then begin
            ColumnArray[J].Column.Index := ColumnArray[J].EndIndex;
            StatusMsg('J = FieldName  ENDindex = '+
                       fDBGrid.Columns.Items[I].FieldName+'  '+
                       IntToStr(ColumnArray[J].Column.Index));
            Break;
          end;
          StatusMsg('J = '+IntToStr(J));

        end;
      end;
    finally
      FreeAndNil(fIniFile);
    end;
  except
    ErrorMsg('Storage Ini file could not be Opened/Found')
  end;
end;


procedure TSelColVisGridBtn.CopyImage(ImageList: TCustomImageList;
  Index: Integer);
begin
  GEMInternalCopyImage(Glyph, ImageList, Index);
end;


procedure TSelColVisGridBtn.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited;

end;


procedure TSelColVisGridBtn.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := GetParentForm(Self);
        if Form <> nil then
          Form.Close
        else
          inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and
              (((Control.HelpType = htContext) and (Control.HelpContext = 0)) or
              ((Control.HelpType = htKeyword) and (Control.HelpKeyword = ''))) do
          Control := Control.Parent;
        if Control <> nil then
        begin
          if Control.HelpType = htContext then
            Application.HelpContext(Control.HelpContext)
          else
            Application.HelpKeyword(Control.HelpKeyword);
        end
        else
          inherited Click;
      end;
    else
      inherited Click;
  end;
end;


procedure TSelColVisGridBtn.ClickButton;
var
  thePoint: TPoint;
begin
  if Assigned(fDBGrid) then begin
    try
      ColSelEditor :=  TGEMDBGridEditForm.Create(nil);

      thePoint.X := fDBGrid.Left;
      thePoint.Y := fDBGrid.Top;
      thePoint := fDBGrid.ClientToScreen(thePoint);

      ColSelEditor.theColumns        := fDBGrid.Columns;
      ColSelEditor.GridComponentName := fDBGrid.Name;

      ColSelEditor.left := thePoint.X - fDBGrid.Left;
      ColSelEditor.top := thePoint.Y - fDBGrid.Top;// div 2;

      ColSelEditor.CreateDataGridFieldVisible;

      ColSelEditor.ShowModal;
    finally
      ColSelEditor.free;
    end;
  end;
end;


procedure TSelColVisGridBtn.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(fDBGrid) then begin
    fDBGrid.Enabled := Enabled;
  end;
  if Assigned(fDBNavigator) then begin
    fDBNavigator.Enabled := Enabled;
  end;
  Invalidate;
end;


procedure TSelColVisGridBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


procedure TSelColVisGridBtn.CMGridVisiblityChange(var message: TCMControlChange);
begin
  inherited;
  if Assigned(fDBGrid) then begin
    fDBGrid.Visible := Visible;
  end;
  if Assigned(fDBNavigator) then begin
    fDBNavigator.Visible := Visible;
  end;
  Invalidate;
  //Repaint;
end;


procedure TSelColVisGridBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and not FMouseInControl and not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
end;


procedure TSelColVisGridBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end;
end;


procedure TSelColVisGridBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;


procedure TSelColVisGridBtn.CNMeasureItem(var Message: TWMMeasureItem);
var
{$IF DEFINED(CLR)}
  Temp: TMeasureItemStruct;
{$ELSE}
  Temp: PMeasureItemStruct;
{$ENDIF}
begin
  Temp := Message.MeasureItemStruct;
  with Temp{$IFNDEF CLR}^{$ENDIF} do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
{$IF DEFINED(CLR)}
  Message.MeasureItemStruct := Temp;
{$ENDIF}
end;


function TSelColVisGridBtn.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TBitBtnActionLink;
end;


procedure TSelColVisGridBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  WordBreakFlag: array[Boolean] of Integer = (0, DT_WORDBREAK);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R: TRect;
  Flags: Longint;
  Details: TThemedElementDetails;
  Button: TThemedButton;
  Offset: TPoint;
  LStyle: TCustomStyleServices;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font := Self.Font;
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;
  end;

  if ThemeControl(Self) then
  begin
    LStyle := StyleServices;
    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if IsDown then
        Button := tbPushButtonPressed
      else
        if FMouseInControl then
          Button := tbPushButtonHot
        else
          if IsFocused or IsDefault then
            Button := tbPushButtonDefaulted
          else
            Button := tbPushButtonNormal;

    Details := LStyle.GetElementDetails(Button);
    // Parent background.
    if not (csGlassPaint in ControlState) then
      LStyle.DrawParentBackground(Handle, DrawItemStruct.hDC, Details, True)
    else
      FillRect(DrawItemStruct.hDC, R, GetStockObject(BLACK_BRUSH));
    // Button shape.
    LStyle.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
    LStyle.GetElementContentRect(FCanvas.Handle, Details, DrawItemStruct.rcItem, R);

    Offset := Point(0, 0);
    TButtonGlyph(FGlyph).FPaintOnGlass := csGlassPaint in ControlState;
    TButtonGlyph(FGlyph).FThemeDetails := Details;
    TButtonGlyph(FGlyph).FThemesEnabled := ThemeControl(Self);
    TButtonGlyph(FGlyph).FThemeTextColor := seFont in StyleElements;
    TButtonGlyph(FGlyph).Draw(FCanvas, R, Offset, Caption, FLayout, FMargin, FSpacing, State, False,
                           DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap]);

    if IsFocused and IsDefault and LStyle.IsSystemStyle then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end
  else
  begin
    R := ClientRect;

    Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if IsDown then Flags := Flags or DFCS_PUSHED;
    if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
      Flags := Flags or DFCS_INACTIVE;

    { DrawFrameControl doesn't allow for drawing a button as the
        default button, so it must be done here. }
    if IsFocused or IsDefault then
    begin
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { DrawFrameControl must draw within this border }
      InflateRect(R, -1, -1);
    end;

    { DrawFrameControl does not draw a pressed button correctly }
    if IsDown then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
      FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      InflateRect(R, -1, -1);
    end
    else
      DrawFrameControl(DrawItemStruct.hDC, R, DFC_BUTTON, Flags);

    if IsFocused then
    begin
      R := ClientRect;
      InflateRect(R, -1, -1);
    end;

    FCanvas.Font := Self.Font;
    if IsDown then
      OffsetRect(R, 1, 1);

    TButtonGlyph(FGlyph).FThemesEnabled := ThemeControl(Self);
    TButtonGlyph(FGlyph).Draw(FCanvas, R, Point(0,0), Caption, FLayout, FMargin,
      FSpacing, State, False, DrawTextBiDiModeFlags(0) or WordBreakFlag[WordWrap]);

    if IsFocused and IsDefault then
    begin
      R := ClientRect;
      InflateRect(R, -4, -4);
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;
  end;

  FCanvas.Handle := 0;
end;


function TSelColVisGridBtn.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;


//function TSelColVisGridBtn.GetKind: TBitBtnKind;
//begin
////  if FKind <> bkCustom then
////    if ((FKind in [bkOK, bkYes]) xor Default) or
////      ((FKind in [bkCancel, bkNo]) xor Cancel) or
////      (ModalResult <> BitBtnModalResults[FKind]) or
////      FModifiedGlyph then
////      FKind := bkCustom;
////  Result := FKind;
//end;


function TSelColVisGridBtn.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;


function TSelColVisGridBtn.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;


procedure TSelColVisGridBtn.SetGlyph(const Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value as TBitmap;
  FModifiedGlyph := True;
  Invalidate;
end;


//procedure TSelColVisGridBtn.SetGridlayout(const Value: Boolean);
//begin
//  FSaveGridLayout := Value;
//end;


procedure TSelColVisGridBtn.GEMInternalCopyImage(Image: TBitmap;
  ImageList: TCustomImageList; Index: Integer);
begin
  with Image do
  begin
    Width := ImageList.Width;
    Height := ImageList.Height;
    Canvas.Brush.Color := clFuchsia;//! for lack of a better color
    Canvas.FillRect(Rect(0,0, Width, Height));
    ImageList.Draw(Canvas, 0, 0, Index);
  end;
end;


function TSelColVisGridBtn.IsCustom: Boolean;
var
  Link: TSelColVisGridBtActionLink;
begin
  Link := TSelColVisGridBtActionLink(ActionLink);
  Result := (Kind = bkCustom) and
    not ((Link <> nil) and Link.IsImageIndexLinked and Link.IsGlyphLinked(Link.FImageIndex));
end;


//function TSelColVisGridBtn.IsCustomCaption: Boolean;
//begin
//  //Result := AnsiCompareStr(Caption, LoadResString(BitBtnCaptions[FKind])) <> 0;
//end;


procedure TSelColVisGridBtn.SetSaveGridlayout(const Value: Boolean);
begin
  FSaveGridLayout := Value;
end;


procedure TSelColVisGridBtn.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
end;


procedure TSelColVisGridBtn.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;


function TSelColVisGridBtn.SetVersion: string;
begin
//  Result := VersionSelColVisGrid;
end;


procedure TSelColVisGridBtn.UpdateStyleElements;
begin
  Invalidate;
end;


procedure TSelColVisGridBtn.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;


//procedure TSelColVisGridBtn.LoadBitBtnGlyph;
//begin
//  Glyph.LoadFromResourceID(HInstance, 0);
//  Caption := '';
//end;
//

procedure TSelColVisGridBtn.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then begin
    FBackColor:= Value;
    Invalidate;
  end;
end;


procedure TSelColVisGridBtn.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;


procedure TSelColVisGridBtn.SetDBGrid(const Value: TDBGrid);
begin
  fDBGrid := Value;
  if Assigned(fDBGrid) then begin
    fDBGrid.Visible := Visible;
    fDBGrid.Enabled := Enabled;
  end;
end;


procedure TSelColVisGridBtn.SetDBNavigator(const Value: TDBNavigator);
begin
  fDBNavigator := Value;
  if Assigned(fDBNavigator) then begin
    fDBNavigator.Visible := Visible;
    fDBNavigator.Enabled := Enabled;
  end;
end;


procedure TSelColVisGridBtn.SetFileName(const Value: string);
begin
  FFileName := Value;
  FSaveGridLayout := FFileName <> ''
end;


procedure TSelColVisGridBtn.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then begin
    FForeColor:= Value;
    Invalidate;
  end;
end;


procedure TSelColVisGridBtn.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then begin
    FHoverColor:= Value;
    Invalidate;
  end;
end;


procedure TSelColVisGridBtn.SetKind(const Value: TBitBtnKind);
begin
//  if Value <> FKind then
//  begin
//    if Value <> bkCustom then
//    begin
//      Default := Value in [bkOK, bkYes];
//      Cancel := Value in [bkCancel, bkNo];
//
//      if ((csLoading in ComponentState) and (Caption = '')) or
//        (not (csLoading in ComponentState)) then
//      begin
//        //if BitBtnCaptions[Value] <> nil then
//          //Caption := LoadResString(BitBtnCaptions[Value]);
//      end;
//
//      ModalResult := BitBtnModalResults[Value];
//      TButtonGlyph(FGlyph).Glyph := GetBitBtnGlyph(Value);
//      NumGlyphs := 2;
//      FModifiedGlyph := False;
//    end;
//    FKind := Value;
//    Invalidate;
//  end;
end;


procedure TSelColVisGridBtn.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
end;


procedure TSelColVisGridBtn.SetMargin(const Value: Integer);
begin
  FMargin := Value;
end;


procedure TSelColVisGridBtn.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > 4 then
    Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;


procedure TSelColVisGridBtn.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin

end;


procedure TSelColVisGridBtn.WndProc(var Message: TMessage);
begin
  inherited;
  if (Message.Msg = CM_MOUSELEAVE) then
  begin
    BackColor := BackBeforeHoverColor;
    invalidate;
  end;
  if (Message.Msg = CM_MOUSEENTER) then
  begin
    BackBeforeHoverColor := BackColor;
    BackColor := HoverColor;
    invalidate;
  end;

end;


{ TButtonGlyph  ===============================================================}
{ TButtonGlyph  ===============================================================}
{ TButtonGlyph  ===============================================================}

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: Integer);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, Caption, Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing < 0 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left + Offset.X);
  Inc(GlyphPos.Y, Client.Top + Offset.Y);

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y + Client.Top + Offset.Y);
end;


constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  FPaintOnGlass := False;
  FThemesEnabled := False;
  FThemeTextColor := True;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;


const
  ROP_DSPDxax = $00E20746;

//[SecurityPermission(SecurityAction.Assert, UnmanagedCode=True)]
function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;


destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;


function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: Integer): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;


procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
  Details: TThemedElementDetails;
  R: TRect;
  Button: TThemedButton;
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  with GlyphPos do
  begin
    if FThemesEnabled then
    begin
      R.Left := GlyphPos.X;
      R.Top := GlyphPos.Y;
      R.Right := R.Left + FOriginal.Width div FNumGlyphs;
      R.Bottom := R.Top + FOriginal.Height;
      case State of
        bsDisabled:
          Button := tbPushButtonDisabled;
        bsDown,
        bsExclusive:
          Button := tbPushButtonPressed;
      else
        // bsUp
        Button := tbPushButtonNormal;
      end;
      Details := StyleServices.GetElementDetails(Button);

      if FPaintOnGlass then
      begin
        PaintBuffer := BeginBufferedPaint(Canvas.Handle, R, BPBF_TOPDOWNDIB, nil, MemDC);
        try
          StyleServices.DrawIcon(MemDC, Details, R, FGlyphList.Handle, Index);
          BufferedPaintMakeOpaque(PaintBuffer, R);
        finally
          EndBufferedPaint(PaintBuffer, True);
        end;
      end
      else
        StyleServices.DrawIcon(Canvas.Handle, Details, R, FGlyphList.Handle, Index);
    end
    else
      if Transparent or (State = bsExclusive) then
      begin
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
      end
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          ColorToRGB(clBtnFace), clNone, ILD_Normal);
  end;
end;


procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; Flags: Integer);
begin
 //
end;


procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;


procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;


procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;


procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

{ TSelColVisGridBtActionLink }


procedure TSelColVisGridBtActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TSelColVisGridBtn;
end;


constructor TSelColVisGridBtActionLink.Create(AClient: TObject);
begin
  inherited;
  FImageIndex := -1;
end;


function TSelColVisGridBtActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FImageIndex = TCustomAction(Action).ImageIndex);
end;


function TSelColVisGridBtActionLink.IsGlyphLinked(Index: System.UITypes.TImageIndex): Boolean;
var
  LBitmap: TBitmap;
  Images: TCustomImageList;
begin
  Images := TCustomAction(Action).ActionList.Images;
  Result := (Images <> nil) and (FClient.Glyph <> nil) and
    (FClient.Glyph.Width = Images.Width) and (FClient.Glyph.Height = Images.Height);
  if Result then
  begin
    LBitmap := TBitmap.Create;
    try
      FClient.GEMInternalCopyImage(LBitmap, Images, Index);
      Result := LBitmap.Equals(FClient.Glyph);
    finally
      LBitmap.Free;
    end;
  end;
end;


procedure TSelColVisGridBtActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked or FClient.Glyph.Empty then
  begin
    if Action is TCustomAction then
      with TCustomAction(Action) do
        { Copy image from action's imagelist }
        if (ActionList <> nil) and (ActionList.Images <> nil) then
          if (Value >= 0) and (Value < ActionList.Images.Count) then
          begin
            if IsGlyphLinked(FImageIndex) or FClient.Glyph.Empty then
              FClient.CopyImage(ActionList.Images, Value);
          end
          else
            FClient.Glyph := nil;
    FImageIndex := Value;
    FClient.GlyphChanged(nil);
  end;
end;

end.
