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

  GEMDBGridEdit, GEMComponentsGlobal, GEMCustomBtns;

type
  TStatusDBGridCol = (sdgc_NoIniFN, sdgc_IniFileNotFound, sdgc_NoGrid, sdgc_NoSection,
                      sdgc_LayOutNotSaved, sdgc_ErrorCreateIni, sdgc_GridSaved, sdgc_GridRestored);

const
  StatusMessage: array[TStatusDBGridCol] of string = ('No storage File Name.',
                                                      'Storage file not Opened/fFund.',
                                                      'No DbGrid compoonent entered',
                                                      'Section Name not found.',
                                                      'Error saving Grid layout',
                                                      'Storage Ini file could not be Created',
                                                      'Grid Layout saved',
                                                      'Grid Layout Restored');

type

  TStatusEvent  = procedure(Sender: TObject; StatusCode: TStatusDBGridCol; const aStrParam: string) of object;

  TGEMDBGridColumnsIniStorage = class(TComponent)
  private
   fDBGrid    : TDBGrid;
   FFileName  : string;
   FOnSave    : TNotifyEvent;
   FOnRestore : TNotifyEvent;
   fOnStatus  : TStatusEvent;

    procedure SetFileName(const Value: string);
    procedure StatusMsg(StatusCode: TStatusDBGridCol);
    procedure SaveColumnsLayout(const IniFileSection: string);
    procedure RestoreColumnsLayout(const Section: string);
    function  ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
    function  WordPosition(const N: Integer; const S: string;
                          const WordDelims: TSysCharSet): Integer;
    procedure SetDBGrid(const Value: TDBGrid);
    function GetDefaultSection: string;
  public
    constructor Create(AOwner: TComponent); override;
//    destructor Destroy;
    procedure RestoreGridFromIni;
    procedure SaveGridToIni;
  published
    property DBGrid    : TDBGrid read fDBGrid write SetDBGrid;
    property FileName  : string read FFileName write SetFileName;
    property OnStatus  : TStatusEvent read fOnStatus write fOnStatus;
    property OnSave    : TNotifyEvent read FOnSave write FOnSave;
    property OnRestore : TNotifyEvent read FOnRestore write FOnRestore;
  end;



  TSaveDBGridColProps = class(TCustomButton)
  strict private
    class constructor Create;
    class destructor Destroy;
  private
    aMouseInControl       : Boolean;
    fDBGrid               : TDBGrid;
    fDBNavigator          : TDBNavigator;
    FSaveGridLayout       : Boolean;
    FKind                 : TBitBtnKind;
    FLayout               : TButtonLayout;
    fAllEnabled           : boolean;

    fIniStorage           : TGEMDBGridColumnsIniStorage;
    ColSelEditor          : TGEMDBGridEditForm;

    fOnStatus             : TStatusEvent;
    FOnSave               : TNotifyEvent;
    FOnRestore            : TNotifyEvent;
    FOnColumnMoved           : TMovedEvent;

    procedure ClickButton(Sender: TObject);
    procedure MovedCol(sender: TObject; FromIndex, ToIndex: Longint);

    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMGridVisiblityChange(var message: TCMControlChange); message CM_VISIBLECHANGED;
    procedure CMGridEnabledChange(var message: TCMControlChange); message CM_ENABLEDCHANGED;
    procedure WmBeforeShow(var Msg: TMessage); message WM_CREATE;
    procedure WmDestroy( var msg: TMessage); message WM_DESTROY;
    procedure WmClose( var msg: TMessage); message WM_CLOSE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;


//    procedure SaveColumnsLayout(const IniFileSection: string);
//    procedure RestoreColumnsLayout(const Section: string);
//    function  ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
//    function  WordPosition(const N: Integer; const S: string;
//                          const WordDelims: TSysCharSet): Integer;
    procedure SetEnabledAll(const Value: Boolean);
    procedure SetDBGrid(const Value: TDBGrid);
    procedure SetDBNavigator(const Value: TDBNavigator);
    procedure SetSaveGridlayout(const Value: Boolean);
    procedure SetKind(const Value: TBitBtnKind);
    procedure SetLayout(const Value: TButtonLayout);
//    procedure ColumnMoved(FromIndex, ToIndex: Longint);
//    procedure ColumnMoved(FromIndex, ToIndex: Longint);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;
    procedure Click; override;
//    procedure ColumnMove;
//    procedure ErrorMsg(const msg: string);
    procedure StatusMsg(StatusCode: TStatusDBGridCol);
  published
    property DBNavigator    : TDBNavigator read fDBNavigator write SetDBNavigator;
    property SaveGridLayout : Boolean read FSaveGridLayout write SetSaveGridlayout default False;
    property DBGrid         : TDBGrid read fDBGrid write SetDBGrid;
    property Layout         : TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Kind           : TBitBtnKind read fKind write SetKind default bkCustom;
    property EnabledAll     : Boolean read fAllEnabled write SetEnabledAll default False;
    property IniStorage     : TGEMDBGridColumnsIniStorage read fIniStorage write fIniStorage;
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property CommandLinkHint;
    property Constraints;
    property Default;
    property DisabledImageIndex;
    property DisabledImageName;
    property DisabledImages;
    property DoubleBuffered;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
//    property DropDownMenu;
    property ElevationRequired;
    property Enabled;
//    property Font;
    property HotImageIndex;
    property HotImageName;
    property ImageAlignment;
    property ImageIndex;
    property ImageName;
    property ImageMargins;
    property Images;
//    property ModalResult;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
//    property ParentFont;
    property ParentShowHint;
//    property PopupMenu;
    property PressedImageIndex;
    property PressedImageName;
    property SelectedImageIndex;
    property SelectedImageName;
    property ShowHint;
    property Style;
    property StylusHotImageIndex;
    property StylusHotImageName;
    property TabOrder;
    property TabStop;
    property Visible;
//    property WordWrap;
    property StyleElements;
    property StyleName;

    property OnClick;
    property OnContextPopup;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnDropDownClick;
//    property OnEndDock;
//    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
//    property OnStartDock;
//    property OnStartDrag;
//    property OnError        : TErrorEvent read fOnError write fOnError;
    property OnStatus       : TStatusEvent read fOnStatus write fOnStatus;
    property OnSave         : TNotifyEvent read FOnSave write FOnSave;
    property OnRestore      : TNotifyEvent read FOnRestore write FOnRestore;
    property OnColumnMoved  : TMovedEvent read FOnColumnMoved write FOnColumnMoved;
  end;


implementation

uses
  Winapi.UxTheme, Winapi.DwmApi,

  System.Types, System.Math, System.IniFiles,

  Vcl.Consts, Vcl.ActnList, Vcl.Forms,

  GemINI;

{ TSaveDBGridColProps }

constructor TSaveDBGridColProps.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKind          := bkCustom;
  FLayout        := blGlyphLeft;
  ControlStyle   := ControlStyle + [csReflector, csPaintBlackOpaqueOnGlass];
  DoubleBuffered := True;
//  Caption := '';
  Self.Width := 15;
  Self.Height := 15;
//  FFileName := '';

//  Caption := '';
//  DBGrid.OnColumnMoved := MovedCol;
  OnClick := ClickButton;
end;

destructor TSaveDBGridColProps.Destroy;
begin
//  Invalidate;
//  SaveGridToIni;
  inherited Destroy;
end;


procedure TSaveDBGridColProps.MovedCol(sender: TObject; FromIndex, ToIndex: Longint);
begin
  if FSaveGridLayout then begin
    if Assigned(FOnSave) then
      FOnSave(Self);
    fIniStorage.fDBGrid := fDBGrid;
    fIniStorage.SaveGridToIni;
  end;
  inherited;
end;


procedure TSaveDBGridColProps.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or BS_OWNERDRAW;
end;


procedure TSaveDBGridColProps.Click;
var
  Form: TCustomForm;
  Control: TWinControl;
begin
//  StatusMsg('in Click');
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


procedure TSaveDBGridColProps.ClickButton;
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
      if FSaveGridLayout then begin
        if Assigned(FOnSave) then
          FOnSave(Self);
        fIniStorage.fDBGrid := fDBGrid;
        fIniStorage.SaveGridToIni;
      end;

    end;
  end;
end;


procedure TSaveDBGridColProps.SetEnabledAll(const Value: Boolean);
begin
  if Value <> fAllEnabled then begin
    fAllEnabled := Value;
    fDBNavigator.Enabled := value;
    fDBGrid.enabled := Value;
    self.Enabled := value;
  end;
end;


procedure TSaveDBGridColProps.SetDBGrid(const Value: TDBGrid);
begin
  fDBGrid := Value;
  if Assigned(fDBGrid) then begin
    fDBGrid.Visible       := Visible;
    fDBGrid.Enabled       := Enabled;
    fdbgrid.OnColumnMoved := MovedCol;
  end;
end;

procedure TSaveDBGridColProps.SetDBNavigator(const Value: TDBNavigator);
begin
  fDBNavigator := Value;
  if Assigned(fDBNavigator) then begin
    fDBNavigator.Visible := Visible;
    fDBNavigator.Enabled := Enabled;
  end;
end;

procedure TSaveDBGridColProps.SetKind(const Value: TBitBtnKind);
begin
  fKind := Value;
end;

procedure TSaveDBGridColProps.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
end;

procedure TSaveDBGridColProps.SetSaveGridlayout(const Value: Boolean);
begin
  FSaveGridLayout := Value;
end;


procedure TSaveDBGridColProps.StatusMsg(StatusCode: TStatusDBGridCol);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, StatusCode, StatusMessage[StatusCode]);
end;


procedure TSaveDBGridColProps.CMGridVisiblityChange(var message: TCMControlChange);
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


procedure TSaveDBGridColProps.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and not aMouseInControl and not (csDesigning in ComponentState) then
  begin
    aMouseInControl := True;
    Repaint;
  end;
end;

procedure TSaveDBGridColProps.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and aMouseInControl then
  begin
    aMouseInControl := False;
    Repaint;
  end;
end;

procedure TSaveDBGridColProps.CNDrawItem(var Message: TWMDrawItem);
begin
  //DrawItem(Message.DrawItemStruct^);
end;

procedure TSaveDBGridColProps.CNMeasureItem(var Message: TWMMeasureItem);
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


//procedure TSaveDBGridColProps.ColumnMoved(FromIndex, ToIndex: Longint);
//begin
//  if FSaveGridLayout then begin
//    if Assigned(FOnSave) then
//      FOnSave(Self);
//    fIniStorage.fDBGrid := fDBGrid;
//    fIniStorage.SaveGridToIni;
//  end;
//  inherited;
//end;

procedure TSaveDBGridColProps.CMGridEnabledChange(var message: TCMControlChange);
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


procedure TSaveDBGridColProps.WmBeforeShow(var Msg: TMessage);
begin
//  var IniSectionName := GetDefaultSection;
//  if  IniSectionName = '' then begin
//    ErrorMsg('Could NOT fine Section Name.');
//    exit;
//  end;

  if Assigned(fDBGrid) and FSaveGridLayout then begin
//    if FileExists(Filename) then
    try
      if Assigned(FOnRestore) then
        FOnRestore(Self);
      fIniStorage.fDBGrid := fDBGrid;
      fIniStorage.RestoreGridFromIni;
//      fIniStorage.
//      RestoreColumnsLayout(IniSectionName);
    except
      on err:exception do
        StatusMsg(sdgc_NoSection);
//        ErrorMsg('Could NOT fine Storage Ini file.')
    end;
  end;
end;


procedure TSaveDBGridColProps.WmClose(var msg: TMessage);
begin
//  if Assigned(fDBGrid) and FSaveGridLayout then begin
//    if Assigned(FOnSave) then
//      FOnSave(Self);
//    fIniStorage.fDBGrid := fDBGrid;
//    fIniStorage.SaveGridToIni;
//  end;
end;

procedure TSaveDBGridColProps.WmDestroy(var msg: TMessage);
begin
//  exit;
//  if Assigned(fDBGrid) and FSaveGridLayout then begin
//    if Assigned(FOnSave) then
//      FOnSave(Self);
//    fIniStorage.fDBGrid := fDBGrid;
//    fIniStorage.SaveGridToIni;
//  end;
end;


procedure TSaveDBGridColProps.WndProc(var Message: TMessage);
begin
  inherited;
//  if (Message.Msg = CM_MOUSELEAVE) then
//  begin
//    BackColor := BackBeforeHoverColor;
//    invalidate;
//  end;
//  if (Message.Msg = CM_MOUSEENTER) then
//  begin
//    BackBeforeHoverColor := BackColor;
//    BackColor := HoverColor;
//    invalidate;
//  end;
end;


class constructor TSaveDBGridColProps.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TSaveDBGridColProps, TButtonStyleHook);
end;


class destructor TSaveDBGridColProps.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TSaveDBGridColProps, TButtonStyleHook);
end;


{ TDBGridColumnsIniStorage }
{ TDBGridColumnsIniStorage }
{ TDBGridColumnsIniStorage }
{ TDBGridColumnsIniStorage }

constructor TGEMDBGridColumnsIniStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFileName := '';
  fDBGrid := nil;
end;



//destructor TDBGridColumnsIniStorage.Destroy;
//begin
//  inherited Destroy;
//end;

//procedure TDBGridColumnsIniStorage.ErrorMsg(const msg: string);
//begin
//  if Assigned(FOnError) then
//    FOnError(Self, msg);
//end;

function TGEMDBGridColumnsIniStorage.ExtractWord(N: Integer; const S: string; const WordDelims: TSysCharSet): string;
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

procedure TGEMDBGridColumnsIniStorage.RestoreColumnsLayout(const Section: string);

  function MyBoolToStr(aBool: Boolean): string;
  begin
    if aBool then
      Result := 'true'
    else
      Result := 'false';
  end;
  //============================
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
  if Section <> '' then
    SectionName := Section
  else begin
    StatusMsg(sdgc_NoSection);
    Exit;
  end;

  if fFileName = '' then begin
    StatusMsg(sdgc_NoIniFN);
    Exit;
  end;

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

        ColumnArray[I].Column   := fDBGrid.Columns.Items[I];
        ColumnArray[I].EndIndex := fDBGrid.Columns.Items[I].Index;
        if S <> '' then begin
          ColumnArray[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
                                     ColumnArray[I].EndIndex);
          S := ExtractWord(2, S, Delims);
          fDBGrid.Columns.Items[I].Width := StrToIntDef(S,
                                                fDBGrid.Columns.Items[I].Width);
          fDBGrid.Columns.Items[I].Visible := (S <> '-1');
        end;
      end;

      for I := 0 to fDBGrid.Columns.Count - 1 do begin
        for J := 0 to fDBGrid.Columns.Count - 1 do begin
          if ColumnArray[J].EndIndex = I then begin
            ColumnArray[J].Column.Index := ColumnArray[J].EndIndex;
            Break;
          end;

        end;
      end;
    finally
//      FreeAndNil(fIniFile);
    end;
    if Assigned(FOnRestore) then
      FOnRestore(Self);
  except
    StatusMsg(sdgc_IniFileNotFound);
  end;
end;


procedure TGEMDBGridColumnsIniStorage.RestoreGridFromIni;
begin
  var IniFileSection := GetDefaultSection;
  RestoreColumnsLayout(IniFileSection);
//  freeAndNil(fDBGrid);
end;


procedure TGEMDBGridColumnsIniStorage.SaveColumnsLayout(const IniFileSection: string);
var
  I: Integer;
  fIniFile   : TIniFile;
begin
//  StatusMsg('Save Columns Layout');
  if IniFileSection = '' then begin
    StatusMsg(sdgc_LayOutNotSaved);
    Exit;
  end;

  if fFileName = '' then begin
    StatusMsg(sdgc_NoIniFN);
    Exit;
  end;

  fIniFile := TIniFile.Create(fFileName);
  try
    try
      fIniFile.EraseSection(IniFileSection);
      for I := 0 to fDBGrid.Columns.Count - 1 do
        fIniFile.WriteString(IniFileSection, format('%s.%s', [fDBGrid.Name, fDBGrid.Columns.Items[I].FieldName]),
          Format('%d,%d', [fDBGrid.Columns.Items[I].Index, fDBGrid.Columns.Items[I].Width]));
    finally
      FreeAndNil(fIniFile);
//      freeAndNil(fDBGrid);
    end;
    if Assigned(FOnSave) then
      FOnSave(Self);
  except
    StatusMsg(sdgc_ErrorCreateIni);
  end;
end;

procedure TGEMDBGridColumnsIniStorage.SaveGridToIni;
begin
  if fDBGrid <> nil then begin
    var IniFileSection := GetDefaultSection;
    SaveColumnsLayout(IniFileSection);
  end;
end;


procedure TGEMDBGridColumnsIniStorage.SetDBGrid(const Value: TDBGrid);
begin
  if fDBGrid <> value then begin
    fDBGrid := value;
  end;
end;

procedure TGEMDBGridColumnsIniStorage.SetFileName(const Value: string);
begin
  if fFileName <> value then
    fFileName := value;
end;

procedure TGEMDBGridColumnsIniStorage.StatusMsg(StatusCode: TStatusDBGridCol);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, StatusCode, StatusMessage[StatusCode]);
end;

function TGEMDBGridColumnsIniStorage.WordPosition(const N: Integer;
  const S: string; const WordDelims: TSysCharSet): Integer;
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


function TGEMDBGridColumnsIniStorage.GetDefaultSection: string;
var
  F: TCustomForm;
begin
  if fDBGrid <> nil then begin
    Result := fDBGrid.Name;
    // only works with 2005 and greater
    F := GetParentForm(TControl(fDBGrid), false);
    if F <> nil then
      Result := Format('%s\%s',[F.ClassName, Result])
    else begin
      if TControl(fDBGrid).Parent <> nil then
        Result := Format('%s\%s',[TControl(fDBGrid).Parent.Name, Result]);
    end;
  end
  else begin
    result := '';
    StatusMsg(sdgc_NoGrid);
  end;
end;


end.
