unit TMSAdvMemoBar;

{

TMemoBar - T(Custom)Memo extender

http://delphi.about.com/library/weekly/aa083005a.htm

Full source code of a MemoBar Delphi component.
MemoBar can be attached to a T(Custom)Memo component to
provide additional info/functionality for a Memo component.
TMemoBar displays: current line and column number,
position of the textual cursor and the total number of characters.
MemoBar adds "GoTo Line" and "overwrite" features to a Memo control.

~Zarko Gajic
http://delphi.about.com

}

interface

uses
  WinAPI.Windows, WinAPI.Messages,

  System.SysUtils,  System.Classes, System.Math,

  Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.Graphics
  AdvMemo;

type
  TInsertState = (isInsert, isOverwrite);
  TSavedState = (fSaved, fModified);

  TAdvMemoBar = class(TPanel)
  private
    fCharsPanel           : TPanel;
    fInsertStatePanel     : TPanel;
    fLocationPanel        : TPanel;
    fSavedStatusPanel     : TPanel;
    fSavedFileNamePanel   : TPanel;
    fMemo                 : TAdvMemo;
    fInsertState          : TInsertState;
    fOverwriteCaretHeight : integer;
    fOverwriteCaretWidth  : integer;
    fTextchanged          : TSavedState;
    fFileName             : string;

    OldMemoWindowProc     : TWndMethod;
    FirstProc             : Boolean;

    function GetLocationPanel     : TPanel;
    function GetInsertStatePanel  : TPanel;
    function GetCharsPanel        : TPanel;
    function GetSavedStatusPanel  : TPanel;
    function GetSavedFileNamePanel: TPanel;
    //function TMemoBar.GetCharsPanel: TPanel;


    procedure ToogleInsert;

    procedure SetSavedState(const Value: TSavedState);
    procedure SetMemo(const Value      : TAdvMemo);
    procedure SetFileName(const Value  : string);


    procedure MemoWindowProc(var Message : TMessage);

    procedure UpdateChars;
    procedure UpdateInsert;
    procedure UpdateLocation;
    procedure UpdateSavedStatus;
    procedure UpdateFileName;

    procedure ApplyOverwriteCaret;
    procedure LocationClick(Sender : TObject);
    procedure FileNameClick(Sender: TObject);

    property LocationPanel : TPanel read GetLocationPanel;
    property InsertStatePanel : TPanel read GetInsertStatePanel;
    property CharsPanel : TPanel read GetCharsPanel;
    property SavedStatusPanel : TPanel read GetSavedStatusPanel;
    property SavedFileNamePanel: TPanel read GetSavedFileNamePanel;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  published

    property Memo : TAdvMemo read fMemo write SetMemo;
    property InsertState : TInsertState read fInsertState write fInsertState default isInsert;
    property OverwriteCaretWidth : integer read fOverwriteCaretWidth write fOverwriteCaretWidth;
    property OverwriteCaretHeight : integer read fOverwriteCaretHeight write fOverwriteCaretHeight;
    property ChangeChangedStatus: TSavedState read fTextchanged write SetSavedState;
    property SavedFileName: string read fFileName write SetFileName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gary"s Stuff', [TAdvMemoBar]);
end;

{ TMemoBar }


procedure TAdvMemoBar.ApplyOverwriteCaret;
begin
  if Memo = nil then Exit;
  DestroyCaret;
  CreateCaret(Memo.handle,0,OverwriteCaretWidth,OverwriteCaretHeight) ;
  ShowCaret(Memo.Handle);
end; (* ApplyOverwriteCaret *)


constructor TAdvMemoBar.Create(AOwner: TComponent);
begin
  inherited;

  Caption := '';
  Height := 21;
  Width := 233;

  fInsertState := isInsert; //default for TCustomMemo
  fOverwriteCaretWidth := 12;
  fOverwriteCaretHeight := 16;
  fTextchanged := fSaved;
  fFileName := '                    Editor Not Saved To File       ';
end; (* Create *)


procedure TAdvMemoBar.CreateWnd;
begin
  inherited;

  SavedFileNamePanel.Refresh;
  InsertStatePanel.Refresh;
  CharsPanel.Refresh;
  SavedStatusPanel.Refresh;
  LocationPanel.Refresh;
end; (* CreateWnd *)


destructor TAdvMemoBar.Destroy;

  function SameMethod(Proc1, Proc2: TWndMethod): Boolean;
  begin
    Result := (TMethod(Proc1).Code = TMethod(Proc2).Code) and (TMethod(Proc1).Data = TMethod(Proc2).Data);
  end;

begin
  if Assigned(Memo) then
  begin
    if SameMethod(memo.WindowProc, MemoWindowProc) then
    begin
      Memo.WindowProc := OldMemoWindowProc;
    end;
  end;
   inherited; //Destroy

end;


(* Destroy *)


function TAdvMemoBar.GetCharsPanel: TPanel;
var
  txt : string;
begin
  if fCharsPanel = nil then begin
    fCharsPanel := TPanel.Create(self);
    fCharsPanel.Parent := Self;
    fCharsPanel.Align := alLeft;
    fCharsPanel.BevelOuter := bvLowered;

    txt := ' 1000/1000 '; //set min width
    fCharsPanel.Width := Self.Canvas.TextWidth(txt);

    fCharsPanel.Visible := true;
  end;

  result := fCharsPanel;
end; (* GetCharsPanel *)


function TAdvMemoBar.GetInsertStatePanel: TPanel;
var
  txt : string;
begin
  if fInsertStatePanel = nil then begin
    fInsertStatePanel := TPanel.Create(self);
    fInsertStatePanel.Parent := Self;
    fInsertStatePanel.Align := alLeft;
    fInsertStatePanel.BevelOuter := bvLowered;
    fInsertStatePanel.ParentBackground := False;
    fInsertStatePanel.Color := clYellow;

    txt := '  OVERWRITE  ';
    fInsertStatePanel.Width := Self.Canvas.TextWidth(txt);

    fInsertStatePanel.Visible := true;
  end;

  result := fInsertStatePanel;
end; (* GetInsertStatePanel *)


function TAdvMemoBar.GetLocationPanel: TPanel;
var
  txt : string;
begin
  if fLocationPanel = nil then begin
    fLocationPanel := TPanel.Create(self);
    fLocationPanel.Parent := Self;
    fLocationPanel.Align := alLeft;
    fLocationPanel.BevelOuter := bvLowered;
    fLocationPanel.Cursor := crHandPoint;
    fLocationPanel.ShowHint := true;
    fLocationPanel.Hint := 'Click: GoTO Line ...';
    //fLocationPanel.ParentBackground := False;
    fLocationPanel.Color := clYellow;

    txt := Format('Ln: %d Col: %d',[1000,1000]);
    fLocationPanel.Width := Self.Canvas.TextWidth(txt);

    fLocationPanel.Visible := true;

  end;
  result := fLocationPanel;
end;


function TAdvMemoBar.GetSavedFileNamePanel: TPanel;
var
  txt : string;
begin
  if fSavedFileNamePanel = nil then
  begin
    fSavedFileNamePanel := TPanel.Create(self);
    fSavedFileNamePanel.Parent := Self;
    fSavedFileNamePanel.Align := alLeft;
    fSavedFileNamePanel.BevelOuter := bvLowered;
    fSavedFileNamePanel.Cursor := crHandPoint;
    fSavedFileNamePanel.ShowHint := true;
    fSavedFileNamePanel.Color := clYellow;
    fSavedFileNamePanel.Hint := 'Saved File Name';
    //fSavedFileNamePanel.ParentBackground := False;

    txt := '                     Editor Not Saved to File       ';

    fSavedFileNamePanel.Width := Self.Canvas.TextWidth(txt);

    fSavedFileNamePanel.Visible := true;

  end;
  result := fSavedFileNamePanel;
end;



(* GetLocationPanel *)

function TAdvMemoBar.GetSavedStatusPanel: TPanel;
var
  txt : string;
begin
  if fSavedStatusPanel = nil then begin
    fSavedStatusPanel := TPanel.Create(self);
    fSavedStatusPanel.Parent := Self;
    fSavedStatusPanel.Align := alLeft;
    fSavedStatusPanel.BevelOuter := bvLowered;
    //fSavedStatusPanel.Cursor := crHandPoint;
    fSavedStatusPanel.ShowHint := true;
    fSavedStatusPanel.Hint := 'Is editor saved or Mofified';
    fSavedStatusPanel.ParentBackground := False;

    txt := '   Saved   ';

    fSavedStatusPanel.Width := Self.Canvas.TextWidth(txt);

    fSavedStatusPanel.Visible := true;

  end;
  result := fSavedStatusPanel;
end;

(* GetLocationPanel *)


procedure TAdvMemoBar.FileNameClick(Sender: TObject);
begin
  ShowMessage(fFileName);
end;




procedure TAdvMemoBar.LocationClick(Sender: TObject);
var
  newLineS : string;
  newLine  : integer;
  minLines : integer;
  maxLines : integer;
  prompt   : string;
  AcceptInput: Boolean;
  CanceInput: Boolean;



  function isIntAndBetween(s: string; MinInt, MaxInt: integer; out NewLine: integer): Boolean;
  var
    i: Integer;
  begin
    try
      i := StrToInt(s);
      NewLine := i;
      //ShowMessage(IntToStr(MinInt)+'   '+IntToSTr(i)+'  '+IntToSTr(MaxInt));
      Result := InRange(i, MinInt, MaxInt);
    except
      result := False;
    end;
  end;

begin
  newLine := Perform(EM_LINEFROMCHAR, Memo.SelStart, 0) ;
  minLines := 1;
  maxLines := Memo.Lines.Count;
  prompt := Format('Line number (1 - %d)',[maxLines]);

  AcceptInput := False;
  repeat
    CanceInput := not InputQuery('Go To line', prompt, newLineS);
    AcceptInput := isIntAndBetween(newLineS, 1, maxLines, NewLine);

  until CanceInput or  AcceptInput;



  if AcceptInput then
  begin
    Memo.SelStart := Memo.Perform(EM_LINEINDEX, newLine-1, 0);
    Memo.Perform(EM_SCROLLCARET,0,0);
  end;
end; (* LocationClick *)


procedure TAdvMemoBar.MemoWindowProc(var Message: TMessage);
begin

  if Message.Msg = WM_LBUTTONUP then begin
    UpdateLocation;
    UpdateChars;
    //ShowMessage('WM_LBUTTONUP');
  end;

  if Message.Msg = WM_CHAR then begin
    if ((Memo.SelLength = 0) and (fInsertState = isOverwrite)) then
    begin
      Memo.SelLength := 1;
    end;
  end;

  OldMemoWindowProc(Message);

  if Message.Msg = WM_KEYUP then begin
    UpdateChars;
    UpdateLocation;
    if Message.WParam = Ord(VK_INSERT) then begin
      //if FirstProc then begin
        ToogleInsert;
        //FirstProc := false;
      //end
      //else
      //  FirstProc := True;
    end;
  end;

  if Message.Msg = WM_KILLFOCUS then begin
    DestroyCaret;
  end;

  if Message.Msg = WM_SETFOCUS then begin
    if fInsertState = isOverwrite then
      ApplyOverwriteCaret;
  end;

end;


(*
procedure TMemoBar.MemoWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_LBUTTONUP then
  begin
    UpdateLocation;
  end;

  if Message.Msg = WM_CHAR then
  begin
    if ((Memo.SelLength = 0) and (InsertState = isOverwrite)) then
    begin
      //ShowMessage('is overwrite');
      Memo.SelLength := 1;
    end;
  end;

  OldMemoWindowProc(Message);

  if Message.Msg = WM_KEYUP then
  begin
    UpdateChars;
    UpdateLocation;
    //Message.
    if Message.WParam = Ord(VK_INSERT) then
    begin
      //ShowMessage('is VK_INSERT');
      ToogleInsert;
    end;
  end;

  if Message.Msg = WM_KILLFOCUS then
  begin
    DestroyCaret;
  end;

  if Message.Msg = WM_SETFOCUS then
  begin
    if InsertState = isOverwrite then begin
      //ShowMessage('is overwrite');
      ApplyOverwriteCaret;
    end;
  end;
end; {MemoWindowProc }
*)



procedure TAdvMemoBar.SetMemo(const Value: TADVMemo);
begin
// SendMessage(RichEdit1.Handle,WM_KEYDOWN,VK_INSERT,0);
  if fMemo <> Value then
  begin
    if Assigned(fMemo) then
      fMemo.WindowProc := OldMemoWindowProc;

    if Value <> nil then
    begin
      fMemo := Value;
      OldMemoWindowProc := fMemo.WindowProc;
      fMemo.WindowProc := MemoWindowProc;
    end;

    LocationPanel.OnClick      := LocationClick;
    SavedFileNamePanel.OnClick := FileNameClick;

  end;

  if Value = nil then
  begin
    fMemo := nil;
    LocationPanel.OnClick := nil;
    SavedFileNamePanel.OnClick := nil;
  end;

  UpdateFileName;
  UpdateInsert;
  UpdateChars;
  UpdateSavedStatus;
  UpdateLocation;
end;


procedure TAdvMemoBar.SetSavedState(const Value: TSavedState);
begin
  fTextchanged := Value;
  UpdateSavedStatus;
end;


procedure TAdvMemoBar.SetFileName(const Value: string);
begin
  fFileName := Value;
  UpdateFileName;
end;


(* SetMemo *)

procedure TAdvMemoBar.ToogleInsert;
begin
  if not Memo.Focused then
    Exit;
  if InsertState = isInsert then
  begin
    InsertState := isOverwrite;
    //ShowMessage('isOverwrite');;
    ApplyOverwriteCaret;
  end
  else
  begin
    InsertState := isInsert;
    //ShowMessage('isInsert');;

    //trick to recreate default caret
    Memo.Perform(WM_KILLFOCUS,Memo.Handle,0);
    Memo.Perform(WM_SETFOCUS,Memo.Handle,0);
  end;

  UpdateInsert;
end; (* ToogleInsert *)


procedure TAdvMemoBar.UpdateFileName;
var
  chars : string;
begin
  if NOT Assigned(Memo) then begin
    chars := 'C:\None';
  end
  else begin
    SavedFileNamePanel.Caption := fFileName;
  end;

end;


procedure TAdvMemoBar.UpdateChars;
var
  chars : string;
begin
  if NOT Assigned(Memo) then begin
    chars := '0/0';
  end
  else begin
    //chars := Format('%d/%d',[Memo.SelStart,Length(Memo.Text)]);
    chars := Format('%d/%d',[Memo.SelStart,Length(Memo.Lines.text)]);
  end;

  CharsPanel.Caption := chars;
end; (* UpdateChars *)


procedure TAdvMemoBar.UpdateInsert;
var
  state : string;
  theColor: TColor;
begin
  if NOT Assigned(Memo) then begin
    state := '?';
  end
  else begin
    if InsertState = isInsert then  begin
      state := 'INSERT';
      theColor := clLime;
    end
    else begin
      state := 'OVERWRITE';
      theColor := clYellow;
    end;
  end;
  InsertStatePanel.Color := theColor;
  InsertStatePanel.Caption := state;
end; (* UpdateInsert *)


procedure TAdvMemoBar.UpdateLocation;
var
  caretLocation : string;
  l,c : integer;
begin
  if NOT Assigned(Memo) then begin
    l := 0; c:= 0;
  end
  else begin
    with Memo do
    begin
      l := 1 + Perform(EM_LINEFROMCHAR, SelStart, 0) ;
      c := SelStart - Perform(EM_LINEINDEX, l - 1, 0);
    end;
  end;

  caretLocation := Format('Ln: %d Col: %d',[l,c]);
  LocationPanel.Caption := caretLocation;
end;


procedure TAdvMemoBar.UpdateSavedStatus;
begin
  if NOT Assigned(Memo) then begin
    SavedStatusPanel.Caption := '?';
  end
  else begin
    //Memo.OnChange
    case fTextchanged of
      fSaved: begin
        SavedStatusPanel.Caption := 'Saved';
        SavedStatusPanel.Color := clLime;
      end;
      fModified: begin
       SavedStatusPanel.Caption := 'Modified';
        SavedStatusPanel.Color := clYellow;
      end;
    end;
  end;
end;

(* UpdateLocation *)

end.
