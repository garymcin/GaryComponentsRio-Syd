unit AboutFindFile;

{
********************************************
Zarko Gajic
About.com Guide to Delphi Programming
http://delphi.about.com
email: delphi@aboutguide.com
free newsletter: http://delphi.about.com/library/blnewsletter.htm
forum: http://forums.about.com/ab-delphi/start/
********************************************
}

interface

uses
  WinAPI.Windows, WinAPI.Messages,

  System.SysUtils, System.Classes,

  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.FileCtrl;

type
  TFileAttrKind = (ffaReadOnly, ffaHidden, ffaSysFile, {ffaVolumeID,} ffaDirectory,
                   ffaArchive, ffaAnyFile);

  TFileAttr = set of TFileAttrKind;

  TGEMFindFile = class(TComponent)
  private
    s : TStringList;

    fSubFolder : boolean;
    fAttr: TFileAttr;
    fPath : string;
    fFileMask : string;

    procedure SetPath(Value: string);
    procedure FileSearch(const inPath : string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SearchForFiles: TStringList;
  published
    property FileAttr: TFileAttr read fAttr write fAttr;
    property InSubFolders : boolean read fSubFolder write fSubFolder;
    property Path : string read fPath write SetPath;
    property FileMask : string read fFileMask write fFileMask ;
  end;

//procedure Register;

implementation

//procedure Register;
//begin
//  RegisterComponents('Gary"s Stuff', [TGEMFindFile]);
//end;


constructor TGEMFindFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Path := IncludeTrailingBackslash(GetCurrentDir);
  FileMask := '*.*';
  FileAttr := [ffaAnyFile];
  s := TStringList.Create;
end;


destructor TGEMFindFile.Destroy;
begin
  s.Free;
  inherited Destroy;
end;


procedure TGEMFindFile.SetPath(Value: string);
begin
  if fPath <> Value then
  begin
    if Value <> '' then
      if System.SysUtils.DirectoryExists(Value) then
        fPath := IncludeTrailingBackslash(Value);
  end;
end;

function TGEMFindFile.SearchForFiles: TStringList;
begin
  s.Clear;
  try
    FileSearch(Path);
  finally
    Result := s;
  end;
end;

procedure TGEMFindFile.FileSearch(const InPath : string);
var Rec  : TSearchRec;
    Attr : integer;
begin
  Attr := 0;
  if ffaReadOnly in FileAttr then
    Attr := Attr + faReadOnly;
  if ffaHidden in FileAttr then
    Attr := Attr + faHidden;
  if ffaSysFile in FileAttr then
    Attr := Attr + faSysFile;
  //if ffaVolumeID in FileAttr then
  //  Attr := Attr + faVolumeID;
  if ffaDirectory in FileAttr then
    Attr := Attr + faDirectory;
  if ffaArchive in FileAttr then
    Attr := Attr + faArchive;
  if ffaAnyFile in FileAttr then
    Attr := Attr + faAnyFile;

  if System.SysUtils.FindFirst(inPath + FileMask, Attr, Rec) = 0 then
   try
     repeat
       s.Add(inPath + Rec.Name);
     until System.SysUtils.FindNext(Rec) <> 0;
   finally
     System.SysUtils.FindClose(Rec);
   end;

  If not InSubFolders then Exit;

  if System.SysUtils.FindFirst(inPath + '*.*', faDirectory, Rec) = 0 then
   try
     repeat
     if ((Rec.Attr and faDirectory) <> 0)  and (Rec.Name<>'.') and (Rec.Name<>'..') then
       begin
         FileSearch(IncludeTrailingBackslash(inPath + Rec.Name));
       end;
     until System.SysUtils.FindNext(Rec) <> 0;
   finally
     System.SysUtils.FindClose(Rec);
   end;
end;

end.



