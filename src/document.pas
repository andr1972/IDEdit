unit document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs, NicePages, ATSynEdit;

type

  { TDocument }

  TDocument = class(TInterfacedObject, IDocument)
  private
    fFileName: string;
    fSheet: TNiceSheet;
    fAtSynEdit: TAtSynEdit;
    fFactory: IDocumentFactory;
//    fHiSyntax: IHiSyntax;
    fUntitledManager: IUntitledManager;
    fUntitledNumber: integer;
    fFileTime: TDateTime;
    fFileSize: int64;
    fRemoved: boolean;
    fChangedTime: boolean;
    fChangedSize: boolean;
    procedure ReleaseUnusedDir(const NewName: string);
    function DoSaveFile: boolean;
    procedure GetFileTimeSize(bInit: boolean);
  public
    constructor Create(AFactory: IDocumentFactory; ASheet: TNiceSheet; ASynEdit: TAtSynEdit);
    destructor Destroy; override;
    function GetPath: string;
    function GetTitle: string;
    procedure OpenFile(AFileName: string);
    class function CompareFileNames(const S1: string; const S2: string): integer;
    function ComparePathWith(AOtherPath: string): integer;
    procedure Activate;
    function TryClose(): Boolean;
    function Save: boolean;
    function SaveAs(AFileName: string): boolean;
    function GetWordWrap: boolean;
    procedure SetWordWrap(AValue: boolean);
    procedure Revert;
    procedure CheckWithDisk;
    function ChangedOutside: boolean;
  end;


implementation
uses
  LCLType, Controls, Dialogs, dateutils;

{ TDocument }

constructor TDocument.Create(AFactory: IDocumentFactory; ASheet: TNiceSheet;
  ASynEdit: TAtSynEdit);
var
  CmdIndex: integer;
begin
  fSheet:=ASheet;
  fSheet.IntfPtr := self;
  fAtSynEdit:=ASynEdit;
  fFactory:=AFactory;
  //fHiSyntax:=fFactory.GetHiSyntax;
  fUntitledManager:=fFactory.GetUntitledManager;
  fUntitledNumber:=0;
  fAtSynEdit.OptRulerVisible:=false;
  fAtSynEdit.OptUnprintedVisible:=false;
end;

destructor TDocument.Destroy;
begin
  fUntitledManager.ReleaseNumber(fUntitledNumber);
  inherited Destroy;
end;

function TDocument.GetPath: string;
begin
  Result := fFileName;
end;

function TDocument.GetTitle: string;
begin
  if fFileName <> '' then
    Result := ExtractFileName(fFileName)
  else
  begin
    if fUntitledNumber=0 then
      fUntitledNumber:=fUntitledManager.GetNewNumber();
    Result := 'Untitled' + IntToStr(fUntitledNumber)
  end;
end;

procedure TDocument.OpenFile(AFileName: string);
begin
  fFileName := AFileName;
  fSheet.Caption := GetTitle;
  if AFileName<>'' then
  begin
    fAtSynEdit.LoadFromFile(AFileName);
    fAtSynEdit.OptWrapMode:=cWrapOn;
//    fAtSynEdit.Highlighter := fHiSyntax.GetHighlighterByFileName(AFileName);
  end;
  GetFileTimeSize(true);
end;

class function TDocument.CompareFileNames(const S1: string; const S2: string): integer;
begin
  if FileNameCaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

function TDocument.ComparePathWith(AOtherPath: string): integer;
begin
  Result := CompareFileNames(fFileName, AOtherPath);
end;

procedure TDocument.Activate;
begin
  fSheet.MakeActive();
end;

function TDocument.TryClose(): Boolean;
begin
  fSheet.TryClose();
end;

function TDocument.Save: boolean;
begin
  if fFileName='' then
  begin
    result:=false;
    raise Exception.Create('This should be impossible');
  end
  else
    result:=SaveAs(fFileName);
end;


{ If current directory is not this editor binary
  and none opened file is in current directory then
  change current directory to editor binary dir to release current dir }
procedure TDocument.ReleaseUnusedDir(const NewName: string);

  function FileIsInDir(const Filename, Directory: string): boolean;
  begin
    Result := CompareFileNames(ExtractFileDir(Filename),
      IncludeTrailingBackslash(Directory)) = 0;
  end;

var
  otherDocument: TDocument;
  i: integer;
begin
  if FileIsInDir(ParamStr(0), GetCurrentDir) then
    exit;
  if FileIsInDir(NewName, GetCurrentDir) then
    exit;
  for i := 0 to fFactory.GetDocumentCount - 1 do
  begin
    otherDocument := fFactory.GetDocument(i) as TDocument;
    if otherDocument <> self then
      if FileIsInDir(otherDocument.GetPath, GetCurrentDir) then
        exit;
  end;
  SetCurrentDir(ExtractFileDir(ParamStr(0)));
end;

function TDocument.DoSaveFile: boolean;
begin
  fAtSynEdit.SaveToFile(fFileName);
end;

procedure TDocument.GetFileTimeSize(bInit: boolean);
var
  sr: TSearchRec;
  SystemTime: TSystemTime;
  newFileTime: TDateTime;
  newFileSize: int64;
begin
  fChangedTime:=false;
  fChangedSize:=false;
  if fFileName='' then
  begin
    fFileTime:=TDateTime(0);
    fFileSize:=0;
    fRemoved:=false;
  end else
  if FindFirst(fFileName, faAnyFile, sr) = 0 then
  begin
{$ifdef windows}
    FileTimeToSystemTime(sr.FindData.ftLastWriteTime, SystemTime);
    newFileTime := SystemTimeToDateTime(SystemTime);
{$else}
    newFileTime := LocalTimeToUniversal(FileDateToDateTime(sr.Time));
{$endif}
    newFileSize:=sr.Size;
    fRemoved:=false;
    if not bInit then
    begin
      if newFileTime<>fFileTime then fChangedTime:=true;
      if newFileSize>fFileSize then fChangedSize:=true;
    end;
    fFileTime:=newFileTime;
    fFileSize:=newFileSize;
  end else
  begin
    fFileTime:=TDateTime(0);
    fFileSize:=0;
    fRemoved:=true;
  end;
end;

function TDocument.SaveAs(AFileName: string): boolean;
begin
  result:=false;
  if AFileName<>fFileName then
  begin
    if FileExists(AFileName) and (MessageDlg('File ' + AFileName +
      ' already exists, overwrite?', mtWarning, [mbYes, mbCancel], 0) <> mrYes) then
    begin
      Result := False;
      exit;
    end;
    ReleaseUnusedDir(fFileName);
    fFileName:=AFileName;
    fSheet.Caption := GetTitle;
  end;
  result := DoSaveFile;
end;

function TDocument.GetWordWrap: boolean;
begin
  result:=fAtSynEdit.OptWrapMode<>cWrapOff;
end;

procedure TDocument.SetWordWrap(AValue: boolean);
var
  opt: TATSynWrapMode;
begin
  if AValue then
    opt:=cWrapOn
  else
    opt:=cWrapOff;
  fAtSynEdit.OptWrapMode:=opt;
end;

procedure TDocument.Revert;
begin
  OpenFile(fFileName);
end;

procedure TDocument.CheckWithDisk;
begin
  GetFileTimeSize(false);
end;

function TDocument.ChangedOutside: boolean;
begin
  result:=fChangedSize or fChangedTime;
end;

end.

