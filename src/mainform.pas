unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, ATSynEdit, NicePages, intfs;

type

  { TForm1 }

  TForm1 = class(TForm)
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileClosePage: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileSaveAll: TAction;
    actRevert: TAction;
    actlFile: TActionList;
    imglFile16: TImageList;
    imglTb16: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miFileClosePage: TMenuItem;
    miFileOpen: TMenuItem;
    miFileNew: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure actFileClosePageExecute(Sender: TObject);
    procedure actFileClosePageUpdate(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    fNotebook: TNicePages;
    fDocumentFactory: IDocumentFactory;
    procedure DoOpenFile(AFileName: string);
  protected
    fWasActivated: boolean;
    function CmdLineOpenFiles: boolean;
    procedure AppActivate(Sender: TObject);
    procedure AppRestore(Sender: TObject);
  public
    procedure UniqInstOtherInstance(Sender: TObject;
      ParamCount: Integer; const Parameters: array of String);
  end;

var
  Form1: TForm1;

implementation
uses
  LCLType, documentfactory, config;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fNotebook:=TNicePages.Create(self);
  InsertControl(fNotebook);
  fNotebook.Align:=alClient;
  fNotebook.TabPosition:=tpBottom;
  //EdNotebook.OnBeforeCloseQuery:=@TabBeforeCloseQuery;
  //EdNotebook.OnCloseQuery:=@TabCloseQuery;
  //EdNotebook.OnClose:=@TabClose;
  //EdNotebook.OnDrawTab:=@TabDraw;
  fDocumentFactory:=TDocumentFactory.Create(fNotebook);
  Application.OnActivate := @AppActivate;
  Application.OnRestore:=@AppRestore;
  //ShowMessage(getConfigPath);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Number: integer;
begin
  if Key in [ord('0')..Ord('9')] then
  begin
    if Shift<>[ssAlt] then exit;
    if Key>ord('0') then Number := Key-ord('1')
    else Number := 9;
    if Number<fNotebook.PageCount then
      fNotebook.PageIndex:=Number;
    Key:=0;//to avoid beep
  end else if (Key=VK_TAB) and (ssCtrl in Shift) then
  begin
    if ssShift in Shift then
      fNotebook.ActivePrev()
    else
      fNotebook.ActiveNext();
  end;
end;
const licznik: integer = 0;
procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.DoOpenFile(AFileName: string);
var
  i: integer;
  LDocument: IDocument;
begin
  if AFileName <> '' then
  begin
    // activate the editor if already open
    for i := fDocumentFactory.GetDocumentCount-1 downto 0 do
    begin
      LDocument := fDocumentFactory.GetDocument(i);
      if LDocument.ComparePathWith(AFileName) = 0 then
      begin
        LDocument.Activate;
        exit;
      end;
    end;
  end;
  LDocument := fDocumentFactory.CreateNew(AFileName);
  LDocument.OpenFile(AFileName);
  LDocument.Activate;
end;

function TForm1.CmdLineOpenFiles: boolean;
var
  i: integer;
begin
  for i:=1 to ParamCount do
    DoOpenFile(ParamStr(i));
  result:=ParamCount>1;
end;

procedure TForm1.AppActivate(Sender: TObject);
begin
end;

procedure TForm1.AppRestore(Sender: TObject);
begin
  inc(licznik);
  Caption:=IntToStr(licznik);
end;

procedure TForm1.UniqInstOtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of String);
var
  i: integer;
begin
  Application.Minimize; // this is what really...
  Application.Restore;  // brings a form to top
  Application.BringToFront; // just in case, for orher platforms
  for i:=0 to ParamCount-1 do
    DoOpenFile(Parameters[i]);
end;

procedure TForm1.actFileOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    DoOpenFile(OpenDialog.FileName);
end;

procedure TForm1.actFileSaveAsExecute(Sender: TObject);
var
  LDocument: IDocument;
begin
  LDocument:=fDocumentFactory.GetActive;
  SaveDialog.FileName:=LDocument.GetPath;
  if SaveDialog.Execute then
    LDocument.SaveAs(SaveDialog.FileName);
end;

procedure TForm1.actFileSaveExecute(Sender: TObject);
var
  LDocument: IDocument;
begin
  LDocument:=fDocumentFactory.GetActive;
  if LDocument.GetPath='' then
  begin
    if SaveDialog.Execute then
      LDocument.SaveAs(SaveDialog.FileName);
  end
  else
  LDocument.Save();
end;

procedure TForm1.actFileSaveUpdate(Sender: TObject);
begin
  actFileSave.Enabled := fDocumentFactory.GetDocumentCount>0;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  inherited;
  try
    if not fWasActivated then CmdLineOpenFiles;
  finally
    fWasActivated := true
  end;
end;

procedure TForm1.actFileNewExecute(Sender: TObject);
begin
  DoOpenFile('');
end;

procedure TForm1.actFileClosePageUpdate(Sender: TObject);
begin
  actFileClosePage.Enabled := fDocumentFactory.GetDocumentCount>0;
end;

procedure TForm1.actFileClosePageExecute(Sender: TObject);
var
  LDocument: IDocument;
begin
  LDocument := fDocumentFactory.GetActive;
  if LDocument<>nil then
    LDocument.TryClose;
end;

end.

