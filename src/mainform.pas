unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, ATSynEdit, NicePages, documentfactory;

type

  { TForm1 }

  TForm1 = class(TForm)
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileClosePage: TAction;
    actlFile: TActionList;
    ATSynEdit1: TATSynEdit;
    imglFile16: TImageList;
    imglTb16: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
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
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    fNotebook: TNicePages;
    fDocumentFactory: TDocumentFactory;
    procedure DoOpenFile(AFileName: string);
  public
  end;

var
  Form1: TForm1;

implementation
uses
  intfs;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fNotebook:=TNicePages.Create(self);
  InsertControl(fNotebook);
  fNotebook.Align:=alClient;
  //EdNotebook.OnBeforeCloseQuery:=@TabBeforeCloseQuery;
  //EdNotebook.OnCloseQuery:=@TabCloseQuery;
  //EdNotebook.OnClose:=@TabClose;
  //EdNotebook.OnDrawTab:=@TabDraw;
  fDocumentFactory:=TDocumentFactory.Create(fNotebook);
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

procedure TForm1.actFileOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    DoOpenFile(OpenDialog.FileName);
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

