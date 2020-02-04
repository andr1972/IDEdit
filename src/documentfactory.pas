unit documentfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs, NicePages;

type

  { TDocumentFactory }

  TDocumentFactory = class(TInterfacedObject, IDocumentFactory)
  private
    fNotebook: TNicePages;
    fUntitledManager: IUntitledManager;
//    fHiSyntax: IHiSyntax;
  public
    constructor Create(ANotebook: TNicePages);
    function GetDocumentCount: integer;
    function GetDocument(Index: integer): IDocument;
    function CreateNew(AFileName: string): IDocument;
    function GetActive: IDocument;
    function GetUntitledManager: IUntitledManager;
    procedure TryCloseAll;
//    function GetHiSyntax: IHiSyntax;
  end;

implementation
uses
  Controls, Graphics, AtSynEdit, Document, untitledmanager, hisyntax;

{ TDocumentFactory }

constructor TDocumentFactory.Create(ANotebook: TNicePages);
begin
  fNotebook := ANotebook;
  fUntitledManager := TUntitledManager.Create();
  //fHiSyntax := THiSyntax.Create();
end;

function TDocumentFactory.GetDocumentCount: integer;
begin
  Result := fNotebook.PageCount;
end;

function TDocumentFactory.GetDocument(Index: integer): IDocument;
begin
  Result := fNotebook.Page[Index].IntfPtr as IDocument;
end;

function TDocumentFactory.CreateNew(AFileName: string): IDocument;
var
  sheet: TNiceSheet;
  syn: TAtSynEdit;
begin
  sheet := FNotebook.AddTabSheet();
  syn := TAtSynEdit.Create(sheet);
  syn.Color := clWhite;
  syn.BorderStyle := bsNone;
  syn.Align := alClient;
  sheet.InsertControl(syn);
  sheet.FocusedControl := syn;
  Result := TDocument.Create(self, sheet, syn);
end;

function TDocumentFactory.GetActive: IDocument;
begin
  if FNotebook.ActiveSheet=nil then
     Result := nil
  else
     Result := FNotebook.ActiveSheet.IntfPtr as IDocument;
end;

function TDocumentFactory.GetUntitledManager: IUntitledManager;
begin
  Result := fUntitledManager;
end;

procedure TDocumentFactory.TryCloseAll;
begin
  fNotebook.TryCloseAll();
end;

{function TDocumentFactory.GetHiSyntax: IHiSyntax;
begin
  Result := fHiSyntax;
end;}

end.
