unit intfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, NicePages;

type

  { IDocument }

  IDocument = interface ['{33F35F97-56E0-44FD-A0AA-5EA0CAD1C6B5}']
    function GetPath: string;
    function GetTitle: string;
    procedure OpenFile(AFileName: string);
    function ComparePathWith(AOtherPath: string): integer;
    procedure Activate;
    function GetWordWrap: boolean;
    procedure SetWordWrap(AValue: boolean);
    function TryClose(): Boolean;
    function Save: boolean;
    function SaveAs(AFileName: string): boolean;
    procedure Revert;
    procedure CheckWithDisk;
    function ChangedOutside: boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
    function GetModified: boolean;
    property WordWrap: boolean read GetWordWrap write SetWordWrap;
    function Consider: TConsiderEnum;
    procedure ActionsBeforeClose;
    procedure AskSaveChangesBeforeClosing(var CanClose: TCloseEnum);
    function AskSaveChangesBeforeReopen: TCloseEnum;
    procedure AfterActivation;
  end;

  IUntitledManager = interface ['{476DA6F2-AAED-4923-BBBA-6726E6F8914F}']
    function GetNewNumber: integer;
    procedure ReleaseNumber(ANumber: integer);
  end;

  (*IHiSyntax = interface ['{F0FF0ECB-A19B-41EC-9696-23FF28437F12}']
    function GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
    function GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
  end;*)

  IDocumentFactory = interface ['{9FC64814-516F-4344-9961-FC0D45A7F3CA}']
    function GetDocumentCount: integer;
    function GetDocument(Index: integer): IDocument;
    function CreateNew(AFileName: string): IDocument;
    function GetActive: IDocument;
    function GetUntitledManager: IUntitledManager;
    procedure TryCloseAll;
//    function GetHiSyntax: IHiSyntax;
  end;

implementation

end.

