unit intfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
    property WordWrap: boolean read GetWordWrap write SetWordWrap;
  end;

  IUntitledManager = interface ['{476DA6F2-AAED-4923-BBBA-6726E6F8914F}']
    function GetNewNumber: integer;
    procedure ReleaseNumber(ANumber: integer);
  end;

{  IHiSyntax = interface ['{F0FF0ECB-A19B-41EC-9696-23FF28437F12}']
    function GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
    function GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
  end;}

  IDocumentFactory = interface ['{9FC64814-516F-4344-9961-FC0D45A7F3CA}']
    function GetDocumentCount: integer;
    function GetDocument(Index: integer): IDocument;
    function CreateNew(AFileName: string): IDocument;
    function GetActive: IDocument;
    function GetUntitledManager: IUntitledManager;
//    function GetHiSyntax: IHiSyntax;
  end;

implementation

end.

