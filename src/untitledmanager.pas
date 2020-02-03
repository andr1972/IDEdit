unit untitledmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs;

type

  { TUntitledManager }

  TUntitledManager = class(TInterfacedObject, IUntitledManager)
  private
    fUntitledBits : TBits;
  public
    function GetNewNumber: integer;
    procedure ReleaseNumber(ANumber: integer);
    constructor Create;
    destructor Destroy;override;
  end;

implementation

{ TUntitledManager }

function TUntitledManager.GetNewNumber: integer;
begin
  Result := fUntitledBits.OpenBit;
  if Result = fUntitledBits.Size then
    fUntitledBits.Size := fUntitledBits.Size + 32;
  fUntitledBits[Result] := True;
end;

procedure TUntitledManager.ReleaseNumber(ANumber: integer);
begin
  if (ANumber > 0) and (ANumber < fUntitledBits.Size) then
    fUntitledBits[ANumber] := False;
end;

constructor TUntitledManager.Create;
begin
  fUntitledBits := TBits.Create;
  fUntitledBits.Size := 32;
  fUntitledBits[0] := true;
end;

destructor TUntitledManager.Destroy;
begin
  fUntitledBits.Free;
end;

end.

