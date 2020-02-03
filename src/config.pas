unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function getConfigPath: string;

implementation

function getConfigPath: string;
begin
  result:=GetAppConfigDir(False)+'idedit.json'
end;

end.

