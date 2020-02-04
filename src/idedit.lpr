program idedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  UniqueInstance, SysUtils, Forms, mainform, intfs, document, documentfactory,
  untitledmanager, hisyntax, config, dlgSearchReplace;

{$R *.res}

var
  UniqInst: TUniqueInstance;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  UniqInst:=TUniqueInstance.Create(nil);
  if FileNameCaseSensitive then
    UniqInst.Identifier:=ParamStr(0)
  else
    UniqInst.Identifier:=LowerCase(ParamStr(0));
  UniqInst.Identifier:=StringReplace(UniqInst.Identifier,'/','_',[rfReplaceAll]);
  UniqInst.Enabled:=true;
  UniqInst.ServerRunning;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  UniqInst.OnOtherInstance:=@Form1.UniqInstOtherInstance;
  UniqInst.Listen;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

