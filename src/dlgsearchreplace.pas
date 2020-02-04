unit dlgSearchReplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnRegMenu: TSpeedButton;
    comboReplaceText: TComboBox;
    comboSearchText: TComboBox;
    cbReplace: TCheckBox;
    Label1: TLabel;
    procedure btnOKClick(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.btnOKClick(Sender: TObject);
begin

end;

end.

