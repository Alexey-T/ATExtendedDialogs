unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FormFontDialog;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnDlgOS: TButton;
    btnDlgAT: TButton;
    FontDialog1: TFontDialog;
    procedure btnDlgOSClick(Sender: TObject);
    procedure btnDlgATClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnDlgOSClick(Sender: TObject);
begin
  FontDialog1.Execute;
end;

procedure TForm1.btnDlgATClick(Sender: TObject);
var
  f: TfrmFont;
begin
  f:= TfrmFont.Create(nil);
  f.Font.Name:= 'Arial';
  f.Font.Style:= [fsItalic];
  f.Font.Size:= 12;
  f.ShowModal;
  f.Free;
end;

end.

