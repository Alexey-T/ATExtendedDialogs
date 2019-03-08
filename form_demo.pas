unit form_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ATFontDialog;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnATFontDialog: TButton;
    chkSelectName: TCheckBox;
    chkSelectStyle: TCheckBox;
    chkSelectSize: TCheckBox;
    chkFontShowNames: TCheckBox;
    chkFontShowStyles: TCheckBox;
    chkFontShowSizes: TCheckBox;
    chkFontShowPreview: TCheckBox;
    chkFontLimitSize: TCheckBox;
    FontDialog1: TFontDialog;
    edFontSizeMin: TSpinEdit;
    edFontSizeMax: TSpinEdit;
    procedure btnATFontDialogClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnATFontDialogClick(Sender: TObject);
var
  f: TfrmFont;
begin
  f:= TfrmFont.Create(nil);

  f.OptFont.Name:= 'Arial';
  f.OptFont.Style:= [fsItalic];
  f.OptFont.Size:= 12;

  f.OptShowNames:= chkFontShowNames.Checked;
  f.OptShowStyles:= chkFontShowStyles.Checked;
  f.OptShowSizes:= chkFontShowSizes.Checked;
  f.OptShowPreview:= chkFontShowPreview.Checked;
  f.OptSizeLimited:= chkFontLimitSize.Checked;
  f.OptSizeMin:= edFontSizeMin.Value;
  f.OptSizeMax:= edFontSizeMax.Value;
  f.OptSelectName:= chkSelectName.Checked;
  f.OptSelectStyle:= chkSelectStyle.Checked;
  f.OptSelectSize:= chkSelectSize.Checked;

  f.ShowModal;
  f.Free;
end;

end.

