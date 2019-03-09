unit form_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ATFontDialog;

type
  { TFormDemo }

  TFormDemo = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    btnATFontDialog: TButton;
    chkFontShowApply: TCheckBox;
    chkFontShowColor: TCheckBox;
    chkFontShowStylesEx: TCheckBox;
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
    LabelFont: TLabel;
    procedure btnATFontDialogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure DoClickApply(Sender: TObject);
    procedure UpdateStatus;
  public

  end;

var
  FormDemo: TFormDemo;

implementation

{$R *.lfm}

{ TFormDemo }

procedure TFormDemo.btnATFontDialogClick(Sender: TObject);
var
  f: TfrmFont;
begin
  f:= TfrmFont.Create(nil);
  f.OptFont.Assign(LabelFont.Font);
  f.OnClickApply:= @DoClickApply;

  f.OptShowNames:= chkFontShowNames.Checked;
  f.OptShowColor:= chkFontShowColor.Checked;
  f.OptShowStyles:= chkFontShowStyles.Checked;
  f.OptShowStylesEx:= chkFontShowStylesEx.Checked;
  f.OptShowSizes:= chkFontShowSizes.Checked;
  f.OptShowPreview:= chkFontShowPreview.Checked;
  f.OptShowApply:= chkFontShowApply.Checked;
  f.OptSizeLimited:= chkFontLimitSize.Checked;
  f.OptSizeMin:= edFontSizeMin.Value;
  f.OptSizeMax:= edFontSizeMax.Value;
  f.OptSelectName:= chkSelectName.Checked;
  f.OptSelectStyle:= chkSelectStyle.Checked;
  f.OptSelectSize:= chkSelectSize.Checked;

  if f.ShowModal=mrOk then
  begin
    LabelFont.Font.Assign(f.OptFont);
    UpdateStatus;
  end;

  f.Free;
end;

procedure TFormDemo.UpdateStatus;
begin
  LabelFont.Caption:= 'Font: '+LabelFont.Font.Name+', '+IntToStr(LabelFont.Font.Size);
end;

procedure TFormDemo.FormCreate(Sender: TObject);
begin
  LabelFont.Font.Name:= 'Arial';
  LabelFont.Font.Style:= [fsItalic];
  LabelFont.Font.Size:= 12;
  LabelFont.Font.Color:= clNavy;

  UpdateStatus;
end;

procedure TFormDemo.DoClickApply(Sender: TObject);
begin
  ShowMessage('Apply clicked');
end;

end.

