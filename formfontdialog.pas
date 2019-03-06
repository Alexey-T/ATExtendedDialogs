unit FormFontDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, Math;

type

  { TfrmFont }

  TfrmFont = class(TForm)
    BtnPanel: TButtonPanel;
    EditSize: TEdit;
    LabelFamily: TLabel;
    LabelStyle: TLabel;
    LabelSize: TLabel;
    LabelPreview: TLabel;
    ListboxStyle: TListBox;
    ListboxFamily: TListBox;
    ListboxSize: TListBox;
    PanelSize: TPanel;
    PanelStyle: TPanel;
    PanelFamily: TPanel;
    PanelMain: TPanel;
    PanelPreviewText: TPanel;
    PanelPreview: TPanel;
    procedure EditSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListboxFamilyClick(Sender: TObject);
    procedure ListboxSizeClick(Sender: TObject);
    procedure ListboxStyleClick(Sender: TObject);
  private
    function GetFont: TFont;
    procedure UpdatePreview;
  public
    property Font: TFont read GetFont;
  end;

var
  frmFont: TfrmFont;

implementation

{$R *.lfm}

{ TfrmFont }

procedure TfrmFont.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Font.Name:= 'Courier';
  Font.Style:= [];
  Font.Size:= 9;

  ListboxFamily.Items.Assign(Screen.Fonts);

  with ListboxStyle do
  begin
    Items.Clear;
    Items.Add('Regular');
    Items.Add('Italic');
    Items.Add('Bold');
    Items.Add('Bold Italic');
  end;

  with ListboxSize do
  begin
    Items.Clear;
    for i:= 6 to 18 do
      Items.Add(IntToStr(i));
    Items.Add('20');
    Items.Add('22');
    Items.Add('24');
    Items.Add('26');
    Items.Add('28');
    Items.Add('32');
    Items.Add('36');
    Items.Add('40');
    Items.Add('48');
    Items.Add('56');
    Items.Add('64');
    Items.Add('72');
  end;
end;

procedure TfrmFont.EditSizeChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfrmFont.FormShow(Sender: TObject);
begin
  ListboxFamily.ItemIndex:= ListboxFamily.Items.IndexOf(Font.Name);

  Font.Style:= Font.Style-[fsUnderline, fsStrikeOut];
  if Font.Style=[fsItalic] then
    ListboxStyle.ItemIndex:= 1
  else
  if Font.Style=[fsBold] then
    ListboxStyle.ItemIndex:= 2
  else
  if Font.Style=[fsBold, fsItalic] then
    ListboxStyle.ItemIndex:= 3
  else
    ListboxStyle.ItemIndex:= 0;

  EditSize.Text:= IntToStr(Font.Size);
  ListboxSize.ItemIndex:= ListboxSize.Items.IndexOf(EditSize.Text);

  UpdatePreview;
end;

procedure TfrmFont.ListboxFamilyClick(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfrmFont.ListboxSizeClick(Sender: TObject);
begin
  if ListboxSize.ItemIndex>=0 then
  begin
    EditSize.Text:= ListboxSize.Items[ListboxSize.ItemIndex];
  end;
end;

procedure TfrmFont.ListboxStyleClick(Sender: TObject);
begin
  UpdatePreview;
end;

function TfrmFont.GetFont: TFont;
begin
  Result:= PanelPreviewText.Font;
end;

procedure TfrmFont.UpdatePreview;
const
  cMinFontSize = 6;
  cMaxFontSize = 72;
var
  N: integer;
begin
  if ListboxFamily.ItemIndex>=0 then
    Font.Name:= ListboxFamily.Items[ListboxFamily.ItemIndex];

  if ListboxStyle.ItemIndex>=0 then
    case ListboxStyle.ItemIndex of
      0: Font.Style:= [];
      1: Font.Style:= [fsItalic];
      2: Font.Style:= [fsBold];
      3: Font.Style:= [fsBold, fsItalic];
    end;

  N:= StrToIntDef(EditSize.Text, -1);
  if N>0 then
  begin
    N:= Max(cMinFontSize, Min(cMaxFontSize, N));
    Font.Size:= N;
  end;
end;


end.

