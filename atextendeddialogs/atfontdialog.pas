(*
This source code is dual licensed: MPL 2.0 and LGPL.
Copyright (c) Alexey Torgashin
*)

unit ATFontDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, ColorBox, Math;

type

  { TfrmFont }

  TfrmFont = class(TForm)
    BtnPanel: TButtonPanel;
    chkCrossed: TCheckBox;
    chkUnderline: TCheckBox;
    Colorbox: TColorBox;
    EditSize: TEdit;
    LabelFamily: TLabel;
    LabelStyle: TLabel;
    LabelSize: TLabel;
    LabelPreview: TLabel;
    ListboxStyle: TListBox;
    ListboxFamily: TListBox;
    ListboxSize: TListBox;
    PanelEffects: TPanel;
    PanelSize: TPanel;
    PanelStyle: TPanel;
    PanelFamily: TPanel;
    PanelMain: TPanel;
    PanelPreviewText: TPanel;
    PanelPreview: TPanel;
    procedure chkCrossedChange(Sender: TObject);
    procedure chkUnderlineChange(Sender: TObject);
    procedure ColorboxChange(Sender: TObject);
    procedure EditSizeKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListboxFamilyClick(Sender: TObject);
    procedure ListboxSizeClick(Sender: TObject);
    procedure ListboxStyleClick(Sender: TObject);
  private
    FPreviewInitialHeight: integer;
    FOptShowNames: boolean;
    FOptShowColor: boolean;
    FOptShowStyles: boolean;
    FOptShowStylesEx: boolean;
    FOptShowSizes: boolean;
    FOptShowPreview: boolean;
    FOptShowApply: boolean;
    FOptSelectName: boolean;
    FOptSelectStyle: boolean;
    FOptSelectSize: boolean;
    FOptSelectColor: boolean;
    FOptSizeLimited: boolean;
    FOptSizeMin: integer;
    FOptSizeMax: integer;
    FOnClickApply: TNotifyEvent;
    function GetOptFont: TFont;
    function GetPreviewText: string;
    procedure SetPreviewText(const AValue: string);
    procedure UpdateLayout;
    procedure UpdatePreview;
  public
    property OptFont: TFont read GetOptFont;
    property OptPreviewText: string read GetPreviewText write SetPreviewText;
    property OptSizeMin: integer read FOptSizeMin write FOptSizeMin;
    property OptSizeMax: integer read FOptSizeMax write FOptSizeMax;
    property OptSizeLimited: boolean read FOptSizeLimited write FOptSizeLimited;
    property OptShowNames: boolean read FOptShowNames write FOptShowNames;
    property OptShowColor: boolean read FOptShowColor write FOptShowColor;
    property OptShowStyles: boolean read FOptShowStyles write FOptShowStyles;
    property OptShowStylesEx: boolean read FOptShowStylesEx write FOptShowStylesEx;
    property OptShowSizes: boolean read FOptShowSizes write FOptShowSizes;
    property OptShowPreview: boolean read FOptShowPreview write FOptShowPreview;
    property OptShowApply: boolean read FOptShowApply write FOptShowApply;
    property OptSelectName: boolean read FOptSelectName write FOptSelectName;
    property OptSelectStyle: boolean read FOptSelectStyle write FOptSelectStyle;
    property OptSelectSize: boolean read FOptSelectSize write FOptSelectSize;
    property OptSelectColor: boolean read FOptSelectColor write FOptSelectColor;
    property OnClickApply: TNotifyEvent read FOnClickApply write FOnClickApply;
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
  PanelPreviewText.Caption:= 'abcdefghijk ABCDEFGHIJK';

  ListboxFamily.Items.Assign(Screen.Fonts);

  Constraints.MinWidth:= 300;
  Constraints.MinHeight:= 200;

  FPreviewInitialHeight:= PanelPreviewText.Height;

  FOptSizeMin:= 6;
  FOptSizeMax:= 72;
  FOptSizeLimited:= true;
  FOptShowNames:= true;
  FOptShowColor:= true;
  FOptShowStyles:= true;
  FOptShowStylesEx:= true;
  FOptShowSizes:= true;
  FOptShowPreview:= true;
  FOptShowApply:= true;
  FOptSelectName:= true;
  FOptSelectStyle:= true;
  FOptSelectSize:= true;
  FOptSelectColor:= true;

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

procedure TfrmFont.EditSizeKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #13:
      begin
        UpdatePreview;
        Key:= #0;
      end;
    //disble letters
    'a'..'z', 'A'..'Z':
      Key:= #0;
  end;
end;

procedure TfrmFont.ColorboxChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfrmFont.chkCrossedChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfrmFont.chkUnderlineChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfrmFont.FormShow(Sender: TObject);
const
  cScroll = 0;
var
  Style: TFontStyles;
begin
  if FOptSizeLimited then
    OptFont.Size:= Min(FOptSizeMax, Max(FOptSizeMin, OptFont.Size));

  if FOptSelectName then
  begin
    ListboxFamily.ItemIndex:= ListboxFamily.Items.IndexOf(OptFont.Name);
    ListboxFamily.TopIndex:= Max(ListboxFamily.ItemIndex-cScroll, 0);
  end;

  if FOptSelectStyle then
  begin
    Style:= OptFont.Style-[fsUnderline, fsStrikeOut];
    if Style=[fsItalic] then
      ListboxStyle.ItemIndex:= 1
    else
    if Style=[fsBold] then
      ListboxStyle.ItemIndex:= 2
    else
    if Style=[fsBold, fsItalic] then
      ListboxStyle.ItemIndex:= 3
    else
      ListboxStyle.ItemIndex:= 0;

    chkCrossed.Checked:= fsStrikeOut in OptFont.Style;
    chkUnderline.Checked:= fsUnderline in OptFont.Style;
  end;

  if FOptSelectSize then
  begin
    EditSize.Text:= IntToStr(OptFont.Size);
    ListboxSize.ItemIndex:= ListboxSize.Items.IndexOf(EditSize.Text);
    ListboxSize.TopIndex:= Max(ListboxSize.ItemIndex-cScroll, 0);
  end;

  if FOptSelectColor then
  begin
    Colorbox.Selected:= OptFont.Color;
  end;

  UpdateLayout;
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
    UpdatePreview;
  end;
end;

procedure TfrmFont.ListboxStyleClick(Sender: TObject);
begin
  UpdatePreview;
end;

function TfrmFont.GetOptFont: TFont;
begin
  Result:= PanelPreviewText.Font;
end;

function TfrmFont.GetPreviewText: string;
begin
  Result:= PanelPreviewText.Caption;
end;

procedure TfrmFont.SetPreviewText(const AValue: string);
begin
  if GetPreviewText=AValue then Exit;
  PanelPreviewText.Caption:= AValue;
  UpdatePreview;
end;

procedure TfrmFont.UpdatePreview;
var
  N, NNewPanelHeight: integer;
  Style: TFontStyles;
begin
  if ListboxFamily.ItemIndex>=0 then
    OptFont.Name:= ListboxFamily.Items[ListboxFamily.ItemIndex];

  if ListboxStyle.ItemIndex>=0 then
    case ListboxStyle.ItemIndex of
      0: Style:= [];
      1: Style:= [fsItalic];
      2: Style:= [fsBold];
      3: Style:= [fsBold, fsItalic];
    end;

  if chkCrossed.Checked then
    Include(Style, fsStrikeOut);
  if chkUnderline.Checked then
    Include(Style, fsUnderline);

  OptFont.Style:= Style;

  N:= StrToIntDef(EditSize.Text, -1);
  if N>0 then
  begin
    if FOptSizeLimited then
      N:= Max(FOptSizeMin, Min(FOptSizeMax, N));
    OptFont.Size:= N;
  end;

  OptFont.Color:= Colorbox.Selected;

  if FOptShowPreview then
  begin
    PanelPreviewText.Canvas.Font.Assign(OptFont);
    NNewPanelHeight:= Max(30, PanelPreviewText.Canvas.TextHeight(PanelPreviewText.Caption));
    Height:= Height + NNewPanelHeight - PanelPreviewText.Height;
    PanelPreview.Height:= NNewPanelHeight + LabelPreview.Height + 6*3;

    BtnPanel.Top:= Height; // fix relative pos of BtnPanel
  end;
end;


procedure TfrmFont.UpdateLayout;
begin
  PanelSize.Visible:= FOptShowSizes;
  PanelStyle.Visible:= FOptShowStyles;
  PanelFamily.Visible:= FOptShowNames;
  PanelPreview.Visible:= FOptShowPreview;

  PanelEffects.Visible:= FOptShowColor or FOptShowStylesEx;
  chkCrossed.Visible:= FOptShowStylesEx;
  chkUnderline.Visible:= FOptShowStylesEx;
  Colorbox.Visible:= FOptShowColor;

  if not FOptShowApply then
    BtnPanel.ShowButtons:= BtnPanel.ShowButtons-[pbHelp];

  PanelStyle.Align:= alRight;
  PanelSize.Align:= alRight;
  if not PanelFamily.Visible then
  begin
    if PanelStyle.Visible then
      PanelStyle.Align:= alClient
    else
    if PanelSize.Visible then
      PanelSize.Align:= alClient;
  end;
end;

procedure TfrmFont.HelpButtonClick(Sender: TObject);
begin
  if Assigned(FOnClickApply) then
    FOnClickApply(Self);
end;


end.

