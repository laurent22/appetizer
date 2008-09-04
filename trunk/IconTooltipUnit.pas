unit IconTooltipUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WComponent, WNineSlicesPanel, Logger;

type
  TIconTooltipForm = class(TForm)
    procedure FormCreate(Sender: TObject);

  private

  	pLabel: TLabel;
  	pGap: Integer;
    pBackgroundPanel: TWNineSlicesPanel;

    procedure LazyInitialization();

  public

    procedure ShowAbove(const component:TWComponent; const text:String);
    property Gap: Integer read pGap write pGap;

  end;


var
  IconTooltipForm: TIconTooltipForm;

implementation

{$R *.dfm}


uses Main;


procedure TIconTooltipForm.FormCreate(Sender: TObject);
begin
	pGap := 4;
end;


procedure TIconTooltipForm.LazyInitialization;
var labelFont: TFont;
begin
	if pBackgroundPanel = nil then begin
  	pBackgroundPanel := TWNineSlicesPanel.Create(Self);
    pBackgroundPanel.ImagePath := TMain.Instance.FilePaths.SkinDirectory + '\TooltipBackground.png';
    pBackgroundPanel.Visible := true;
    pBackgroundPanel.Parent := Self;
  end;

  if pLabel = nil then begin
		pLabel := TLabel.Create(Self);
    pLabel.Parent := Self;
    pLabel.AutoSize := true;
    pLabel.Visible := true;

    labelFont := TFont.Create();
    labelFont.Style := TMain.Instance.Style.IconTooltip.FontStyles;
    labelFont.Color := TMain.Instance.Style.IconTooltip.FontColor;

    pLabel.Font := labelFont;
  end;
end;


procedure TIconTooltipForm.ShowAbove(const component: TWComponent; const text: String);
var point: TPoint;
begin
	if component.Parent = nil then Exit;

  LazyInitialization();

	pLabel.Caption := text;

  Width := pLabel.Width + TMain.Instance.Style.IconTooltip.paddingH;
  Height := pLabel.Height + TMain.Instance.Style.IconTooltip.paddingV;
  pBackgroundPanel.Width := Width;
  pBackgroundPanel.Height := Height;
  pLabel.Left := TMain.Instance.Style.IconTooltip.paddingLeft;
  pLabel.Top := TMain.Instance.Style.IconTooltip.paddingTop;

  Left := Round(component.ScreenLeft + component.Width / 2 - Width / 2);
  Top := component.ScreenTop - Height - pGap;

  Repaint();

  Show();
end;

end.
