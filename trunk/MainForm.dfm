object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Mini Launch Bar'
  ClientHeight = 64
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object trayIconPopupMenu: TPopupMenu
    Left = 16
    Top = 8
    object trayIconPopupMenuClose: TMenuItem
      Caption = 'Close'
      OnClick = trayIconPopupMenuCloseClick
    end
  end
end
