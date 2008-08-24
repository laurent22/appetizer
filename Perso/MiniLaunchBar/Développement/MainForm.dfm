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
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object iconPopupMenu: TPopupMenu
    Left = 8
    Top = 8
    object cddd1: TMenuItem
      Caption = 'Remove'
    end
    object N1: TMenuItem
      Caption = '-'
      Enabled = False
    end
    object Properties1: TMenuItem
      Caption = 'Properties'
    end
  end
  object XMLDocument1: TXMLDocument
    Active = True
    Left = 96
    Top = 16
    DOMVendorDesc = 'MSXML'
  end
end
