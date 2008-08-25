object EditFolderItemForm: TEditFolderItemForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit folder item'
  ClientHeight = 120
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object nameLabel: TLabel
    Left = 16
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object locationLabel: TLabel
    Left = 16
    Top = 43
    Width = 44
    Height = 13
    Caption = 'Location:'
  end
  object nameEdit: TEdit
    Left = 72
    Top = 13
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object locationEdit: TEdit
    Left = 72
    Top = 40
    Width = 194
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 222
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 141
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 272
    Top = 40
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 16
    Top = 80
  end
end
