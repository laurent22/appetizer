object frMain: TfrMain
  Left = 219
  Top = 178
  Caption = 'Searching for files'
  ClientHeight = 173
  ClientWidth = 192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 12
    Width = 52
    Height = 13
    Caption = 'In directory'
  end
  object Label2: TLabel
    Left = 4
    Top = 40
    Width = 44
    Height = 13
    Caption = 'File mask'
  end
  object lblNumberFound: TLabel
    Left = 4
    Top = 156
    Width = 60
    Height = 13
    Caption = 'Files found...'
  end
  object CheckBox1: TCheckBox
    Left = 4
    Top = 72
    Width = 113
    Height = 17
    Caption = 'Include subfolders'
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 4
    Top = 96
    Width = 181
    Height = 53
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = ListBox1DblClick
  end
  object Edit1: TEdit
    Left = 64
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'c:\'
  end
  object Button1: TButton
    Left = 120
    Top = 60
    Width = 65
    Height = 33
    Caption = '&Search'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 64
    Top = 36
    Width = 49
    Height = 21
    TabOrder = 4
    Text = '*.*'
  end
end
