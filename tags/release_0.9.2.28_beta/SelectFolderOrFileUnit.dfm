object SelectFolderOrFileForm: TSelectFolderOrFileForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select a folder or file'
  ClientHeight = 272
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DirectoryListBox1: TDirectoryListBox
    Left = 16
    Top = 40
    Width = 209
    Height = 177
    ItemHeight = 16
    TabOrder = 0
    OnChange = DirectoryListBox1Change
  end
  object FileListBox1: TFileListBox
    Left = 231
    Top = 16
    Width = 202
    Height = 201
    ItemHeight = 16
    ShowGlyphs = True
    TabOrder = 1
    OnChange = FileListBox1Change
    OnDblClick = FileListBox1DblClick
  end
  object DriveComboBox1: TDriveComboBox
    Left = 16
    Top = 15
    Width = 209
    Height = 19
    TabOrder = 2
    OnChange = DriveComboBox1Change
  end
  object cancelButton: TButton
    Left = 333
    Top = 232
    Width = 100
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = cancelButtonClick
  end
  object selectFileButton: TButton
    Left = 229
    Top = 232
    Width = 100
    Height = 25
    Caption = 'Select file'
    TabOrder = 4
    OnClick = selectFileButtonClick
  end
  object selectFolderButton: TButton
    Left = 124
    Top = 232
    Width = 100
    Height = 25
    Caption = 'Select folder'
    TabOrder = 5
    OnClick = selectFolderButtonClick
  end
end
