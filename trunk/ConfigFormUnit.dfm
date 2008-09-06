object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Config form'
  ClientHeight = 169
  ClientWidth = 297
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
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 281
    Height = 121
    ActivePage = generalTab
    TabOrder = 0
    object generalTab: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object languageLabel: TLabel
        Left = 16
        Top = 19
        Width = 51
        Height = 13
        Caption = 'Language:'
      end
      object iconSizeLabel: TLabel
        Left = 16
        Top = 56
        Width = 46
        Height = 13
        Caption = 'Icon size:'
      end
      object languageComboBox: TComboBox
        Left = 121
        Top = 16
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
      object iconSizeComboBox: TComboBox
        Left = 121
        Top = 56
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
      end
    end
  end
  object cancelButton: TButton
    Left = 214
    Top = 135
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object okButton: TButton
    Left = 133
    Top = 135
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = okButtonClick
  end
end
