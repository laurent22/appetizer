object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  Caption = 'Config form'
  ClientHeight = 345
  ClientWidth = 538
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 521
    Height = 329
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object CheckBox1: TCheckBox
        Left = 16
        Top = 16
        Width = 257
        Height = 17
        Caption = 'Auto-detect new applications'
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Quick Launch'
      ImageIndex = 2
      object Label2: TLabel
        Left = 16
        Top = 16
        Width = 123
        Height = 13
        Caption = 'Quick launch applications:'
      end
      object Memo2: TMemo
        Left = 16
        Top = 48
        Width = 480
        Height = 201
        Lines.Strings = (
          'Memo2')
        TabOrder = 0
      end
      object Button1: TButton
        Left = 16
        Top = 263
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
      end
    end
  end
end
