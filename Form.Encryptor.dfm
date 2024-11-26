object FormEncryptor: TFormEncryptor
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'FileEncryptorDecryptor'
  ClientHeight = 147
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu
  Visible = True
  StyleElements = [seFont, seClient]
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    331
    147)
  TextHeight = 13
  object LabelPassword1: TLabel
    Left = 8
    Top = 23
    Width = 50
    Height = 13
    Anchors = [akLeft]
    AutoSize = False
    Caption = 'Password:'
  end
  object LabelPassword2: TLabel
    Left = 8
    Top = 50
    Width = 56
    Height = 13
    Anchors = [akLeft]
    Caption = 'Password2:'
    ExplicitTop = 37
  end
  object btnDecrypt: TButton
    Left = 0
    Top = 97
    Width = 331
    Height = 25
    Align = alBottom
    Caption = 'Decrypt'
    TabOrder = 0
    OnClick = btnDecryptClick
  end
  object btnEncrypt: TButton
    Left = 0
    Top = 122
    Width = 331
    Height = 25
    Align = alBottom
    Caption = 'Encrypt'
    TabOrder = 1
    OnClick = btnEncryptClick
  end
  object EditPassword1: TEdit
    Left = 120
    Top = 20
    Width = 203
    Height = 21
    Anchors = [akLeft, akRight]
    PasswordChar = '*'
    TabOrder = 2
    Text = 'YourPassword'
  end
  object EditPassword2: TEdit
    Left = 120
    Top = 47
    Width = 203
    Height = 21
    Anchors = [akLeft, akRight]
    PasswordChar = '*'
    TabOrder = 3
    Text = 'YourPassword'
  end
  object MainMenu: TMainMenu
    Left = 80
    Top = 56
    object MenuItemProgram: TMenuItem
      Caption = 'Program'
      object SubMenuItemSettings: TMenuItem
        Caption = 'Settings'
        Visible = False
      end
      object SubMenuItemExit: TMenuItem
        Caption = 'Exit'
        OnClick = SubMenuItemExitClick
      end
    end
    object MenuItemHelp: TMenuItem
      Caption = 'Help'
      object MenuItemAbout: TMenuItem
        Caption = 'About'
        OnClick = MenuItemAboutClick
      end
    end
  end
end
