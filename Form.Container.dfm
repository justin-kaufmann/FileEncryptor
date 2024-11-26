object FormContainer: TFormContainer
  Left = 0
  Top = 0
  Caption = 'FileEncryptorDecryptor'
  ClientHeight = 128
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu
  Position = poMainFormCenter
  PrintScale = poPrintToFit
  StyleName = 'Windows'
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  TextHeight = 15
  object MainMenu: TMainMenu
    Left = 64
    Top = 88
    object MenuItemProgram: TMenuItem
      Caption = 'Program'
      object SubMenuItemSettings: TMenuItem
        Caption = 'Settings'
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
