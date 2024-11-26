object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FormAbout'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  Visible = True
  TextHeight = 15
  object ButtonClose: TButton
    Left = 0
    Top = 417
    Width = 628
    Height = 25
    Align = alBottom
    Caption = 'Close'
    TabOrder = 0
    OnClick = ButtonCloseClick
    ExplicitTop = 416
    ExplicitWidth = 624
  end
  object MemoAbout: TMemo
    Left = 0
    Top = 0
    Width = 628
    Height = 417
    Align = alClient
    Lines.Strings = (
      'Mein Name ist Justin, 25 Jahre jung.'
      ''
      'Ich habe eine Ausbildung zum Softwareentwickler absolviert.'
      
        'Derzeit bin ich mittellos und programmiere in meiner Freizeit se' +
        'hr gerne.'
      ''
      'Bei Fragen oder Feedback stehe ich unter folgenden '
      'Kontaktm'#246'glichkeiten zur Verf'#252'gung.'
      ''
      'Kontakt:'
      '  E-Mail: Justin.Kaufmann@outlook.de')
    TabOrder = 1
    ExplicitWidth = 624
    ExplicitHeight = 416
  end
end
