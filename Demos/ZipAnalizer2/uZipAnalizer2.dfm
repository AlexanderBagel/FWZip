object dlgZipAnalizer: TdlgZipAnalizer
  Left = 301
  Top = 184
  Caption = #1042#1099#1074#1086#1076' '#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074' ZIP '#1072#1088#1093#1080#1074#1072
  ClientHeight = 300
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    685
    300)
  PixelsPerInch = 96
  TextHeight = 13
  object edPath: TLabeledEdit
    Left = 8
    Top = 24
    Width = 550
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 124
    EditLabel.Height = 13
    EditLabel.Caption = #1059#1082#1072#1078#1080#1090#1077' '#1087#1091#1090#1100' '#1082' '#1072#1088#1093#1080#1074#1091':'
    TabOrder = 0
    OnChange = edPathChange
  end
  object btnBrowse: TButton
    Left = 560
    Top = 22
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnAnalize: TButton
    Left = 591
    Top = 22
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1057#1090#1072#1088#1090
    Enabled = False
    TabOrder = 2
    OnClick = btnAnalizeClick
  end
  object GroupBox: TGroupBox
    Left = 8
    Top = 56
    Width = 669
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1072#1088#1093#1080#1074#1072':'
    TabOrder = 3
    object edReport: TRichEdit
      Left = 2
      Top = 15
      Width = 665
      Height = 216
      Align = alClient
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      PopupMenu = PopupMenu
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'zip'
    Filter = 'ZIP '#1072#1088#1093#1080#1074#1099' (*.zip)|*.zip|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 88
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 120
    Top = 88
    object mnuSave: TMenuItem
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100'...'
      ShortCut = 16467
      OnClick = mnuSaveClick
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = #1058#1077#1082#1089#1090#1086#1074#1099#1077' '#1092#1072#1081#1083#1099' (*.txt)|*.txt|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Left = 208
    Top = 88
  end
end
