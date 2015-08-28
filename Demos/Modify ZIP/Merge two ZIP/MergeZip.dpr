////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : MergeZip
//  * Purpose   : Демонстрация обьединения нескольких архивов
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2015.
//  * Version   : 1.0.11
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  Используемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//  http://www.base2ti.com/
//

// Данный пример показывает как можно обьединить несколько
// ранее созданных архивов в один,
// без необходимости распаковки данных и повторного сжатия.

program MergeZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  FWZipModifier;

var
  Modifier: TFWZipModifier;
  Index1, Index2: TReaderIndex;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try
    // создаем экземпляр модификатора архивов
    Modifier := TFWZipModifier.Create;
    try
      // подключаем первый архив, который был создан в примере SplitZip
      Index1 := Modifier.AddZipFile('..\..\DemoResults\splited_archive1.zip');
      // подключаем второй архив
      Index2 := Modifier.AddZipFile('..\..\DemoResults\splited_archive2.zip');
      // добавляем все элементы из первого архива
      Modifier.AddFromZip(Index1);
      // и из второго
      Modifier.AddFromZip(Index2);
      // теперь создаем новый архив который будет в себя все элементы обоих архивов
      // и технически будет идентичен архиву split_main_archive.zip, который
      // был создан в примере SplitZip (оба архива будут совпадать вплоть до контрольной суммы)
      Modifier.BuildZip('..\..\DemoResults\merged_archive.zip')
    finally
      Modifier.Free;
    end;
    // пример изменения данных, не трогая остальные элементы архива
    // можно увидеть в примере ReplaceZipItemData
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
