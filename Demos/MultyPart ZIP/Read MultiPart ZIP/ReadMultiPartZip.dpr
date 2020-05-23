////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : FWZip
//  * Unit Name : ReadMultiPartZip
//  * Purpose   : ƒемонстраци€ чтени€ многотомного архива
//  * Author    : јлександр (Rouse_) Ѕагель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2020.
//  * Version   : 1.1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Stable Release : http://rouse.drkb.ru/components.php#fwzip
//  * Latest Source  : https://github.com/AlexanderBagel/FWZip
//  ****************************************************************************
//
//  »спользуемые источники:
//  ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip
//  http://zlib.net/zlib-1.2.5.tar.gz
//  http://www.base2ti.com/
//

program ReadMultiPartZip;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  TypInfo,
  FWZipStream,
  FWZipReader;

var
  Reader: TFWZipReader;
  MultiStream: TFWFileMultiStream;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  try

    // ƒл€ чтени€ многотомных архивов так-же потребуетс€ класс
    // наследник от TFWAbstractMultiStream.
    // ¬ случае чтени€ используетс€ конструктор CreateRead.
    // ѕервый параметр - путь к заголовку архива.
    // ¬торой параметр, режим открыти€.
    // rsmQuick - ищутс€ все тома с конца не совпадающие размером с
    // первым томом, вплоть до тех пор, пока не найдетс€ равный.
    // ќстальные тома считаютс€ равными первому.
    // Ёто быстрый способ открыти€ многотомного архива, но не совсем надежный,
    // по причине того что в тех случа€х, когда один из томов
    // содержащий записи TCentralDirectoryFileHeader совпадет по размерам
    // с первым томом, все предыдущие будут считатьс€ полными томами
    // содержащими данные, хот€ это может быть не так.
    // ƒл€ гарантированного открыти€ нужно использовать режим rsmFull.
    // ¬ этом случае будет зачитан фактический размер каждого тома архива.
    // Ќо, это более медленный вариант, особенно в том случае, когда
    // реальных томов достаточно много.
    // Ќа количестве томов 60 тыс€ч и более врем€ открыти€ может
    // исчисл€тс€ минутами!
    MultiStream := TFWFileMultiStream.CreateRead(
      '..\..\DemoResults\MultyPartZip\MultyPartZip.zip', rsmQuick);
    try

      Reader := TFWZipReader.Create;
      try

        // „тение многотомных архивов осуществл€етс€ “ќЋ№ ќ через вызов метода
        // LoadFromStream с передачей параметров наследника класса TFWAbstractMultiStream
        Reader.LoadFromStream(MultiStream);

        // ¬с€ остальна€ работа с архивом выгл€дит так-же как и с обычным.
        Reader.PasswordList.Add('password');

        // Ќапример проверка целостности архива
        Reader.Check;

        // ... или его распаковка
        Reader.ExtractAll('*.pas', '..\..\DemoResults\MultyPartZip\');

        Writeln('done.');

      finally
        Reader.Free;
      end;
    finally
      MultiStream.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
