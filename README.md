# **_File System_**

## **Как се използва**
Определен набор от инструкции се използват за да се навигира във симулираната файлова система. Реализирани са и команди за добавяне,премахване на папки и файлове, както и преглеждане и променяне на съдържанието на файлове.

## **FileSystem**

    data FileSystem x = Folder x [(x,x)] [FileSystem x]

Структурата `FileSystem` е дърво с произволен брой върхове като всеки връх преставлява папка като:
1. `x` - името на папката 
2. `[(x,x)]` - лист от tuples на съдържащите се файловете
3. `[FileSystem x]` - лист от `FileSystem`
    
## **get - ъри**

1. `getFiles` - връща лист от tuples като първия елемент е името а втория съдържанието
2. `getFilesNames` - връща лист от имената на файловете в дадена директория
3. `getName` - връща името на дадена директория
4. `getSubFoldersList` - връща лист от папките в дадена директория
5. `getSubFoldersNames` - връща лист от имената на папките в дадена директория
6. `getContent` - връща съдържанието (тоест лист имената на файловете и папките) в дадена директория

## **remove files**
1. `rmFromTree` - Връща се папка в която са премахнати изредените в line файлове и папки

## **Build tree**
1. `compareFold` и `compareFolders` - се използват за да сравняват папки по всички елемнти в тях и в случай на пълна идентичност се водят за еквивалентни.
2. `buildFileSys` и `buildDirectory` - използва се за да се построи дърво като се подава директория и тя се замества с идентичната и

## **make folder**
1. `mkdir` - връща се папка с подаденото име 

## **helpers**
1. `findFolder` - подава се лист с имена на папки като се преминава през него и се търси къде се намира тази папка
2. `splitToList` - според подаден параметър разделя стринг в лист от отделните имена на папки или файлове
3. `cd` - използва се при навигиране през папките
4. `ls` - използва се за да видим съдържанието на дадена папка
5. `extendedls` - подобно на ls но се използва при подаване на лист от папки в който взимаме последната директория и проверяваме какво всъщност се съдържа там
6. `pathDo` - взимаме стринг от директории и ги съединяваме като ползваме intersperse (от Data.List) добавяйки "/" между тях и ги конкатенираме в стринг

## **looper**
Тук въвеждаме line от конзолата и проверяваме чрез if else клаузи стойноста проверяваме какво иска потребителя да изпълни. Това което можем да въведем от конзолата е:
1. `exit` - затваряме пограмата
2. `ls` - прегреждаме съдържанието на една папка или нейните subFolders
3. `cd` - навигираме във файловата система
4. `rm` - премахване на файл
5. `dirname` - получаваме името на текущата директория
6. `pwd` - получаваме пълния път от началната директория до крайната
7. `mkdir` - създаване на папка чрез `buildDirectory`
8.  `cat > *името на файла*` - въвеждаме името на файла след което пишем текста който искаме добавим до срещане на нов ред
9. `cat` - получаваме съдържанието на файла
10.  `touch` - въвеждаме името на файла който искаме да създадем

## **main**
Съдържа looper функцията

## **Обобщение на реализацията**
Реализацията покрива почти всички базови изисквания, остава да се реализират функциията за конкатенация на файлове
