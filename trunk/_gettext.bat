@echo off
xgettext --from-code=utf-8 -C -n -k_ -o Data\Locales\appetizer.pot *.cpp
xgettext --from-code=utf-8 -C -n -k_ -j -o Data\Locales\appetizer.pot utilities\*.cpp
xgettext --from-code=utf-8 -C -n -k_ -j -o Data\Locales\appetizer.pot gui\*.cpp
msgmerge -U Data\Locales\da\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\de\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\el\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\en\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\es\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\et\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\fr\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\hu\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\it\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\ja\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\ko\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\nl\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\pl\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\pt_BR\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\ro\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\sv\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\tr\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\zh\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\pt\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\ru\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\sk\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\vi\appetizer.po Data\Locales\appetizer.pot
msgmerge -U Data\Locales\zh_CN\appetizer.po Data\Locales\appetizer.pot
