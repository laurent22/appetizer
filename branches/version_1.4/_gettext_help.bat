@echo off
xgettext --from-code=utf-8 -C -n -k_ -o Data\Help\appetizer_help.pot generate_help\main.cpp
msgmerge -U Data\Help\de\appetizer_help.po Data\help\appetizer_help.pot
msgmerge -U Data\Help\en\appetizer_help.po Data\help\appetizer_help.pot
msgmerge -U Data\Help\et\appetizer_help.po Data\help\appetizer_help.pot
msgmerge -U Data\Help\fr\appetizer_help.po Data\help\appetizer_help.pot
msgmerge -U Data\Help\ja\appetizer_help.po Data\help\appetizer_help.pot
msgmerge -U Data\Help\zh\appetizer_help.po Data\help\appetizer_help.pot