@echo off
xgettext --from-code=utf-8 -C -n -k_ -o Securizer\Locales\plugin.pot Securizer\*.lua
msgmerge -U Securizer\Locales\en\plugin.po Securizer\Locales\plugin.pot
msgmerge -U Securizer\Locales\fr\plugin.po Securizer\Locales\plugin.pot

