/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <StringUtil.h>
using namespace appetizer;


QString StringUtil::convertFields(const QString& s) {
  QString output = "";
  output.reserve(s.length());
  int currentIndex = 0;
  bool replaceNextOne = false;

  for (int i = 0; i < s.length(); i++) {
    const QChar c = s.at(i);

    if (replaceNextOne) {
      QString n;
      n.setNum(currentIndex);
      output += n;
      replaceNextOne = false;
    } else {
      if (c == '%') {
        currentIndex++;
        replaceNextOne = true;
      }
      output += c;
    }
  }

  return output;
}
