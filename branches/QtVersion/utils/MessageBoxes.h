/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_MessageBoxes_H
#define Appetizer_MessageBoxes_H

namespace appetizer {

class MessageBoxes {

public:
  
  static int error(const QString& message);
  static int warning(const QString& message);
  static int info(const QString& message);
  static int confirmation(const QString& message);

};

}
#endif // Appetizer_MessageBoxes_H
