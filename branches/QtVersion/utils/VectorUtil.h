/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_VectorUtil_H
#define Appetizer_VectorUtil_H

namespace appetizer {

class VectorUtil {

public:

  template <class T>
  static int getElementIndex(std::vector<T> v, T element) {
    for (int i = 0; i < (int)v.size(); i++) {
      if (v[i] == element) return i; 
    }
    return -1;
  }

};

}
#endif // Appetizer_VectorUtil_H
