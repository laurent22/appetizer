/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_ScrollBar_H
#define Appetizer_ScrollBar_H

#include <GraphicsItem.h>
#include <NineSliceItem.h>

namespace appetizer {

class ScrollBar : public GraphicsItem {

public:

  ScrollBar();
  void updateDisplay();
  int defaultWidth() const;

protected:

  void resizeEvent();

private:

  NineSliceItem* background_;
  NineSliceItem* knob_;
  int knobHeight();

};

}
#endif // Appetizer_ScrollBar_H
