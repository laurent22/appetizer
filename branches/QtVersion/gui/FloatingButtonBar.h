/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_FloatingButtonBar_H
#define Appetizer_FloatingButtonBar_H

#include <FloatingButton.h>
#include <GraphicsItem.h>
#include <GraphicsWindow.h>

namespace appetizer {

class FloatingButtonBar : public GraphicsItem {

  Q_OBJECT

public:

  FloatingButtonBar(GraphicsWindow* parentWindow);
  void updateDisplay();
  void applySkin();
  inline int width() const;
  inline int height() const;
  
private:

  std::vector<FloatingButton*> buttons_;

private slots:

  void button_clicked();
	
};

}
#endif // Appetizer_FloatingButtonBar_H
