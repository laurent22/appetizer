/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FloatingButtonBar.h>

using namespace appetizer;


FloatingButtonBar::FloatingButtonBar(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  for (int i = 0; i < 3; i++) {
    FloatingButton* b = new FloatingButton(this->parentWindow());
    if (i == 0) b->setAction("eject");
    if (i == 1) b->setAction("minimize");
    if (i == 2) b->setAction("close");
    buttons_.push_back(b);
    addItem(b);
  }
}


int FloatingButtonBar::width() const {
  if (buttons_.size() == 0) return 0;
  return buttons_.size() * buttons_[0]->width();
}


int FloatingButtonBar::height() const {
  if (buttons_.size() == 0) return 0;
  return buttons_[0]->height();
}


void FloatingButtonBar::applySkin() {
  for (int i = (int)buttons_.size() - 1; i >= 0; i--) {
    FloatingButton* b = buttons_.at(i);
    b->applySkin();
  }
}


void FloatingButtonBar::updateDisplay() {
	GraphicsItem::updateDisplay();
	
  int buttonX = 0;
  int buttonY = 0;
  for (int i = (int)buttons_.size() - 1; i >= 0; i--) {
    FloatingButton* b = buttons_.at(i);
    b->move(buttonX, buttonY);
    buttonX += b->height();
  }
}