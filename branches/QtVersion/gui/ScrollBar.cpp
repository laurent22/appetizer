/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FilePaths.h>
#include <ScrollBar.h>

using namespace appetizer;


ScrollBar::ScrollBar() {
  background_ = new NineSliceItem();
  background_->loadBackgroundImage(FilePaths::GetSkinFile("ScrollBarBackground.png"));
  addItem(background_);

  knob_ = new NineSliceItem();
  knob_->loadBackgroundImage(FilePaths::GetSkinFile("ScrollBarKnob.png"));
  addItem(knob_);
}


int ScrollBar::defaultWidth() const {
  QImage* image = background_->nineSlicePainter().image();
  if (!image || image->isNull()) return 20;
  return image->width();
}


int ScrollBar::knobHeight() {
  return 50;
}


void ScrollBar::resizeEvent() {
  GraphicsItem::resizeEvent();

  invalidate();
}


void ScrollBar::updateDisplay() {
  GraphicsItem::updateDisplay();

  background_->setWidth(width());
  background_->setHeight(height());

  knob_->setWidth(width());
  knob_->setHeight(knobHeight());
}