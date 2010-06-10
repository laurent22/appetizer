/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <FilePaths.h>
#include <ScrollBar.h>

using namespace appetizer;


ScrollBar::ScrollBar(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  dragData_ = NULL;
  value_ = 0;
  contentHeight_ = 0;

  background_ = new NineSliceItem(this->parentWindow());
  background_->loadBackgroundImage(FilePaths::GetSkinFile("ScrollBarBackground.png"));
  addItem(background_);

  knob_ = new NineSliceItem(this->parentWindow());
  knob_->loadBackgroundImage(FilePaths::GetSkinFile("ScrollBarKnob.png"));
  addItem(knob_);

  QObject::connect(knob_, SIGNAL(mousePressed()), this, SLOT(knob_mousePressed()));
  QObject::connect(knob_, SIGNAL(mouseMoved()), this, SLOT(knob_mouseMoved()));
  QObject::connect(knob_, SIGNAL(mouseReleased()), this, SLOT(knob_mouseReleased()));
}


void ScrollBar::knob_mousePressed() {
  dragData_ = new DragData();
  dragData_->startMouse = QCursor::pos();
  dragData_->startPosition = QPoint(knob_->x(), knob_->y());
}


void ScrollBar::knob_mouseMoved() {
  QPoint p = QCursor::pos();
  int dy = p.y() - dragData_->startMouse.y();
  int newY = dragData_->startPosition.y() + dy;

  if (newY < 0) newY = 0;
  if (newY > maxKnobY()) newY = maxKnobY();

  setValue((float)newY / (float)maxKnobY());
}


void ScrollBar::knob_mouseReleased() {
  SAFE_DELETE(dragData_);
  invalidate();
}


void ScrollBar::setValue(float value) {
  if (value == value_) return;
  if (value < 0) value = 0;
  if (value > 1) value = 1;
  if (value == value_) return;

  value_ = value;
  emit valueChanged();
  invalidate();
}


int ScrollBar::defaultWidth() const {
  QImage* image = background_->nineSlicePainter().image();
  if (!image || image->isNull()) return 20;
  return image->width();
}


int ScrollBar::knobHeight() {
  if (!scrollable()) return 0;
  int output = (int)(((float)height() / (float)contentHeight_) * (float)height());
  if (output < 20) output = 20;
	return output;
}


int ScrollBar::maxKnobY() {
  return height() - knobHeight();
}


int ScrollBar::knobY() {
  return value() * (height() - knobHeight());	
}


bool ScrollBar::scrollable() {
  return height() < contentHeight();
}


int ScrollBar::contentHeight() {
  return contentHeight_;
}


void ScrollBar::setContentHeight(int v) {
  if (contentHeight_ == v) return;
  contentHeight_ = v;
  invalidate();
}


void ScrollBar::resizeEvent() {
  GraphicsItem::resizeEvent();

  invalidate();
}


void ScrollBar::updateDisplay() {
  GraphicsItem::updateDisplay();

  background_->setWidth(width());
  background_->setHeight(height());

  knob_->setVisible(scrollable());

  if (scrollable()) {
    knob_->setWidth(width());
    knob_->setHeight(knobHeight());
    knob_->setY(knobY());
  }
}