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

  Q_OBJECT

public:

  ScrollBar();
  void updateDisplay();
  int defaultWidth() const;
  inline float value() const { return value_; }
  void setValue(float value);
  bool scrollable();
  int contentHeight();
  void setContentHeight(int v);

protected:

  void resizeEvent();

protected slots:

  void knob_mousePressed();
  void knob_mouseMoved();
  void knob_mouseReleased();

signals:

  void valueChanged();

private:

  struct DragData {
    QPoint startMouse;
    QPoint startPosition;
  };

  NineSliceItem* background_;
  NineSliceItem* knob_;
  int contentHeight_;
  int knobHeight();
  int maxKnobY();
  int knobY();
  float value_;
  DragData* dragData_;

};

}
#endif // Appetizer_ScrollBar_H
