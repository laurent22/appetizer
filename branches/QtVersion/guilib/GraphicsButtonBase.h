/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_GraphicsButtonBase_H
#define Appetizer_GraphicsButtonBase_H

#include <GraphicsItem.h>
#include <GraphicsWindow.h>

namespace appetizer {

class GraphicsButtonBase : public GraphicsItem {

  Q_OBJECT

public:

  enum ButtonState {
    Up,
    Over,
    Down
  };

  GraphicsButtonBase(GraphicsWindow* parentWindow);
  ButtonState inline state() const { return state_; }
  void setState(ButtonState v);

protected:

  void mousePressEvent(QGraphicsSceneMouseEvent* event);
  void mouseReleaseEvent(QGraphicsSceneMouseEvent* event);
  void hoverEnterEvent(QGraphicsSceneHoverEvent* event); 
  void hoverLeaveEvent(QGraphicsSceneHoverEvent* event);

private:

  ButtonState state_;

};

}
#endif // Appetizer_GraphicsButtonBase_H
